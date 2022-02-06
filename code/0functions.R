# NOTES
#
# Basic learning curve model: Y_x = A*x^b
# where
#   Y = the cost of unit x (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   b = a constant representing the slope (slope = 2^b)
#
# Log transformation: ln(Y) = ln(A) + b*ln(x)
#           Re-write: Y'    = int   + b*x'
#
# To convert log-space estimated coefficients back to original model:
# A = exp(int)
# b = b
# Learning curve slope = 2^b
# Learning Rate        = 1 - slope
#
# Approximation of the cumulative total cost of producing N units:
# C = (A*N^(b + 1)) / (b + 1)
#
# Two factor learning curve model: Y = A * x^b * p^c
# where
#   Y = the cost of unit x at silicon price p (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   p = silicon price
#   b = lnCap_estimate = learning coefficient on capacity
#   c = lnSi_estimate = coefficient on silicon price

# Data formatting ----

getFutureCapRate <- function(target_capacity, cap_begin, num_years) {
  # Percent annual growth in cumulative capacity
  return(((target_capacity / cap_begin)^(1 / (num_years))) - 1)
}

getFutureCapacities <- function(
    rate, cap_begin, num_years, year_min_proj, price_si
) {
  cumCapacityKw <- rep(cap_begin, num_years + 1)
  for (i in 2:(num_years + 1)) {
      cumCapacityKw[i] <- cumCapacityKw[i-1]*(1 + rate)
  }
  # Add capacity to reach target
  result <- data.frame(
      year = year_min_proj - 1 + seq(num_years + 1),
      cumCapacityKw = cumCapacityKw) %>%
      mutate(price_si = price_si)
  return(result)
}

formatCapData <- function(data_nation, data_world, year_beg, year_max) {
    cap_data_nation <- getAnnCapData(data_nation, year_beg)
    cap_data_world <- getAnnCapData(data_world, year_beg)
    result <- cap_data_nation %>%
        select(year, annCapKw_nation = annCapKw) %>%
        left_join(
            cap_data_world %>%
                select(
                    year, cumCapKw_world = cumCapKw, annCapKw_world = annCapKw
                ),
            by = "year"
        ) %>%
        mutate(
            annCapKw_other = annCapKw_world - annCapKw_nation,
            cumCapKw_other = cumsum(annCapKw_other)
        ) %>%
        filter(year <= year_max) %>%
        select(year, cumCapKw_world, cumCapKw_other) %>% 
        # Add price data
        left_join(select(data_nation, year, costPerKw), by = "year") %>% 
        left_join(select(data_world, year, price_si), by = "year")
    return(result)
}

getAnnCapData <- function(df, year_beg) {
    result <- df %>%
        filter(year >= year_beg) %>%
        select(year, cumCapKw = cumCapacityKw) %>%
        mutate(
            annCapKw = cumCapKw - lag(cumCapKw, 1),
            annCapKw = ifelse(is.na(annCapKw), 0, annCapKw))
    return(result)
}

make_stan_data <- function(df) {
    return(list(
        N    = nrow(df),
        qw   = df$cumCapKw_world,
        qj   = df$cumCapKw_other,
        p    = df$price_si,
        logc = log(df$costPerKw)
    ))
}

# Scenario analyses ----

get_csim_draws <- function(params, data, year_beg = NULL) {
    nobs <- data$N
    if (is.null(year_beg)) {
        year_beg <-min(data$year)
    }
    # Extract par vectors
    alpha <- params$alpha
    beta <- params$beta
    gamma <- params$gamma
    lambda <- params$lambda
    # Compute c_sim 
    c_sim <- list()
    for (i in 1:nobs) {
        q <- data$qw[i] - (lambda * data$qj[i])
        c_sim[[i]] <- alpha + beta * log(q) + gamma * log(data$p[i])
    }
    return(c_sim)
}

get_csim_draws_national <- function(
    params, 
    data, 
    year_beg = NULL, 
    delay_years = 10, 
    lambda_end = 0.9
) {
    nobs <- data$N
    if (is.null(year_beg)) {
        year_beg <-min(data$year)
    } 
    # Set lambda vector
    lambda_start <- mean(params$lambda)
    lambda <- seq(lambda_start, lambda_end, length.out = delay_years + 1)
    lambda <- c(lambda, rep(lambda_end, nobs - length(lambda)))
    # Extract par vectors
    alpha <- params$alpha
    beta <- params$beta
    gamma <- params$gamma
    # Compute c_sim 
    c_sim <- list()
    for (i in 1:nobs) {
        q <- data$qw[i] - (lambda[i] * data$qj[i])
        c_sim[[i]] <- alpha + beta * log(q) + gamma * log(data$p[i])
    }
    return(c_sim)
}

predict_cost <- function(
    params, 
    data, 
    year_beg = NULL, 
    ci = 0.95, 
    delay_years = NULL, 
    lambda_end = NULL
) {
    nobs <- data$N
    if (is.null(delay_years)) {
        c_sim_draws <- get_csim_draws(params, data, year_beg)
    } else {
        c_sim_draws <- get_csim_draws_national(
            params, data, year_beg, delay_years, lambda_end)
    }
    c_sim <- matrix(0, ncol = 3, nrow = nobs)
    for (i in 1:nobs) {
        c_sim[i,] <- as.matrix(get_ci(c_sim_draws[[i]], ci))
    }
    c_sim <- as.data.frame(exp(c_sim))
    names(c_sim) <- c("cost_per_kw", "cost_per_kw_lb", "cost_per_kw_ub")
    c_sim <- cbind(c_sim, cost_per_kw_true = exp(data$logc)) %>% 
        mutate(year = seq(year_beg, year_beg + nobs - 1))
    return(c_sim)
}

compute_cost_diff <- function(
    params, 
    data, 
    year_beg = NULL, 
    ci = 0.95,
    delay_years = NULL, 
    lambda_end = NULL
) {
    nobs <- data$N
    c_sim_draws_global <- get_csim_draws(params, data, year_beg)
    c_sim_draws_national <- get_csim_draws_national(
            params, data, year_beg, delay_years, lambda_end)
    cost_diff <- matrix(0, ncol = 3, nrow = nobs)
    alpha_ci <- (1 - ci) / 2
    probs <- c(alpha_ci, 1 - alpha_ci)
    for (i in 1:nobs) {
        diff <- exp(c_sim_draws_national[[i]]) - exp(c_sim_draws_global[[i]])
        cost_diff[i,] <- as.matrix(get_ci(diff, ci))
    }
    cost_diff <- as.data.frame(cost_diff)
    names(cost_diff) <- c("cost_per_kw", "cost_per_kw_lb", "cost_per_kw_ub")
    return(cost_diff)
}









makeGlobalCapData <- function(
    data_nation, data_world, year_beg = NULL, lambda = 0.1
) {
    if (is.null(year_beg)) { year_beg = min(data$year) }
    # Setup data
    data_nation <- prepNationData(data_nation, year_beg)
    data_world <- prepWorldData(data_world, year_beg)
    cap_beg_world <- data_world[1,]$cumCapKw_world
    # Compute global cumulative capacity additions for each year
    result <- computeCapData(data_nation, data_world, cap_beg_world, lambda)
    return(result)
}

makeNationalCapData <- function(
    data_nation, data_world, year_beg = NULL, delay_years = 10, 
    lambda_start = 0.1, lambda_end = 0.9
) {
    if (is.null(year_beg)) { year_beg = min(data$year) }
    # Setup data
    data_nation <- prepNationData(data_nation, year_beg)
    data_world <- prepWorldData(data_world, year_beg)
    cap_beg_world <- data_world[1,]$cumCapKw_world
    # Compute national cumulative capacity additions starting with global 
    # capacity and gradually shifting down to only national capacity after 
    # delay_years years
    # First set up the lambda to phase out global cap
    lambda <- seq(lambda_start, lambda_end, length.out = delay_years + 1)
    lambda <- c(lambda, rep(lambda_end, nrow(data_nation) - length(lambda)))
    result <- computeCapData(data_nation, data_world, cap_beg_world, lambda)
    return(result)
}

prepNationData <- function(df, year_beg) {
    result <- df %>%
        filter(year >= year_beg) %>%
        select(year, cumCapKw_nation = cumCapacityKw) %>%
        mutate(
            annCapKw_nation = cumCapKw_nation - lag(cumCapKw_nation, 1),
            annCapKw_nation = ifelse(
                is.na(annCapKw_nation), 0, annCapKw_nation))
    return(result)
}

prepWorldData <- function(df, year_beg) {
    result <- df %>%
        filter(year >= year_beg) %>%
        select(year, cumCapKw_world = cumCapacityKw, price_si) %>%
        mutate(
            annCapKw_world = cumCapKw_world - lag(cumCapKw_world, 1),
            annCapKw_world = ifelse(is.na(annCapKw_world), 0, annCapKw_world))
    return(result)
}

computeCapData <- function(data_nation, data_world, cap_beg_world, lambda) {
    result <- data_nation %>%
        left_join(data_world, by = "year") %>%
        mutate(
            lambda = lambda,
            annCapKw_other = (1 - lambda)*(annCapKw_world - annCapKw_nation),
            annCapKw_new = annCapKw_other + annCapKw_nation,
            cumCapKw_new = cumsum(annCapKw_new),
            cumCapacityKw = cap_beg_world + cumCapKw_new) %>%
        select(year, cumCapacityKw, annCapKw_new, price_si)
    return(result)
}

computeSavings <- function(cost_national, cost_global, cap_additions) {
    result <- cost_national %>% 
        left_join(cost_global, by = c("year", "country")) %>% 
        left_join(cap_additions, by = c("year", "country")) %>% 
        mutate(
            ann_savings_bil = annCapKw_new*(national - global) / 10^9) %>%
        select(year, country, ann_savings_bil)
    return(result)
}

getStartingCapcity <- function(df, year_min_proj) {
  return(df[which(df$year == year_min_proj),]$cumCapacityKw)
}

getStartingCost <- function(df, year_min_proj) {
  return(df[which(df$year == year_min_proj),]$costPerKw)
}


# General utility ----

get_ci <- function(x, ci = 0.95) {
  alpha <- (1 - ci)/2
  df <- data.frame(
    mean  = mean(x),
    lower = stats::quantile(x, alpha),
    upper = stats::quantile(x, 1 - alpha)
  )
  return(df)
}




# Stan utility ----

# Check transitions that ended with a divergence
check_div <- function(fit, quiet=FALSE) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)

  if (!quiet) print(sprintf('%s of %s iterations ended with a divergence (%s%%)',
                    n, N, 100 * n / N))
  if (n > 0) {
    if (!quiet) print('  Try running with larger adapt_delta to remove the divergences')
    if (quiet) return(FALSE)
  } else {
    if (quiet) return(TRUE)
  }
}

# Check transitions that ended prematurely due to maximum tree depth limit
check_treedepth <- function(fit, max_depth = 10, quiet=FALSE) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)

  if (!quiet)
    print(sprintf('%s of %s iterations saturated the maximum tree depth (%s%%)',
                            n, N, 100 * n / N))

  if (n > 0) {
    if (!quiet) print('  Run again with max_treedepth set to a larger value to avoid saturation')
    if (quiet) return(FALSE)
  } else {
    if (quiet) return(TRUE)
  }
}

# Checks the energy fraction of missing information (E-FMI)
check_energy <- function(fit, quiet=FALSE) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      if (!quiet) print(sprintf('Chain %s: E-FMI = %s', n, numer / denom))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) print('E-FMI indicated no pathological behavior')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) print('  E-FMI below 0.2 indicates you may need to reparameterize your model')
    if (quiet) return(FALSE)
  }
}

# Checks the effective sample size per iteration
check_n_eff <- function(fit, quiet=FALSE) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]

  iter <- dim(extract(fit)[[1]])[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    ratio <- fit_summary[,5][n] / iter
    if (ratio < 0.001) {
      if (!quiet) print(sprintf('n_eff / iter for parameter %s is %s!',
                        rownames(fit_summary)[n], ratio))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) print('n_eff / iter looks reasonable for all parameters')
    if (quiet) return(TRUE)
  }
  else {
    if (!quiet) print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
    if (quiet) return(FALSE)
  }
}

# Checks the potential scale reduction factors
check_rhat <- function(fit, quiet=FALSE) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      if (!quiet) print(sprintf('Rhat for parameter %s is %s!',
                        rownames(fit_summary)[n], rhat))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) print('Rhat looks reasonable for all parameters')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) print('  Rhat above 1.1 indicates that the chains very likely have not mixed')
    if (quiet) return(FALSE)
  }
}

check_all_diagnostics <- function(fit, quiet=FALSE) {
  if (!quiet) {
    check_n_eff(fit)
    check_rhat(fit)
    check_div(fit)
    check_treedepth(fit)
    check_energy(fit)
  } else {
    warning_code <- 0

    if (!check_n_eff(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 0))
    if (!check_rhat(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 1))
    if (!check_div(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 2))
    if (!check_treedepth(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 3))
    if (!check_energy(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 4))

    return(warning_code)
  }
}

parse_warning_code <- function(warning_code) {
  if (bitwAnd(warning_code, bitwShiftL(1, 0)))
    print("n_eff / iteration warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 1)))
    print("rhat warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 2)))
    print("divergence warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 3)))
    print("treedepth warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 4)))
    print("energy warning")
}

# Returns parameter arrays separated into divergent and non-divergent transitions
partition_div <- function(fit) {
  nom_params <- extract(fit, permuted=FALSE)
  n_chains <- dim(nom_params)[2]
  params <- as.data.frame(do.call(rbind, lapply(1:n_chains, function(n) nom_params[,n,])))

  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  params$divergent <- divergent

  div_params <- params[params$divergent == 1,]
  nondiv_params <- params[params$divergent == 0,]

  return(list(div_params, nondiv_params))
}

