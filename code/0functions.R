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
      addAnnCap() %>% 
      mutate(price_si = price_si)
  return(result)
}

addAnnCap <- function(df) {
    result <- df %>%
        mutate(
            annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1),
            annCapacityKw = ifelse(is.na(annCapacityKw), 0, annCapacityKw))
    return(result)
}

formatCapData <- function(data_nation, data_world, year_beg, year_max) {
    result <- data_nation %>%
        select(year, annCapKw_nation = annCapacityKw) %>%
        left_join(
            data_world %>%
                select(
                    year, cumCapKw_world = cumCapacityKw, 
                    annCapKw_world = annCapacityKw
                ),
            by = "year"
        ) %>%
        filter(year >= year_beg, year <= year_max) %>% 
        mutate(annCapKw_other = annCapKw_world - annCapKw_nation) %>% 
        # Add price data
        left_join(select(data_world, year, price_si), by = "year")
    return(result)
}

# Modeling ----


# load custom functions
run_model <- function(df, lambda) {
    # Run the linear model for a given lambda
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_p, data = temp))
}

predict_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    q0 <- df$cumCapKw_world[1]
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(
        y_sim,
        cumCapKw = temp$q,
        costPerKw = df$costPerKw,
        year = df$year
    )
    return(y_sim)
}

project_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    q0 <- df$cumCapKw_world[1]
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(y_sim, year = df$year, cumCapKw = temp$q)
    return(y_sim)
}

make_historical_plot <- function(cost_national, cost_global) {
    plot <- rbind(
        cost_global %>% mutate(scenario = "global"),
        cost_national %>% mutate(scenario = "national")) %>%
        mutate(scenario = fct_relevel(scenario, c("national", "global"))) %>%
        ggplot() +
        geom_line(
            mapping = aes(x = year, y = mean, color = scenario),
        ) +
        geom_ribbon(
            mapping = aes(
                x = year, ymin = lower, ymax = upper, fill = scenario),
            alpha = 0.2
        ) +
        geom_point(aes(x = year, y = costPerKw)) +
        scale_x_continuous(breaks = cost_global$year) +
        scale_y_log10() +
        theme_bw() +
        labs(
            title = "Estimated Module Cost Under Global vs. National Markets",
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW)"
        )
    return(plot)
}

make_projection_plot <- function(
    proj_national_nt, proj_global_nt, proj_national_sd, proj_global_sd
) {
    plot <- rbind(
        proj_global_nt %>%
            mutate(learning = "global", scenario = "National Trends"),
        proj_national_nt %>%
            mutate(learning = "national", scenario = "National Trends"),
        proj_global_sd %>%
            mutate(learning = "global", scenario = "Sustainable Development"),
        proj_national_sd %>%
            mutate(learning = "national", scenario = "Sustainable Development")
        ) %>%
        mutate(
            learning = fct_relevel(learning, c("national", "global"))
        ) %>%
        ggplot() +
        geom_line(
            mapping = aes(x = year, y = mean, color = learning),
        ) +
        geom_ribbon(
            mapping = aes(
                x = year, ymin = lower, ymax = upper, fill = learning),
            alpha = 0.2
        ) +
        scale_x_continuous(breaks = proj_global_nt$year) +
        facet_wrap(vars(scenario), nrow = 1) +
        theme_bw() +
        labs(
            title = "Estimated Module Cost Under Global vs. National Markets",
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW, $USD)"
        )
    return(plot)
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
    if (is.null(year_beg)) {
        year_beg <-min(data$year)
    } 
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
    cost_diff$year <- year_beg + seq(0, nrow(cost_diff) - 1)
    return(cost_diff)
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



