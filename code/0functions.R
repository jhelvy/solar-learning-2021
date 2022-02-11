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



