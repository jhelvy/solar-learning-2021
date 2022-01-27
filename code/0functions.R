
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

run_model <- function(data) {
    model <- lm(
        formula = log(costPerKw) ~ log(cumCapacityKw) + log(price_si),
        data = data)
    return(model)
}

ci <- function(data, alpha = 0.025) {
  B <- mean(data, na.rm = T)
  L <- stats::quantile(data, alpha, na.rm = T)
  U <- stats::quantile(data, 1 - alpha, na.rm = T)
  ests <- c(B, L, U)
  names(ests) <- c("mean", "low", "high")
  return(ests)
}

find_lambda <- function(
  data_nation, data_world, year_beg, lr_model, beg
) {
  lambda <- seq(0, 1, 0.01)
  error_us <- list()
  for (i in 1:length(lambda)) {
    data_global <- makeGlobalCapData(
      data_nation = data_nation,
      data_world  = data_world,
      year_beg    = year_beg,
      lambda      = lambda[i])
    cost_global <- predict_cost(
      model    = lr_model,
      data     = data_global,
      cost_beg = beg$costPerKw,
      cap_beg  = beg$cumCapacityKw,
      si_beg   = beg$price_si,
      year_beg = year_beg,
      ci       = 0.95)
    error_us[[i]] <- cost_global %>% 
      select(year, cost_per_kw) %>% 
      left_join(
        data_nation %>% 
          select(year, cost_per_kw_true = costPerKw), 
        by = "year"
      ) %>% 
      mutate(err_sq = log(abs(cost_per_kw - cost_per_kw_true))^2) %>% 
      filter(err_sq != Inf)
  }
  result <- data.frame(
    sse = unlist(lapply(error_us, function(x) sum(x$err_sq))), 
    lambda = lambda) %>% 
    arrange(sse)
  return(result)
}

predict_cost <- function(
  model, 
  data,
  cost_beg,
  cap_beg,
  si_beg,
  year_beg = NULL,
  ci = 0.95) {
    if (is.null(year_beg)) {
        year_beg <-min(data$year)
    }
    data <- filter(data, year >= year_beg)
    # Create multivariate normal draws of the model parameters to 
    # incorporate the full covariance matrix of the model
    draws <- data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    colnames(draws) <- c('int', 'b', 'c')
    result <- list()
    # Compute cost_per_kw with uncertainty using parameter draws
    for (i in seq(nrow(data))) {
        row <- data[i,]
        result[[i]] <- ci(
            cost_beg * 
            (row$cumCapacityKw / cap_beg)^draws$b * 
            (row$price_si / si_beg)^draws$c)
    }
    # Combine results
    result <- do.call(rbind, result) %>% 
        as.data.frame() %>% 
        mutate(year = data$year) %>% 
        select(
            year,
            cost_per_kw = mean,
            cost_per_kw_lb = low,
            cost_per_kw_ub = high)
    return(result)
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
