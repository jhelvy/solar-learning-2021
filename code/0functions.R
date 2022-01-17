
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

makeNationalCapData <- function(
    data_country, data_world, year_beg = NULL, delay_years = 10
) {
    if (is.null(year_beg)) {
        year_beg = min(data$year)
    }
    # Setup data
    data_country <- data_country %>%
        filter(year >= year_beg) %>% 
        select(year, cumCapacityKw)
    cap_beg_country <- data_country[1,]$cumCapacityKw
    data_country <- data_country %>% 
        mutate(cumCapacityKw = cumCapacityKw - cap_beg_country)
    data_world <- data_world %>%
        filter(year >= year_beg) %>% 
        rename(cumCapacityKw_world = cumCapacityKw)
    cap_beg_world <- data_world[1,]$cumCapacityKw_world
    data_world <- data_world %>% 
        mutate(cumCapacityKw_world = cumCapacityKw_world - cap_beg_world)
    # Compute national cumulative capacity additions starting with global 
    # capacity and gradually shifting down to only national capacity after 
    # delay_years years
    # First set up the lambda to phase out global cap
    lambda <- seq(0, 1, length.out = delay_years + 1)
    lambda <- c(lambda, rep(1, nrow(data_country) - length(lambda)))
    result <- data_country %>%
        left_join(data_world, by = "year") %>% 
        mutate(
            cumCapacityKw_other = cumCapacityKw_world - cumCapacityKw, 
            lambda = lambda, 
            cumCapacityKw_other = cumCapacityKw_other*(1 - lambda),
            cum_cap_addition = cumCapacityKw + cumCapacityKw_other,
            cumCapacityKw = cap_beg_world + cum_cap_addition) %>% 
        select(year, cumCapacityKw, cum_cap_addition, price_si)

# Preview results     
# result %>% 
#     select(year, starts_with("cumCapacityKw_"), -cumCapacityKw_other) %>% 
#     pivot_longer(
#         names_to = "cumCapacityKw", cols = starts_with("cumCapacityKw_")) %>% 
#     separate(cumCapacityKw, into = c("drop", "type"), sep = "_") %>% 
#     filter(year < 2018) %>% 
#     ggplot() + 
#     geom_line(aes(x = year, y = value, group = type, color = type)) +
#     theme_minimal()

    return(result)
}

computeSavings <- function(cost_national, cost_global, cap_additions) {
    result <- cost_national %>% 
        left_join(cost_global, by = c("year", "country")) %>% 
        left_join(cap_additions, by = c("year", "country")) %>% 
        mutate(
            ann_savings_bil = ann_cap_addition*(national - global) / 10^9) %>% 
        select(year, country, ann_savings_bil)
    return(result)
}

repDf <- function(df, n) {
  return(df[rep(seq_len(nrow(df)), n), ])
}

getStartingCapcity <- function(df, year_min_proj) {
  return(df[which(df$year == year_min_proj),]$cumCapacityKw)
}

getStartingCost <- function(df, year_min_proj) {
  return(df[which(df$year == year_min_proj),]$costPerKw)
}
