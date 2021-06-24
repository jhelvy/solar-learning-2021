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

getFutureCapacities <- function(
  df, year_min_proj, year_max_proj, target_capacity
) {
  num_years <- year_max_proj - year_min_proj
  result <- df %>%
    filter(year == year_min_proj) %>%
    mutate(annualCap = (target_capacity - cumCapacityKw) / num_years) %>%
    select(begCap = cumCapacityKw, annualCap) %>%
    repDf(num_years) %>%
    mutate(year = year_min_proj + seq(num_years)) %>%
    mutate(cumCapacityKw = cumsum(annualCap) + begCap) %>%
    select(year, cumCapacityKw)
  return(result)
}

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
        year_beg = min(data$year)
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

makeNationalLearningData <- function(df_country, df_model, year_beg = NULL) {
    if (is.null(year_beg)) {
        year_beg = min(data$year)
    }
    if (! "component" %in% names(df_country)) {
      df_country$component <- "Module"
    }
    # Aggregate country installed capacity if data is broken down by 
    # install type 
    df_country <- df_country %>% 
        filter(year >= year_beg, component  == "Module") %>%
        group_by(year) %>%
        summarise(cap_country = sum(cumCapacityKw))
    # Add first year capacity 
    df_country$cap_beg_country <- 
        df_country[df_country$year == year_beg,]$cap_country
    # Get world capacity data
    df_model <- df_model %>% 
        filter(year >= year_beg) %>%
        mutate(cap_world = cumCapacityKw)
    # Add first year capacity 
    df_model$cap_beg_world <- df_model[df_model$year == year_beg,]$cap_world
    # Join and compute national capacity
    result <- df_model %>% 
        left_join(df_country, by = "year") %>%
        mutate(
            cum_cap_addition = cap_country - cap_beg_country,
            cumCapacityKw = cap_beg_world + cum_cap_addition) %>% 
        select(
            year, costPerKw, price_si, cumCapacityKw, cap_beg_country,
            cum_cap_addition)
    return(result)
}

computeSavings <- function(df, cap_additions, year_min) {
    result <- df %>% 
        left_join(cap_additions, by = c("year", "country")) %>% 
        filter(year > year_min) %>% 
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
