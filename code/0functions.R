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

predict_cost <- function(model, data, year_min = NULL, ci = 0.95) {
    if (is.null(year_min)) {
        year_min = min(data$year)
    }
    data <- filter(data, year >= year_min)
    # Create multivariate normal draws of the model parameters to 
    # incorporate the full covariance matrix of the model
    draws <- data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    colnames(draws) <- c('int', 'b', 'c')
    # Compute starting values
    cost_beg <- data[1,]$costPerKw
    cap_beg <- data[1,]$cumCapacityKw
    si_beg <- data[1,]$price_si
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

makeNationalLearningData <- function(df_country, df_model, year_min = NULL) {
    if (is.null(year_min)) {
        year_min = min(data$year)
    }
    # Aggregate country installed capacity if data is broken down by 
    # install type 
    df_country <- df_country %>% 
        filter(year >= year_min, component  == "Module") %>% 
        group_by(year) %>%
        summarise(cap_country = sum(cumCapacityKw))
    # Add first year capacity 
    df_country$cap_beg_country <- 
        df_country[df_country$year == year_min,]$cap_country
    # Get world capacity dat
    df_model <- df_model %>% 
        filter(year >= year_min) %>% 
        mutate(cap_world = cumCapacityKw)
    # Add first year capacity 
    df_model$cap_beg_world <- df_model[df_model$year == year_min,]$cap_world
    # Join and compute national capacity
    result <- df_model %>% 
        left_join(df_country, by = "year") %>%
        mutate(
            cap_addition = cap_country - cap_beg_country,
            cumCapacityKw = cap_beg_world + cap_addition) %>% 
        select(
            year, costPerKw, price_si, cumCapacityKw, cap_beg_country,
            cap_addition)
    return(result)
}









computeCostSavings <- function(df, data) {
    result <- df %>%
        merge(get_country_cap_additions(data)) %>%
        group_by(year, installType) %>%
        mutate(
            diff = cost_per_kw - lag(cost_per_kw, 1),
            diff_ub = cost_per_kw_lb - lag(cost_per_kw_lb, 1),
            diff_lb = cost_per_kw_ub - lag(cost_per_kw_ub, 1)) %>%
        drop_na() %>%
        select(-scenario) %>% 
        select(year, installType, addCap, everything()) %>% 
        mutate(
            cost_diff_bil = addCap*diff / 10^9,
            cost_diff_bil_lb = addCap*diff_lb / 10^9,
            cost_diff_bil_ub = addCap*diff_ub / 10^9) %>% 
        arrange(installType, year) %>% 
        group_by(installType) %>% 
        mutate(
            cost_diff_cum = cumsum(cost_diff_bil), 
            cost_diff_cum_lb = cumsum(cost_diff_bil_lb), 
            cost_diff_cum_ub = cumsum(cost_diff_bil_ub))
    return(result)
}
