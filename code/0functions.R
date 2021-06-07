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
    colnames(draws) <- c('int', 'b', 'lnSi_est')
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
            (row$price_si / si_beg)^draws$lnSi_est)
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
        group_by(year) %>%
        summarise(cap_country = sum(cumCapacityKw)) %>% 
        filter(year >= year_min)
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
            cap_country_diff = cap_country - cap_beg_country,
            cumCapacityKw = cap_beg_world + cap_country_diff) %>% 
        select(year, costPerKw, price_si, cumCapacityKw)
    return(result)
}












# Get cumulative capacity at each year in range
get_country_cap_range <- function(df, year_min = NULL) {
    if (is.null(year_min)) {
        year_min = min(df$year)
    }
    result <- df %>%
        select(component, installType, year, cap = cumCapacityKw) %>%
        distinct() %>%
        merge(
            df %>% 
                filter(year == year_min) %>%
                select(
                    component, installType, cap_beg = cumCapacityKw,
                    cost_beg = costPerKw, si_beg = price_si) %>% 
                distinct() 
        ) %>%
        filter(year >= year_min)
    return(result)
}

# Get additional capacity installed at each year in range
# Drops the first year in the series
get_country_cap_additions <- function(df) {
    vars_key <- unique(df$installType)
    result <- df %>%
        select(installType, year, cumCap = cumCapacityKw) %>%
        distinct() %>%
        spread(key = installType, value = cumCap) %>%
        mutate_at(vars_key, ~ .-c(0,lag(.)[-1])) %>%  # calculate difference between rows
        slice(-1) %>% # remove the first row
        pivot_longer(-year, names_to = "installType", values_to = "addCap")
    return(result)
}

# Get cumulative counterfactual capacity at each year in range:
# cap_beg = world_cap_beg to
# cap_end = world_cap_beg + country_cap_end - country_cap_beg
# Note: world capacity does not currently breakdown by type (Commercial, Residential, Utility)
get_country_world_cap_range <- function(df_country, df_world, year_min = NULL) {
    if (is.null(year_min)) {
        year_min = min(df$year)
    }
    df <- df_country %>%
            merge(df_world %>%
                select(year, world_cap = cumCapacityKw, price_si))
    result <- df %>%
        select(component, installType, year, cap = cumCapacityKw) %>%
        distinct() %>%
        merge(df %>% 
              filter(year == year_min) %>%
              select(
                  component, installType, cap_beg = world_cap, 
                  cap_country_beg = cumCapacityKw, cost_beg = costPerKw, 
                  si_beg = price_si) %>% 
              distinct() ) %>%
        filter(year >= year_min) %>%
        mutate(cap = cap_beg + cap - cap_country_beg) %>%
        select(year, component, installType, cap_beg, cap, cost_beg, si_beg)
    return(result)
}

# Compute the estimated cost of meeting the amount of capacity installed 
# from an initial capacity (cap_beg) and cost (cost_beg)
cost_constant_cap_range <- function(lr, cap, capData = FALSE) { 
    result <- lr %>%
        left_join(cap) %>% 
        mutate(
            cost_per_kw = cost_beg * cap_beg^(-b) * cap^b,
            cost_per_kw_lb = cost_beg * cap_beg^(-b_lb) * cap^b_lb,
            cost_per_kw_ub = cost_beg * cap_beg^(-b_ub) * cap^b_ub
        )
    if (capData) {
        return(select(result,
            component, installType, year, cap, cost_per_kw,
            cost_per_kw_lb, cost_per_kw_ub, capData)
        )
    }
    return(select(result,
            component, installType, year, cap, cost_per_kw, cost_per_kw_lb,
            cost_per_kw_ub)
    )
}

# Compute the estimated cost of meeting the amount of capacity installed 
# from an initial capacity (cap_beg) and cost (cost_beg)
# using two factor model (Si)
cost_constant_cap_range_2f <- function(lr, cap, capData = FALSE) {
    result <- lr %>%
        left_join(cap) %>% 
            mutate(
                cost_per_kw = cost_beg * cap_beg^(-b) * 
                    si_beg^(-lnSi_est) * cap^b * 
                    price_si^lnSi_est,
                cost_per_kw_lb = cost_beg * cap_beg^(-b_lb) *
                    si_beg^(-lnSi_est_lb) * cap^b_lb *
                    price_si^lnSi_est_lb,
                cost_per_kw_ub = cost_beg * cap_beg^(-b_ub) *
                    si_beg^(-lnSi_est_ub) * cap^b_ub *
                    price_si^lnSi_est_ub)
    if (capData) {
        return(select(result,
            component, installType, year, cap, cost_per_kw, 
            cost_per_kw_lb, cost_per_kw_ub, capData)
        )
    }
    return(select(result,
        component, installType, year, cap, cost_per_kw, cost_per_kw_lb,
        cost_per_kw_ub)
    )
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
