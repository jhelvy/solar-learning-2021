
# Two-factor learning model
# This model assumes that the 'data' input has already been nested
# It estimates a log-log model with cumulative capacity and input prices 
# and returns the results as a data frame with model coefficients as well as
# the computed learning rates
fit_2flr_model <- function(data) {
    fit <- data %>%
        # Estimate linear model on log-transformed variables
        mutate(
            model  = map(data, function(df) lm(lnCost ~ lnCap + log(price_si), data = df)),
            tidied = map(model, tidy)) %>%
        unnest(tidied) %>%
        select(-data) %>%
        # Rename estimate variable names
        mutate(term = str_replace_all(term, fixed('(Intercept)'), 'int'),
               term = str_replace_all(term, fixed('log(price_si)'),'lnSi')) %>%
        rename(pval = p.value,
                stderr = std.error) %>%
        gather(key = 'var', value = 'val', estimate:pval) %>%
        mutate(
            term = ifelse(term == "lnCap", "b", term), 
            var  = ifelse(var == "estimate", "est", var)) %>%
        unite(temp, term, var) %>%
        spread(temp, val) %>%
        rename(int = int_est, b = b_est) %>% 
        mutate(
            A    = exp(int),
            b_lb = b - 2*b_stderr,
            b_ub = b + 2*b_stderr, 
            lnSi_est_lb = lnSi_est - 2*lnSi_stderr, 
            lnSi_est_ub = lnSi_est + 2*lnSi_stderr)
    return(fit)
}

# Get cumulative capacity of latest year in data
get_country_cap <- function(df) {
    result <- df %>%
        filter(year == max(year)) %>%
        select(componentType, installType, capacity = cumCapacityKw) %>%
        distinct() 
    return(result)
}

# Get cumulative capacity at each year in range
get_country_cap_range <- function(df.country, df.world, min_year = NULL) {
    if (is.null(min_year)) {
        min_year = min(df$year)
    }
    df <- df.world %>% 
        merge(
            df.country %>% 
                select(year, component, installType, costPerKw)
        )
    result <- df %>%
        select(component, installType, year, cap = cumCapacityKw) %>%
        distinct() %>%
        merge(
            df %>% 
                filter(year == min_year) %>%
                select(
                    component, installType, cap_beg = cumCapacityKw,
                    cost_beg = costPerKw, si_beg = price_si) %>% 
                distinct() 
        ) %>%
        filter(year >= min_year)
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
# cap_beg = world_cap_beg to cap_end = world_cap_beg + country_cap_end - country_cap_beg
# Note: world capacity does not currently breakdown by type (Commercial, Residential, Utility)
get_country_world_cap_range <- function(df.country, df.world, min_year = NULL) {
    if (is.null(min_year)) {
        min_year = min(df$year)
    }
    df <- df.country %>% 
            merge(df.world %>% 
                select(year, world_cap = cumCapacityKw, price_si))
    result <- df %>%
        select(component, installType, year, cap = cumCapacityKw) %>%
        distinct() %>%
        merge(df %>% 
              filter(year == min_year) %>%
              select(
                  component, installType, cap_beg = world_cap, 
                  cap_country_beg = cumCapacityKw, cost_beg = costPerKw, 
                  si_beg = price_si) %>% 
              distinct() ) %>%
        filter(year >= min_year) %>%
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