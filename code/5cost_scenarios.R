# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Notes -----------------------------------------------------------------------

# Basic learning curve model: Y_x = A*x^b
# where
#   Y_x = the cost of unit x (dependent variable)
#   A   = the theoretical cost of unit 1 (a.k.a. T1)
#   x   = the unit number (independent variable)
#   b   = a constant representing the slope (slope = 2^b)


# Log transformation: ln(Y_x) = ln(A) + b*ln(x)
#           Re-write: Y'      = int    + b*x'

# To convert log-space estimated coefficients back to original model:
# A = exp(int)
# b = b
# Learning curve slope = 2^b
# Learning Rate        = 1 - slope

# Approximation of the cumulative total cost of producing N units:
# C = (A*N^(b + 1)) / (b + 1)

## Two factor learning curve model: Y(x,p) = A * x^b * p^c
# where
#   Y(x,p) = the cost of unit x at silicon price p (dependent variable)
#   A   = the theoretical cost of unit 1 (a.k.a. T1)
#   x   = the unit number (independent variable)
#   p   = silicon price
#   b   = lnCap_estimate = learning coefficient on capacity
#   c   = lnSi_estimate = coefficient on silicon price

## Two-level models:
#   World capacity for hard costs
#   Local capacity for soft costs

# HISTORICAL COST SCENARIOS --------------------------------------------------------------

min_year <- 2008
min_year_projection <- 2018

# Use two-factor model on these costs only:
CostsTwoFactor <- c('Module')

# Different LRs estimated:
#   us
#   us_twoLevel
#   us_twoFactor
#   china
#   china_twoLevel
#   china_twoFactor
#   germany
#   germany_twoLevel
#   germany_twoFactor
#   module_twoLevel
#   soft_twoLevel

# Load Historical Costs ----

data_historical_us <- data$usSeiaLbnl %>%
    filter(year >= min_year) %>%         
    mutate(
        cost_per_kw_lb = costPerKw, 
        cost_per_kw_ub = costPerKw) %>% 
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw, cost_per_kw_lb, cost_per_kw_ub)
data_historical_china <- data$china %>%
    filter(year >= min_year) %>%
    mutate(
        cost_per_kw_lb = costPerKw, 
        cost_per_kw_ub = costPerKw) %>% 
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw, cost_per_kw_lb, cost_per_kw_ub) 
data_historical_germany <- data$germany %>%
    filter(year >= min_year) %>%
    mutate(
        cost_per_kw_lb = costPerKw, 
        cost_per_kw_ub = costPerKw) %>% 
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw, cost_per_kw_lb, cost_per_kw_ub) 

# Load Projected Costs ----

data_projected_us2030 <- data$us2030 %>%
    filter(year >= min_year_projection) %>%
    mutate(
        cost_per_kw_lb = costPerKw, 
        cost_per_kw_ub = costPerKw) %>% 
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw, cost_per_kw_lb, cost_per_kw_ub) 

# LOAD DATA TO HERE ----

# BAU ----
#   See google doc for 2x2
#       hard costs -- use world capacity data and local installed costs
#       soft costs -- use historical local installed costs

## Estimation for hard costs -- learning rates on world cumulative capacity
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared)

#   Integrating from cap=world_cap_beg to cap=world_cap_end

#   US
cap_world_range <- get_country_cap_range(
    data$usSeiaLbnl,
    data$world %>%
        merge(expand.grid(
            installType = unique(lr$us_twoLevel$installType),
            component = unique(lr$us_twoLevel$component))),
    min_year)
us_hard_bau_range <- lr$us_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_world_range)

#   US Two-factor model (each point in range)
us_hard_bau_2f_range <- lr$us_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_world_range,
            select(data$world, year, price_si)))

#   Germany
cap_world_range <- get_country_cap_range(
    data$germany,
    data$world %>%
        merge(expand.grid(
            installType = unique(lr$germany_twoLevel$installType),
            component = unique(lr$germany_twoLevel$component))),
    min_year)
germany_hard_bau_range <- lr$germany_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_world_range)

#   Germany Two-factor model (each point in range)
germany_hard_bau_2f_range <- lr$germany_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_world_range,
            select(data$world, year, price_si)))

#   China
cap_world_range <- get_country_cap_range(
    data$china,
    data$world %>%
        merge(expand.grid(
            installType = unique(lr$china_twoLevel$installType),
            component = unique(lr$china_twoLevel$component))),
    min_year)
china_hard_bau_range <- lr$china_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_world_range)

#   Germany Two-factor model (each point in range)
china_hard_bau_2f_range <- lr$china_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_world_range,
            select(data$world, year, price_si)))

# Combine both hard and soft costs together
us_bau_range <- rbind(data_historical_us, us_hard_bau_range)
us_bau_2f_range <- rbind(
    us_hard_bau_2f_range %>%
        filter(component %in% CostsTwoFactor),
    us_bau_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component, installType, year, .keep_all = T)

germany_bau_range <- rbind(data_historical_germany, germany_hard_bau_range)
germany_bau_2f_range <- rbind(
    germany_hard_bau_2f_range %>%
        filter(component %in% CostsTwoFactor),
    germany_bau_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component,installType,year, .keep_all = T)

china_bau_range <- rbind(data_historical_china, china_hard_bau_range)
china_bau_2f_range <- rbind(
    china_hard_bau_2f_range %>%
        filter(component %in% CostsTwoFactor),
    china_bau_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component,installType,year, .keep_all = T)

# S1: Local protectionism ----
#   See google doc for 2x2
#     hard costs: cost declines using world learning rate
#                 applied to incremental local capacity
#     soft costs: same as BAU

## Estimation for hard costs -- learning rates on local cumulative capacity
#   Integrating from cap=world_cap_beg to cap=world_cap_beg + cap_end - cap_beg

#   US
cap_us_world_range <- get_country_world_cap_range(
    data$usSeiaLbnl, data$world, min_year)
us_hard_s1_range <- lr$us_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_us_world_range)

#   US Two-factor model (each point in range)
us_hard_s1_2f_range <- lr$us_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_us_world_range,
            select(data$world, year, price_si)))

#   Germany
cap_germany_world_range <- get_country_world_cap_range(
    data$germany, data$world, min_year)
germany_hard_s1_range <- lr$germany_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_germany_world_range)

#   Germany Two-factor model (each point in range)
germany_hard_s1_2f_range <- lr$germany_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_germany_world_range,
            select(data$world, year, price_si)))

#   China
cap_china_world_range <- get_country_world_cap_range(
    data$china, data$world, min_year)
china_hard_s1_range <- lr$china_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_china_world_range)

#   China Two-factor model (each point in range)
china_hard_s1_2f_range <- lr$china_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_china_world_range,
            select(data$world, year, price_si)))

# Combine both hard and soft costs together
us_s1_range <- rbind(data_historical_us, us_hard_s1_range)
us_s1_2f_range <- rbind(
    us_hard_s1_2f_range %>%
        filter(component %in% CostsTwoFactor),
    us_s1_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component,installType,year, .keep_all = T)

germany_s1_range <- rbind(data_historical_germany, germany_hard_s1_range)
germany_s1_2f_range <- rbind(
    germany_hard_s1_2f_range %>%
        filter(component %in% CostsTwoFactor),
    germany_s1_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component,installType,year, .keep_all = T)

china_s1_range <- rbind(data_historical_china, china_hard_s1_range)
china_s1_2f_range <- rbind(
    china_hard_s1_2f_range %>%
        filter(component %in% CostsTwoFactor),
    china_s1_range %>%
        filter(!(component %in% CostsTwoFactor))
    ) %>%
    distinct(component,installType,year, .keep_all = T)

# Combine BAU and S1
us_range <- us_bau_range %>%
    mutate(scenario = 'bau') %>%
    rbind(
        us_s1_range %>%
            mutate(scenario = 's1'))

germany_range <- germany_bau_range %>%
    mutate(scenario = 'bau') %>%
    rbind(
        germany_s1_range %>%
            mutate(scenario = 's1'))

china_range <- china_bau_range %>%
    mutate(scenario = 'bau') %>%
    rbind(
        china_s1_range %>%
            mutate(scenario = 's1'))

# Combine BAU and S1 -- Two factor models
us_2f_range <- rbind(
    us_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    us_bau_range %>%
        mutate(scenario = 'bau', model = '1factor'),
    us_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor')
    )

germany_2f_range <- rbind(
    germany_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    germany_bau_range %>%
        mutate(scenario = 'bau', model = '1factor'),
    germany_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor')
    )

china_2f_range <- rbind(
    china_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    china_bau_range %>%
        mutate(scenario = 'bau', model = '1factor'),
    china_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor')
    )

## Total costs and cost differences b/t BAU and S1 ----
#   1. Combine components (hard and soft) into totals
us_2f_range_tot <- us_2f_range %>%
    filter(model == '2factor') %>%
    group_by(installType, year, scenario) %>%
    summarise(
        cost_per_kw = sum(cost_per_kw),
        cost_per_kw_lb = sum(cost_per_kw_lb),
        cost_per_kw_ub = sum(cost_per_kw_ub))

germany_2f_range_tot <- germany_2f_range %>%
    filter(model == '2factor') %>%
    group_by(installType, year, scenario) %>%
    summarise(
        cost_per_kw = sum(cost_per_kw),
        cost_per_kw_lb = sum(cost_per_kw_lb),
        cost_per_kw_ub = sum(cost_per_kw_ub))

china_2f_range_tot <- china_2f_range %>%
    filter(model == '2factor') %>%
    group_by(installType, year, scenario) %>%
    summarise(
        cost_per_kw = sum(cost_per_kw),
        cost_per_kw_lb = sum(cost_per_kw_lb),
        cost_per_kw_ub = sum(cost_per_kw_ub))

#   2. Calculate cost differences and cumulative savings

us_2f_range_cum <- computeCostSavings(
    us_2f_range_tot, data$usSeiaLbnl)
germany_2f_range_cum <- computeCostSavings(
    germany_2f_range_tot, data$germany)
china_2f_range_cum <- computeCostSavings(
    china_2f_range_tot, data$china) %>% 
    filter(year <= 2018)

# S2: Local protectionism + High silicon prices ----
#   See google doc for 2x2
#     hard costs: cost declines using world learning rate
#                 applied to incremental local capacity
#     soft costs: same as BAU
#   High silicon prices: silicon prices remain at 2008 levels

## Estimation for hard costs -- learning rates on local cumulative capacity
#   Integrating from cap=world_cap_beg to cap=world_cap_beg + cap_end - cap_beg

# Silicon prices
year_si_cutoff = 2008
price_si_cutoff = data$world[data$world$year == year_si_cutoff,'price_si'] %>% unlist()
data$world_highsi <-
    data$world %>%
    filter(year > year_si_cutoff) %>%
    mutate(price_si = price_si_cutoff) %>%
    rbind(data$world %>%
              filter(year <= year_si_cutoff))

#   US
cap_us_world_range <- get_country_world_cap_range(
    data$usSeiaLbnl, data$world, min_year)
us_hard_s2_range <- lr$us_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_us_world_range)

#   US Two-factor model (each point in range)
us_hard_s2_2f_range <- lr$us_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_us_world_range,
            select(data$world_highsi, year, price_si)))

#   Germany
cap_germany_world_range <- get_country_world_cap_range(
    data$germany, data$world, min_year)
germany_hard_s2_range <- lr$germany_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_germany_world_range)

#   Germany Two-factor model (each point in range)
germany_hard_s2_2f_range <- lr$germany_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_germany_world_range,
            select(data$world_highsi, year, price_si)))

#   China
cap_china_world_range <- get_country_world_cap_range(
    data$china, data$world, min_year)
china_hard_s2_range <- lr$china_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_china_world_range)

#   China Two-factor model (each point in range)
china_hard_s2_2f_range <- lr$china_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_china_world_range,
            select(data$world_highsi, year, price_si)))

# Combine both hard and soft costs together
us_s2_range <- rbind(data_historical_us, us_hard_s2_range)
us_s2_2f_range <- rbind(
    us_hard_s2_2f_range %>%
        filter(component %in% CostsTwoFactor),
    us_s2_range %>%
        filter(!(component %in% CostsTwoFactor))
) %>%
    distinct(component,installType,year, .keep_all = T)

germany_s2_range <- rbind(data_historical_germany, germany_hard_s2_range)
germany_s2_2f_range <- rbind(
    germany_hard_s2_2f_range %>%
        filter(component %in% CostsTwoFactor),
    germany_s2_range %>%
        filter(!(component %in% CostsTwoFactor))
) %>%
    distinct(component,installType,year, .keep_all = T)

china_s2_range <- rbind(data_historical_china, china_hard_s2_range)
china_s2_2f_range <- rbind(
    china_hard_s2_2f_range %>%
        filter(component %in% CostsTwoFactor),
    china_s1_range %>%
        filter(!(component %in% CostsTwoFactor))
) %>%
    distinct(component,installType,year, .keep_all = T)

# Combine BAU, S1, S2 -- Two factor models
us_2f_range_s2 <- rbind(
    us_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    us_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor'),
    us_s2_2f_range %>%
        mutate(scenario = 's2', model = '2factor')
)

germany_2f_range_s2 <- rbind(
    germany_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    germany_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor'),
    germany_s2_2f_range %>%
        mutate(scenario = 's2', model = '2factor')
)

china_2f_range_s2 <- rbind(
    china_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    china_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor'),
    china_s2_2f_range %>%
        mutate(scenario = 's2', model = '2factor')
)

# BAU - 2030 ----
#   2018-2030 projection given BAU assumptions  
#       hard costs -- use world capacity data and local installed costs
#       soft costs -- use projected local installed costs

## Estimation for hard costs -- learning rates on world cumulative capacity
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared)

#   Integrating from cap=world_cap_beg to cap=world_cap_end

#   US
cap_world_range <- get_country_cap_range(
    data$us2030,
    data$world2030 %>%
        merge(expand.grid(
            installType = unique(lr$us_twoLevel$installType),
            component = unique(lr$us_twoLevel$component))),
    min_year_projection)
us2030_hard_bau_range <- lr$us_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_world_range)
us2030_hard_bau_2f_range <- lr$us_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_world_range,
            select(data$world2030, year, price_si)))

# Combine both hard and soft costs together
us2030_bau_range <- rbind(data_projected_us2030, us2030_hard_bau_range)
us2030_bau_2f_range <- rbind(
    us2030_hard_bau_2f_range %>%
        filter(component %in% CostsTwoFactor),
    us2030_bau_range %>%
        filter(!(component %in% CostsTwoFactor))
) %>%
    distinct(component, installType, year, .keep_all = T)

# S1: Local protectionism - 2030 ----
#   2018-2030 projection given S1 assumptions

## Estimation for hard costs -- learning rates on local cumulative capacity
#   Integrating from cap=world_cap_beg to cap=world_cap_beg + cap_end - cap_beg

#   US
cap_us_world_range <- get_country_world_cap_range(
    data$us2030,
    data$world2030 %>%
        merge(expand.grid(
            installType = unique(lr$us_twoLevel$installType),
            component = unique(lr$us_twoLevel$component))),
    min_year_projection)
us2030_hard_s1_range <- lr$us_twoLevel %>%
    filter(capData == "world") %>%
    cost_constant_cap_range(cap_us_world_range)
us2030_hard_s1_2f_range <- lr$us_twoFactor %>%
    cost_constant_cap_range_2f(
        merge(
            cap_us_world_range,
            select(data$world2030, year, price_si)))

# Combine both hard and soft costs together
us2030_s1_range <- rbind(data_projected_us2030, us2030_hard_s1_range)
us2030_s1_2f_range <- rbind(
    us2030_hard_s1_2f_range %>%
        filter(component %in% CostsTwoFactor),
    us2030_s1_range %>%
        filter(!(component %in% CostsTwoFactor))
) %>%
    distinct(component,installType,year, .keep_all = T)

# Combine BAU and S1
us2030_2f_range <- rbind(
    us2030_bau_2f_range %>%
        mutate(scenario = 'bau', model = '2factor'),
    us2030_bau_range %>%
        mutate(scenario = 'bau', model = '1factor'),
    us2030_s1_2f_range %>%
        mutate(scenario = 's1', model = '2factor')
)

# Save outputs ----

saveRDS(list(
    us_2f_range           = us_2f_range,
    us_2f_range_s2        = us_2f_range_s2,
    us_2f_range_cum       = us_2f_range_cum,
    us2030_2f_range       = us2030_2f_range,
    germany_2f_range      = germany_2f_range,
    germany_2f_range_cum  = germany_2f_range_cum,
    germany_2f_range_s2   = germany_2f_range_s2,
    china_2f_range        = china_2f_range,
    china_2f_range_cum    = china_2f_range_cum,
    china_2f_range_s2     = china_2f_range_s2),
    dir$cost_scenarios
)
