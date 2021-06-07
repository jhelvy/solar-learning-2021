# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Set period dates
year_min <- 2008
year_max <- 2018
year_min_projection <- 2018

# Notes -----------------------------------------------------------------------


# HISTORICAL COST SCENARIOS ---------------------------------------------------

# Load historical cost data ----

data_historical_us <- data$usSeiaLbnl %>%
    filter(year >= year_min) %>%
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw)

data_historical_china <- data$china %>%
    filter(year >= year_min) %>%
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw)

data_historical_germany <- data$germany %>%
    filter(year >= year_min) %>%
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw)

# Load projected cost data ----

data_projected_us2030 <- data$us2030 %>%
    filter(year >= year_min_projection) %>%
    select(
        component, installType, year, cap = cumCapacityKw, 
        cost_per_kw = costPerKw)


# GLOBAL LEARNING ----

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

predict_cost_manual(
    model    = lr$model_us,
    data     = lr$data_us,
    year_min = year_min,
    ci       = 0.95)

predict_cost(
    model    = lr$model_us,
    data     = lr$data_us,
    year_min = year_min,
    ci       = 0.95)

cost_scenarios_global_us <- predict_cost(
    model    = lr$model_us,
    data     = lr$data_us,
    year_min = year_min,
    ci       = 0.95)

cost_scenarios_global_china <- predict_cost(
    model    = lr$model_china,
    data     = lr$data_china,
    year_min = year_min,
    ci       = 0.95)

cost_scenarios_global_germany <- predict_cost(
    model    = lr$model_germany,
    data     = lr$data_germany,
    year_min = year_min,
    ci       = 0.95)

# NATIONAL LEARNING ----

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Create national capacity data sets for predictions
data_national_us <- makeNationalLearningData(
    df_country = data$usSeiaLbnl,
    df_model = lr$data_us,
    year_min = year_min)

data_national_china <- makeNationalLearningData(
    df_country = data$china,
    df_model = lr$data_china,
    year_min = year_min)

data_national_germany <- makeNationalLearningData(
    df_country = data$germany,
    df_model = lr$data_germany,
    year_min = year_min)

# Compute national cost predictions
cost_scenarios_national_us <- predict_cost(
    model    = lr$model_us,
    data     = data_national_us,
    year_min = year_min,
    ci       = 0.95)

cost_scenarios_national_china <- predict_cost(
    model    = lr$model_china,
    data     = data_national_china,
    year_min = year_min,
    ci       = 0.95)

cost_scenarios_national_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_national_germany,
    year_min = year_min,
    ci       = 0.95)

# COMBINE GLOBAL AND NATIONAL COST SCENARIOS ----

cost_scenarios_us <- combineScenarios(
    global = cost_scenarios_global_us,
    national = cost_scenarios_national_us,
    country = "china"
)

cost_scenarios_china <- combineGlobalNationalData(
    global = cost_scenarios_global_china,
    national = cost_scenarios_national_china,
    country = "china"
)

cost_scenarios_germany <- combineGlobalNationalData(
    global = cost_scenarios_global_germany,
    national = cost_scenarios_national_germany,
    country = "china"
)


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
    data$usSeiaLbnl, data$world, year_min)
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
    data$germany, data$world, year_min)
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
    data$china, data$world, year_min)
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
        filter(component == "Module"),
    us_s2_range %>%
        filter(!(component == "Module"))
) %>%
    distinct(component,installType,year, .keep_all = T)

germany_s2_range <- rbind(data_historical_germany, germany_hard_s2_range)
germany_s2_2f_range <- rbind(
    germany_hard_s2_2f_range %>%
        filter(component == "Module"),
    germany_s2_range %>%
        filter(!(component == "Module"))
) %>%
    distinct(component,installType,year, .keep_all = T)

china_s2_range <- rbind(data_historical_china, china_hard_s2_range)
china_s2_2f_range <- rbind(
    china_hard_s2_2f_range %>%
        filter(component == "Module"),
    china_s1_range %>%
        filter(!(component == "Module"))
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
    year_min_projection)
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
        filter(component == "Module"),
    us2030_bau_range %>%
        filter(!(component == "Module"))
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
    year_min_projection)
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
        filter(component == "Module"),
    us2030_s1_range %>%
        filter(!(component == "Module"))
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
    cost_scenarios_us = cost_scenarios_us,
    cost_scenarios_china = cost_scenarios_china,
    cost_scenarios_germany = cost_scenarios_germany),
    dir$cost_scenarios
)
