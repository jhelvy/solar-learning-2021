# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Historical range
year_min <- 2008
year_max <- 2018

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

# Global Learning ----

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

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

# National Learning ----

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

# Combine Global And National Cost Scenarios ----

cost_scenarios_historical <- rbind(
    mutate(cost_scenarios_global_us,
           scenario = "global", country = "U.S."),
    mutate(cost_scenarios_national_us,
           scenario = "national", country = "U.S."),
    mutate(cost_scenarios_global_china,
           scenario = "global", country = "China"),
    mutate(cost_scenarios_national_china,
           scenario = "national", country = "China"),
    mutate(cost_scenarios_global_germany,
           scenario = "global", country = "Germany"),
    mutate(cost_scenarios_national_germany,
           scenario = "national", country = "Germany")
)

# Calculate savings between national and global learning scenarios

# Combine additional capacity data for each country into one data frame
cap_additions <- rbind(
    mutate(data_national_us, country = "U.S."),
    mutate(data_national_china, country = "China"),
    mutate(data_national_germany, country = "Germany")) %>% 
    select(year, country, cum_cap_addition) %>% 
    mutate(ann_cap_addition = cum_cap_addition - lag(cum_cap_addition, 1))

savings_historical_mean <- cost_scenarios_historical %>% 
    select(year, scenario, country, cost_per_kw) %>% 
    spread(key = scenario, value = cost_per_kw) %>%
    computeSavings(cap_additions, year_min)

savings_historical_lb <- cost_scenarios_historical %>% 
    select(year, scenario, country, cost_per_kw_lb) %>% 
    spread(key = scenario, value = cost_per_kw_lb) %>% 
    computeSavings(cap_additions, year_min)

savings_historical_ub <- cost_scenarios_historical %>% 
    select(year, scenario, country, cost_per_kw_ub) %>% 
    spread(key = scenario, value = cost_per_kw_ub) %>% 
    computeSavings(cap_additions, year_min)

# Merge savings 
savings_historical <- rbind(
    mutate(savings_historical_mean, est = "mean"),
    mutate(savings_historical_lb, est = "lb"),
    mutate(savings_historical_ub, est = "ub")) %>% 
    spread(key = est, value = ann_savings_bil) %>% 
    rename(
        ann_savings_bil = mean, 
        ann_savings_bil_lb = lb, 
        ann_savings_bil_ub = ub) %>% 
    group_by(country) %>% 
    mutate(
        cum_savings_bil = cumsum(ann_savings_bil), 
        cum_savings_bil_lb = cumsum(ann_savings_bil_lb),
        cum_savings_bil_ub = cumsum(ann_savings_bil_ub)) %>% 
    ungroup()

# Save outputs ----

saveRDS(list(
    cost_scenarios_historical = cost_scenarios_historical,
    savings_historical = savings_historical),
    dir$historical_scenarios
)
