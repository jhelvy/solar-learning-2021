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

# Global Learning ----

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

us_beg <- lr$data_us %>%
    filter(year == year_min)
cost_scenarios_global_us <- predict_cost(
    model    = lr$model_us,
    data     = lr$data_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

china_beg <- lr$data_china %>%
    filter(year == year_min)
cost_scenarios_global_china <- predict_cost(
    model    = lr$model_china,
    data     = lr$data_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

germany_beg <- lr$data_germany %>%
    filter(year == year_min)
cost_scenarios_global_germany <- predict_cost(
    model    = lr$model_germany,
    data     = lr$data_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

# National Learning ----

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Compute national cost predictions

data_national_us <- makeNationalLearningData(
    df_country = data$usSeiaLbnl,
    df_model   = lr$data_us,
    year_beg   = year_min)
cost_scenarios_national_us <- predict_cost(
    model    = lr$model_us,
    data     = data_national_us,
    cost_beg = data_national_us[1,]$costPerKw,
    cap_beg  = data_national_us[1,]$cumCapacityKw,
    si_beg   = data_national_us[1,]$price_si,
    year_beg = year_min,
    ci       = 0.95)

data_national_china <- makeNationalLearningData(
    df_country = data$china,
    df_model   = lr$data_china,
    year_beg   = year_min)
cost_scenarios_national_china <- predict_cost(
    model    = lr$model_china,
    data     = data_national_china,
    cost_beg = data_national_china[1,]$costPerKw,
    cap_beg  = data_national_china[1,]$cumCapacityKw,
    si_beg   = data_national_china[1,]$price_si,
    year_beg = year_min,
    ci       = 0.95)

data_national_germany <- makeNationalLearningData(
    df_country = data$germany,
    df_model   = lr$data_germany,
    year_beg   = year_min)
cost_scenarios_national_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_national_germany,
    cost_beg = data_national_germany[1,]$costPerKw,
    cap_beg  = data_national_germany[1,]$cumCapacityKw,
    si_beg   = data_national_germany[1,]$price_si,
    year_beg = year_min,
    ci       = 0.95)

# Combine Cost Scenarios ----

cost_scenarios <- rbind(
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

savings_mean <- cost_scenarios %>%
    select(year, scenario, country, cost_per_kw) %>% 
    spread(key = scenario, value = cost_per_kw) %>%
    computeSavings(cap_additions, year_min)

savings_lb <- cost_scenarios %>%
    select(year, scenario, country, cost_per_kw_lb) %>% 
    spread(key = scenario, value = cost_per_kw_lb) %>% 
    computeSavings(cap_additions, year_min)

savings_ub <- cost_scenarios %>%
    select(year, scenario, country, cost_per_kw_ub) %>% 
    spread(key = scenario, value = cost_per_kw_ub) %>% 
    computeSavings(cap_additions, year_min)

# Merge savings 
savings <- rbind(
    mutate(savings_mean, est = "mean"),
    mutate(savings_lb, est = "lb"),
    mutate(savings_ub, est = "ub")) %>%
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
    cost_scenarios = cost_scenarios,
    savings = savings),
    dir$historical_scenarios
)
