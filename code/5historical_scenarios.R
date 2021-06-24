# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Historical range
year_min <- 2008
year_max <- 2018

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_min)
china_beg <- lr$data_china %>%
    filter(year == year_min)
germany_beg <- lr$data_germany %>%
    filter(year == year_min)

# GLOBAL LEARNING ---------------------------------------------------

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

cost_global_us <- predict_cost(
    model    = lr$model_us,
    data     = lr$data_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

cost_global_china <- predict_cost(
    model    = lr$model_china,
    data     = lr$data_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

cost_global_germany <- predict_cost(
    model    = lr$model_germany,
    data     = lr$data_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

# NATIONAL LEARNING ---------------------------------------------------

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Create national learning capacity data for each country
data_national_us <- makeNationalCapData(
    df_country = data$usSeiaLbnl,
    df_world   = lr$data_us,
    year_beg   = year_min)
data_national_china <- makeNationalCapData(
    df_country = data$china,
    df_world   = lr$data_china,
    year_beg   = year_min)
data_national_germany <- makeNationalCapData(
    df_country = data$germany,
    df_world   = lr$data_germany,
    year_beg   = year_min)

# Compute cost scenarios by country
cost_national_us <- predict_cost(
    model    = lr$model_us,
    data     = data_national_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

cost_national_china <- predict_cost(
    model    = lr$model_china,
    data     = data_national_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

cost_national_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_national_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_min,
    ci       = 0.95)

# Combine Cost Scenarios ----

cost <- rbind(
    mutate(cost_global_us,
           learning = "global", country = "U.S."),
    mutate(cost_national_us,
           learning = "national", country = "U.S."),
    mutate(cost_global_china,
           learning = "global", country = "China"),
    mutate(cost_national_china,
           learning = "national", country = "China"),
    mutate(cost_global_germany,
           learning = "global", country = "Germany"),
    mutate(cost_national_germany,
           learning = "national", country = "Germany")
)

# Calculate savings between national and global learning scenarios

# Combine additional capacity data for each country into one data frame
cap_additions <- rbind(
    mutate(data_national_us, country = "U.S."),
    mutate(data_national_china, country = "China"),
    mutate(data_national_germany, country = "Germany")) %>% 
    select(year, country, cum_cap_addition) %>% 
    mutate(ann_cap_addition = cum_cap_addition - lag(cum_cap_addition, 1))

savings_mean <- cost %>%
    select(year, learning, country, cost_per_kw) %>%
    spread(key = learning, value = cost_per_kw) %>%
    computeSavings(cap_additions, year_min)

savings_lb <- cost %>%
    select(year, learning, country, cost_per_kw_lb) %>%
    spread(key = learning, value = cost_per_kw_lb) %>%
    computeSavings(cap_additions, year_min)

savings_ub <- cost %>%
    select(year, learning, country, cost_per_kw_ub) %>%
    spread(key = learning, value = cost_per_kw_ub) %>%
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
    cost = cost,
    savings = savings),
    dir$historical_scenarios
)
