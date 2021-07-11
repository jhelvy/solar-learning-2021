# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_model_us_min)
china_beg <- lr$data_china %>%
    filter(year == year_model_china_min)
germany_beg <- lr$data_germany %>%
    filter(year == year_model_germany_min)
world_beg <- data$world %>%
    filter(year == year_model_world_min)

# GLOBAL LEARNING ---------------------------------------------------

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

cost_global_us <- predict_cost(
    model    = lr$model_us,
    data     = data$world %>% filter(year <= year_model_us_max),
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_model_us_min,
    ci       = 0.95)

cost_global_china <- predict_cost(
    model    = lr$model_china,
    data     = data$world %>% filter(year <= year_model_china_max),
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_model_china_min,
    ci       = 0.95)

cost_global_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data$world %>% filter(year <= year_model_germany_max),
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_model_germany_min,
    ci       = 0.95)

# NATIONAL LEARNING ---------------------------------------------------

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Define country capacity data
cap_data_us <- data$us %>% 
    filter(year >= year_model_us_min, year <= year_model_us_max)
cap_data_china <- data$china %>%
    filter(component == "Module") %>% 
    filter(year <= year_model_china_max) %>%
    select(year, cumCapacityKw)
cap_data_germany <- data$germany %>%
    select(year, cumCapacityKw) %>% 
    filter(year <= year_model_germany_max)

# Create national learning capacity data for each country
data_national_us <- makeNationalCapData(
    data_country = cap_data_us,
    data_world   = data$world,
    year_beg   = year_model_us_min)
data_national_china <- makeNationalCapData(
    data_country = cap_data_china,
    data_world   = data$world,
    year_beg   = year_model_china_min)
data_national_germany <- makeNationalCapData(
    data_country = cap_data_germany,
    data_world   = data$world,
    year_beg   = year_model_germany_min)

# Compute cost scenarios by country
cost_national_us <- predict_cost(
    model    = lr$model_us,
    data     = data_national_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_model_us_min,
    ci       = 0.95)

cost_national_china <- predict_cost(
    model    = lr$model_china,
    data     = data_national_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_model_china_min,
    ci       = 0.95)

cost_national_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_national_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_model_germany_min,
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

# Preview results
cost %>% 
    ggplot() + 
    facet_wrap(vars(country)) +
    geom_line(
        aes(x = year, y = cost_per_kw, color = learning)) + 
    geom_ribbon(
        aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub, 
            fill = learning), alpha = 0.22) +
    geom_line(
        data = rbind(
            lr$data_us %>% mutate(country = "U.S."),
            lr$data_china %>% mutate(country = "China"),
            lr$data_germany %>% mutate(country = "Germany")), 
        aes(x = year, y = costPerKw), linetype = 2)

# Calculate savings between national and global learning scenarios

# Combine additional capacity data for each country into one data frame
cap_additions <- rbind(
    mutate(data_national_us, country = "U.S."),
    mutate(data_national_china, country = "China"),
    mutate(data_national_germany, country = "Germany")) %>% 
    select(year, country, cum_cap_addition) %>%
    group_by(country) %>% 
    mutate(ann_cap_addition = cum_cap_addition - lag(cum_cap_addition, 1)) %>% 
    select(year, country, ann_cap_addition) %>% 
    filter(!is.na(ann_cap_addition)) %>%  
    filter(year >= year_savings_min, year <= year_savings_max)

# True historical cost per kW
cost_historical_true <- rbind(
    lr$data_us %>% mutate(country = "U.S."),
    lr$data_china %>% mutate(country = "China"),
    lr$data_germany %>% mutate(country = "Germany"))

savings_mean <- computeSavings(
    cost_national = cost %>% 
        filter(learning == "national") %>% 
        select(year, country, national = cost_per_kw),
    cost_global = cost_historical_true %>% 
        select(year, country, global = costPerKw), 
    cap_additions) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

savings_lb <- computeSavings(
    cost_national = cost %>% 
        filter(learning == "national") %>% 
        select(year, country, national = cost_per_kw_lb),
    cost_global = cost_historical_true %>% 
        select(year, country, global = costPerKw), 
    cap_additions) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

savings_ub <- computeSavings(
    cost_national = cost %>% 
        filter(learning == "national") %>% 
        select(year, country, national = cost_per_kw_ub),
    cost_global = cost_historical_true %>% 
        select(year, country, global = costPerKw), 
    cap_additions) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

# Merge savings 
savings <- rbind(
    mutate(savings_mean, est = "mean"),
    mutate(savings_lb, est = "lb"),
    mutate(savings_ub, est = "ub")) %>%
    filter(!is.na(ann_savings_bil)) %>% 
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
    cost_historical_true = cost_historical_true,
    cost = cost,
    savings = savings),
    dir$historical_scenarios
)
