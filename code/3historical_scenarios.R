# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)
lr_stan <- readRDS(dir$lr_models_stan)

# Get baseline lambda values from model
params_us <- extract(lr_stan$fit_us)
params_china <- extract(lr_stan$fit_china)
params_germany <- extract(lr_stan$fit_germany)
lambda_us <- mean(params_us$lambda)
lambda_china <- mean(params_china$lambda)
lambda_germany <- mean(params_germany$lambda)
data_us <- lr_stan$data_us
data_china <- lr_stan$data_china
data_germany <- lr_stan$data_germany

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_model_us_min)
china_beg <- lr$data_china %>%
    filter(year == year_model_china_min)
germany_beg <- lr$data_germany %>%
    filter(year == year_model_germany_min)
world_beg <- data$world %>%
    filter(year == year_model_world_min)

# Merge together true historical cost per kW
cost_historical_true <- rbind(
    lr$data_us %>% mutate(country = "U.S."),
    lr$data_china %>% mutate(country = "China"),
    lr$data_germany %>% mutate(country = "Germany"))

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Set global "delay" variable 
# Controls how many years until 100% of national capacity is 
# domestically-supplied
delay <- 6

# Set global ci value
ci_all <- 0.95

# Set global final lambda value
lambda_final <- 0.9

# Compute GLOBAL cost scenarios by country
cost_global_us <- predict_cost(
    params   = params_us,
    data     = data_us,
    year_beg = year_model_us_min,
    ci       = ci_all)

cost_global_china <- predict_cost(
    params   = params_china,
    data     = data_china,
    year_beg = year_model_china_min,
    ci       = ci_all)

cost_global_germany <- predict_cost(
    params   = params_germany,
    data     = data_germany,
    year_beg = year_model_germany_min,
    ci       = ci_all)

# Compute NATIONAL cost scenarios by country
cost_national_us <- predict_cost(
    params   = params_us,
    data     = data_us,
    year_beg = year_model_us_min,
    ci       = ci_all, 
    delay_years = delay, 
    lambda_end = lambda_final)

cost_national_china <- predict_cost(
    params   = params_china,
    data     = data_china,
    year_beg = year_model_china_min,
    ci       = ci_all, 
    delay_years = delay, 
    lambda_end = lambda_final)

cost_national_germany <- predict_cost(
    params   = params_germany,
    data     = data_germany,
    year_beg = year_model_germany_min,
    ci       = ci_all, 
    delay_years = delay, 
    lambda_end = lambda_final)

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
        aes(x = year, y = costPerKw), linetype = 2) +
    theme_bw() +
    scale_y_log10()

ggsave("cost_historical.png", width = 15, height = 5)

# Calculate savings between national and global learning scenarios

# First, compute the cost difference CIs for each country

cost_diff_us <- compute_cost_diff(
    params   = params_us,
    data     = data_us,
    year_beg = year_model_us_min,
    ci       = ci_all,
    delay_years = 10,
    lambda_end = 0.9)

cost_diff_china <- compute_cost_diff(
    params   = params_china,
    data     = data_china,
    year_beg = year_model_china_min,
    ci       = ci_all,
    delay_years = 10,
    lambda_end = 0.9)

cost_diff_germany <- compute_cost_diff(
    params   = params_germany,
    data     = data_germany,
    year_beg = year_model_germany_min,
    ci       = ci_all,
    delay_years = 10,
    lambda_end = 0.9)

# Compute the additional capacity in each country in each year
cap_additions <- rbind(
    cap_data_us %>%
        select(year, cumCapacityKw)  %>%
        mutate(country = "U.S."),
    cap_data_china %>%
        mutate(country = "China"),
    cap_data_germany %>%
        mutate(country = "Germany")) %>%
    group_by(country) %>%
    mutate(annCapKw_new = cumCapacityKw - lag(cumCapacityKw, 1)) %>%
    select(year, country, annCapKw_new) %>%
    filter(year >= year_savings_min, year <= year_savings_max)

# Compute savings
savings <- cost_stats %>%
    left_join(cap_additions, by = c("year", "country")) %>%
    mutate(
        ann_savings_bil = (annCapKw_new * mean) / 10^9,
        ann_savings_bil_lb = (annCapKw_new * low) / 10^9,
        ann_savings_bil_ub = (annCapKw_new * high) / 10^9
    ) %>%
    select(
        year, country, ann_savings_bil, ann_savings_bil_lb,
        ann_savings_bil_ub) %>%
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
