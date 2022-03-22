# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Naming convention for objects:
#    scenario ("nat_trends" or "sus_dev") +
#    market   ("global" or "national") +
#    country  ("us", "china", or "germany")

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Set baseline "delay" variable
# Controls how many years until 100% of national capacity is
# domestically-supplied
delay <- 10

# Set starting and ending lambda values for national market scenarios
lambda_start <- 0
lambda_end <- 1

# Set params data frames
params_us <- lr$params_us
params_china <- lr$params_china
params_germany <- lr$params_germany

# Setup proj df for each country and scenario
df_nat_trends_us <- data$proj_nat_trends_us
df_sus_dev_us <- data$proj_sus_dev_us
df_nat_trends_china <- data$proj_nat_trends_china
df_sus_dev_china <- data$proj_sus_dev_china
df_nat_trends_germany <- data$proj_nat_trends_germany
df_sus_dev_germany <- data$proj_sus_dev_germany

# Merge together for each projection scenario
df_nat_trends <- rbind(
  df_nat_trends_us %>% mutate(country = "U.S."),
  df_nat_trends_china %>% mutate(country = "China"),
  df_nat_trends_germany %>% mutate(country = "Germany")
)
df_sus_dev <- rbind(
  df_sus_dev_us %>% mutate(country = "U.S."),
  df_sus_dev_china %>% mutate(country = "China"),
  df_sus_dev_germany %>% mutate(country = "Germany")
)

# Compute the additional capacity in each country in each year
cap_additions_nat_trends <- df_nat_trends %>%
    select(year, country, annCapKw_nation) %>%
    filter(year >= year_proj_min, year <= year_proj_max)

cap_additions_sus_dev <- df_sus_dev %>%
    select(year, country, annCapKw_nation) %>%
    filter(year >= year_proj_min, year <= year_proj_max)

# Set lambda values for national markets scenario
lambda_nat_us <- make_lambda_national(
    lambda_start, lambda_end, delay, df_nat_trends_us)
lambda_nat_china <- make_lambda_national(
    lambda_start, lambda_end, delay, df_nat_trends_china)
lambda_nat_germany <- make_lambda_national(
    lambda_start, lambda_end, delay, df_nat_trends_germany)

# Set exchange rates
er_us <- 1
er_china <- data$exchangeRatesRMB %>%
    filter(year == year_proj_min) %>%
    pull(average_of_rate)
er_germany <- data$exchangeRatesEUR %>%
    filter(year == year_proj_min) %>%
    pull(average_of_rate)

# Compute GLOBAL cost scenarios by country & scenario ----

proj_nat_trends_global_us <- predict_cost(
    params = params_us,
    df     = df_nat_trends_us,
    lambda = 0,
    exchange_rate = er_us)

proj_sus_dev_global_us <- predict_cost(
    params = params_us,
    df     = df_sus_dev_us,
    lambda = 0,
    exchange_rate = er_us)

proj_nat_trends_global_china <- predict_cost(
    params = params_china,
    df     = df_nat_trends_china,
    lambda = 0,
    exchange_rate = er_china)

proj_sus_dev_global_china <- predict_cost(
    params = params_china,
    df     = df_sus_dev_china,
    lambda = 0,
    exchange_rate = er_china)

proj_nat_trends_global_germany <- predict_cost(
    params = params_germany,
    df     = df_nat_trends_germany,
    lambda = 0,
    exchange_rate = er_germany)

proj_sus_dev_global_germany <- predict_cost(
    params = params_germany,
    df     = df_sus_dev_germany,
    lambda = 0,
    exchange_rate = er_germany)

# Compute NATIONAL cost scenarios by country & scenario ----

proj_nat_trends_national_us <- predict_cost(
    params = params_us,
    df     = df_nat_trends_us,
    lambda = lambda_nat_us,
    exchange_rate = er_us)

proj_sus_dev_national_us <- predict_cost(
    params = params_us,
    df     = df_sus_dev_us,
    lambda = lambda_nat_us,
    exchange_rate = er_us)

proj_nat_trends_national_china <- predict_cost(
    params = params_china,
    df     = df_nat_trends_china,
    lambda = lambda_nat_china,
    exchange_rate = er_china)

proj_sus_dev_national_china <- predict_cost(
    params = params_china,
    df     = df_sus_dev_china,
    lambda = lambda_nat_china,
    exchange_rate = er_china)

proj_nat_trends_national_germany <- predict_cost(
    params = params_germany,
    df     = df_nat_trends_germany,
    lambda = lambda_nat_germany,
    exchange_rate = er_germany)

proj_sus_dev_national_germany <- predict_cost(
    params = params_germany,
    df     = df_sus_dev_germany,
    lambda = lambda_nat_germany,
    exchange_rate = er_germany)

# Preview results

nat_trends <- combine(
    global_us = proj_nat_trends_global_us,
    national_us = proj_nat_trends_national_us,
    global_china = proj_nat_trends_global_china,
    national_china = proj_nat_trends_national_china,
    global_germany = proj_nat_trends_global_germany,
    national_germany = proj_nat_trends_national_germany) %>%
    mutate(scenario = "nat_trends")

sus_dev <- combine(
    global_us = proj_sus_dev_global_us,
    national_us = proj_sus_dev_national_us,
    global_china = proj_sus_dev_global_china,
    national_china = proj_sus_dev_national_china,
    global_germany = proj_sus_dev_global_germany,
    national_germany = proj_sus_dev_national_germany) %>%
    mutate(scenario = "sus_dev")

make_projection_plot(nat_trends, sus_dev)
make_projection_plot(nat_trends, sus_dev, log_scale = TRUE)

# Calculate savings between national and global markets

# First, compute the cost difference CIs for each country & scenario

# National Trends

cost_diff_nat_trends_us <- compute_cost_diff(
    params     = params_us,
    df         = df_nat_trends_us,
    lambda_nat = lambda_nat_us,
    exchange_rate = er_us)

cost_diff_nat_trends_china <- compute_cost_diff(
    params     = params_china,
    df         = df_nat_trends_china,
    lambda_nat = lambda_nat_china,
    exchange_rate = er_china)

cost_diff_nat_trends_germany <- compute_cost_diff(
    params     = params_germany,
    df         = df_nat_trends_germany,
    lambda_nat = lambda_nat_germany,
    exchange_rate = er_germany)

cost_diffs_nat_trends <- combine_cost_diffs(
    us = cost_diff_nat_trends_us,
    china = cost_diff_nat_trends_china,
    germany = cost_diff_nat_trends_germany,
    year_min = year_proj_min,
    year_max = year_proj_max)

# Sustainable Development

cost_diff_sus_dev_us <- compute_cost_diff(
    params     = params_us,
    df         = df_sus_dev_us,
    lambda_nat = lambda_nat_us,
    exchange_rate = er_us)

cost_diff_sus_dev_china <- compute_cost_diff(
    params     = params_china,
    df         = df_sus_dev_china,
    lambda_nat = lambda_nat_china,
    exchange_rate = er_china)

cost_diff_sus_dev_germany <- compute_cost_diff(
    params     = params_germany,
    df         = df_sus_dev_germany,
    lambda_nat = lambda_nat_germany,
    exchange_rate = er_germany)

cost_diffs_sus_dev <- combine_cost_diffs(
    us = cost_diff_sus_dev_us,
    china = cost_diff_sus_dev_china,
    germany = cost_diff_sus_dev_germany,
    year_min = year_proj_min,
    year_max = year_proj_max)

# Then compute savings

savings_nat_trends <- compute_savings(
    cost_diffs_nat_trends, cap_additions_nat_trends)

savings_sus_dev <- compute_savings(
    cost_diffs_sus_dev, cap_additions_sus_dev)

# Save results --------

saveRDS(list(
    nat_trends = nat_trends,
    sus_dev    = sus_dev, 
    savings_nat_trends = savings_nat_trends, 
    savings_sus_dev = savings_sus_dev),
    dir$scenarios_proj
)
