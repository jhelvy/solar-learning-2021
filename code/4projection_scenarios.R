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

# Load historical cost scenario data
cost <- readRDS(dir$scenarios_hist)

# Set global baseline "delay" variable
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

# Set lambda values for national markets scenario
lambda_nat_us <- make_lambda_national(
    lambda_start, lambda_end, df_nat_trends_us
)
lambda_nat_china <- make_lambda_national(
    lambda_start, lambda_end, df_nat_trends_china
)
lambda_nat_germany <- make_lambda_national(
    lambda_start, lambda_end, df_nat_trends_germany
)

# Compute GLOBAL cost scenarios by country & scenario ----

proj_nat_trends_global_us <- predict_cost(
    params = params_us,
    df     = df_nat_trends_us,
    lambda = 0)

proj_sus_dev_global_us <- predict_cost(
    params = params_us,
    df     = df_sus_dev_us,
    lambda = 0)

proj_nat_trends_global_china <- predict_cost(
    params = params_china,
    df     = df_nat_trends_china,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

proj_sus_dev_global_china <- predict_cost(
    params = params_china,
    df     = df_sus_dev_china,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

proj_nat_trends_global_germany <- predict_cost(
    params = params_germany,
    df     = df_nat_trends_germany,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

proj_sus_dev_global_germany <- predict_cost(
    params = params_germany,
    df     = df_sus_dev_germany,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

# Compute NATIONAL cost scenarios by country & scenario ----

proj_nat_trends_national_us <- predict_cost(
    params = params_us,
    df     = df_nat_trends_us,
    lambda = lambda_nat_us)

proj_sus_dev_national_us <- predict_cost(
    params = params_us,
    df     = df_sus_dev_us,
    lambda = lambda_nat_us)

proj_nat_trends_national_china <- predict_cost(
    params = params_china,
    df     = df_nat_trends_china,
    lambda = lambda_nat_china) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

proj_sus_dev_national_china <- predict_cost(
    params = params_china,
    df     = df_sus_dev_china,
    lambda = lambda_nat_china) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

proj_nat_trends_national_germany <- predict_cost(
    params = params_germany,
    df     = df_nat_trends_germany,
    lambda = lambda_nat_germany) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

proj_sus_dev_national_germany <- predict_cost(
    params = params_germany,
    df     = df_sus_dev_germany,
    lambda = lambda_nat_germany) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

# Combine Results -----

projections <- rbind(
  proj_nat_trends_global_us %>% 
    mutate(
      country = "U.S.", learning = "global", scenario = "nat_trends"),
  proj_sus_dev_global_us %>%
    mutate(
      country = "U.S.", learning = "global", scenario = "sus_dev"),
  proj_nat_trends_national_us %>%
    mutate(
      country = "U.S.", learning = "national", scenario = "nat_trends"),
  proj_sus_dev_national_us %>%
    mutate(
      country = "U.S.", learning = "national", scenario = "sus_dev"),
  proj_nat_trends_global_china %>%
    mutate(
      country = "China", learning = "global", scenario = "nat_trends"),
  proj_sus_dev_global_china %>%
    mutate(
      country = "China", learning = "global", scenario = "sus_dev"),
  proj_nat_trends_national_china %>%
    mutate(
      country = "China", learning = "national", scenario = "nat_trends"),
  proj_sus_dev_national_china %>%
    mutate(
      country = "China", learning = "national", scenario = "sus_dev"),
  proj_nat_trends_global_germany %>%
    mutate(
      country = "Germany", learning = "global", scenario = "nat_trends"),
  proj_sus_dev_global_germany %>%
    mutate(
      country = "Germany", learning = "global", scenario = "sus_dev"),
  proj_nat_trends_national_germany %>%
    mutate(
      country = "Germany", learning = "national", scenario = "nat_trends"),
  proj_sus_dev_national_germany %>%
    mutate(
      country = "Germany", learning = "national", scenario = "sus_dev"))

# Save results --------
saveRDS(
    list(projections = projections),
    dir$scenarios_proj
)
