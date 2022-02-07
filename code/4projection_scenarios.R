# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Naming convention for objects:
#    scenario ("nat_trends" or "sus_dev") +
#    market   ("global" or "national") +
#    country  ("us", "china", or "germany")

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr_lambda <- readRDS(dir$lr_models_lambda)

# Get baseline lambda values from model
params_us <- extract(lr_lambda$fit_us)
params_china <- extract(lr_lambda$fit_china)
params_germany <- extract(lr_lambda$fit_germany)
lambda_us <- mean(params_us$lambda)
lambda_china <- mean(params_china$lambda)
lambda_germany <- mean(params_germany$lambda)
data_us <- lr_lambda$data_us
data_china <- lr_lambda$data_china
data_germany <- lr_lambda$data_germany

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Setup proj data for each country and scenario
data_nat_trends_us <- make_proj_data(data$proj_nat_trends_us)
data_sus_dev_us <- make_proj_data(data$proj_sus_dev_us)

data_nat_trends_china <- make_proj_data(data$proj_nat_trends_china)
data_sus_dev_china <- make_proj_data(data$proj_sus_dev_china)

data_nat_trends_germany <- make_proj_data(data$proj_nat_trends_germany)
data_sus_dev_germany <- make_proj_data(data$proj_sus_dev_germany)

# Set global "delay" variable
# Controls how many years until 100% of national capacity is
# domestically-supplied
delay <- 1

# Set global ci value
ci_all <- 0.95

# Set global final lambda value
lambda_final <- 0.9

# Compute GLOBAL cost scenarios by country & scenario ----

proj_nat_trends_global_us <- project_cost(
    params   = params_us,
    data     = data_nat_trends_us,
    year_beg = year_proj_min,
    ci       = ci_all
)
proj_sus_dev_global_us <- project_cost(
    params   = params_us,
    data     = data_sus_dev_us,
    year_beg = year_proj_min,
    ci       = ci_all
)

proj_nat_trends_global_china <- project_cost(
    params   = params_china,
    data     = data_nat_trends_china,
    year_beg = year_proj_min,
    ci       = ci_all
)
proj_sus_dev_global_china <- project_cost(
    params   = params_china,
    data     = data_sus_dev_china,
    year_beg = year_proj_min,
    ci       = ci_all
)

proj_nat_trends_global_germany <- project_cost(
    params   = params_germany,
    data     = data_nat_trends_germany,
    year_beg = year_proj_min,
    ci       = ci_all
)
proj_sus_dev_global_germany <- project_cost(
    params   = params_germany,
    data     = data_sus_dev_germany,
    year_beg = year_proj_min,
    ci       = ci_all
)



# Compute NATIONAL cost scenarios by country & scenario ----

proj_nat_trends_national_us <- project_cost(
    params   = params_us,
    data     = data_nat_trends_us,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)
proj_sus_dev_national_us <- project_cost(
    params   = params_us,
    data     = data_sus_dev_us,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)

proj_nat_trends_national_china <- project_cost(
    params   = params_china,
    data     = data_nat_trends_china,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)
proj_sus_dev_national_china <- project_cost(
    params   = params_china,
    data     = data_sus_dev_china,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)

proj_nat_trends_national_germany <- project_cost(
    params   = params_germany,
    data     = data_nat_trends_germany,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)
proj_sus_dev_national_germany <- project_cost(
    params   = params_germany,
    data     = data_sus_dev_germany,
    year_beg = year_proj_min,
    ci       = ci_all,
    delay_years = delay,
    lambda_end = lambda_final
)

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

# # SENSITIVITY CHECK ---------------------------------------------------
# 
# # This replicates the same calculations as above, except
# # using +/- 25% of the 2020 starting prices
# 
# range <- c(0.75, 1.25)
# cost_orig_us <- us_beg$costPerKw * range
# cost_orig_china <- china_beg$costPerKw * range
# cost_orig_germany <- germany_beg$costPerKw * range

# # GLOBAL LEARNING ----
# 
# # National trends projections ---
# 
# proj_nat_trends_global_us_lb <- predict_cost(
#   cost_beg = cost_orig_us[1],
#   model = lr$model_us, data = data_world_nat_trends,
#   cap_beg  = us_beg$cumCapacityKw,
#   si_beg = us_beg$price_si, year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_global_us_ub <- predict_cost(
#   cost_beg = cost_orig_us[2],
#   model = lr$model_us, data = data_world_nat_trends,
#   cap_beg  = us_beg$cumCapacityKw,
#   si_beg = us_beg$price_si, year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_global_china_lb <- predict_cost(
#   cost_beg = cost_orig_china[1],
#   model = lr$model_china, data = data_world_nat_trends,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_global_china_ub <- predict_cost(
#   cost_beg = cost_orig_china[2],
#   model = lr$model_china, data = data_world_nat_trends,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_global_germany_lb <- predict_cost(
#   cost_beg = cost_orig_germany[1],
#   model = lr$model_germany, data = data_world_nat_trends,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_global_germany_ub <- predict_cost(
#   cost_beg = cost_orig_germany[2],
#   model = lr$model_germany, data = data_world_nat_trends,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# # Sustainable development projections ---
# 
# proj_sus_dev_global_us_lb <- predict_cost(
#   cost_beg = cost_orig_us[1],
#   model = lr$model_us, data = data_world_sus_dev,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_global_us_ub <- predict_cost(
#   cost_beg = cost_orig_us[2],
#   model = lr$model_us, data = data_world_sus_dev,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_global_china_lb <- predict_cost(
#   cost_beg = cost_orig_china[1],
#   model = lr$model_china, data = data_world_sus_dev,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_global_china_ub <- predict_cost(
#   cost_beg = cost_orig_china[2],
#   model = lr$model_china, data = data_world_sus_dev,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_global_germany_lb <- predict_cost(
#   cost_beg = cost_orig_germany[1],
#   model = lr$model_germany, data = data_world_sus_dev,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_global_germany_ub <- predict_cost(
#   cost_beg = cost_orig_germany[2],
#   model = lr$model_germany, data = data_world_sus_dev,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# # NATIONAL LEARNING ---------------------------------------------------
# 
# # National trends projections ---
# 
# proj_nat_trends_national_us_lb <- predict_cost(
#   cost_beg = cost_orig_us[1],
#   model = lr$model_us, data = data_nat_trends_national_us,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_national_us_ub <- predict_cost(
#   cost_beg = cost_orig_us[2],
#   model = lr$model_us, data = data_nat_trends_national_us,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_national_china_lb <- predict_cost(
#   cost_beg = cost_orig_china[1],
#   model = lr$model_china, data = data_nat_trends_national_china,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_national_china_ub <- predict_cost(
#   cost_beg = cost_orig_china[2],
#   model = lr$model_china, data = data_nat_trends_national_china,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_national_germany_lb <- predict_cost(
#   cost_beg = cost_orig_germany[1],
#   model = lr$model_germany, data = data_nat_trends_national_germany,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_nat_trends_national_germany_ub <- predict_cost(
#   cost_beg = cost_orig_germany[2],
#   model = lr$model_germany, data = data_nat_trends_national_germany,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# # Sustainable development projections ---
# 
# proj_sus_dev_national_us_lb <- predict_cost(
#   cost_beg = cost_orig_us[1],
#   model = lr$model_us, data = data_sus_dev_national_us,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_national_us_ub <- predict_cost(
#   cost_beg = cost_orig_us[2],
#   model = lr$model_us, data = data_sus_dev_national_us,
#   cap_beg = us_beg$cumCapacityKw, si_beg = us_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_national_china_lb <- predict_cost(
#   cost_beg = cost_orig_china[1],
#   model = lr$model_china, data = data_sus_dev_national_china,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_national_china_ub <- predict_cost(
#   cost_beg = cost_orig_china[2],
#   model = lr$model_china, data = data_sus_dev_national_china,
#   cap_beg = china_beg$cumCapacityKw, si_beg = china_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_national_germany_lb <- predict_cost(
#   cost_beg = cost_orig_germany[1],
#   model = lr$model_germany, data = data_sus_dev_national_germany,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# proj_sus_dev_national_germany_ub <- predict_cost(
#   cost_beg = cost_orig_germany[2],
#   model = lr$model_germany, data = data_sus_dev_national_germany,
#   cap_beg = germany_beg$cumCapacityKw, si_beg = germany_beg$price_si,
#   year_beg = year_proj_min, ci = 0.95)
# 
# # Combine Results -----
# 
# projections_lb <- rbind(
#   proj_nat_trends_global_us_lb %>%
#     mutate(
#       country = "U.S.", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_us_lb %>%
#     mutate(
#       country = "U.S.", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_us_lb %>%
#     mutate(
#       country = "U.S.", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_us_lb %>%
#     mutate(
#       country = "U.S.", learning = "national", scenario = "sus_dev"),
#   proj_nat_trends_global_china_lb %>%
#     mutate(
#       country = "China", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_china_lb %>%
#     mutate(
#       country = "China", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_china_lb %>%
#     mutate(
#       country = "China", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_china_lb %>%
#     mutate(
#       country = "China", learning = "national", scenario = "sus_dev"),
#   proj_nat_trends_global_germany_lb %>%
#     mutate(
#       country = "Germany", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_germany_lb %>%
#     mutate(
#       country = "Germany", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_germany_lb %>%
#     mutate(
#       country = "Germany", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_germany_lb %>%
#     mutate(
#       country = "Germany", learning = "national", scenario = "sus_dev"))
# 
# projections_ub <- rbind(
#   proj_nat_trends_global_us_ub %>%
#     mutate(
#       country = "U.S.", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_us_ub %>%
#     mutate(
#       country = "U.S.", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_us_ub %>%
#     mutate(
#       country = "U.S.", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_us_ub %>%
#     mutate(
#       country = "U.S.", learning = "national", scenario = "sus_dev"),
#   proj_nat_trends_global_china_ub %>%
#     mutate(
#       country = "China", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_china_ub %>%
#     mutate(
#       country = "China", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_china_ub %>%
#     mutate(
#       country = "China", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_china_ub %>%
#     mutate(
#       country = "China", learning = "national", scenario = "sus_dev"),
#   proj_nat_trends_global_germany_ub %>%
#     mutate(
#       country = "Germany", learning = "global", scenario = "nat_trends"),
#   proj_sus_dev_global_germany_ub %>%
#     mutate(
#       country = "Germany", learning = "global", scenario = "sus_dev"),
#   proj_nat_trends_national_germany_ub %>%
#     mutate(
#       country = "Germany", learning = "national", scenario = "nat_trends"),
#   proj_sus_dev_national_germany_ub %>%
#     mutate(
#       country = "Germany", learning = "national", scenario = "sus_dev"))

# Save results --------
saveRDS(list(
  base = projections),
  # lb = projections_lb,
  # ub = projections_ub),
  dir$projection_scenarios
)
