# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr_lambda <- readRDS(dir$lr_models_lambda)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Set beginning values
beg_us <- data$cap_data_us %>%
    filter(year == year_proj_min)
beg_china <- data$cap_data_china %>%
    filter(year == year_proj_min)
beg_germany <- data$cap_data_germany %>%
    filter(year == year_proj_min)

# Naming convention for objects:
#    scenario ("nat_trends" or "sus_dev") +
#    market   ("global" or "national") +
#    country  ("us", "china", or "germany")

# Set global data for each scenario
data_world_nat_trends <- data$proj_nat_trends %>%
  filter(country == "World")
data_world_sus_dev <- data$proj_sus_dev %>%
  filter(country == "World")

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Define country capacity data for each scenario
cap_data_nat_trends_us <- data$proj_nat_trends %>%
  filter(country == "U.S.")
cap_data_sus_dev_us <- data$proj_sus_dev %>%
  filter(country == "U.S.")
cap_data_nat_trends_china <- data$proj_nat_trends %>%
  filter(country == "China")
cap_data_sus_dev_china <- data$proj_sus_dev %>%
  filter(country == "China")
cap_data_nat_trends_germany <- data$proj_nat_trends %>%
  filter(country == "Germany")
cap_data_sus_dev_germany <- data$proj_sus_dev %>%
  filter(country == "Germany")

# Create GLOBAL learning capacity data for each country & scenario
data_global_nat_trends_us <- makeGlobalCapData(
  data_nation = cap_data_nat_trends_us,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_us_min,
  lambda      = lambda_us)
data_global_sus_dev_us <- makeGlobalCapData(
  data_nation = cap_data_sus_dev_us,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_us_min,
  lambda      = lambda_us)

data_global_nat_trends_china <- makeGlobalCapData(
  data_nation = cap_data_nat_trends_china,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_china_min,
  lambda      = lambda_china)
data_global_sus_dev_china <- makeGlobalCapData(
  data_nation = cap_data_sus_dev_china,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_china_min,
  lambda      = lambda_china)

data_global_nat_trends_germany <- makeGlobalCapData(
  data_nation = cap_data_nat_trends_germany,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_germany_min,
  lambda      = lambda_germany)
data_global_sus_dev_germany <- makeGlobalCapData(
  data_nation = cap_data_sus_dev_germany,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_germany_min,
  lambda      = lambda_germany)

# Create NATIONAL learning capacity data for each country & scenario
data_national_nat_trends_us <- makeNationalCapData(
  data_nation = cap_data_nat_trends_us,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_us_min,
  delay_years = delay,
  lambda_start = lambda_us)
data_national_sus_dev_us <- makeNationalCapData(
  data_nation = cap_data_sus_dev_us,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_us_min,
  delay_years = delay,
  lambda_start = lambda_us)

data_national_nat_trends_china <- makeNationalCapData(
  data_nation = cap_data_nat_trends_china,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_china_min,
  delay_years = delay,
  lambda_start = lambda_china)
data_national_sus_dev_china <- makeNationalCapData(
  data_nation = cap_data_sus_dev_china,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_china_min,
  delay_years = delay,
  lambda_start = lambda_china)

data_national_nat_trends_germany <- makeNationalCapData(
  data_nation = cap_data_nat_trends_germany,
  data_world  = data_world_nat_trends,
  year_beg    = year_model_germany_min,
  delay_years = delay,
  lambda_start = lambda_germany)
data_national_sus_dev_germany <- makeNationalCapData(
  data_nation = cap_data_sus_dev_germany,
  data_world  = data_world_sus_dev,
  year_beg    = year_model_germany_min,
  delay_years = delay,
  lambda_start = lambda_germany)

# Compute GLOBAL cost scenarios by country & scenario
proj_nat_trends_global_us <- predict_cost(
  model    = lr$model_us,
  data     = data_global_nat_trends_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_model_us_min,
  ci       = 0.95)
proj_sus_dev_global_us <- predict_cost(
  model    = lr$model_us,
  data     = data_global_sus_dev_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_model_us_min,
  ci       = 0.95)

proj_nat_trends_global_china <- predict_cost(
  model    = lr$model_china,
  data     = data_global_nat_trends_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_model_china_min,
  ci       = 0.95)
proj_sus_dev_global_china <- predict_cost(
  model    = lr$model_china,
  data     = data_global_sus_dev_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_model_china_min,
  ci       = 0.95)

proj_nat_trends_global_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_global_nat_trends_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_model_germany_min,
  ci       = 0.95)
proj_sus_dev_global_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_global_sus_dev_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_model_germany_min,
  ci       = 0.95)


# Compute NATIONAL cost scenarios by country & scenario
proj_nat_trends_national_us <- predict_cost(
  model    = lr$model_us,
  data     = data_national_nat_trends_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_model_us_min,
  ci       = 0.95)
proj_sus_dev_national_us <- predict_cost(
  model    = lr$model_us,
  data     = data_national_sus_dev_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_model_us_min,
  ci       = 0.95)

proj_nat_trends_national_china <- predict_cost(
  model    = lr$model_china,
  data     = data_national_nat_trends_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_model_china_min,
  ci       = 0.95)
proj_sus_dev_national_china <- predict_cost(
  model    = lr$model_china,
  data     = data_national_sus_dev_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_model_china_min,
  ci       = 0.95)

proj_nat_trends_national_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_national_nat_trends_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_model_germany_min,
  ci       = 0.95)
proj_sus_dev_national_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_national_sus_dev_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_model_germany_min,
  ci       = 0.95)

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

# SENSITIVITY CHECK ---------------------------------------------------

# This replicates the same calculations as above, except
# using +/- 25% of the 2020 starting prices

range <- c(0.75, 1.25)
cost_orig_us <- us_beg$costPerKw * range
cost_orig_china <- china_beg$costPerKw * range
cost_orig_germany <- germany_beg$costPerKw * range

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
  dir$projection_scenarios)
