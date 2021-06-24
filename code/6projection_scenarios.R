# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Projection range
year_min_proj <- 2018
year_max_proj <- 2030

# GLOBAL LEARNING ---------------------------------------------------

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_min_proj)
china_beg <- lr$data_china %>%
    filter(year == year_min_proj)
germany_beg <- lr$data_germany %>%
    filter(year == year_min_proj)

# National trends projections ---

proj_nat_trends_global_us <- predict_cost(
  model    = lr$model_us,
  data     = data$proj_nat_trends %>% filter(country == "World"),
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_nat_trends_global_china <- predict_cost(
  model    = lr$model_china,
  data     = data$proj_nat_trends %>% filter(country == "World"),
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_nat_trends_global_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data$proj_nat_trends %>% filter(country == "World"),
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

# Sustainable development projections ---

proj_sus_dev_global_us <- predict_cost(
  model    = lr$model_us,
  data     = data$proj_sus_dev %>% filter(country == "World"),
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_sus_dev_global_china <- predict_cost(
  model    = lr$model_china,
  data     = data$proj_sus_dev %>% filter(country == "World"),
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_sus_dev_global_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data$proj_sus_dev %>% filter(country == "World"),
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

# NATIONAL LEARNING ---------------------------------------------------










# Save all formatted data as a list object ---

saveRDS(list(
  capacity_us               = capacity_us,
  capacity_us_national      = capacity_us_national,
  capacity_china            = capacity_china,
  capacity_china_national   = capacity_china_national,
  capacity_germany          = capacity_germany,
  capacity_germany_national = capacity_germany_national,
  capacity_world            = capacity_world,
  us_global_hist_cost      = us_global_hist_cost,
  us_global_modeled_cost   = us_global_modeled_cost,
  us_national_hist_cost    = us_national_hist_cost,
  us_national_modeled_cost = us_national_modeled_cost,
  china_global_hist_cost      = china_global_hist_cost,
  china_global_modeled_cost   = china_global_modeled_cost,
  china_national_hist_cost    = china_national_hist_cost,
  china_national_modeled_cost = china_national_modeled_cost,
  germany_global_hist_cost      = germany_global_hist_cost,
  germany_global_modeled_cost   = germany_global_modeled_cost,
  germany_national_hist_cost    = germany_national_hist_cost,
  germany_national_modeled_cost = germany_national_modeled_cost),
  dir$projection_scenarios
)
