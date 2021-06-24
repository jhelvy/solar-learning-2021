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

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_min_proj)
china_beg <- lr$data_china %>%
    filter(year == year_min_proj)
germany_beg <- lr$data_germany %>%
    filter(year == year_min_proj)

# Naming convention for objects is:
#    scenario ("nat_trends" or "sus_dev") +
#    learning ("global" or "national") +
#    country ("us", "china", or "germany")

# GLOBAL LEARNING ---------------------------------------------------

# Learning rates based on world cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

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

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Create national learning data for each country
data_nat_trends_national_us <- makeNationalCapData(
    df_country = data$proj_nat_trends %>% filter(country == "U.S."),
    df_world   = data$proj_nat_trends %>% filter(country == "World"),
    year_beg   = year_min_proj)
data_sus_dev_national_us <- makeNationalCapData(
    df_country = data$proj_sus_dev %>% filter(country == "U.S."),
    df_world   = data$proj_sus_dev %>% filter(country == "World"),
    year_beg   = year_min_proj)
data_nat_trends_national_china <- makeNationalCapData(
    df_country = data$proj_nat_trends %>% filter(country == "China"),
    df_world   = data$proj_nat_trends %>% filter(country == "World"),
    year_beg   = year_min_proj)
data_sus_dev_national_china <- makeNationalCapData(
    df_country = data$proj_sus_dev %>% filter(country == "China"),
    df_world   = data$proj_sus_dev %>% filter(country == "World"),
    year_beg   = year_min_proj)
data_nat_trends_national_germany <- makeNationalCapData(
    df_country = data$proj_nat_trends %>% filter(country == "Germany"),
    df_world   = data$proj_nat_trends %>% filter(country == "World"),
    year_beg   = year_min_proj)
data_sus_dev_national_germany <- makeNationalCapData(
    df_country = data$proj_sus_dev %>% filter(country == "Germany"),
    df_world   = data$proj_sus_dev %>% filter(country == "World"),
    year_beg   = year_min_proj)

# National trends projections ---

proj_nat_trends_national_us <- predict_cost(
  model    = lr$model_us,
  data     = data_nat_trends_national_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_nat_trends_national_china <- predict_cost(
  model    = lr$model_china,
  data     = data_nat_trends_national_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_nat_trends_national_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_nat_trends_national_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

# Sustainable development projections ---

proj_sus_dev_national_us <- predict_cost(
  model    = lr$model_us,
  data     = data_sus_dev_national_us,
  cost_beg = us_beg$costPerKw,
  cap_beg  = us_beg$cumCapacityKw,
  si_beg   = us_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_sus_dev_national_china <- predict_cost(
  model    = lr$model_china,
  data     = data_sus_dev_national_china,
  cost_beg = china_beg$costPerKw,
  cap_beg  = china_beg$cumCapacityKw,
  si_beg   = china_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

proj_sus_dev_national_germany <- predict_cost(
  model    = lr$model_germany,
  data     = data_sus_dev_national_germany,
  cost_beg = germany_beg$costPerKw,
  cap_beg  = germany_beg$cumCapacityKw,
  si_beg   = germany_beg$price_si,
  year_beg = year_min_proj,
  ci       = 0.95)

# FORMATING -------------------------------------------------------

projections
  proj_nat_trends_global_us %>% 
    mutate(
      country = "U.S.", 
      scenario = "global", 
      
    )
proj_nat_trends_global_china
proj_nat_trends_global_germany
proj_sus_dev_global_us
proj_sus_dev_global_china
proj_sus_dev_global_germany

proj_nat_trends_national_us
proj_nat_trends_national_china
proj_nat_trends_national_germany
proj_sus_dev_national_us
proj_sus_dev_national_china
proj_sus_dev_national_germany

# Save all formatted data as a list object ---

saveRDS(list(
  dir$projection_scenarios
)
