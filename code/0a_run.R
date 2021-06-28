# Run full analysis -----

# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Format all the data
source(here::here('code', '1format_data.R'))

# Estimate learning models
source(here::here('code', '3learning_curves.R'))

# Compute historical cost scenarios
source(here::here('code', '4historical_scenarios.R'))
 
# Compute future cost scenarios
source(here::here('code', '5projection_scenarios.R'))

# Make all charts
source(here::here('code', '6charts.R'))

# Summaries of results -----

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Load projections
proj <- readRDS(dir$projection_scenarios)

# Learning rates:
cat(
    "Learning rates:\n",
    "U.S.: ", 1 - 2^coef(lr$model_us)["log(cumCapacityKw)"], "\n",
    "China: ", 1 - 2^coef(lr$model_china)["log(cumCapacityKw)"], "\n",
    "Germany: ", 1 - 2^coef(lr$model_germany)["log(cumCapacityKw)"], "\n"
)
