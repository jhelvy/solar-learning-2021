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

# Learning rates:
lr <- readRDS(dir$lr_models)
b_us <- coef(lr$model_us)["log(cumCapacityKw)"]
b_china <- coef(lr$model_china)["log(cumCapacityKw)"]
b_germany <- coef(lr$model_germany)["log(cumCapacityKw)"]
cat(
    "Learning rates:\n",
    "U.S.: ", scales::percent(round(1 - 2^b_us, 3)), "\n",
    "China: ", scales::percent(round(1 - 2^b_china, 2)), "\n",
    "Germany: ", scales::percent(round(1 - 2^b_germany, 2)), "\n"
)

# Historical cost savings
cost <- readRDS(dir$historical_scenarios)
savings <- cost$savings %>% 
    filter(year == max(year)) %>% 
    mutate(savings = paste0(
        country, ": ",
        scales::dollar(round(cum_savings_bil)), " (", 
        scales::dollar(round(cum_savings_bil_lb)), ", ",
        scales::dollar(round(cum_savings_bil_ub)), ")\n"))
cat("Historical savings (Billions 2019 $USD):\n", savings$savings)

# Load projections
proj <- readRDS(dir$projection_scenarios)

