# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Format all the data
source(here::here('code', '2format_data.R'))

# Estimate learning models
source(here::here('code', '4learning_curves.R'))

# Compute historical cost scenarios
source(here::here('code', '5historical_scenarios.R'))

# Compute future cost scenarios
source(here::here('code', '6projection_scenarios.R'))

# Make all charts
source(here::here('code', '7charts.R'))
