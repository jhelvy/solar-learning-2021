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
