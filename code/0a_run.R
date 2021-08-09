# Reproduce full analysis

# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Format all the data
source(here::here('code', '1format_data.R'))

# Estimate learning models
source(here::here('code', '2learning_curves.R'))

# Compute historical cost scenarios
source(here::here('code', '3historical_scenarios.R'))
 
# Compute future cost scenarios
source(here::here('code', '4projection_scenarios.R'))

# Make all charts
source(here::here('code', '5charts.R'))

# Print summary of all results
source(here::here('code', '6summary.R'))

# Render summary tables
rmarkdown::render(
    here::here('code', '7tables.Rmd'), 
    output_file = here::here('output', 'tables.docx'))
