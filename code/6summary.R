# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

data <- readRDS(dir$data_formatted)

# Global installed capacity over period -----

cat(
    "China, Germany, and the U.S. combined comprised",
    data$irenaCumCapacityMw %>% 
        filter(year == year_model_world_max) %>% 
        mutate(result = scales::percent((usa + china + germany) / world)) %>% 
        pull(result),
    "of all global installed PV capacity between 2007 - 2020\n\n"
)

# Learning rates -----

lr <- readRDS(dir$lr_models)
cat(
    "Learning rates:\n",
    "U.S.: ", scales::percent(lr$lr_us), "\n",
    "China: ", scales::percent(lr$lr_china), "\n",
    "Germany: ", scales::percent(lr$lr_germany), "\n\n"
)

# Historical cost implications -----

cost <- readRDS(dir$scenarios_hist)

# Comparison of 2020 costs under global vs national learning 

cat(get_cost_summary_hist(cost$cost))
    
# Savings in each country

cat(get_savings_summary_hist(cost$savings))

# Future cost projections -----

# Projection groth rates (CAGR)

data$rates %>% 
    mutate(rate = scales::percent(rate, accuracy = 1))

proj <- readRDS(dir$scenarios_proj)
cat(get_cost_summary_proj(proj$nat_trends, proj$sus_dev))

# Savings in each country

cat(get_savings_summary_proj(proj$savings_nat_trends, proj$savings_sus_dev))
