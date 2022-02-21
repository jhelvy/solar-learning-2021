# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

data <- readRDS(dir$data_formatted)

# # PV production growth rates by country -----
# 
# data$pvProduction %>% 
#     group_by(country) %>% 
#     mutate(
#         growth = production_gw - lag(production_gw, 1),
#         growth = lead(growth, 1), 
#         p_growth = growth / production_gw) %>% 
#     filter(!is.na(p_growth)) %>% 
#     filter(country %in% c("china", "us"))
# 
# # Compute growth rate needed in US to meet domestic demand
# 
# year_start <- 2007
# cap_begin <- data$us %>% 
#     filter(year == year_start) %>% 
#     pull(cumCapacityKw)
# data$us %>% 
#     filter(year > year_start) %>% 
#     mutate(
#         num_years = row_number(),
#         rate = ((cumCapacityKw / cap_begin)^(1 / (num_years))) - 1)
        
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

cost_summary <- get_cost_compare_df_hist(cost$cost)
    
cat(
    "2020 solar PV module prices under national versus global markets scenarios",    
    ":\n\n", cost_summary$summary, "\n", sep = ""
)

# Savings in each country

savings_summary <- get_savings_summary_df_hist(cost$savings)

cat(
    "Historical savings from global over national learning\n",
    "(2008 - 2020, Billions 2020 $USD):\n\n", savings_summary$summary, "\n"
)

# Future cost projections -----

proj <- readRDS(dir$scenarios_proj)
proj_summary <- get_cost_compare_df_proj(proj$nat_trends, proj$sus_dev)

cat(
    "2030 solar PV module prices under national versus global markets scenarios\n\n",
    '"NATIONAL TRENDS" scenario:\n\n',
    proj_summary$summary[proj_summary$scenario == "National Trends"], "\n",
    '"SUSTAINABLE DEVELOPMENT" scenario:\n\n',
    proj_summary$summary[proj_summary$scenario == "Sustainable Development"], "\n",
    sep = ""
)
