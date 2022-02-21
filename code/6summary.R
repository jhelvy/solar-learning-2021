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

cost_summary <- get_cost_compare_df(cost$cost)
    
cat(
    "2020 solar PV module prices under national versus global markets scenarios",    
    ":\n\n", cost_summary$costs, "\n", sep = ""
)

# Savings in each country

savings <- cost$savings %>% 
    filter(year == max(year)) %>% 
    mutate(savings = paste0(
        country, ": ",
        scales::dollar(round(cum_savings_bil)), " (", 
        scales::dollar(round(cum_savings_bil_lb)), ", ",
        scales::dollar(round(cum_savings_bil_ub)), ")\n"))
cat(
    "Historical savings from global over national learning\n",
    "(2008 - 2020, Billions 2020 $USD):\n\n", savings$savings, "\n"
)

# Future cost projections -----

proj <- readRDS(dir$scenarios_proj)
proj_summary <- rbind(proj$nat_trends, proj$sus_dev) %>% 
    mutate(
        learning = str_to_title(learning),
        learning = fct_relevel(learning, c("National", "Global")),
        scenario = fct_recode(scenario, 
        "National Trends" = "nat_trends", 
        "Sustainable Development" = "sus_dev")
    ) %>% 
    filter(year == year_proj_max) %>% 
    select(year, learning, country, scenario, cost_per_kw) %>% 
    pivot_wider(
        names_from = learning, 
        values_from = cost_per_kw) %>% 
    arrange(scenario) %>% 
    group_by(scenario) %>% 
    mutate(
        diff = National - Global, 
        pDiff = scales::percent(round(diff / Global, 2)), 
        cost_summary = paste0("\t", country, ": ", 
            scales::dollar(round(Global)), " (Global) vs. ",
            scales::dollar(round(National)), " (National)\n"), 
        p_summary = paste0("\t", country, ": ", pDiff, "\n")
    )

cat(
    '"NATIONAL TRENDS" scenario:\n\n',
    'Projected 2030 module costs:\n',
    proj_summary$cost_summary[proj_summary$scenario == "National Trends"], "\n",
    "% premium under national vs. global markets:\n",
    proj_summary$p_summary[proj_summary$scenario == "National Trends"], "\n",
    
    '"SUSTAINABLE DEVELOPMENT" scenario:\n\n',
    'Projected 2030 module costs:\n',
    proj_summary$cost_summary[proj_summary$scenario == "Sustainable Development"], "\n",
    "% premium under national vs. global markets:\n",
    proj_summary$p_summary[proj_summary$scenario == "Sustainable Development"], "\n\n"
)
