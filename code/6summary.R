# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

data <- readRDS(dir$data_formatted)

# Global installed capacity over period -----

cum_cap_china <- data$china %>% 
    filter(component == "Module") %>% 
    filter(year == year_model_china_max) %>% 
    pull(cumCapacityKw)
cum_cap_germany <- data$germany %>% 
    filter(year == year_model_germany_max) %>% 
    pull(cumCapacityKw)
cum_cap_us <- data$us %>% 
    filter(year == year_model_us_max) %>% 
    pull(cumCapacityKw)
cum_cap_world <- data$world %>% 
    filter(year == year_model_world_max) %>% 
    pull(cumCapacityKw)

cat(
    "China, Germany, and the U.S. combined comprised",
    scales::percent(
        (cum_cap_china + cum_cap_us + cum_cap_germany) / cum_cap_world
    ),
    "of all global installed PV capacity between 2007 - 2020\n\n"
)

# Learning rates -----

lr <- readRDS(dir$lr_models)
b_us <- coef(lr$model_us)["log(cumCapacityKw)"]
b_china <- coef(lr$model_china)["log(cumCapacityKw)"]
b_germany <- coef(lr$model_germany)["log(cumCapacityKw)"]
cat(
    "Learning rates:\n",
    "U.S.: ", scales::percent(round(1 - 2^b_us, 3)), "\n",
    "China: ", scales::percent(round(1 - 2^b_china, 2)), "\n",
    "Germany: ", scales::percent(round(1 - 2^b_germany, 2)), "\n\n"
)

# Historical cost implications -----

cost <- readRDS(dir$historical_scenarios)

# Comparison of 2020 costs under global vs national learning 
cost_percentage <- cost$cost %>% 
    filter(year == 2020) %>% 
    select(country, learning, cost_per_kw) %>% 
    spread(learning, cost_per_kw) %>% 
    mutate(
        diff = national - global,
        p = scales::percent(diff / global),
        global = scales::dollar(round(global)),
        national = scales::dollar(round(national)),
        costs = paste0(
            country, ": ", p, " (", national, " versus ", global, ")\n"))
    
cat(
    "Prices in 2020 would be this much higher under national learning",    
    "compared to actual 2020 costs:\n\n", cost_percentage$costs, "\n"
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
    "(2008 - 2020, Billions 2020 $USD):\n", savings$savings, "\n"
)



# Future cost projections -----

proj <- readRDS(dir$projection_scenarios)
proj_summary <- proj$base %>% 
    filter(year == year_proj_max) %>% 
    select(-cost_per_kw_lb, -cost_per_kw_ub) %>% 
    spread(key = learning, value = cost_per_kw) %>% 
    arrange(scenario) %>% 
    group_by(scenario) %>% 
    mutate(
        diff = national - global, 
        pDiff = scales::percent(diff / global), 
        cost_summary = paste0(country, ": ", 
            scales::dollar(round(global)), " (global) vs. ",
            scales::dollar(round(national)), " (national)\n"), 
        p_summary = paste0(country, ": ", pDiff, "\n")
    )

cat(
    '"NATIONAL TRENDS" scenario:\n\n',
    'Projected 2030 module costs:\n',
    proj_summary$cost_summary[proj_summary$scenario == "nat_trends"], "\n",
    "% premium under national vs. global learning:\n",
    proj_summary$p_summary[proj_summary$scenario == "nat_trends"], "\n",
    
    '"SUSTAINABLE DEVELOPMENT" scenario:\n\n',
    'Projected 2030 module costs:\n',
    proj_summary$cost_summary[proj_summary$scenario == "sus_dev"], "\n",
    "% premium under national vs. global learning:\n",
    proj_summary$p_summary[proj_summary$scenario == "sus_dev"], "\n\n"
)
