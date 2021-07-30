# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

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

# Historical cost savings -----

cost <- readRDS(dir$historical_scenarios)
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
proj_summary <- proj %>% 
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
    
