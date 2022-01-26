# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_model_us_min)
china_beg <- lr$data_china %>%
    filter(year == year_model_china_min)
germany_beg <- lr$data_germany %>%
    filter(year == year_model_germany_min)
world_beg <- data$world %>%
    filter(year == year_model_world_min)

# Merge together true historical cost per kW
cost_historical_true <- rbind(
    lr$data_us %>% mutate(country = "U.S."),
    lr$data_china %>% mutate(country = "China"),
    lr$data_germany %>% mutate(country = "Germany"))

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Define country capacity data
cap_data_us <- data$us %>%
    filter(year >= year_model_us_min, year <= year_model_us_max)
cap_data_china <- data$china %>%
    filter(component == "Module") %>%
    filter(year <= year_model_china_max) %>%
    select(year, cumCapacityKw)
cap_data_germany <- data$germany %>%
    select(year, cumCapacityKw) %>%
    filter(year <= year_model_germany_max)

# Create GLOBAL learning capacity data for each country
data_global_us <- makeGlobalCapData(
    data_nation = cap_data_us,
    data_world  = data$world,
    year_beg    = year_model_us_min,
    lambda      = 0.1)
data_global_china <- makeGlobalCapData(
    data_nation = cap_data_china,
    data_world  = data$world,
    year_beg    = year_model_china_min,
    lambda      = 0.1)
data_global_germany <- makeGlobalCapData(
    data_nation = cap_data_germany,
    data_world  = data$world,
    year_beg    = year_model_germany_min,
    lambda      = 0.1)

# Create NATIONAL learning capacity data for each country
data_national_us <- makeNationalCapData(
    data_nation = cap_data_us,
    data_world  = data$world,
    year_beg    = year_model_us_min,
    delay_years = delay)
data_national_china <- makeNationalCapData(
    data_nation = cap_data_china,
    data_world  = data$world,
    year_beg    = year_model_china_min,
    delay_years = delay)
data_national_germany <- makeNationalCapData(
    data_nation = cap_data_germany,
    data_world  = data$world,
    year_beg    = year_model_germany_min,
    delay_years = delay)

# # Preview capacity results
# rbind(
#     data_global_us %>%
#         mutate(scenario = "modeled_global", country = "U.S."),
#     data_national_us %>%
#         mutate(scenario = "modeled_national", country = "U.S."),
#     data_global_china %>%
#         mutate(scenario = "modeled_global", country = "China"),
#     data_national_china %>%
#         mutate(scenario = "modeled_national", country = "China"),
#     data_global_germany %>%
#         mutate(scenario = "modeled_global", country = "Germany"),
#     data_national_germany %>%
#         mutate(scenario = "modeled_national", country = "Germany")
#     ) %>%
#     select(year, cumCapacityKw, scenario, country) %>%
#     rbind(
#         cap_data_us %>%
#             select(-costPerKw) %>%
#             mutate(scenario = "historical_national", country = "U.S."),
#         cap_data_china %>%
#             mutate(scenario = "historical_national", country = "China"),
#         cap_data_germany %>%
#             mutate(scenario = "historical_national", country = "Germany")
#     ) %>%
#     select(year, cumCapacityKw, scenario, country) %>%
#     ggplot() +
#     geom_line(
#         aes(
#             x = year,
#             y = cumCapacityKw,
#             group = scenario,
#             color = scenario)) +
#     facet_wrap(vars(country)) +
#     geom_line(
#         data = data$world %>%
#             mutate(scenario = "historical_world"),
#         aes(
#             x = year,
#             y = cumCapacityKw,
#             group = scenario,
#             color = scenario)) +
#     theme_bw()
#
# ggsave("cum_capcity_historical.png", width = 15, height = 5)

# Compute GLOBAL cost scenarios by country
cost_global_us <- predict_cost(
    model    = lr$model_us,
    data     = data_global_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_model_us_min,
    ci       = 0.95)

cost_global_china <- predict_cost(
    model    = lr$model_china,
    data     = data_global_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_model_china_min,
    ci       = 0.95)

cost_global_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_global_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_model_germany_min,
    ci       = 0.95)

# Compute NATIONAL cost scenarios by country
cost_national_us <- predict_cost(
    model    = lr$model_us,
    data     = data_national_us,
    cost_beg = us_beg$costPerKw,
    cap_beg  = us_beg$cumCapacityKw,
    si_beg   = us_beg$price_si,
    year_beg = year_model_us_min,
    ci       = 0.95)

cost_national_china <- predict_cost(
    model    = lr$model_china,
    data     = data_national_china,
    cost_beg = china_beg$costPerKw,
    cap_beg  = china_beg$cumCapacityKw,
    si_beg   = china_beg$price_si,
    year_beg = year_model_china_min,
    ci       = 0.95)

cost_national_germany <- predict_cost(
    model    = lr$model_germany,
    data     = data_national_germany,
    cost_beg = germany_beg$costPerKw,
    cap_beg  = germany_beg$cumCapacityKw,
    si_beg   = germany_beg$price_si,
    year_beg = year_model_germany_min,
    ci       = 0.95)

# Combine Cost Scenarios ----

cost <- rbind(
    mutate(cost_global_us,
           learning = "global", country = "U.S."),
    mutate(cost_national_us,
           learning = "national", country = "U.S."),
    mutate(cost_global_china,
           learning = "global", country = "China"),
    mutate(cost_national_china,
           learning = "national", country = "China"),
    mutate(cost_global_germany,
           learning = "global", country = "Germany"),
    mutate(cost_national_germany,
           learning = "national", country = "Germany")
)

# # Preview results
# cost %>%
#     ggplot() +
#     facet_wrap(vars(country)) +
#     geom_line(
#         aes(x = year, y = cost_per_kw, color = learning)) +
#     geom_ribbon(
#         aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
#             fill = learning), alpha = 0.22) +
#     geom_line(
#         data = rbind(
#             lr$data_us %>% mutate(country = "U.S."),
#             lr$data_china %>% mutate(country = "China"),
#             lr$data_germany %>% mutate(country = "Germany")),
#         aes(x = year, y = costPerKw), linetype = 2) +
#     theme_bw()
# 
# ggsave("cost_historical.png", width = 15, height = 5)

# Calculate savings between national and global learning scenarios

# Compute the additional capacity in each country in each year
cap_additions <- rbind(
    cap_data_us %>% 
        select(year, cumCapacityKw)  %>% 
        mutate(country = "U.S."),
    cap_data_china %>% 
        mutate(country = "China"),
    cap_data_germany %>% 
        mutate(country = "Germany")) %>% 
    group_by(country) %>% 
    mutate(annCapKw_new = cumCapacityKw - lag(cumCapacityKw, 1)) %>% 
    select(year, country, annCapKw_new) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

# Compute price differences with uncertainty
cost_stats <- cost %>% 
    select(-cost_per_kw_lb, -cost_per_kw_ub) %>% 
    pivot_wider(names_from = learning, values_from = cost_per_kw) %>% 
    rename(global_mean = global, national_mean = national) %>% 
    left_join(
        cost %>% 
            mutate(
                sd1 = (cost_per_kw_ub - cost_per_kw) / 2, 
                sd2 = (cost_per_kw - cost_per_kw_lb) / 2, 
                sd  = (sd1 + sd2) / 2) %>% 
            select(year, country, sd, learning) %>% 
            pivot_wider(names_from = learning, values_from = sd) %>% 
            rename(global_sd = global, national_sd = national),
        by = c("year", "country")) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

price_diff <- list()
for (i in 1:nrow(cost_stats)) {
    stats <- cost_stats[i,]
    national <- rnorm(10^4, stats$national_mean, stats$national_sd)
    global <- rnorm(10^4, stats$global_mean, stats$global_sd)
    price_diff[[i]] <- ci(national - global)
}
price_diff <- as.data.frame(do.call(rbind, price_diff))
cost_stats <- cbind(cost_stats, price_diff)

# Compute savings
savings <- cost_stats %>% 
    left_join(cap_additions, by = c("year", "country")) %>% 
    mutate(
        ann_savings_bil = (annCapKw_new * mean) / 10^9,
        ann_savings_bil_lb = (annCapKw_new * low) / 10^9,
        ann_savings_bil_ub = (annCapKw_new * high) / 10^9
    ) %>% 
    select(
        year, country, ann_savings_bil, ann_savings_bil_lb, 
        ann_savings_bil_ub) %>% 
    group_by(country) %>% 
    mutate(
        cum_savings_bil = cumsum(ann_savings_bil), 
        cum_savings_bil_lb = cumsum(ann_savings_bil_lb),
        cum_savings_bil_ub = cumsum(ann_savings_bil_ub)) %>% 
    ungroup() 

# Save outputs ----

saveRDS(list(
    cost_historical_true = cost_historical_true,
    cost = cost,
    savings = savings),
    dir$historical_scenarios
)
