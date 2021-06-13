# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# -----------------------------------------------------------------------
# NOTES
#
# Projection range
year_min_proj <- 2018
year_max_proj <- 2030
num_years_proj <- year_max_proj - year_min_proj
#
# Projections to 2030, based on achieving fixed capacity by 2030
#
# Targets:
#
# U.S.:    300 GW, taken from NAM, Committee on Accelerating Decarbonization
#          in the United States (2021)
# China:   570 GW, taken from total renewable goal of 1200 GW in 2030, using
#          same proportions of wind + solar as in 2020
# Germany: 100 GW, from Germany's Renewable Energy Act 2021  
# World:   3100 GW, from WEO 2020 Sustainable Development Scenario

target_capacity_us <- 300*1e6 
target_capacity_china <- 570*1e6 
target_capacity_germany <- 100*1e6 
target_capacity_world <- 3100*1e6

# Assuming silicon prices held constant at 2018 level
price_si <- data$world[which(data$world$year == 2008),]$price_si

# Capacity calculations --------------------------------------------------

# Compute annual, linear capacity increase to meet target

# US projections ---

# Install type breakdown taken from 2020 SEIA Capacity shares:
start_year_us <- year_min_proj
num_years_us <- year_max_proj - start_year_us
capacity_us <- data$seiaCapacity %>%
  filter(year >= year_min_proj) %>%
  rbind(
    data$seiaCapacity %>%
      filter(year == start_year_us) %>%
      mutate(
        shares = cumCapacityKw / sum(cumCapacityKw),
        installType = installType,
        endCapacityKw = shares * target_capacity_us,
        annualCap = (endCapacityKw - cumCapacityKw) / num_years_us) %>%
      select(installType, begCap = cumCapacityKw, annualCap) %>%
      repDf(num_years_us) %>%
      mutate(year = rep(start_year_us + seq(num_years_us), each = 3)) %>%
      group_by(installType) %>%
      mutate(cumCapacityKw = cumsum(annualCap) + begCap) %>%
      select(year, installType, cumCapacityKw)
  ) %>% 
  # Merge install types
  group_by(year) %>% 
  summarise(cumCapacityKw = sum(cumCapacityKw)) %>% 
  ungroup() %>% 
  mutate(price_si = 15.4)

# # Preview
# ggplot(capacity_us) +
#   geom_point(aes(x = year, y = cumCapacityKw))

# China projections ---

start_year_china <- max(data$china$year)
num_years_china <- year_max_proj - start_year_china
capacity_china <- data$china %>%
  filter(year >= year_min_proj, component == "Module") %>%
  select(year, cumCapacityKw) %>%
  rbind(
    data$china %>%
      filter(year == start_year_china, component == "Module") %>%
      mutate(
        endCapacityKw = target_capacity_china,
        annualCap = (endCapacityKw - cumCapacityKw) / num_years_china) %>%
      select(installType, begCap = cumCapacityKw, annualCap) %>%
      repDf(num_years_china) %>%
      mutate(
        year = start_year_china + seq(num_years_china),
        cumCapacityKw = cumsum(annualCap) + begCap) %>%
      select(year, cumCapacityKw)
  ) %>% 
  mutate(price_si = 15.4)

# # Preview
# ggplot(capacity_china) +
#   geom_point(aes(x = year, y = cumCapacityKw))

# Germany projections ---

start_year_germany <- max(data$germany$year)
num_years_germany <- year_max_proj - start_year_germany
capacity_germany <- data$germany %>%
  filter(year >= year_min_proj, component == "Module") %>%
  select(year, cumCapacityKw) %>%
  rbind(
    data$germany %>%
      filter(year == start_year_germany, component == "Module") %>%
      mutate(
        endCapacityKw = target_capacity_germany,
        annualCap = (endCapacityKw - cumCapacityKw) / num_years_germany) %>%
      select(installType, begCap = cumCapacityKw, annualCap) %>%
      repDf(num_years_germany) %>%
      mutate(
        year = start_year_germany + seq(num_years_germany),
        cumCapacityKw = cumsum(annualCap) + begCap) %>%
      select(year, cumCapacityKw)
  ) %>% 
  mutate(price_si = 15.4)

# # Preview
# ggplot(capacity_germany) +
#   geom_point(aes(x = year, y = cumCapacityKw))

# World projections ---

start_year_world <- max(data$world$year)
num_years_world <- year_max_proj - start_year_world
capacity_world <- data.frame(
  year = seq(start_year_world + 1, year_max_proj),
  price_si = 15.4,
  cumCapacityKw = NA)
beg_capacity <- data$world[which(data$world$year == start_year_world),]$cumCapacityKw
annualCap <- (target_capacity_world - beg_capacity) / num_years_world
capacity_world <- data$world %>%
  filter(year >= year_min_proj) %>%
  rbind(
    capacity_world %>%
      mutate(cumCapacityKw = beg_capacity + (year - start_year_world)*annualCap)
  )
  
# # Preview
# ggplot(capacity_world) +
#   geom_point(aes(x = year, y = cumCapacityKw))

world_old <- lr$data_us %>% 
  filter(year < 2018) %>% 
  select(year, price_si, cumCapacityKw)

capacity_us <- rbind(
  data$usSeiaLbnl %>% 
    filter(year < 2018) %>% 
    filter(component == "Module") %>% 
    group_by(year) %>%
    summarise(cumCapacityKw = sum(cumCapacityKw)) %>% 
    left_join(select(world_old, year, price_si)) %>% 
    select(year, price_si, cumCapacityKw),
  capacity_us
) %>% 
  filter(year >= 2008)

capacity_china <- rbind(
  data$china %>% 
    filter(year < 2018) %>% 
    filter(component == "Module") %>% 
    left_join(select(world_old, year, price_si)) %>% 
    select(year, price_si, cumCapacityKw),
  capacity_china
) %>% 
  filter(year >= 2008)

capacity_germany <- rbind(
  data$germany %>% 
    filter(year < 2018) %>% 
    filter(component == "Module") %>% 
    left_join(select(world_old, year, price_si)) %>% 
    select(year, price_si, cumCapacityKw),
  capacity_germany
) %>% 
  filter(year >= 2008)

capacity_world <- rbind(world_old, capacity_world) %>% 
  filter(year >= 2008)

as.data.frame(capacity_us)
as.data.frame(capacity_world)

# Cost calculations ------------------------------------------------------

# Starting COSTS for global versus national learning in each country
costPerKw_start <- cost$cost_scenarios %>%
  filter(year == 2008) %>%
  select(country, scenario, costPerKw = cost_per_kw)
costPerKw_start <- split(costPerKw_start, costPerKw_start$country)
costPerKw_start_us <- split(costPerKw_start$U.S., costPerKw_start$U.S.$scenario)
costPerKw_start_us_national <- costPerKw_start_us$national$costPerKw
costPerKw_start_us_global <- costPerKw_start_us$global$costPerKw
costPerKw_start_us_historical <- getStartingCost(lr$data_us, 2008)
costPerKw_start_china <- split(costPerKw_start$China, costPerKw_start$China$scenario)
costPerKw_start_china_national <- costPerKw_start_china$national$costPerKw
costPerKw_start_china_global <- costPerKw_start_china$global$costPerKw
costPerKw_start_china_historical <- getStartingCost(lr$data_china, 2008)
costPerKw_start_germany <- split(costPerKw_start$Germany, costPerKw_start$Germany$scenario)
costPerKw_start_germany_national <- costPerKw_start_germany$national$costPerKw
costPerKw_start_germany_global <- costPerKw_start_germany$global$costPerKw
costPerKw_start_germany_historical <- getStartingCost(lr$data_germany, 2008)

# Starting CAPACITIES for global versus national learning in each country
cap_beg_us <- getStartingCapcity(capacity_us, 2008)
cap_beg_china <- getStartingCapcity(capacity_china, 2008)
cap_beg_germany <- getStartingCapcity(capacity_germany, 2008)
cap_beg_world <- getStartingCapcity(capacity_world, 2008)

# US projections ---

# Global - Historical costs
# Capacity: world projections from 2018 levels
# Cost: Starting from historical 2018 levels
us_global_hist_cost <- predict_cost(
  model    = lr$model_us,
  data     = capacity_world,
  cost_beg = costPerKw_start_us_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# Global - Modeled costs
# Capacity: world projections from 2018 levels
# Cost: Starting from modeled 2018 levels
us_global_modeled_cost <- predict_cost(
  model    = lr$model_us,
  data     = capacity_world,
  cost_beg = costPerKw_start_us_global,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Historical costs
# Capacity: US projections from 2018 levels
# Cost: Starting from historical 2018 levels
capacity_us_national <- capacity_us %>% 
    mutate(
        cum_cap_addition = cumCapacityKw - cap_beg_us,
        cumCapacityKw = cap_beg_world + cum_cap_addition)
us_national_hist_cost <- predict_cost(
  model    = lr$model_us,
  data     = capacity_us_national,
  cost_beg = costPerKw_start_us_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Modeled costs
# Capacity: US projections from 2018 levels
# Cost: Starting from modeled 2018 levels
us_national_modeled_cost <- predict_cost(
  model    = lr$model_us,
  data     = capacity_us_national,
  cost_beg = costPerKw_start_us_national,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# China projections ---

# Global - Historical costs
# Capacity: world projections from 2018 levels
# Cost: Starting from historical 2018 levels
china_global_hist_cost <- predict_cost(
  model    = lr$model_china,
  data     = capacity_world,
  cost_beg = costPerKw_start_china_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# Global - Modeled costs
# Capacity: world projections from 2018 levels
# Cost: Starting from modeled 2018 levels
china_global_modeled_cost <- predict_cost(
  model    = lr$model_china,
  data     = capacity_world,
  cost_beg = costPerKw_start_china_global,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Historical costs
# Capacity: China projections from 2018 levels
# Cost: Starting from historical 2018 levels
capacity_china_national <- capacity_china %>%
    mutate(
        cum_cap_addition = cumCapacityKw - cap_beg_china,
        cumCapacityKw = cap_beg_world + cum_cap_addition)
china_national_hist_cost <- predict_cost(
  model    = lr$model_china,
  data     = capacity_china_national,
  cost_beg = costPerKw_start_china_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Modeled costs
# Capacity: China projections from 2018 levels
# Cost: Starting from modeled 2018 levels
china_national_modeled_cost <- predict_cost(
  model    = lr$model_china,
  data     = capacity_china_national,
  cost_beg = costPerKw_start_china_national,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# Germany projections ---

# Global - Historical costs
# Capacity: world projections from 2018 levels
# Cost: Starting from historical 2018 levels
germany_global_hist_cost <- predict_cost(
  model    = lr$model_germany,
  data     = capacity_world,
  cost_beg = costPerKw_start_germany_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# Global - Modeled costs
# Capacity: world projections from 2018 levels
# Cost: Starting from modeled 2018 levels
germany_global_modeled_cost <- predict_cost(
  model    = lr$model_germany,
  data     = capacity_world,
  cost_beg = costPerKw_start_germany_global,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Historical costs
# Capacity: Germany projections from 2018 levels
# Cost: Starting from historical 2018 levels
capacity_germany_national <- capacity_germany %>%
    mutate(
        cum_cap_addition = cumCapacityKw - cap_beg_germany,
        cumCapacityKw = cap_beg_world + cum_cap_addition)
germany_national_hist_cost <- predict_cost(
  model    = lr$model_germany,
  data     = capacity_germany_national,
  cost_beg = costPerKw_start_germany_historical,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)

# National - Modeled costs
# Capacity: Germany projections from 2018 levels
# Cost: Starting from modeled 2018 levels
germany_national_modeled_cost <- predict_cost(
  model    = lr$model_germany,
  data     = capacity_germany_national,
  cost_beg = costPerKw_start_germany_national,
  cap_beg  = cap_beg_world,
  si_beg   = price_si,
  year_min = 2008,
  ci       = 0.95)
# Save all formatted data as a list object ---


# Plot 


rbind(
  us_national_modeled_cost %>%
    mutate(scenario = "national", country = "U.S."),
  us_global_modeled_cost %>%
    mutate(scenario = "global", country = "U.S."),
  china_national_modeled_cost %>%
    mutate(scenario = "national", country = "China"),
  china_global_modeled_cost %>%
    mutate(scenario = "global", country = "China"),
  germany_national_modeled_cost %>%
    mutate(scenario = "national", country = "Germany"),
  germany_global_modeled_cost %>%
    mutate(scenario = "global", country = "Germany")) %>%
  mutate(
    scenario = fct_relevel(scenario, c("national", "global")),
    scenario = fct_recode(scenario,
                          "Global learning" = "global",
                          "National learning" = "national"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  # Now add modeled costs
  geom_ribbon(
    aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
        fill = scenario), alpha = 0.25) +
  geom_line(
    aes(x = year, y = cost_per_kw, color = scenario),
    alpha = 0.6, size = 1) +
  facet_wrap(vars(country), nrow = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2007-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  expand_limits(y = 0) +
  scale_color_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  scale_fill_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  theme_minimal_grid(
    font_size = 16,
    font_family = "Fira Sans Condensed") +
  panel_border() +
  theme(
    plot.title.position = "plot",
    legend.position = "right",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(
      size = 0.5, colour = "grey90")
  ) +
  labs(
    title = 'Module Costs Using Global vs. National Learning Rates',
    y = "Cost per kW (2018 $USD)",
    x = "Year")

