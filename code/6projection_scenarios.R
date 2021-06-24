# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Capacity calculations --------------------------------------------------

# Compute annual, linear capacity increase to meet target

# US projections ---

# Install type breakdown taken from 2020 SEIA Capacity shares:
start_year_us <- max(data$seiaCapacity$year)
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
  mutate(price_si = price_si)

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
  mutate(price_si = price_si)

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
  mutate(price_si = price_si)

# # Preview
# ggplot(capacity_germany) +
#   geom_point(aes(x = year, y = cumCapacityKw))

# World projections ---

start_year_world <- max(data$world$year)
num_years_world <- year_max_proj - start_year_world
capacity_world <- data.frame(
  year = seq(start_year_world + 1, year_max_proj),
  price_si = price_si,
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

# Cost calculations ------------------------------------------------------

# Starting COSTS for global versus national learning in each country
costPerKw_start <- cost$cost_scenarios %>%
  filter(year == year_min_proj) %>%
  select(country, scenario, costPerKw = cost_per_kw)
costPerKw_start <- split(costPerKw_start, costPerKw_start$country)
costPerKw_start_us <- split(costPerKw_start$U.S., costPerKw_start$U.S.$scenario)
costPerKw_start_us_national <- costPerKw_start_us$national$costPerKw
costPerKw_start_us_global <- costPerKw_start_us$global$costPerKw
costPerKw_start_us_historical <- getStartingCost(lr$data_us, year_min_proj)
costPerKw_start_china <- split(costPerKw_start$China, costPerKw_start$China$scenario)
costPerKw_start_china_national <- costPerKw_start_china$national$costPerKw
costPerKw_start_china_global <- costPerKw_start_china$global$costPerKw
costPerKw_start_china_historical <- getStartingCost(lr$data_china, year_min_proj)
costPerKw_start_germany <- split(costPerKw_start$Germany, costPerKw_start$Germany$scenario)
costPerKw_start_germany_national <- costPerKw_start_germany$national$costPerKw
costPerKw_start_germany_global <- costPerKw_start_germany$global$costPerKw
costPerKw_start_germany_historical <- getStartingCost(lr$data_germany, year_min_proj)

# Starting CAPACITIES for global versus national learning in each country
cap_beg_us <- getStartingCapcity(capacity_us, year_min_proj)
cap_beg_china <- getStartingCapcity(capacity_china, year_min_proj)
cap_beg_germany <- getStartingCapcity(capacity_germany, year_min_proj)
cap_beg_world <- getStartingCapcity(capacity_world, year_min_proj)

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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
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
  year_min = year_min_proj,
  ci       = 0.95)
# Save all formatted data as a list object ---

saveRDS(list(
  capacity_us               = capacity_us,
  capacity_us_national      = capacity_us_national,
  capacity_china            = capacity_china,
  capacity_china_national   = capacity_china_national,
  capacity_germany          = capacity_germany,
  capacity_germany_national = capacity_germany_national,
  capacity_world            = capacity_world,
  us_global_hist_cost      = us_global_hist_cost,
  us_global_modeled_cost   = us_global_modeled_cost,
  us_national_hist_cost    = us_national_hist_cost,
  us_national_modeled_cost = us_national_modeled_cost,
  china_global_hist_cost      = china_global_hist_cost,
  china_global_modeled_cost   = china_global_modeled_cost,
  china_national_hist_cost    = china_national_hist_cost,
  china_national_modeled_cost = china_national_modeled_cost,
  germany_global_hist_cost      = germany_global_hist_cost,
  germany_global_modeled_cost   = germany_global_modeled_cost,
  germany_national_hist_cost    = germany_national_hist_cost,
  germany_national_modeled_cost = germany_national_modeled_cost),
  dir$projection_scenarios
)
