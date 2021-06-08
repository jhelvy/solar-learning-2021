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

# Silicon prices held constant at 2018 level 
price_si <- data$world[which(data$world$year == year_min_proj),]$price_si

# Costs for global versus national learning
costPerKw_start <- cost$cost_scenarios %>% 
    filter(year == year_min_proj) %>% 
    select(country, scenario, costPerKw = cost_per_kw)

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

# Preview
ggplot(capacity_us) +
  geom_point(aes(x = year, y = cumCapacityKw))

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

# Preview
ggplot(capacity_china) +
  geom_point(aes(x = year, y = cumCapacityKw))

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

# Preview
ggplot(capacity_germany) +
  geom_point(aes(x = year, y = cumCapacityKw))

# World projections ---

# Silicon price: Assume constant from 2018
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
  
# Preview
ggplot(capacity_world) +
  geom_point(aes(x = year, y = cumCapacityKw))

# Cost calculations ------------------------------------------------------

# US projections ---

capacity_us_global <- capacity_us %>% 
    mutate(
        costPerKw = costPerKw_start %>% 
            filter(country == "U.S.", scenario == "global") %>% 
            pull(costPerKw))
cost_proj_global_us <- predict_cost(
  model    = lr$model_us,
  data     = capacity_us_global,
  year_min = year_min_proj,
  ci       = 0.95)



# Save all formatted data as a list object ---

saveRDS(list(
  capacity_us      = capacity_us,
  capacity_china   = capacity_china,
  capacity_germany = capacity_germany,
  capacity_world   = capacity_world),
  dir$projection_scenarios
)
