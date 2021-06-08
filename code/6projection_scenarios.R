# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

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

# Capacity calculations --------------------------------------------------

# Compute annual, linear capacity increase to meet target

# US projections ---

# install type breakdown taken from 2020 SEIA Capacity shares:
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
  )

# Preview
ggplot(capacity_us) +
  geom_point(aes(x = year, y = cumCapacityKw)) +
  facet_wrap(vars(installType))

# China projections ---

start_year_china <- max(data$china$year)
num_years_china <- year_max_proj - start_year_china
capacity_china <- data$china %>%
  filter(year >= year_min_proj, component == "Module") %>%
  select(year, installType, cumCapacityKw) %>%
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
      select(year, installType, cumCapacityKw)
  )

# Preview
ggplot(capacity_china) +
  geom_point(aes(x = year, y = cumCapacityKw))

# Germany projections ---

start_year_germany <- max(data$germany$year)
num_years_germany <- year_max_proj - start_year_germany
capacity_germany <- data$germany %>%
  filter(year >= year_min_proj, component == "Module") %>%
  select(year, installType, cumCapacityKw) %>%
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
      select(year, installType, cumCapacityKw)
  )

# Preview
ggplot(capacity_germany) +
  geom_point(aes(x = year, y = cumCapacityKw))

# World projections ---

# Silicon price: Assume constant from 2018
start_year_world <- max(world$year)
num_years_world <- year_max_proj - start_year_world
capacity_world <- data.frame(
  year = seq(start_year_world + 1, year_max_proj),
  price_si = silicon[which(silicon$year == year_min_proj),]$price_si,
  cumCapacityKw = NA)
beg_capacity <- world[which(world$year == start_year_world),]$cumCapacityKw
annualCap <- (target_capacity_world - beg_capacity) / num_years_world
capacity_world <- world %>%
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

cost_scenarios_global_us <- predict_cost(
  model    = lr$model_us,
  data     = lr$data_us,
  year_min = year_min,
  ci       = 0.95)




#   US
cap_world_range <- get_country_cap_range(
  data$us2030,
  data$world2030 %>%
    merge(expand.grid(
      installType = unique(lr$us_twoLevel$installType),
      component = unique(lr$us_twoLevel$component))),
  year_min_projection)
us2030_hard_bau_range <- lr$us_twoLevel %>%
  filter(capData == "world") %>%
  cost_constant_cap_range(cap_world_range)
us2030_hard_bau_2f_range <- lr$us_twoFactor %>%
  cost_constant_cap_range_2f(
    merge(
      cap_world_range,
      select(data$world2030, year, price_si)))

# Combine both hard and soft costs together
us2030_bau_range <- rbind(data_projected_us2030, us2030_hard_bau_range)
us2030_bau_2f_range <- rbind(
  us2030_hard_bau_2f_range %>%
    filter(component == "Module"),
  us2030_bau_range %>%
    filter(!(component == "Module"))
) %>%
  distinct(component, installType, year, .keep_all = T)

# S1: Local protectionism - 2030 ----
#   2018-2030 projection given S1 assumptions

## Estimation for hard costs -- learning rates on local cumulative capacity
#   Integrating from cap=world_cap_beg to cap=world_cap_beg + cap_end - cap_beg

#   US
cap_us_world_range <- get_country_world_cap_range(
  data$us2030,
  data$world2030 %>%
    merge(expand.grid(
      installType = unique(lr$us_twoLevel$installType),
      component = unique(lr$us_twoLevel$component))),
  year_min_projection)
us2030_hard_s1_range <- lr$us_twoLevel %>%
  filter(capData == "world") %>%
  cost_constant_cap_range(cap_us_world_range)
us2030_hard_s1_2f_range <- lr$us_twoFactor %>%
  cost_constant_cap_range_2f(
    merge(
      cap_us_world_range,
      select(data$world2030, year, price_si)))

# Combine both hard and soft costs together
us2030_s1_range <- rbind(data_projected_us2030, us2030_hard_s1_range)
us2030_s1_2f_range <- rbind(
  us2030_hard_s1_2f_range %>%
    filter(component == "Module"),
  us2030_s1_range %>%
    filter(!(component == "Module"))
) %>%
  distinct(component,installType,year, .keep_all = T)

# Combine BAU and S1
us2030_2f_range <- rbind(
  us2030_bau_2f_range %>%
    mutate(scenario = 'bau', model = '2factor'),
  us2030_bau_range %>%
    mutate(scenario = 'bau', model = '1factor'),
  us2030_s1_2f_range %>%
    mutate(scenario = 's1', model = '2factor')
)







# Save all formatted data as a list object ---

saveRDS(list(
  capacity_us      = capacity_us,
  capacity_china   = capacity_china,
  capacity_germany = capacity_germany,
  capacity_world   = capacity_world),
  dir$projection_scenarios
)
