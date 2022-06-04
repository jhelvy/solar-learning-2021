# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Notes -------------------------------------------------------

# This file reads in and standardizes the formatting of all cost
# and capacity data for the US, China, Germany, and World.
# In some cases, only cost or capacity data are available.
#
# COST:
# costPerKw     = The cost per installed Kw
# component     = "BOS", "Inverter", "Module", "Labor", "Other"
# componentType = "Hard", "Soft"
#
# Categories of each component by type:
components <- tribble(
  ~"component",   ~"componentType",
  "BOS_Inverter", "Other",
  "Inverter",     "Hard",
  "Module",       "Hard",
  "BOS",          "Soft",
  "Labor",        "Soft",
  "Other",        "Soft")

# In some cases, the components aren't broken down to this
# degree and might only distinguish between "hard" and "soft"
#
# CAPACITY:
# installType   = "Residential", "Commercial", "Utility"
# cumCapacityKw = The cumulative installed capacity, in Kw

# Set paths to data files----------------------------------------------------

usNrel2018FilePath <- file.path(
  dir$data, "nrel",
"Data File (U.S. Solar Photovoltaic System Cost Benchmark Q1 2018 Report).xlsx")
usNrel2020FilePath <- file.path(
  dir$data, "nrel", "Data File (U.S. Solar Photovoltaic  BESS System Cost Benchmark Q1 2020 Report).xlsx")
usLbnlFilePath <- file.path(
  dir$data, "lbnl", "tts_2019_summary_data_tables_0.xlsx")
usSeiaEarlyFilePath <- file.path(dir$data, "seia", "seiaEarlyYears.csv")
usSeiaFilePath <- file.path(dir$data, "seia", "seiaCapacity.json")
germanyFilePath <- file.path(dir$data, "germany", "fraunhofer_fig2.csv")
germany2020FilePath <- file.path(dir$data, "germany", "Fig2-2020.csv")
chinaFilePath <- file.path(dir$data, "china", "wang_ndrc_data.csv")
irenaCapFilePath <- file.path(dir$data, "irena", "irenaCumCapacityMw.csv")
worldSpvFilePath <- file.path(dir$data, "spv", "Table 4.2.xlsx")
siliconFilePath <- file.path(dir$data, "nemet_silicon.csv")
exchangeRatesPath <- file.path(dir$data, "exchange-rates.xlsx")
productionFilePath <- file.path(dir$data, "production", "production.csv")
inflationPath <- file.path(dir$data, "inflation.Rds")
shipmentsPath <- file.path(dir$data, "top-manufactuer-shipment.xlsx")

# Load exchange rates --------------------------------------------------

exchangeRatesRMB <- read_excel(
  exchangeRatesPath, sheet = "usd-rmb", skip = 2) %>%
  clean_names() %>%
  rename(year = row_labels) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))
    
exchangeRatesEUR <- read_excel(
  exchangeRatesPath, sheet = "usd-euro", skip = 2) %>%
  clean_names() %>%
  rename(year = row_labels) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

# # Get inflation and country data frames from priceR package
# inflation_df <- priceR::retrieve_inflation_data(country = "US")
# countries_df <- priceR::show_countries()
# # Save
# saveRDS(list(
#   inflation_df = inflation_df, countries_df = countries_df),
#   inflationPath
# )

# Read in from previously-saved values
inflation <- readRDS(inflationPath)





# Format PV production data ----------------------------------------------------

# Read in & format 2010 to 2019 data (Production in GW) from Engauge Digitizer
pvProduction <- read_csv(productionFilePath) %>% 
  clean_names() %>% 
  rename(year = x) %>% 
  mutate(year = floor(year) + 1) %>% 
  gather(key = "country", value = "n", china:total) %>% 
  group_by(year, country) %>% 
  summarise(production_gw = mean(n)) %>% 
  spread(key = country, value = production_gw) %>% 
  mutate(
    taiwan = china_taiwan - china, 
    europe = china_taiwan_europe - china_taiwan, 
    japan = china_taiwan_europe_japan - china_taiwan_europe, 
    malaysia = china_taiwan_europe_japan_malaysia - china_taiwan_europe_japan, 
    us = china_taiwan_europe_japan_malaysia_us - china_taiwan_europe_japan_malaysia, 
    row = total - china_taiwan_europe_japan_malaysia_us) %>% 
  select(year, china, taiwan, europe, japan, malaysia, us, row) %>% 
  ungroup() %>% 
  gather(key = "country", value = "production_gw", china:row) 

# Format shipments data 

shipments <- read_excel(shipmentsPath, skip = 1) %>% 
    clean_names()
shipments





# U.S. ----

# U.S. SEIA capacity data ---------------------------------------------

# Capacity data from 2000 - 2013 are from this image in the raw data:
# seia_2013_year_review_fig_2.1.jpg
seiaEarlyYears <- read_csv(usSeiaEarlyFilePath) %>% 
  gather(
    key = "installType", value = "capacityMw",
    Residential:Utility) %>% 
  select(year, capacityMw, installType) %>%
  group_by(installType) %>%
  mutate(cumCapacityMw = cumsum(capacityMw)) %>%
  ungroup()

# Capacity from 2005 - 2020 are from the chart on this html page in the # raw data folder: seiaCapacityChartSource.html
seiaRaw <- map(fromJSON(file = usSeiaFilePath), data.frame)
seiaCapacity <- do.call(rbind, seiaRaw)
seiaCapacity$installType <- rep(c(
  "Residential", "Commercial", "Community Solar", "Utility", "CSP"),
  length(seiaRaw))
seiaCapacity <- as_tibble(seiaCapacity) %>%
  rename(year = label, cumCapacityMw = value) %>%
  mutate(
    year = parse_number(as.character(year)),
    cumCapacityMw = parse_number(as.character(cumCapacityMw))
  ) %>%
  filter(!installType %in% c("CSP", "Community Solar"))

# Compare the hand-copied data to that from the json file
seiaEarlyYears %>%
  select(year, installType, cumCapacityMw1 = cumCapacityMw) %>%
  full_join(seiaCapacity, by = c("year", "installType")) %>%
  arrange(installType, year) %>%
  as.data.frame()
# Looks good - the two data sets line up pretty well over common years

# Add in years 2000 - 2004, hand-copied from the 2013 SEIA report
seiaCapacity <- seiaCapacity %>%
  rbind(
    seiaEarlyYears %>%
      filter(year < 2007) %>%
      select(year, cumCapacityMw, installType)) %>%
  mutate(cumCapacityKw = cumCapacityMw * 1000) %>%
  # Can't have 0 when taking log
  mutate(cumCapacityKw = ifelse(cumCapacityKw == 0, 1, cumCapacityKw)) %>%
  select(year, installType, cumCapacityKw) %>%
  arrange(year)

# U.S. LBNL cost data -----------------------------------------------

# LBNL cost data are in 2018 dollars
lbnlCost <- read_excel(usLbnlFilePath, sheet = "Fig 17", skip = 3) %>%
  clean_names() %>%
  rename(
    soft_cost_residential = residential,
    soft_cost_commercial = small_non_residential,
    soft_cost_utility = large_non_residential
  ) %>%
  gather(
    key = "component", 
    value = "costPerW", 
    module_price_index:soft_cost_utility) %>%
  mutate(
    component = str_to_title(str_replace(component, "_price_index", "")),
    costPerW = str_replace(costPerW, "-", ""),
    costPerW = as.numeric(costPerW),
    costPerKw = costPerW * 1000,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw,
      from_date = 2018,
      country = "US",
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df)) %>%
  select(year = x1, component, costPerKw)

# Separate out "soft" cost and installType
lbnlCost_soft <- lbnlCost %>%
  filter(str_detect(component, "Soft_")) %>%
  separate(
    component, into = c("d1", "d2", "installType"), sep = "_") %>%
  mutate(
    component = "Other",
    componentType = "Soft",
    installType = str_to_title(installType)) %>%
  select(year, component, componentType, installType, costPerKw) %>% 
  mutate(
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = 2018, 
      country = "US", 
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df))
lbnlCost_hard <- lbnlCost %>%
  filter(! str_detect(component, "Soft_")) %>%
  mutate(
    component = ifelse(
      component == "Inverter_residential", "Inverter", component),
    componentType = "Hard") %>%
    merge(expand.grid(installType = unique(lbnlCost_soft$installType))) %>%
  select(year, component, componentType, installType, costPerKw)
lbnlCost <- rbind(lbnlCost_hard, lbnlCost_soft) %>%
  filter(!is.na(costPerKw))

# U.S. NREL cost data -------------------------------------------------

# NREL cost data are in 2019 dollars
nrelCost <- read_excel(
  usNrel2020FilePath, sheet = "Figures ES-1 and ES-2", skip = 1) %>%
  clean_names() %>%
  rename(component = x1) %>%
  select(-c(x24, x36)) %>% 
  slice(1:2) %>%
  gather(key = "year", value = "cost", -component) %>% 
  mutate(year = str_replace(year, "x", "")) %>% 
  separate(year, into = c("year", "drop"), sep = "_", convert = TRUE) %>% 
  select(-drop)
nrelCost_type <- nrelCost %>% 
  filter(is.na(component)) %>% 
  select(year, type = cost)
nrelCost_type <- unique(na.omit(nrelCost_type$type))
nrelCost <- nrelCost %>% 
  filter(!is.na(component)) %>% 
  select(-component) %>% 
  mutate(
    installType = rep(nrelCost_type, each = length(unique(nrelCost$year)))) %>% 
  mutate(
    costPerKw = as.numeric(cost) * 10^3,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = 2019, 
      country = "US", 
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df),
    installType = fct_recode(installType,
      "utility_fixed" = "Utility ground mount (Fixed axis)",
      "utility_tracker" = "Utility ground mount (one-axis tracker)", 
      "Commercial" = "Commercial Rooftop"
    )) %>%
  select(year, installType, costPerKw)

# Take the mean of the utility installType for fixed vs. tracker
nrelUtility <- nrelCost %>%
  filter(installType %in% c('utility_fixed', 'utility_tracker')) %>%
  spread(key = installType, value = costPerKw) %>%
  mutate(
    costPerKw = (utility_fixed + utility_tracker) / 2,
    installType = "Utility") %>%
  select(year, installType, costPerKw)
nrelCost <- nrelCost %>%
  filter(! str_detect(installType, "utility")) %>%
  rbind(nrelUtility)

# U.S. NREL capacity data --------------------------------------------

nrelCapacity <- read_excel(usNrel2018FilePath, sheet = "Figure 1") %>%
  clean_names() %>%
  rename(installType = usa_installation_mw) %>%
  filter(!is.na(installType)) %>%
  gather(key = "year", value = "installation_mw", -installType) %>%
  mutate(
    year = parse_number(year),
    installType = str_replace_all(installType, " PV", "")) %>%
  separate(
    installType, into = c("amount", "installType"), sep = " ") %>%
  spread(key = amount, value = installation_mw) %>%
  rename(cumCapacityMw = Cumulative) %>%
  mutate(
    installType = str_replace(installType, "-scale", ""),
    cumCapacityKw = cumCapacityMw * 10^3) %>% 
  select(year, installType, cumCapacityKw)

# Merge NREL cost and capacity data
usNrel <- nrelCost %>%
  left_join(nrelCapacity) %>% 
  ungroup()

# Make final US data set: 
# Capacity: SEIA 
# Cost: LBNL (2000 - 2018), NREL (2019 - 2020)
us <- seiaCapacity %>%
  left_join(
    rbind(
      lbnlCost %>% 
        filter(component == "Module") %>% 
        select(year, installType, costPerKw),
      nrelCost %>% 
        filter(year >= 2019)
    ), by = c("year", "installType")) %>%
  arrange(year, installType) %>% 
  filter(installType == "Utility") %>% 
  select(year, costPerKw, cumCapacityKw)

  





# Germany ----

# Fraunhofer -- costs
# IRENA -- capacity

irenaCumCapacityMw <- read_csv(irenaCapFilePath)

germany_cap <- irenaCumCapacityMw %>%
  select(year, capacityCumulativeMw = germany)

# Germany cost data are in real Euros (not inflation adjusted)
germany2020 <- read_csv(germany2020FilePath) %>% 
  rename(year = x, costPerKw = Curve1) %>% 
  mutate(year = round(year))
germany <- read_csv(germanyFilePath) %>% 
  select(year = x, costPerKw = Curve1) %>%
  mutate(year = round(year)) %>% 
  # Add 2020 year
  rbind(germany2020) %>% 
  # Currency conversion to USD
  # left_join(exchangeRatesEUR, by = "year") %>%
  # mutate(costPerKw = costPerKw / average_of_rate) %>% 
  left_join(germany_cap) %>%
  mutate(
    cumCapacityKw = capacityCumulativeMw * 10^3,
    installType = "All") %>%
  select(year, costPerKw, cumCapacityKw)


  
  
# China ----

# Format Wang NDRC data 

# China cost data are in real dollars (not inflation adjusted)
china <- read_csv(chinaFilePath) %>%
  gather(key = "year", value = "value", `2007`:`2020`) %>%
  spread(key = "Year", value = "value") %>%
  clean_names() %>%
  gather(
    key = "component", value = "rmbPerW",
    inverter_price_rmb_w:system_price_rmb_w) %>%
  mutate(
    cumCapacityKw = total_capacity_gw * 10^6,
    # convert to cost / kW
    costPerKw = rmbPerW * 10^3) %>%
  # Currency conversion
  # merge(exchangeRatesRMB) %>%
  # mutate(costPerKw = costPerKw / average_of_rate) %>% 
  # Adjust for inflation
  mutate(
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = 2019, 
      country = "China", 
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df, 
      countries_dataframe = inflation$countries_df),
    component = str_to_title(
      str_replace_all(component, "_price_rmb_w", "")),
    component = ifelse(component == "System", "BOS", component)
  ) %>%
  left_join(components) %>%
  mutate(
    year = as.numeric(year),
    installType = "All") %>%
  select(
    year, component, componentType, installType, costPerKw, cumCapacityKw)






# World ----

# Source cost: SPV

# "Constant 2020 $" = adjusted for inflation
# "Current $" = no adjustment for inflation

worldSpvCost <- read_excel(worldSpvFilePath) %>% 
  clean_names() %>% 
  select(x1, x7) %>% 
  slice(-1) %>% 
  mutate(
    year = as.numeric(x1), 
    # Already in 2020 dollars, so no need for inflation adjustment
    costPerKw = 1000*as.numeric(x7)) %>% 
  select(year, costPerKw)

# Source capacity: IRENA

# Note: for silicon prices, extend last observation to missing years

world <- irenaCumCapacityMw %>%
  select(year, capacityCumulativeMw = world) %>% 
  mutate(
    cumCapacityKw = capacityCumulativeMw * 10^3,
    installType = "All")
silicon <- read_csv(siliconFilePath) %>%
  select(year = yr, price_si = price)
world <- left_join(world, silicon) %>%
  select(year, price_si, cumCapacityKw) %>%
  fill(price_si, .direction = "down") %>% 
  left_join(worldSpvCost, by = "year")




# Projections ----

#
# Projections to 2030, based on linear growth to achieve fixed capacity target

# Assuming silicon prices held constant level from last data point
price_si <- world[which(world$year == year_proj_min),]$price_si

# Compute annual, linear capacity increase to meet target

num_years <- year_proj_max - year_proj_min

# US ---

# Get historical capacity
proj_df_us <- seiaCapacity %>%
    group_by(year) %>% 
    summarise(cumCapacityKw = sum(cumCapacityKw))

# Get starting year capacity
cap_begin_us <- proj_df_us %>%
    filter(year == year_proj_min) %>%
    pull(cumCapacityKw)

# Get growth rates
rate_nat_trends_us <- getFutureCapRate(
  target_capacity = target_nat_trends_us,
  cap_begin = cap_begin_us,
  num_years = num_years)

rate_sus_dev_us <- getFutureCapRate(
  target_capacity = target_sus_dev_us,
  cap_begin = cap_begin_us,
  num_years = num_years)

# Get projected capacities
proj_nat_trends_us <- getFutureCapacities(
  rate = rate_nat_trends_us,
  cap_begin = cap_begin_us,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

proj_sus_dev_us <- getFutureCapacities(
  rate = rate_sus_dev_us,
  cap_begin = cap_begin_us,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

# China ---

# Get historical capacity
proj_df_china <- china %>%
    filter(component == "Module")

# Get starting year capacity
cap_begin_china <- proj_df_china %>%
    filter(year == year_proj_min) %>%
    pull(cumCapacityKw)

# Get growth rates
rate_nat_trends_china <- getFutureCapRate(
  target_capacity = target_nat_trends_china,
  cap_begin = cap_begin_china,
  num_years = num_years)

rate_sus_dev_china <- getFutureCapRate(
  target_capacity = target_sus_dev_china,
  cap_begin = cap_begin_china,
  num_years = num_years)

# Get projected capacities
proj_nat_trends_china <- getFutureCapacities(
  rate = rate_nat_trends_china,
  cap_begin = cap_begin_china,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

proj_sus_dev_china <- getFutureCapacities(
  rate = rate_sus_dev_china,
  cap_begin = cap_begin_china,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

# Germany ---

# Get historical capacity
proj_df_germany <- germany

# Get starting year capacity
cap_begin_germany <- proj_df_germany %>%
    filter(year == year_proj_min) %>%
    pull(cumCapacityKw)

# Get growth rates
rate_nat_trends_germany <- getFutureCapRate(
  target_capacity = target_nat_trends_germany,
  cap_begin = cap_begin_germany,
  num_years = num_years)

rate_sus_dev_germany <- getFutureCapRate(
  target_capacity = target_sus_dev_germany,
  cap_begin = cap_begin_germany,
  num_years = num_years)

# Get projected capacities
proj_nat_trends_germany <- getFutureCapacities(
  rate = rate_nat_trends_germany,
  cap_begin = cap_begin_germany,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

proj_sus_dev_germany <- getFutureCapacities(
  rate = rate_sus_dev_germany,
  cap_begin = cap_begin_germany,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

# World ---

# Get historical capacity
proj_df_world <- world

# Get starting year capacity
cap_begin_world <- proj_df_world %>%
    filter(year == year_proj_min) %>%
    pull(cumCapacityKw)

# Get growth rates
rate_nat_trends_world <- getFutureCapRate(
  target_capacity = target_nat_trends_world,
  cap_begin = cap_begin_world,
  num_years = num_years)

rate_sus_dev_world <- getFutureCapRate(
  target_capacity = target_sus_dev_world,
  cap_begin = cap_begin_world,
  num_years = num_years)

# Get projected capacities
proj_nat_trends_world <- getFutureCapacities(
  rate = rate_nat_trends_world,
  cap_begin = cap_begin_world,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

proj_sus_dev_world <- getFutureCapacities(
  rate = rate_sus_dev_world,
  cap_begin = cap_begin_world,
  num_years = num_years,
  year_min_proj = year_proj_min,
  price_si = price_si)

# Combine rates ---

rates <- data.frame(
  country = rep(c("U.S.", "Germany", "China", "World"), 2),
  scenario = c(rep("National Trends", 4), rep("Sustainable Development", 4)),
  rate = c(
    rate_nat_trends_us, rate_nat_trends_germany, rate_nat_trends_china,
    rate_nat_trends_world, rate_sus_dev_us, rate_sus_dev_germany,
    rate_sus_dev_china, rate_sus_dev_world)
)


# Final formatting ----
# Data frames for modeling & scenarios

# Historical - make columns for annual capacity

hist_us <- formatCapData_hist(
    data_nation = us,
    data_world  = world,
    year_beg    = year_model_us_min,
    year_max    = year_model_us_max
)

hist_china <- formatCapData_hist(
    data_nation = china %>% filter(component == "Module"),
    data_world  = world,
    year_beg    = year_model_china_min,
    year_max    = year_model_china_max
)

hist_germany <- formatCapData_hist(
    data_nation = germany,
    data_world  = world,
    year_beg    = year_model_germany_min,
    year_max    = year_model_germany_max
)

# Projections

proj_nat_trends_us <- formatCapData_proj(
    data_nation = proj_nat_trends_us,
    data_world  = proj_nat_trends_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

proj_sus_dev_us <- formatCapData_proj(
    data_nation = proj_sus_dev_us,
    data_world  = proj_sus_dev_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

proj_nat_trends_china <- formatCapData_proj(
    data_nation = proj_nat_trends_china,
    data_world  = proj_nat_trends_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

proj_sus_dev_china <- formatCapData_proj(
    data_nation = proj_sus_dev_china,
    data_world  = proj_sus_dev_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

proj_nat_trends_germany <- formatCapData_proj(
    data_nation = proj_nat_trends_germany,
    data_world  = proj_nat_trends_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

proj_sus_dev_germany <- formatCapData_proj(
    data_nation = proj_sus_dev_germany,
    data_world  = proj_sus_dev_world,
    year_beg    = year_proj_min,
    year_max    = year_proj_max
)

# Save all formatted data as a list object ----

saveRDS(list(
    pvProduction            = pvProduction,
    silicon                 = silicon,
    shipments               = shipments,
    irenaCumCapacityMw      = irenaCumCapacityMw,
    nrelCapacity            = nrelCapacity,
    nrelCost                = nrelCost,
    seiaCapacity            = seiaCapacity,
    lbnlCost                = lbnlCost,
    usNrel                  = usNrel,
    us                      = us,
    china                   = china,
    germany                 = germany,
    world                   = world,
    rates                   = rates,
    hist_us                 = hist_us,
    hist_china              = hist_china,
    hist_germany            = hist_germany,
    proj_nat_trends_us      = proj_nat_trends_us,
    proj_sus_dev_us         = proj_sus_dev_us,
    proj_nat_trends_china   = proj_nat_trends_china,
    proj_sus_dev_china      = proj_sus_dev_china,
    proj_nat_trends_germany = proj_nat_trends_germany,
    proj_sus_dev_germany    = proj_sus_dev_germany, 
    exchangeRatesRMB        = exchangeRatesRMB,
    exchangeRatesEUR        = exchangeRatesEUR
    ),
    dir$data_formatted
)
