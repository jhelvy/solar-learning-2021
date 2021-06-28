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
  dir$data, "lbnl", "summary_tables_and_figures.xlsx")
usSeiaEarlyFilePath <- file.path(dir$data, "seia", "seiaEarlyYears.csv")
usSeiaFilePath <- file.path(dir$data, "seia", "seiaCapacity.json")
irenaCapFilePath <- file.path(dir$data, "irena", "irenaCumCapacityMw.csv")
germanyFilePath <- file.path(dir$data, "germany", "fraunhofer_fig2.csv")
chinaFilePath <- file.path(dir$data, "china", "wang_ndrc_data.csv")
siliconFilePath <- file.path(dir$data, "nemet_silicon.csv")
exchangeRatesPath <- file.path(dir$data, "exchange-rates.xlsx")
productionFilePath <- file.path(dir$data, "production", "production.csv")
inflationPath <- file.path(dir$data, "inflation.Rds")
  
# Load exchange rates --------------------------------------------------------

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

# Format PV production data ----------------------------------------------------

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

# -----------------------------------------------------------------------
# U.S.
# -----------------------------------------------------------------------

# Format SEIA capacity data (US) ---------------------------------------------

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
  "Residential", "Commercial", "Utility", "CSP"),
  length(seiaRaw))
seiaCapacity <- as_tibble(seiaCapacity) %>%
  rename(year = label, cumCapacityMw = value) %>%
  mutate(
    year = parse_number(as.character(year)),
    cumCapacityMw = parse_number(as.character(cumCapacityMw))
  ) %>%
  filter(installType != "CSP")

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
      filter(year < 2006) %>%
      select(year, cumCapacityMw, installType)) %>%
  mutate(cumCapacityKw = cumCapacityMw * 1000) %>%
  # Can't have 0 when taking log
  mutate(cumCapacityKw = ifelse(cumCapacityKw == 0, 1, cumCapacityKw)) %>%
  select(year, installType, cumCapacityKw) %>%
  arrange(year)

# Format LBNL cost data (US) -----------------------------------------------

lbnlCost <- read_excel(
  usLbnlFilePath, sheet = "Component Cost Trends", skip = 1) %>%
  clean_names() %>%
  select(
    year, 
    Residential = module_price_index, 
    Commercial = x6, 
    Utility = x7) %>% 
  filter(!is.na(year)) %>%
  gather(
    key = "installType", 
    value = "costPerW", 
    -year) %>% 
  mutate(
    costPerW = as.numeric(costPerW),
    costPerKw = costPerW * 1000) %>% 
  select(year, installType, costPerKw)

# Merge SEIA capacity with LBNL cost data ----
usSeiaLbnl <- seiaCapacity %>%
  left_join(lbnlCost, by = c("year", "installType")) %>%
  filter(!is.na(costPerKw)) %>%
  select(year, installType, costPerKw, cumCapacityKw)

# Format NREL cost data (US) -------------------------------------------------

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

# Format NREL capacity data (US) --------------------------------------------

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

# -----------------------------------------------------------------------
# Germany
# -----------------------------------------------------------------------

# Fraunhofer -- costs
# IRENA -- capacity

irenaCumCapacityMw <- read_csv(irenaCapFilePath)

germany_cap <- irenaCumCapacityMw %>%
  select(year, capacityCumulativeMw = germany)

germany <- read_csv(germanyFilePath) %>% 
  select(year = x, costPerKw = Curve1) %>%
  mutate(year = round(year)) %>% 
  # Currency conversion first, then adjust for inflation
  left_join(exchangeRatesEUR, by = "year") %>%
  mutate(
    costPerKw = costPerKw / average_of_rate,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = year, 
      country = "US", 
      to_date = year_max,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df)) %>%
  left_join(germany_cap) %>%
  mutate(
    cumCapacityKw = capacityCumulativeMw * 10^3,
    installType = "All") %>%
  select(year, costPerKw, cumCapacityKw)

# -----------------------------------------------------------------------
# China
# -----------------------------------------------------------------------

# Format Wang NDRC data 

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
  # Currency conversion first, then adjust for inflation
  merge(exchangeRatesRMB) %>%
  mutate(
    costPerKw = costPerKw / average_of_rate,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = year, 
      country = "US", 
      to_date = year_max, 
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

# -----------------------------------------------------------------------
# World
# -----------------------------------------------------------------------

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
  fill(price_si, .direction = "down")

# -----------------------------------------------------------------------
# Projections
# -----------------------------------------------------------------------
#
# Projections to 2030, based on achieving fixed capacity by 2030
#
# National trends scenario is based on capacity at end of 2020 plus continuation of recent trends. Note that this lines up well with Germany's stated national target of 100GW, and is equivalent to China solar share in solar+wind reaching ~ 63% of the 1200GW target (currently at 47%). There are analyses that indicate China will need to exceed 1200 to reach its other targets, and I expect solar growth to increase relative to wind for the next decade. The U.S. scenario is also in line with other scenarios (e.g. EIA low-RE cost, NAM decarbonization study). World capacity is computed by taking the shares of the three countries in current world capacity (2019 from IRENA), which comes out to 54%, then scaling up capacity linearly to 2030.
#
# Sustainable development scenario is from IEA WEO 2020 (the net zero study does not provide a country breakdown), then splitting up EU into Germany via the 2019 shares of capacity (IRENA).
#
# Targets:
target_nat_trends_us <- 295*1e6
target_nat_trends_china <- 750*1e6
target_nat_trends_germany <- 103*1e6
target_nat_trends_world <- 2115*1e6
target_sus_dev_us <- 628*1e6
target_sus_dev_china <- 1106*1e6
target_sus_dev_germany <- 147*1e6
target_sus_dev_world <- 3125*1e6

# Assuming silicon prices held constant level from last data point
price_si <- world[which(world$year == year_min_proj),]$price_si

# Compute annual, linear capacity increase to meet target

# US ---

proj_df_us <- seiaCapacity %>%
    group_by(year) %>% 
    summarise(cumCapacityKw = sum(cumCapacityKw))

proj_nat_trends_us <- getFutureCapacities(
  df = proj_df_us,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_nat_trends_us) %>%
  mutate(price_si = price_si)

proj_sus_dev_us <- getFutureCapacities(
  df = proj_df_us,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_sus_dev_us) %>%
  mutate(price_si = price_si)

# China ---

proj_df_china <- china %>%
    filter(component == "Module")

proj_nat_trends_china <- getFutureCapacities(
  df = proj_df_china,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_nat_trends_china) %>%
  mutate(price_si = price_si)

proj_sus_dev_china <- getFutureCapacities(
  df = proj_df_china,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_sus_dev_china) %>%
  mutate(price_si = price_si)

# Germany ---

proj_df_germany <- germany

proj_nat_trends_germany <- getFutureCapacities(
  df = proj_df_germany,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_nat_trends_germany) %>%
  mutate(price_si = price_si)

proj_sus_dev_germany <- getFutureCapacities(
  df = proj_df_germany,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_sus_dev_germany) %>%
  mutate(price_si = price_si)

# World ---

proj_nat_trends_world <- getFutureCapacities(
  df = world,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_nat_trends_world) %>%
  mutate(price_si = price_si)

proj_sus_dev_world <- getFutureCapacities(
  df = world,
  year_min_proj = year_min_proj,
  year_max_proj = year_max_proj,
  target_capacity = target_sus_dev_world) %>%
  mutate(price_si = price_si)

# Combine ---

proj_nat_trends <- rbind(
  proj_nat_trends_us %>% mutate(country = "U.S."),
  proj_nat_trends_germany %>% mutate(country = "Germany"),
  proj_nat_trends_china %>% mutate(country = "China"),
  proj_nat_trends_world %>% mutate(country = "World")
)

proj_sus_dev <- rbind(
  proj_sus_dev_us %>% mutate(country = "U.S."),
  proj_sus_dev_germany %>% mutate(country = "Germany"),
  proj_sus_dev_china %>% mutate(country = "China"),
  proj_sus_dev_world %>% mutate(country = "World")
)

# Save all formatted data as a list object ---

saveRDS(list(
    pvProduction       = pvProduction,
    irenaCumCapacityMw = irenaCumCapacityMw,
    nrelCapacity       = nrelCapacity,
    nrelCost           = nrelCost,
    seiaCapacity       = seiaCapacity,
    lbnlCost           = lbnlCost,
    usNrel             = usNrel,
    usSeiaLbnl         = usSeiaLbnl,
    china              = china,
    germany            = germany,
    world              = world, 
    proj_nat_trends    = proj_nat_trends,
    proj_sus_dev       = proj_sus_dev),
    dir$data_formatted
)
