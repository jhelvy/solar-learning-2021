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

usSpv2010FilePath <- file.path(dir$data, "spv", "us2000-2010.csv")
usSpv2020FilePath <- file.path(dir$data, "spv", "us2010-2020.csv")
usNrel2018FilePath <- file.path(
  dir$data, "nrel",
"Data File (U.S. Solar Photovoltaic System Cost Benchmark Q1 2018 Report).xlsx")
usNrel2020FilePath <- file.path(
  dir$data, "nrel", "Data File (U.S. Solar Photovoltaic  BESS System Cost Benchmark Q1 2020 Report).xlsx")
usLbnlFilePath <- file.path(
  dir$data, "lbnl", "tts_2019_summary_data_tables_0.xlsx")
usLbnlUpdateFilePath <- file.path(
  dir$data, "lbnl", "summary_tables_and_figures.xlsx")
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







# U.S. ----

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

# Format SPV cost data (US) ---------------------------------------------

usSpv10 <- read_csv(usSpv2010FilePath) %>% 
  clean_names() %>% 
  mutate(
    year = round(x),
    # Already in 2020 dollars, so no need for inflation adjustment
    costPerKw = 1000*as.numeric(curve1)) %>% 
  select(year, costPerKw) %>% 
  filter(year < 2010)
usSpv20 <- read_csv(usSpv2020FilePath) %>% 
  clean_names() %>% 
  mutate(
    year = round(x),
    # Already in 2020 dollars, so no need for inflation adjustment
    costPerKw = 1000*as.numeric(curve1)) %>% 
  select(year, costPerKw)
usSpvCost <- rbind(usSpv10, usSpv20)

# Merge SEIA capacity with SPV cost data ----
usSeiaSpv <- seiaCapacity %>%
  filter(installType == "Utility") %>% 
  left_join(usSpvCost, by = "year") %>% 
  select(year, costPerKw, cumCapacityKw)

# Format LBNL cost data (US) -----------------------------------------------

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

lbnlUpdate <- read_excel(
  usLbnlUpdateFilePath, sheet = "Component Cost Trends", skip = 2) %>%
  clean_names() %>% 
  mutate(
    year = x1, 
    costPerKw = 1000*large_non_res_7,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw,
      from_date = 2019,
      country = "US",
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df)) %>%
  select(year, costPerKw)

# Merge SEIA capacity with LBNL cost data ----
usSeiaLbnl <- seiaCapacity %>%
  left_join(
    lbnlCost %>% 
      filter(component == "Module") %>% 
      select(year, installType, costPerKw)) %>%
  select(year, installType, costPerKw, cumCapacityKw)

# Format NREL cost data (US) -------------------------------------------------

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
  # Currency conversion first, then adjust for inflation
  left_join(exchangeRatesEUR, by = "year") %>%
  mutate(
    costPerKw = costPerKw / average_of_rate,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = year, 
      country = "US", 
      to_date = year_inflation,
      inflation_dataframe = inflation$inflation_df,
      countries_dataframe = inflation$countries_df)) %>%
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
  # Currency conversion first, then adjust for inflation
  merge(exchangeRatesRMB) %>%
  mutate(
    costPerKw = costPerKw / average_of_rate,
    costPerKw = priceR::adjust_for_inflation(
      price = costPerKw, 
      from_date = year, 
      country = "US", 
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

# US ---

proj_df_us <- seiaCapacity %>%
    group_by(year) %>% 
    summarise(cumCapacityKw = sum(cumCapacityKw))

proj_nat_trends_us <- getFutureCapacities(
  df = proj_df_us,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_nat_trends_us) %>%
  mutate(price_si = price_si)

proj_sus_dev_us <- getFutureCapacities(
  df = proj_df_us,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_sus_dev_us) %>%
  mutate(price_si = price_si)

# China ---

proj_df_china <- china %>%
    filter(component == "Module")

proj_nat_trends_china <- getFutureCapacities(
  df = proj_df_china,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_nat_trends_china) %>%
  mutate(price_si = price_si)

proj_sus_dev_china <- getFutureCapacities(
  df = proj_df_china,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_sus_dev_china) %>%
  mutate(price_si = price_si)

# Germany ---

proj_df_germany <- germany

proj_nat_trends_germany <- getFutureCapacities(
  df = proj_df_germany,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_nat_trends_germany) %>%
  mutate(price_si = price_si)

proj_sus_dev_germany <- getFutureCapacities(
  df = proj_df_germany,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_sus_dev_germany) %>%
  mutate(price_si = price_si)

# World ---

proj_nat_trends_world <- getFutureCapacities(
  df = world,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
  target_capacity = target_nat_trends_world) %>%
  mutate(price_si = price_si)

proj_sus_dev_world <- getFutureCapacities(
  df = world,
  year_min_proj = year_proj_min,
  year_max_proj = year_proj_max,
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

# Save all formatted data as a list object ----

us <- seiaCapacity %>%
  filter(installType == "Utility") %>% 
  left_join(lbnlUpdate, by = "year") %>% 
  select(year, installType, costPerKw, cumCapacityKw)
us$costPerKw[which(us$year == 2020)] <- 
  world$costPerKw[which(world$year == 2020)]

saveRDS(list(
    pvProduction       = pvProduction,
    irenaCumCapacityMw = irenaCumCapacityMw,
    usSpvCost          = usSpvCost,
    nrelCapacity       = nrelCapacity,
    nrelCost           = nrelCost,
    seiaCapacity       = seiaCapacity,
    lbnlCost           = lbnlCost,
    usNrel             = usNrel,
    usSeiaLbnl         = usSeiaLbnl,
    us   = us,
    usSeiaSpv          = usSeiaSpv,
    china              = china,
    germany            = germany,
    world              = world, 
    proj_nat_trends    = proj_nat_trends,
    proj_sus_dev       = proj_sus_dev),
    dir$data_formatted
)
