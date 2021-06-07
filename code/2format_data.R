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
usSeiaFilePath <- file.path(dir$data, "seia", "seiaCapacity.json")
irenaCapFilePath <- file.path(dir$data, "irena", "irenaCumCapacityMw.csv")
germanyFilePath <- file.path(
  dir$data, "digitize", "germany", "fraunhofer_fig2.csv")
chinaFilePath <- file.path(dir$data, "china", "wang_ndrc_data.csv")
siliconFilePath <- file.path(dir$data, "nemet_silicon.csv")
exchangeRatesPath <- file.path(dir$data, "exchange-rates.xlsx")
productionFilePath <- file.path(
  dir$data, "digitize", "production", "production.csv")

# Load exchange rates --------------------------------------------------------

exchangeRatesRMB <- read_excel(
  exchangeRatesPath, sheet = "usd-rmb", skip = 2) %>%
  clean_names() %>%
  rename(year = row_labels)
    
exchangeRatesEUR <- read_excel(
  exchangeRatesPath, sheet = "usd-euro", skip = 2) %>%
  clean_names() %>%
  rename(year = row_labels)

# -----------------------------------------------------------------------
# U.S.
# -----------------------------------------------------------------------

# Format SEIA capacity data (US) ---------------------------------------------

# Capacity data from 2000 - 2013 are from this image in the raw data:
# seia_2013_year_review_fig_2.1.jpg
seiaEarlyYears <- tribble(
  ~"year", ~"Residential", ~"Commercial", ~"Utility",
  2000, 1, 2, 0,
  2001, 5, 3, 3,
  2002, 11, 9, 2,
  2003, 15, 27, 3,
  2004, 24, 32, 2,
  2005, 27, 51, 1,
  2006, 38, 67, 0,
  2007, 58, 93, 9,
  2008, 82, 200, 16,
  2009, 164, 213, 58,
  2010, 246, 339, 267,
  2011, 304, 831, 784,
  2012, 494, 1072, 1803,
  2013, 792, 1112, 2847
) %>%
  gather(
    key = "installType", value = "capacityMw",
    Residential:Utility
  ) %>%
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
# Looks good!

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
    costPerKw = costPerW * 1000) %>%
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
  select(year, component, componentType, installType, costPerKw)
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

# Format NREL cost data (US) -------------------------------------------------

nrelCost <- read_excel(
  usNrel2020FilePath, sheet = "Figures ES-1 and ES-2", skip = 1) %>%
  clean_names() %>%
  rename(component = x1) %>%
  select(-c(x24, x36)) %>%
  gather(key = "year", value = "cost", -component) %>%
  filter(
    !is.na(cost),
    !component %in% c("Total", "Total Soft Costs", "Soft Cost %"))
nrelTypes <- nrelCost %>%
  filter(is.na(component)) %>%
  select(year, installType = cost)
nrelCost <- nrelCost %>%
  left_join(nrelTypes) %>%
  filter(!is.na(component)) %>%
  mutate(year = str_replace(year, "x", "")) %>%
  separate(
    year, into = c("year", "drop"), sep = "_", convert = T) %>%
  select(-drop) %>%
  filter(!is.na(installType)) %>%
  mutate(
    costPerKw = as.numeric(cost) * 10^3,
    component = fct_recode(component,
      "BOS" = "Hardware BOS - Structural and Electrical Components",
      "Labor" =  "Soft Costs - Install Labor",
      "Other" = "Soft Costs - Others (PII, Land Acquisition, Transmission Line, Sales Tax, Overhead, and Profit)"
    ),
    installType = fct_recode(installType,
      "utility_fixed" = "Utility ground mount (Fixed axis)",
      "utility_tracker" = "Utility ground mount (one-axis tracker)"
    )) %>%
  filter(!is.na(cost)) %>%
  select(-cost) %>%
  left_join(components) %>%
  select(year, component, componentType, installType, costPerKw)

# Take the mean of the utility installType for fixed vs. tracker
nrelUtility <- nrelCost %>%
  filter(installType %in% c('utility_fixed', 'utility_tracker')) %>%
  spread(key = installType, value = costPerKw) %>%
  mutate(
    costPerKw = (utility_fixed + utility_tracker) / 2,
    installType = "Utility") %>%
  select(year, component, componentType, installType, costPerKw)
nrelCost <- nrelCost %>%
  filter(! str_detect(installType, "utility")) %>%
  rbind(nrelUtility) %>%
  # 2020 data have other category called
  # "Additional Costs from Model Updates (including some soft costs)"
  # Merging this into "Other"
  mutate(component = fct_other(component, keep = c(
    "Module", "Inverter", "BOS", "Labor", "Other")),
    installType = str_replace(installType, " Rooftop", "")) %>%
  group_by(year, component, componentType, installType) %>%
  summarise(costPerKw = sum(costPerKw)) %>%
  arrange(year, component)

# Format NREL capacity data (US) --------------------------------------------

nrelCapacity <- read_excel(usNrel2018FilePath, sheet="Figure 1") %>%
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

# Create US 2030 projections ----------------------------------------

# Cost projections from 2019 to 2030, copied from 2018 LBNL data 
# (assuming no cost reduction)
newcols <- as.character(seq(2019, 2030, 1))
us2030_cost <- lbnlCost %>%
  filter(year == 2018) %>%
  select(-year) %>%
  rename(`2018` = costPerKw) %>%
  # Create copies of 2018 costPerKw for each year out to 2030
  add_column(!!!set_names(as.list(rep(NA, length(newcols))), nm = newcols)) %>%
  mutate_at(newcols, ~ `2018`) %>%
  # Reshape
  gather(key = 'year','costPerKw', all_of(c(newcols,'2018'))) %>%
  mutate(year = as.numeric(year)) %>%
  select(all_of(names(lbnlCost))) 
  
# Capacity projections to 2030, based on achieving fixed capacity by 2030
# 300 GW, taken from NAM, Committee on Accelerating Decarbonization in the
# United States (2021)
target_capacity <- 300*1e6 
# Breakdown taken from 2020 SEIA Capacity shares:
type_capacity <- target_capacity * c(
  "Commercial" = 0.180629266,   
  "Residential" = 0.207163921,
  "Utility" = 0.612206813)
us2030_cap_add <- seiaCapacity %>%
  filter(year == 2020) %>% # latest year in dataset
  merge(data.frame(
    installType = names(type_capacity), 
    endCapacityKw = type_capacity)) %>%
  mutate(annualCap = (endCapacityKw - cumCapacityKw) / 10) %>%
  select(installType, begCap = cumCapacityKw, annualCap)

us2030_capacity <- seiaCapacity %>%
  filter(year >= 2018)

for (i in seq(2021, 2030, 1)) {
  df <- us2030_cap_add %>%
    mutate(
      cumCapacityKw = begCap + (i - 2020) * annualCap,
      year = i) %>%
    select(year, installType, cumCapacityKw)
  us2030_capacity <- rbind(us2030_capacity, df)
}

# Format all U.S. data -----------------------------------------------------

# Merge NREL cost and capacity data
usNrel <- nrelCost %>%
  left_join(nrelCapacity)

# Merge SEIA capacity with LBNL cost data
usSeia <- seiaCapacity %>%
  spread(key = installType, value = cumCapacityKw) %>%
  mutate(All = Commercial + Residential + Utility) %>%
  gather(
    key = "installType", value = "cumCapacityKw",
    Commercial:All) %>%
  left_join(lbnlCost) %>%
  filter(!is.na(costPerKw)) %>%
  select(
    year, component, componentType, installType, costPerKw, cumCapacityKw)

# Merge SEIA capacity with LBNL cost data ----

usSeiaLbnl <- seiaCapacity %>%
  left_join(lbnlCost) %>%
  filter(!is.na(costPerKw)) %>%
  select(
    year, component, componentType, installType, costPerKw, cumCapacityKw)

# Merge 2030 projections -- future capacity and cost data ----

us2030 <- us2030_capacity %>%
  left_join(us2030_cost) %>%
  filter(!is.na(costPerKw)) %>%
  select(
    year, component, componentType, installType, costPerKw, cumCapacityKw)

# Format Germany data ------------------------------------------------------
#   Fraunhofer -- costs
#   IRENA -- capacity

irenaCumCapacityMw <- read_csv(irenaCapFilePath)

germany_cap <- irenaCumCapacityMw %>%
  select(year, capacityCumulativeMw = germany)

germany <- read_csv(germanyFilePath) %>%
  rename(
    component = type,
    costPerKw = y) %>%
  # Currency conversion first, then adjust for inflation
  merge(exchangeRatesEUR) %>%
  mutate(
    costPerKw = costPerKw / average_of_rate,
    costPerKw = priceR::adjust_for_inflation(
      costPerKw, year, "US", to_date = 2018)) %>%
  left_join(germany_cap) %>%
  mutate(
    component = str_to_title(component),
    component = ifelse(
      component == "Bos_inverter", "BOS_Inverter", component),
    cumCapacityKw = capacityCumulativeMw * 10^3,
    installType = "All") %>%
  left_join(components) %>%
  select(
    year, component, componentType, installType, costPerKw, cumCapacityKw)

# Format Wang NDRC data -------------------------------------------------------

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
    costPerKw = adjust_for_inflation(costPerKw, year, "US", to_date = 2018),
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

# Format World data ----------------------------------------------------
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

# World 2030 projections -----
# Silicon price: Assume constant from 2018
newcols <- as.character(seq(2019, 2030, 1))
price_si_new <- rep(
  silicon[which(silicon$year == 2018),]$price_si,
  length(newcols))
world2030_add <- data.frame(
  year = as.numeric(newcols), 
  price_si = price_si_new,
  cumCapacityKw = NA)

# Capacity projections to 2030, based on achieving fixed capacity by 2030

# 3100 GW, taken from WEO 2020, Sustainable Development Scenario:
target_capacity <- 3100*1e6  
beg_capacity <- world[which(world$year == 2018),]$cumCapacityKw
annualCap <- (target_capacity - beg_capacity) / (2030 - 2018)
world2030_add <- world2030_add %>%
  mutate(cumCapacityKw = beg_capacity + (year - 2018)*annualCap)

world2030 <- world %>%
  filter(year >= 2018) %>%
  rbind(world2030_add)

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

# Save all formatted data as a list object ---

saveRDS(list(
    irenaCumCapacityMw = irenaCumCapacityMw,
    nrelCapacity       = nrelCapacity,
    nrelCost           = nrelCost,
    seiaCapacity       = seiaCapacity,
    lbnlCost           = lbnlCost,
    usNrel             = usNrel,
    usSeia             = usSeia,
    usSeiaLbnl         = usSeiaLbnl,
    us2030             = us2030,
    china              = china,
    germany            = germany,
    world              = world,
    world2030          = world2030,
    pvProduction       = pvProduction),
    dir$data_formatted
)
