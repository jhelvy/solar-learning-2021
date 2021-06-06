# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Compare capacity data from NREL, SEIA, and IRENA ----------------------------

# Merge NREL and SEIA capacity data
nrelSeia <- data$nrelCapacity %>%
  filter(!is.na(cumCapacityKw)) %>%
  mutate(source = "NREL") %>%
  rbind(
    data$seiaCapacity %>%
      mutate(source = "SEIA")
  ) %>%
  mutate(
    cumCapacityGw = cumCapacityKw / 10^6,
    source = fct_relevel(source, c("NREL", "SEIA")))

# Plot NREL vs. SEIA capacity comparison 
nrelSeia %>% 
  filter(year >= 2008, year <= 2018) %>% # Our study period
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line() +
  geom_point(pch = 21, fill = "white") +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    values = c("red", "dodgerblue"),
    breaks = c("NREL", "SEIA")) +
  facet_wrap(vars(installType)) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Cumulative Installed Capacity (GW)")

# During the overlapping period, NREL and SEIA data match quite closely
# Only major deviation is that NREL Commercial installations are slightly lower
# than those in SEIA

# Merge in IRENA data to compare total capacity across all 3 sources
nrelSeia %>% 
  group_by(year, source) %>%
  summarise(cumCapacityGw = sum(cumCapacityKw) / 10^6) %>%
  rbind(
    data$irenaCumCapacityMw %>%
      mutate(
        source = "IRENA", 
        cumCapacityGw = usa / 10^3) %>% 
      select(year, source, cumCapacityGw)
  ) %>%
  filter(year >= 2008, year <= 2018) %>% # Our study period
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "SEIA", "IRENA")) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Cumulative Installed Capacity (GW)")

# IRENA data track differently from NREL and SEIA. 
# They're slightly higher in the period before 2014 and lower afterwards

# Compare NREL and LBNL Cost data ---------------------------------------------

nrel_lbnl_compare <- data$nrelCost %>%
  select(year, component, installType, costPerKw) %>%
  spread(key = component, value = costPerKw) %>%
  mutate(Soft = BOS + Labor + Other) %>%
  select(-BOS, -Labor, -Other) %>%
  gather(key = "component", value = "costPerKw", Inverter:Soft) %>%
  select(year, component, installType, costPerKw) %>%
  mutate(source = "NREL") %>%
  rbind(lbnlCost %>%
    select(year, component, componentType, installType, costPerKw) %>%
    mutate(
      component = ifelse(componentType == "Soft", "Soft", component),
      source = "LBNL") %>%
    select(-componentType)) %>%
    mutate(sourceType = paste0(source, ": ", installType))

nrel_lbnl_compare %>%
  filter(year >= 2008, year <= 2018) %>% # Our study period
  ggplot(aes(x = year, y = costPerKw, color = source, group = source)) +
  geom_line() +
  geom_point(pch = 21, fill = "white") +
  facet_grid(component ~ installType) +
  scale_x_continuous(breaks = seq(2008, 2018, 2)) +
  scale_color_manual(
    values = c("red", "dodgerblue"),
    breaks = c("NREL", "LBNL")) +
  theme_bw() +
  labs(x = NULL,
       y = "Cost per Kw ($USD)",
       color = "Data source",
       title = "NREL vs. LBNL: Cost Comparison")

# NREL and LBNL cost data are quite similar for inverters and modules, with 
# only small variations. But there are large differences in soft costs. 
# Since we only look at module costs, we use LBNL cost data as it covers 
# a larger period.

