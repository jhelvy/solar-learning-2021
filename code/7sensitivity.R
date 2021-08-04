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
sens_compare_capacity_type <- nrelSeia %>% 
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue"),
    breaks = c("NREL", "SEIA")) +
  facet_wrap(vars(installType)) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Comparison of Installed Capacity by Type and Data Source")

ggsave(here::here(dir$figs, 'pdf', "sens_compare_capacity_type.pdf"),
       sens_compare_capacity_type, width = 9, height = 3, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "sens_compare_capacity_type.png"),
       sens_compare_capacity_type, width = 9, height = 3, dpi = 300)

# During the overlapping period, NREL and SEIA data match quite closely
# Only deviation is that NREL Commercial installations are slightly lower
# than those in SEIA

# Merge in IRENA data to compare total capacity across all 3 sources
sens_compare_capacity_cumulative <- nrelSeia %>% 
  group_by(year, source) %>%
  summarise(cumCapacityGw = sum(cumCapacityKw) / 10^6) %>%
  rbind(
    data$irenaCumCapacityMw %>%
      mutate(
        source = "IRENA", 
        cumCapacityGw = usa / 10^3) %>% 
      select(year, source, cumCapacityGw)
  ) %>%
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "SEIA", "IRENA")) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Comparison of Cumulative Installed Capacity by Data Source")

ggsave(here::here(dir$figs, 'pdf', "sens_compare_capacity_cumulative.pdf"),
       sens_compare_capacity_cumulative, 
       width = 5, height = 3, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "sens_compare_capacity_cumulative.png"),
       sens_compare_capacity_cumulative, 
       width = 5, height = 3, dpi = 300)

# IRENA data track differently from NREL and SEIA. 
# They're slightly higher in the period before 2014 and lower afterwards

# Based on these comparisons, we use SEIA data for installed capacity as
# it tracks with NREL and covers a longer time period

# Compare NREL, LBNL, and SPV Cost data --------------------------------------

cost_compare <- data$nrelCost %>%
  filter(installType == "Utility") %>% 
  select(-installType) %>% 
  mutate(source = "NREL") %>%
  rbind(
    data$lbnlCost %>%
      filter(installType == "Utility", component == "Module") %>% 
      select(-installType) %>% 
      mutate(source = "LBNL") %>% 
      select(year, costPerKw, source)
  ) 

sens_compare_cost <- cost_compare %>%
  ggplot(aes(x = year, y = costPerKw, color = source, group = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "LBNL", "SPV")) +
  theme_bw() +
  labs(x = NULL,
       y = "Cost per kW ($USD)",
       color = "Data source",
       title = "Comparison of Cost per kW by Data Source")

ggsave(here::here(dir$figs, 'pdf', "sens_compare_cost.pdf"),
       sens_compare_cost, width = 5, height = 3, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "sens_compare_cost.png"),
       sens_compare_cost, width = 5, height = 3, dpi = 300)

# NREL and LBNL cost data are relatively similar for modules, with 
# the biggest disagreement in the earlier years. We decided to use LBNL cost
# data for the earlier period (2000 - 2018) and NREL for 2019 & 2020 as 
# the two appear to converge in the later years.

# Compare predicted 2030 costs based on different assumptions ----------------


