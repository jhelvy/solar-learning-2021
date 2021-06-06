# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Compare NREL and SEIA Capacity data -----------------------------------------

nrel_seia_capacity <- data$nrelCapacity %>%
    filter(!is.na(cumCapacityKw)) %>%
    mutate(source = "NREL") %>%
    rbind(
      data$seiaCapacity %>%
        mutate(source = "SEIA")
    ) %>%
    mutate(
      cumCapacityGw = cumCapacityKw / 10^6,
      source = fct_relevel(source, c("NREL", "SEIA"))) %>%
    ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
    geom_line() +
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
      title = "Cumulative Installed Capacity (GW)")

ggsave(file.path(
  dir$figs, "compare_us_data", "nrel_seia_capacity_comparison.pdf"),
  nrel_seia_capacity, width = 10, height = 3.5, device = cairo_pdf)

# Given how closely the capacity numbers overlap, we decided to use
# the SEIA data for US capacity since it has a larger coverage period.

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

nrel_lbnl_cost <- nrel_lbnl_compare %>%
  ggplot(aes(x = year, y = costPerKw, color = source)) +
  geom_line() +
  geom_point(pch = 21, fill = "white") +
  facet_grid(component ~ installType) +
  scale_color_manual(
    values = c("red", "dodgerblue"),
    breaks = c("NREL", "LBNL")) +
  theme_bw() +
  labs(x = NULL,
       y = "Cost per Kw ($USD)",
       color = "Data source",
       title = "NREL vs. LBNL: Cost Comparison")

ggsave(file.path(dir$figs, "compare_us_data", "nrel_lbnl_cost.pdf"),
  nrel_lbnl_cost, width = 10, height = 7, device = cairo_pdf)
