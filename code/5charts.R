# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$historical_scenarios)

# Load projections
proj <- readRDS(dir$projection_scenarios)

# Chart settings
colors_learning <- c("National" = "#E5601A", "Global" = "#1A9FE5")
colors_country <- c("#E5C61A", "#1A9FE5", "#E5601A")
font_main <- "Fira Sans Condensed"

# Set boundaries for plots 
min_year <- min(year_model_china_min, year_model_us_min, year_model_germany_min)
max_year <- max(year_model_china_max, year_model_us_max, year_model_germany_max)

# # Check for color blindness
# colorblindr::cvd_grid(plot)

# Global PV production ------------

# Read in and format data
pvProduction <- data$pvProduction %>% 
  mutate(
    country = str_to_title(country), 
    country = ifelse(country == "Row", "ROW", country), 
    country = ifelse(country == "Us", "USA", country), 
    country = fct_relevel(country, rev(c(
      "China", "Taiwan", "Malaysia", "Japan", "Europe", "USA", "ROW"))),
    year = as.factor(year)) %>% 
  ggplot(aes(x = year, y = production_gw)) +
  geom_col(aes(fill = country), width = 0.7, alpha = 0.9) +
  scale_y_continuous(
    limits = c(0, 140), breaks=seq(0, 140, 20), 
    expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "#767676", "#0088cc", "#ff6611", "#ffee55", "#aa6688", "#aacc22",
    "#ff1417")) +
  theme_minimal_hgrid(font_family = font_main, font_size = 16) +
  theme(
    legend.position = c(0.01, 0.7),
    legend.background = element_rect(
      fill = "white", color = "black", size = 0.2),
    legend.margin = margin(6, 8, 8, 6), 
    plot.caption = element_text(
      hjust = 0, size = 12, family = font_main, face = "italic"),
    plot.caption.position = "plot",
    plot.title.position = "plot") +
  labs(x = NULL,
       y = 'Annual Cell Production (GW)',
       title = 'Annual Solar Photovoltaic Cell Production (GW)',
       fill  = 'Origin', 
       caption = "Data from Jäger-Waldau, A. (2020) https://doi.org/10.3390/en13040930")

ggsave(
    here::here(dir$figs, 'pdf', "pvProduction.pdf"),
    pvProduction, width = 8, height = 6, device = cairo_pdf
)
ggsave(
    here::here(dir$figs, 'png', "pvProduction.png"),
    pvProduction, width = 8, height = 6, dpi = 300
)

# Cost per kw for global vs. national learning ------

x_lim <- lubridate::ymd(c(
    paste0(min_year - 1, "-07-01"),
    paste0(max_year, "-07-01")))
cost_historical_plot <- cost$cost %>%
    mutate(
      learning = str_to_title(learning),
      learning = fct_relevel(learning, c("National", "Global")),
      year = lubridate::ymd(paste0(year, "-01-01"))) %>% 
    ggplot() +
    facet_wrap(vars(country), nrow = 1) +
    # First add historical cost line as points
    geom_point(
        data = cost$cost_historical_true %>% 
            mutate(year = lubridate::ymd(paste0(year, "-01-01"))),
        aes(x = year, y = costPerKw), 
        size = 1) +
    # Now add modeled cost lines with uncertainty bands
    geom_ribbon(
        aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub, 
            fill = learning), alpha = 0.22) +
    geom_line(
        aes(x = year, y = cost_per_kw, color = learning),
        alpha = 0.6, size = 1) +
    scale_x_date(
        limits = x_lim,
        date_labels = "'%y",
        date_breaks = "2 years") +
    scale_y_continuous(
      limits = c(0, 8000),
      breaks = seq(0, 8000, 1000),
      labels = scales::dollar) +
    # scale_y_log10(labels = scales::dollar) +
    scale_color_manual("Learning", values = colors_learning) +
    scale_fill_manual("Learning", values = colors_learning) +
    theme_minimal_grid(
        font_size = 16,
        font_family = font_main) +
    panel_border() +
    labs(
      title = paste0(
        "Estimated Module Cost Under <span style = 'color: ",
        colors_learning["Global"], 
        ";'>Global</span> vs. <span style = 'color: ", 
        colors_learning["National"], 
        ";'>National</span> Market Scenarios"),
        y = paste0("Cost per kW (", year_inflation, " $USD)"),
        x = "Year"
    ) + 
    theme(
        plot.title.position = "plot",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(size = 0.3, colour = "grey90"),
        axis.line.x = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1, size = 11, face = "italic"),
        plot.title = element_markdown(),
        legend.position = "none"
    ) +
    # Add "historical" labels
    geom_text(
        data = data.frame(
            x = lubridate::ymd(rep("2009-01-01", 3)), 
            y = c(500, 500, 500),
            country = c("China", "Germany", "U.S."),
            label = rep("Historical", 3)), 
        aes(x = x, y = y, label = label), 
        color = "black", size = 5, family = font_main
    ) + 
    geom_segment(
        data = data.frame(
            x = lubridate::ymd(rep("2010-11-01", 3)), 
            xend = lubridate::ymd(c("2011-10-01", "2011-10-01", "2011-11-01")), 
            y = c(500, 500, 500), 
            yend = c(720, 600, 850), 
            country = c("China", "Germany", "U.S.")),
        aes(x = x, y = y, xend = xend, yend = yend), 
        color = "black", size = 0.5
    )

ggsave(
    file.path(dir$figs, 'pdf', 'cost_historical.pdf'),
    cost_historical_plot, height = 4.25, width = 11, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'cost_historical.png'),
    cost_historical_plot, height = 4.25, width = 11
)

# Cumulative savings historical ----

savings_cum_historical_plot <- cost$savings %>%
    mutate(country = fct_relevel(country, c("Germany", "U.S.", "China"))) %>%
    ggplot() +
    geom_area(aes(x = year, y = cum_savings_bil, fill = country)) +
    scale_fill_manual(values = colors_country) +
    scale_x_continuous(
      breaks = seq(year_savings_min, year_savings_max, 2),
      limits = c(year_savings_min, year_savings_max)) +
    scale_y_continuous(
      labels = dollar,
      breaks = seq(0, 80, 20),
      limits = c(0, 80),
      expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_family = font_main) +
    scale_color_manual(values = c("white", "black", "white")) +
    labs(
        title = "Cumulative Module Savings Under Global vs. National Market Scenarios (2008 - 2020)",
        x = "Year",
        y = paste0("Cumulative Savings (Billion ", year_inflation, " $USD)"),
        fill = "Country") +
    theme(
        plot.title.position = "plot",
        legend.position = "none"
    ) +
    # Add country labels
    geom_text(
        data = data.frame(
            x = c(2018, 2018, 2017), 
            y = c(20, 44, 64), 
            label = c("China", "U.S.", "Germany")), 
        aes(x = x, y = y, label = label, color = label), 
        size = 6, family = font_main
    ) +
    annotate(
        "segment", x = 2018, xend = 2018.5, y = 63, yend = 59,
        colour = "black"
    )

ggsave(
    file.path(dir$figs, 'pdf', 'savings_cum_historical_plot.pdf'),
    savings_cum_historical_plot, height = 4, width = 6.5, 
    device = cairo_pdf
)
ggsave(
    file.path(dir$figs, 'png', 'savings_cum_historical_plot.png'),
    savings_cum_historical_plot, height = 4, width = 6.5
)

# Annual savings historical ----

cum_savings_labels <- cost$savings %>% 
    filter(year == max(year)) %>%
    mutate(
        mean = round(cum_savings_bil), 
        lb = round(cum_savings_bil_lb),
        ub = round(cum_savings_bil_ub),
        x = 2008, 
        y = 12.5,
        label = paste0(
            "Cumulative savings:\nB $ ", mean, " (", lb, " - ", ub, ")"))
savings_ann_historical_plot <- cost$savings %>% 
    mutate(country = fct_relevel(country, c("Germany", "U.S.", "China"))) %>%
    ggplot() + 
    facet_wrap(vars(country), nrow = 1) +
    geom_col(aes(x = year, y = ann_savings_bil, fill = country)) + 
    geom_errorbar(
        aes(x = year, ymin = ann_savings_bil_lb, ymax = ann_savings_bil_ub), 
        color = "grey42", width = 0.5) + 
    scale_x_continuous(breaks = seq(year_savings_min, year_savings_max, 2)) +
    scale_y_continuous(
        labels = scales::dollar, 
        breaks = seq(-5, 20, 5),
        limits = c(-5, 20),
        expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = colors_country) +
    theme_minimal_hgrid(
        font_size = 16,
        font_family = font_main) +
    panel_border() +
    theme(
        plot.title.position = "plot",
        legend.position = "none",
        axis.line.x = element_blank(),
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(
            size = 0.5, colour = "grey90")
    ) +
     labs(
        title = "Annual Module Savings Under Global vs. National Market Scenarios (2008 - 2020)",
        x = NULL,
        y = paste0("Annual Savings (Billion ", year_inflation, " $USD)"),
        fill = "Country") + 
    # Add totals
    geom_text(
        data = cum_savings_labels,
        aes(x = x, y = y, label = label), 
        size = 4.5, family = "Roboto Condensed", hjust = 0
    ) 

ggsave(
    file.path(dir$figs, 'pdf', 'savings_ann_historical_plot.pdf'),
    savings_ann_historical_plot, height = 4, width = 12, 
    device = cairo_pdf
)
ggsave(
    file.path(dir$figs, 'png', 'savings_ann_historical_plot.png'),
    savings_ann_historical_plot, height = 4, width = 12
)

# 2030 Projections -----

cost_proj <- proj$base %>% 
  mutate(
    learning = str_to_title(learning),
    learning = fct_relevel(learning, c("National", "Global")),
    scenario = fct_recode(scenario, 
      "National Trends" = "nat_trends", 
      "Sustainable Development" = "sus_dev"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  facet_grid(scenario ~ country) +
  geom_ribbon(
    aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
        fill = learning), alpha = 0.25) +
  geom_line(
    aes(x = year, y = cost_per_kw, color = learning),
    alpha = 0.6, size = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2019-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  # expand_limits(y = 0) +
  scale_color_manual("Scenario", values = colors_learning) +
  scale_fill_manual("Scenario", values = colors_learning) +
  theme_minimal_grid(
    font_size = 16,
    font_family = font_main) +
  panel_border() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    legend.position = "none",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(size = 0.5, colour = "grey90")
  ) +
  labs(
    y = paste0("Cost per kW (", year_inflation, " $USD)"),
    x = "Year",
    title = paste0(
      "Projected Module Cost Under <span style = 'color: ",
      colors_learning["Global"], 
      ";'>Global</span> vs. <span style = 'color: ", 
      colors_learning["National"], 
      ";'>National</span> Market Scenarios (2020 - 2030)"))

ggsave(
  file.path(dir$figs, 'pdf', 'cost_proj.pdf'),
  cost_proj, height = 6.5, width = 11, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'cost_proj.png'),
  cost_proj, height = 6.5, width = 11)

# Compare predicted 2030 costs based on different starting costs --------

# Get the main cost_per_kw in each scenario
sens_cost_proj <- rbind(
  mutate(proj$base, bound = "main"), 
  mutate(proj$lb, bound = "lb"), 
  mutate(proj$ub, bound = "ub")
) %>% 
  select(-c("cost_per_kw_lb", "cost_per_kw_ub")) %>% 
  spread(key = bound, value = cost_per_kw) %>% 
  mutate(
    learning = str_to_title(learning),
    learning = fct_relevel(learning, c("National", "Global")),
    scenario = fct_recode(scenario, 
      "National Trends" = "nat_trends", 
      "Sustainable Development" = "sus_dev"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  facet_grid(scenario ~ country) +
  geom_ribbon(
    aes(x = year, ymin = lb, ymax = ub,
        fill = learning), alpha = 0.25) +
  geom_line(
    aes(x = year, y = main, color = learning),
    alpha = 0.6, size = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2019-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  # expand_limits(y = 0) +
  scale_color_manual("Scenario", values = colors_learning) +
  scale_fill_manual("Scenario", values = colors_learning) +
  theme_minimal_grid(
    font_size = 16,
    font_family = font_main) +
  panel_border() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    legend.position = "none",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(size = 0.5, colour = "grey90"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 1, size = 11, face = "italic")
  ) +
  labs(
    y = paste0("Cost per kW (", year_inflation, " $USD)"),
    x = "Year",
    title = paste0(
      "Projected Module Cost Under <span style = 'color: ",
      colors_learning["Global"], 
      ";'>Global</span> vs. <span style = 'color: ", 
      colors_learning["National"], 
      ";'>National</span> Market Scenarios (2020 - 2030)"),
    subtitle = "Bands reflect a 25% range around the 2020 starting cost")

ggsave(
  file.path(dir$figs, 'pdf', 'sens_cost_proj.pdf'),
  sens_cost_proj, height = 6.5, width = 11, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'sens_cost_proj.png'),
  sens_cost_proj, height = 6.5, width = 11)

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
  filter(year <= max_year) %>%
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
    title = "Comparison of installed capacity by type and data source")

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
  filter(year <= max_year) %>% 
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
    title = "Comparison of cumulative installed data")

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
       title = "Comparison of cost per kW by data source")

ggsave(here::here(dir$figs, 'pdf', "sens_compare_cost.pdf"),
       sens_compare_cost, width = 5, height = 3, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "sens_compare_cost.png"),
       sens_compare_cost, width = 5, height = 3, dpi = 300)

# NREL and LBNL cost data are relatively similar for modules, with 
# the biggest disagreement in the earlier years. We decided to use LBNL cost
# data for the earlier period (2000 - 2018) and NREL for 2019 & 2020 as 
# the two appear to converge in the later years.
