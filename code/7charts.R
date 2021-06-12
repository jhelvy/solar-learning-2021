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
  theme_minimal_hgrid(font_family = "Fira Sans Condensed", font_size = 16) +
  theme(
    legend.position = c(0.01, 0.7),
    legend.background = element_rect(
      fill = "white", color = "black", size = 0.2),
    legend.margin = margin(6, 8, 8, 6), 
    plot.caption = element_text(
      hjust = 0, size = 12, family = "Fira Sans Condensed", 
      face = "italic"),
    plot.caption.position = "plot",
    plot.title.position = "plot") +
  labs(x = NULL,
       y = 'Annual Cell Production (GW)',
       title = 'Annual Solar Voltaic Cell Production (GW)',
       fill  = 'Origin', 
       caption = "Data from Jäger-Waldau, A. (2020) https://doi.org/10.3390/en13040930")

ggsave(here::here(dir$figs, 'pdf', "pvProduction.pdf"),
       pvProduction, width = 8, height = 6, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "pvProduction.png"),
       pvProduction, width = 8, height = 6, dpi = 300)

# Cost per kw for global vs. national learning ------

# True historical cost per kW
cost_historical <- rbind(
    data$usSeiaLbnl %>%
        filter(installType == "Utility") %>% 
        mutate(country = "U.S."),
    data$china %>%
        mutate(country = "China"),
    data$germany %>%
        mutate(country = "Germany")
    ) %>% 
    filter(
      year >= 2008, year <= 2018,
      component == "Module")

cost_historical_plot <- cost$cost_scenarios %>% 
    mutate(
        scenario = fct_relevel(scenario, c("national", "global")),
        scenario = fct_recode(scenario,
            "Global learning" = "global",
            "National learning" = "national"),
        year = lubridate::ymd(paste0(year, "-01-01"))) %>% 
    ggplot() +
    # First add historical cost line as dashed line
    geom_line(
        data = cost_historical %>% 
            filter(component == "Module") %>% 
            mutate(year = lubridate::ymd(paste0(year, "-01-01"))),
        aes(x = year, y = costPerKw), linetype = 2, alpha = 0.4, size = 1) +
    # Now add modeled cost lines with uncertainty bands
    geom_ribbon(
        aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub, 
            fill = scenario), alpha = 0.25) +
    geom_line(
        aes(x = year, y = cost_per_kw, color = scenario),
        alpha = 0.6, size = 1) +
    facet_wrap(vars(country), nrow = 1) +
    scale_x_date(
        limits = lubridate::ymd(c("2007-07-01", "2018-07-01")),
        date_labels = "'%y",
        date_breaks = "2 years") +
    scale_y_continuous(labels = scales::dollar) +
    expand_limits(y = 0) +
    scale_color_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
    scale_fill_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
    theme_minimal_grid(
        font_size = 16,
        font_family = "Fira Sans Condensed") +
    panel_border() +
    theme(
        plot.title.position = "plot",
        legend.position = "right",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(
            size = 0.5, colour = "grey90")
    ) +
    labs(
        title = 'Estimated Module Costs Using Global vs. National Learning Rates',
        y = "Cost per kW (2018 $USD)",
        x = "Year") + 
    # Add "historical" labels
    geom_text(
        data = data.frame(
            x = lubridate::ymd(rep("2009-03-01", 3)), 
            y = c(500, 500, 500), 
            label = rep("Historical", 3)), 
        aes(x = x, y = y, label = label), 
        color = "grey60", size = 5, family = "Fira Sans Condensed"
    ) + 
    geom_segment(
        data = data.frame(
            x = lubridate::ymd(rep("2011-01-01", 3)), 
            xend = lubridate::ymd(rep("2011-12-01", 3)), 
            y = c(500, 500, 500), 
            yend = c(700, 650, 800), 
            country = c("China", "Germany", "U.S.")),
        aes(x = x, y = y, xend = xend, yend = yend), 
        color = "grey60", size = 0.5
    )

ggsave(
    file.path(dir$figs, 'pdf', 'cost_historical.pdf'),
    cost_historical_plot, height = 4, width = 12, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'cost_historical.png'),
    cost_historical_plot, height = 4, width = 12)

# Cumulative savings historical ----

cum_savings_historical_plot <- cost$savings %>%
    mutate(country = fct_relevel(country, c("Germany", "U.S.", "China"))) %>%
    ggplot() +
    geom_area(aes(x = year, y = cum_savings_bil, fill = country)) +
    scale_fill_manual(values = c("#E5C61A", "#1A9FE5", "#E5601A")) +
    scale_x_continuous(breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        labels = dollar,
        breaks = seq(0, 150, 50),
        limits = c(0, 150),
        expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_family = "Fira Sans Condensed") +
    theme(
        plot.title.position = "plot",
        legend.position = "none"
    ) +
    # Add country labels
    geom_text(
        data = data.frame(
            x = c(2017, 2017, 2016), 
            y = c(30, 85, 130), 
            label = c("China", "U.S.", "Germany")), 
        aes(x = x, y = y, label = label, color = label), 
        size = 6, family = "Fira Sans Condensed"
    ) +
    annotate(
        "segment", x = 2016.8, xend = 2017.5, y = 128, yend = 117,
        colour = "black") +
    scale_color_manual(values = c("white", "black", "white")) + 
    labs(
        title = "Cumulative module cost savings from global vs. national learning",
        x = NULL,
        y = "Cumulative savings (Billion 2018 $USD)",
        fill = "Country")

ggsave(
    file.path(dir$figs, 'pdf', 'cum_savings_historical.pdf'),
    cum_savings_historical_plot, height = 4, width = 6.5, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'cum_savings_historical.png'),
    cum_savings_historical_plot, height = 4, width = 6.5)

# Annual savings historical ----

cum_savings_labels <- cost$savings %>% 
    filter(year == 2018) %>% 
    mutate(
        mean = scales::dollar(round(cum_savings_bil)), 
        lb = scales::dollar(round(cum_savings_bil_lb)),
        ub = scales::dollar(round(cum_savings_bil_ub)),
        x = 2009, 
        y = 22,
        label = paste0(
            "Cumulative savings:\n", mean, " (", lb, " - ", ub, ") billion"))
ann_savings_historical_plot <- cost$savings %>% 
    filter(year >= 2009) %>% 
    mutate(country = fct_relevel(country, c("Germany", "U.S.", "China"))) %>%
    ggplot() + 
    geom_col(aes(x = year, y = ann_savings_bil, fill = country)) + 
    geom_errorbar(
        aes(x = year, ymin = ann_savings_bil_lb, ymax = ann_savings_bil_ub), 
        color = "grey42", width = 0.5) + 
    facet_wrap(vars(country), nrow = 1) +
    scale_x_continuous(breaks = seq(2009, 2017, 2)) +
    scale_y_continuous(
        labels = scales::dollar, 
        expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = c("#E5C61A", "#1A9FE5", "#E5601A")) +
    theme_minimal_hgrid(
        font_size = 16,
        font_family = "Fira Sans Condensed") +
    panel_border() +
    theme(
        plot.title.position = "plot",
        legend.position = "none",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(
            size = 0.5, colour = "grey90")
    ) +
     labs(
        title = "Annual module cost savings from global vs. national learning",
        x = NULL,
        y = "Annual savings (Billion 2018 $USD)",
        fill = "Country") + 
    # Add totals
    geom_text(
        data = cum_savings_labels,
        aes(x = x, y = y, label = label), 
        size = 4.5, family = "Roboto Condensed", hjust = 0
    ) 

ggsave(
    file.path(dir$figs, 'pdf', 'ann_savings_historical_plot.pdf'),
    ann_savings_historical_plot, height = 4, width = 12, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'ann_savings_historical_plot.png'),
    ann_savings_historical_plot, height = 4, width = 12)

# 2030 Projections - From historical 2018 levels-----

cost_proj_hist_cost <- rbind(
  proj$us_national_hist_cost %>%
    mutate(country = "U.S.", scenario = "National"),
  proj$us_global_hist_cost %>%
    mutate(country = "U.S.", scenario = "Global"),
  proj$china_national_hist_cost %>%
    mutate(country = "China", scenario = "National"),
  proj$china_global_hist_cost %>%
    mutate(country = "China", scenario = "Global"),
  proj$germany_national_hist_cost %>%
    mutate(country = "Germany", scenario = "National"),
  proj$germany_global_hist_cost %>%
    mutate(country = "Germany", scenario = "Global")) %>%
  mutate(
    scenario = fct_relevel(scenario, c("National", "Global")),
    scenario = fct_recode(scenario,
      "Global learning" = "Global",
      "National learning" = "National"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  geom_ribbon(
    aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
        fill = scenario), alpha = 0.25) +
  geom_line(
    aes(x = year, y = cost_per_kw, color = scenario),
    alpha = 0.6, size = 1) +
  facet_wrap(vars(country), nrow = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2017-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  expand_limits(y = 0) +
  scale_color_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  scale_fill_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  theme_minimal_grid(
    font_size = 16,
    font_family = "Fira Sans Condensed") +
  panel_border() +
  theme(
    plot.title.position = "plot",
    legend.position = "right",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(
      size = 0.5, colour = "grey90")
  ) +
  labs(
    title = 'Projected Module Costs Using Global vs. National Learning Rates',
    y = "Cost per kW (2018 $USD)",
    x = "Year")

ggsave(
  file.path(dir$figs, 'pdf', 'cost_proj_hist_cost.pdf'),
  cost_proj_hist_cost, height = 4, width = 12, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'cost_proj_hist_cost.png'),
  cost_proj_hist_cost, height = 4, width = 12)

# 2030 Projections - From modeled 2018 levels-----

cost_proj_modeled_cost <- rbind(
  proj$us_national_modeled_cost %>%
    mutate(country = "U.S.", scenario = "National"),
  proj$us_global_modeled_cost %>%
    mutate(country = "U.S.", scenario = "Global"),
  proj$china_national_modeled_cost %>%
    mutate(country = "China", scenario = "National"),
  proj$china_global_modeled_cost %>%
    mutate(country = "China", scenario = "Global"),
  proj$germany_national_modeled_cost %>%
    mutate(country = "Germany", scenario = "National"),
  proj$germany_global_modeled_cost %>%
    mutate(country = "Germany", scenario = "Global")) %>%
  mutate(
    scenario = fct_relevel(scenario, c("National", "Global")),
    scenario = fct_recode(scenario,
      "Global learning" = "Global",
      "National learning" = "National"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  geom_ribbon(
    aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
        fill = scenario), alpha = 0.25) +
  geom_line(
    aes(x = year, y = cost_per_kw, color = scenario),
    alpha = 0.6, size = 1) +
  facet_wrap(vars(country), nrow = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2017-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  expand_limits(y = 0) +
  scale_color_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  scale_fill_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  theme_minimal_grid(
    font_size = 16,
    font_family = "Fira Sans Condensed") +
  panel_border() +
  theme(
    plot.title.position = "plot",
    legend.position = "right",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(
      size = 0.5, colour = "grey90")
  ) +
  labs(
    title = 'Projected Module Costs Using Global vs. National Learning Rates',
    y = "Cost per kW (2018 $USD)",
    x = "Year")

ggsave(
  file.path(dir$figs, 'pdf', 'cost_proj_modeled_cost.pdf'),
  cost_proj_modeled_cost, height = 4, width = 12, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'cost_proj_modeled_cost.png'),
  cost_proj_modeled_cost, height = 4, width = 12)







# 2008 - 2030 Projections - From historical 2018 levels-----

cost_proj_modeled_cost_08_30 <- rbind(
  cost$cost_scenarios,
  proj$us_national_modeled_cost %>%
    mutate(scenario = "national", country = "U.S."),
  proj$us_global_modeled_cost %>%
    mutate(scenario = "global", country = "U.S."),
  proj$china_national_modeled_cost %>%
    mutate(scenario = "national", country = "China"),
  proj$china_global_modeled_cost %>%
    mutate(scenario = "global", country = "China"),
  proj$germany_national_modeled_cost %>%
    mutate(scenario = "national", country = "Germany"),
  proj$germany_global_modeled_cost %>%
    mutate(scenario = "global", country = "Germany")) %>%
  mutate(
    scenario = fct_relevel(scenario, c("national", "global")),
    scenario = fct_recode(scenario,
                          "Global learning" = "global",
                          "National learning" = "national"),
    year = lubridate::ymd(paste0(year, "-01-01"))) %>%
  ggplot() +
  # First add historical cost line as dashed line
  geom_line(
    data = cost_historical %>%
      filter(component == "Module") %>%
      mutate(year = lubridate::ymd(paste0(year, "-01-01"))),
    aes(x = year, y = costPerKw), linetype = 2, alpha = 0.4, size = 1) +
  # Now add modeled costs
  geom_ribbon(
    aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
        fill = scenario), alpha = 0.25) +
  geom_line(
    aes(x = year, y = cost_per_kw, color = scenario),
    alpha = 0.6, size = 1) +
  facet_wrap(vars(country), nrow = 1) +
  scale_x_date(
    limits = lubridate::ymd(c("2007-07-01", "2030-07-01")),
    date_labels = "'%y",
    date_breaks = "2 years") +
  scale_y_continuous(labels = scales::dollar) +
  expand_limits(y = 0) +
  scale_color_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  scale_fill_manual("Scenario", values = c("#E5601A", "#1A9FE5")) +
  theme_minimal_grid(
    font_size = 16,
    font_family = "Fira Sans Condensed") +
  panel_border() +
  theme(
    plot.title.position = "plot",
    legend.position = "right",
    strip.background = element_rect(fill = "grey80"),
    panel.grid.major = element_line(
      size = 0.5, colour = "grey90")
  ) +
  labs(
    title = 'Module Costs Using Global vs. National Learning Rates',
    y = "Cost per kW (2018 $USD)",
    x = "Year") +
  # Add "historical" labels
  geom_text(
    data = data.frame(
      x = lubridate::ymd(rep("2009-03-01", 3)),
      y = c(500, 500, 500),
      label = rep("Historical", 3)),
    aes(x = x, y = y, label = label),
    color = "grey60", size = 5, family = "Fira Sans Condensed"
  ) +
  geom_segment(
    data = data.frame(
      x = lubridate::ymd(rep("2011-01-01", 3)),
      xend = lubridate::ymd(rep("2011-12-01", 3)),
      y = c(500, 500, 500),
      yend = c(700, 650, 800),
      country = c("China", "Germany", "U.S.")),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "grey60", size = 0.5
  )

ggsave(
  file.path(dir$figs, 'pdf', 'cost_proj_modeled_cost_08_30.pdf'),
  cost_proj_modeled_cost_08_30, height = 4, width = 12, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'cost_proj_modeled_cost_08_30.png'),
  cost_proj_modeled_cost_08_30, height = 4, width = 12)







# # Check for color blindness
# colorblindr::cvd_grid(plot)
