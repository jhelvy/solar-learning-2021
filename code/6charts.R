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

ggsave(here::here(dir$figs, 'pdf', "pvProduction.pdf"),
       pvProduction, width = 8, height = 6, device = cairo_pdf)
ggsave(here::here(dir$figs, 'png', "pvProduction.png"),
       pvProduction, width = 8, height = 6, dpi = 300)

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
    # First add historical cost line as dashed line
    geom_line(
        data = cost_historical_true %>% 
            mutate(year = lubridate::ymd(paste0(year, "-01-01"))),
        aes(x = year, y = costPerKw), linetype = 2, alpha = 0.4, size = 1) +
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
      limits = c(0, 6500),
      breaks = seq(0, 6000, 1000),
      labels = scales::dollar) +
    scale_color_manual("Learning", values = colors_learning) +
    scale_fill_manual("Learning", values = colors_learning) +
    theme_minimal_grid(
        font_size = 16,
        font_family = font_main) +
    panel_border() +
    labs(
      title = paste0(
        "Estimated module costs using <span style = 'color: ",
        colors_learning["Global"], 
        ";'>Global</span> vs. <span style = 'color: ", 
        colors_learning["National"], 
        ";'>National</span> learning"),
        y = paste0("Cost per kW (", year_inflation, " $USD)"),
        x = "Year", 
        caption = "Uncertainty bands represent 95% confidence interval from estimated learning model"
    ) + 
    theme(
        plot.title.position = "plot",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(size = 0.5, colour = "grey90"),
        axis.line.x = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1, size = 11, face = "italic"),
        plot.title = element_markdown(),
        legend.position = "none"
        # legend.position = c(0.87, 0.78),
        # legend.margin = margin(6, 6, 6, 6),
        # legend.background = element_rect(
        #     fill = "white", color = "black", size = 0.2),
    ) +
    # Add "historical" labels
    geom_text(
        data = data.frame(
            x = lubridate::ymd(rep("2009-03-01", 3)), 
            y = c(500, 500, 500), 
            label = rep("Historical", 3)), 
        aes(x = x, y = y, label = label), 
        color = "grey60", size = 5, family = font_main
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
    cost_historical_plot, height = 4.25, width = 11, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'cost_historical.png'),
    cost_historical_plot, height = 4.25, width = 11)

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
      breaks = seq(0, 200, 50),
      limits = c(0, 200),
      expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_family = font_main) +
    scale_color_manual(values = c("white", "black", "white")) +
    labs(
        title = "Cumulative module cost savings from global vs. national learning",
        x = "Year",
        y = paste0("Cumulative savings (Billion ", year_inflation, " $USD)"),
        fill = "Country") +
    theme(
        plot.title.position = "plot",
        legend.position = "none"
    ) +
    # Add country labels
    geom_text(
        data = data.frame(
            x = c(2018, 2018, 2016.5), 
            y = c(45, 110, 175), 
            label = c("China", "U.S.", "Germany")), 
        aes(x = x, y = y, label = label, color = label), 
        size = 6, family = font_main
    ) +
    annotate(
        "segment", x = 2017.5, xend = 2018.2, y = 171, yend = 155,
        colour = "black")

ggsave(
    file.path(dir$figs, 'pdf', 'savings_cum_historical_plot.pdf'),
    savings_cum_historical_plot, height = 4, width = 6.5, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'savings_cum_historical_plot.png'),
    savings_cum_historical_plot, height = 4, width = 6.5)

# Annual savings historical ----

cum_savings_labels <- cost$savings %>% 
    filter(year == max(year)) %>%
    mutate(
        mean = round(cum_savings_bil), 
        lb = round(cum_savings_bil_lb),
        ub = round(cum_savings_bil_ub),
        x = 2007, 
        y = 26,
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
        breaks = seq(0, 30, 10),
        limits = c(-3, 30),
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
        title = "Annual module cost savings from global vs. national learning (2008 - 2019)",
        x = NULL,
        y = paste0("Annual savings (Billion ", year_inflation, " $USD)"),
        fill = "Country") + 
    # Add totals
    geom_text(
        data = cum_savings_labels,
        aes(x = x, y = y, label = label), 
        size = 4.5, family = "Roboto Condensed", hjust = 0
    ) 

ggsave(
    file.path(dir$figs, 'pdf', 'savings_ann_historical_plot.pdf'),
    savings_ann_historical_plot, height = 4, width = 12, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'png', 'savings_ann_historical_plot.png'),
    savings_ann_historical_plot, height = 4, width = 12)

# 2030 Projections -----

cost_proj <- proj %>% 
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
    limits = lubridate::ymd(c("2017-07-01", "2030-07-01")),
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
      "Projected module costs using <span style = 'color: ",
      colors_learning["Global"], 
      ";'>Global</span> vs. <span style = 'color: ", 
      colors_learning["National"], 
      ";'>National</span> learning (2019 - 2030)"),
    caption = "Uncertainty bands represent 95% confidence interval from estimated learning model")

ggsave(
  file.path(dir$figs, 'pdf', 'cost_proj.pdf'),
  cost_proj, height = 6.5, width = 11, device = cairo_pdf)
ggsave(
  file.path(dir$figs, 'png', 'cost_proj.png'),
  cost_proj, height = 6.5, width = 11)

