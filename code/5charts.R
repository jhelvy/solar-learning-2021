# Load librbase_family = # Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load historical cost scenario data
cost <- readRDS(dir$scenarios_hist)

# Load projections
proj <- readRDS(dir$scenarios_proj)

# Check any plot for color blindness
# colorblindr::cvd_grid(plot)

# Global PV production ----

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
    breaks = seq(0, 200, 50),
    expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "#767676", "#0088cc", "#ff6611", "#ffee55", "#aa6688", "#aacc22",
    "#ff1417"
  )) +
  theme_minimal_hgrid(font_family = font_main, font_size = 16) +
  theme(
    legend.position = c(0.01, 0.706),
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
       caption = "Data from Jäger-Waldau, A. (2022) https://doi.org/10.1051/epjpv/2022010")

save_fig(pvProduction, "pvProduction", width = 8, height = 6)

# Cost per kw for global vs. national learning ----

# First, just plot the global markets scenario
cost_historical_global_plot <- cost$cost %>%
    mutate(
        learning = str_to_title(learning),
        learning = fct_relevel(learning, c("National", "Global")),
        year = lubridate::ymd(paste0(year, "-01-01"))) %>%
    filter(learning == "Global") %>%
    ggplot() +
    facet_wrap(vars(country), nrow = 1) +
    geom_point(aes(x = year, y = cost_per_kw_hist), size = 1) +
    geom_line(
        aes(
            x = year,
            y = cost_per_kw,
        ),
        alpha = 0.6, size = 1
    ) +
    geom_ribbon(
        aes(
            x = year,
            ymin = cost_per_kw_lb,
            ymax = cost_per_kw_ub,
        ),
        alpha = 0.22
    ) +
    scale_x_date(
        limits = lubridate::ymd(c(
            paste0(plot_min_year - 1, "-07-01"),
            paste0(plot_max_year, "-07-01"))
        ),
        date_labels = "'%y",
        date_breaks = "2 years") +
    scale_y_continuous(labels = scales::dollar, breaks = seq(0, 6000, 1000)) +
    labs(
      title = "Estimated Module Prices",
        y = paste0("Price per kW (", year_inflation, " $USD)"),
        x = "Year"
    ) +
    theme_minimal_grid(
        font_size = 16,
        font_family = font_main
    ) +
    panel_border() +
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
            y = rep(400, 3),
            country = c("China", "Germany", "U.S."),
            label = rep("Historical", 3)),
        aes(x = x, y = y, label = label),
        color = "black", size = 5, family = font_main
    ) +
    geom_segment(
        data = data.frame(
            x = lubridate::ymd(c("2011-02-01", "2011-02-01", "2011-02-01")),
            xend = lubridate::ymd(c("2011-10-01", "2011-10-01", "2011-11-01")),
            y = c(500, 400, 500),
            yend = c(660, 520, 850),
            country = c("China", "Germany", "U.S.")),
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "black", size = 0.5
    )

save_fig(
    cost_historical_global_plot, "cost_historical_global_plot",
    width = 11, height = 4.25
)

cost_historical_plot <- 
    make_historical_plot(cost$cost, size = 16) +
    scale_y_continuous(
        limits = c(0, 6000),
        breaks = seq(0, 6000, 1000),
        labels = scales::dollar
    ) +
    # Add "historical" labels
    geom_text(
        data = data.frame(
            x = lubridate::ymd(rep("2009-01-01", 3)), 
            y = rep(400, 3),
            country = c("China", "Germany", "U.S."),
            label = rep("Historical", 3)), 
        aes(x = x, y = y, label = label), 
        color = "black", size = 5, family = font_main
    ) + 
    geom_segment(
        data = data.frame(
            x = lubridate::ymd(c("2011-02-01", "2011-02-01", "2011-02-01")), 
            xend = lubridate::ymd(c("2011-10-01", "2011-10-01", "2011-11-01")), 
            y = c(500, 400, 500), 
            yend = c(660, 520, 850), 
            country = c("China", "Germany", "U.S.")),
        aes(x = x, y = y, xend = xend, yend = yend), 
        color = "black", size = 0.5
    )

save_fig(
    cost_historical_plot, "cost_historical_plot",
    width = 11, height = 4.25
)



# Cumulative savings historical ----

savings_cum_historical_plot <- make_cum_savings_plot(cost$savings)

save_fig(
    savings_cum_historical_plot, "savings_cum_historical_plot",
    width = 6, height = 5
)



# Annual savings historical ----

# First make label data
cum_savings_labels <- cost$savings %>% 
    filter(year == max(year)) %>%
    mutate(
        mean = round(cum_savings_bil), 
        lb = round(cum_savings_bil_lb),
        ub = round(cum_savings_bil_ub),
        x = 2008, 
        y = 9, 
        label = paste0(
            "Cumulative savings:\nB $ ", mean, " (", lb, " - ", ub, ")")
    )

# Now make the plot
savings_ann_historical_plot <- make_ann_savings_plot(cost$savings, size = 16) +
    scale_y_continuous(
        labels = scales::dollar, 
        breaks = seq(0, 10, 2),
        limits = c(0, 10),
        expand = expansion(mult = c(0, 0.05))
    ) +
    # Add totals labels
    geom_text(
        data = cum_savings_labels,
        aes(x = x, y = y, label = label),
        size = 4.5, family = "Roboto Condensed", hjust = 0
    )

save_fig(
    savings_ann_historical_plot, "savings_ann_historical_plot",
    width = 11, height = 4.25
)




# 2030 Projections ----

cost_proj <- make_projection_plot(proj$nat_trends, proj$sus_dev, size = 16)

save_fig(cost_proj, "cost_proj", width = 11, height = 6.5)




# Annual savings projection ----

# First make label data
cum_savings_labels_nat_trends <- proj$savings_nat_trends %>% 
    filter(year == max(year)) %>%
    mutate(
        mean = round(cum_savings_bil), 
        lb = round(cum_savings_bil_lb),
        ub = round(cum_savings_bil_ub),
        x = 2020, 
        y = 5, 
        label = paste0(
            "Cumulative savings:\nB $ ", mean, " (", lb, " - ", ub, ")")
    )
cum_savings_labels_sus_dev <- proj$savings_sus_dev %>% 
    filter(year == max(year)) %>%
    mutate(
        mean = round(cum_savings_bil), 
        lb = round(cum_savings_bil_lb),
        ub = round(cum_savings_bil_ub),
        x = 2020, 
        y = 5, 
        label = paste0(
            "Cumulative savings:\nB $ ", mean, " (", lb, " - ", ub, ")")
    )
cum_savings_labels_proj <- rbind(
  cum_savings_labels_nat_trends, 
  cum_savings_labels_sus_dev) %>% 
  mutate(
    scenario = fct_recode(scenario, 
      "National Trends" = "nat_trends", 
      "Sustainable Development" = "sus_dev"))

# Now make the plot
savings_ann_proj_plot <- make_ann_savings_proj_plot(
  proj$savings_nat_trends, proj$savings_sus_dev, size = 16) +
    scale_y_continuous(
        labels = scales::dollar, 
        breaks = seq(0, 6, 2),
        limits = c(0, 6),
        expand = expansion(mult = c(0, 0.05))
    ) +
    # Add totals labels
    geom_text(
        data = cum_savings_labels_proj,
        aes(x = x, y = y, label = label),
        size = 4.5, family = "Roboto Condensed", hjust = 0
    )

save_fig(
    savings_ann_proj_plot, "savings_ann_proj_plot",
    width = 11, height = 6.5
)



# Compare capacity data from NREL, SEIA, and IRENA ----

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
ex_compare_capacity_type <- nrelSeia %>%
  filter(year <= plot_max_year) %>%
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue"),
    breaks = c("NREL", "SEIA")) +
  facet_wrap(vars(installType)) +
  theme_bw(base_family = font_main) +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Comparison of installed capacity by type and data source")

save_fig(
    ex_compare_capacity_type, "ex_compare_capacity_type",
    width = 9, height = 3
)




# During the overlapping period, NREL and SEIA data match quite closely
# Only deviation is that NREL Commercial installations are slightly lower
# than those in SEIA

# Merge in IRENA data to compare total capacity across all 3 sources
ex_compare_capacity_cumulative <- nrelSeia %>%
  group_by(year, source) %>%
  summarise(cumCapacityGw = sum(cumCapacityKw) / 10^6) %>%
  rbind(
    data$irenaCumCapacityMw %>%
      mutate(
        source = "IRENA", 
        cumCapacityGw = usa / 10^3) %>% 
      select(year, source, cumCapacityGw)
  ) %>%
  filter(year <= plot_max_year) %>%
  ggplot(aes(x = year, y = cumCapacityGw, color = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "SEIA", "IRENA")) +
  theme_bw(base_family = font_main) +
  labs(
    x = NULL,
    y = "Installed Capacity (GW)",
    color = "Data source",
    title = "Comparison of cumulative installed data")

save_fig(
    ex_compare_capacity_cumulative, "ex_compare_capacity_cumulative",
    width = 5, height = 3
)



# IRENA data track differently from NREL and SEIA. 
# They're slightly higher in the period before 2014 and lower afterwards

# Based on these comparisons, we use SEIA data for installed capacity as
# it tracks with NREL and covers a longer time period

# Compare NREL, LBNL, and SPV Cost data ----

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

ex_compare_cost <- cost_compare %>%
  ggplot(aes(x = year, y = costPerKw, color = source, group = source)) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "LBNL", "SPV")) +
  theme_bw(base_family = font_main) +
  labs(x = NULL,
       y = "Price per kW ($USD)",
       color = "Data source",
       title = "Comparison of price per kW by data source")

save_fig(ex_compare_cost, "ex_compare_cost", width = 5, height = 3)



# NREL and LBNL cost data are relatively similar for modules, with 
# the biggest disagreement in the earlier years. We decided to use LBNL cost
# data for the earlier period (2000 - 2018) and NREL for 2019 & 2020 as 
# the two appear to converge in the later years.

# Interpreting lambda ----

plotData <- rbind(
    prep_lambda_df(data$hist_us) %>% mutate(country = "U.S."),
    prep_lambda_df(data$hist_china) %>% mutate(country = "China"),
    prep_lambda_df(data$hist_germany) %>% mutate(country = "Germany")) %>%
    mutate(
      q = q / 10^6,
      date = lubridate::ymd(paste0(year, "-07-01"))
    )
  
lambda_compare <- ggplot(plotData) + 
    geom_line(
        aes(x = date, y = q, group = lambda)
    ) + 
    geom_text(
        data = plotData %>% 
            group_by(country, lambda) %>% 
            filter(year == max(year)),
        aes(x = date, y = q, label = label),
        hjust = 0, nudge_x = 150
    ) +
    facet_wrap(vars(country)) +
    expand_limits(x = lubridate::ymd(paste0(c(2005, 2030), "-07-01"))) +
    scale_x_date(
        breaks = lubridate::ymd(paste0(seq(2005, 2030, 5), "-07-01")),
        date_labels = "'%y") +
    theme_minimal_grid(
        font_size = 16,
        font_family = font_main
    ) +
    panel_border() +
    theme(
        plot.title.position = "plot",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(size = 0.3, colour = "grey90"),
        axis.line.x = element_blank(),
        plot.title = element_markdown(),
        legend.position = "none"
    ) +
    labs(
        x = "Year", 
        y = "Cumulative installed capacity (GW)"
    )

save_fig(lambda_compare, "lambda_compare", width = 11, height = 3.75)




# Historical Silicon prices -----

silicon_prices <- data$silicon %>%
  rbind(data.frame(
    year = c(2019, 2020), 
    price_si = c(15.4, 15.4))) %>% 
  ggplot(aes(x = year, y = price_si)) +
  annotate(
    geom = "rect", xmin = 2006, xmax = 2020,
    ymin = 0, ymax = 400, fill = "#8C8C8C",
    alpha = 0.15
  ) +
  geom_line(alpha = 0.5) +
  geom_point(pch = 21, fill = "white") +
  scale_color_manual(
    values = c("red", "dodgerblue", "black"),
    breaks = c("NREL", "SEIA", "IRENA")) +
  scale_x_continuous(
    limits = c(1980, 2020),
    breaks = seq(1980, 2020, 10)) +
  theme_bw(base_family = font_main) + 
  theme(
    plot.caption = element_text(
      hjust = 0, size = 10, family = font_main, face = "italic"),
    plot.caption.position = "plot") +
  annotate(
    geom = "text", x = 2010, y = 385, 
    label = "Period of study", 
    family = font_main, hjust = 0
  ) +
  labs(
    x = NULL,
    y = "Price per kg ($USD)",
    title = "Historical global silicon prices (1980 - 2020)", 
    caption = "Data from Nemet, G. (2019) https://doi.org/10.4324/9780367136604"
  )

save_fig(silicon_prices, "silicon_prices", width = 5, height = 3.75)
