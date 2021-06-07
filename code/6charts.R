# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load cost scenario data
cost <- readRDS(dir$cost_scenarios)

# Create all data frames for plotting -----------------------------

# **Modeled costs: all using the 2 factor models**




# Cost per kw for global vs. national learning ------

# True historical cost per kW
cost_historical <- rbind(
    data$usSeiaLbnl %>%
        filter(
            year >= 2008, year <= 2018,
            component == "Module", installType == "Utility") %>%         
        mutate(country = "U.S."),
    data$china %>%
        filter(
            year >= 2008, year <= 2018,
            component == "Module") %>%
        mutate(country = "China"),
    data$germany %>%
        filter(
            year >= 2008, year <= 2018,
            component == "Module") %>%
        mutate(country = "Germany")
    )

cost_historical_plot <- rbind(
  cost$cost_scenarios_us, 
  cost$cost_scenarios_china, 
  cost$cost_scenarios_germany) %>% 
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
            xend = lubridate::ymd(rep("2012-01-01", 3)), 
            y = c(500, 500, 500), 
            yend = c(700, 850, 800), 
            country = c("China", "Germany", "U.S.")),
        aes(x = x, y = y, xend = xend, yend = yend), 
        color = "grey60", size = 0.5
    )

# colorblindr::cvd_grid(cost_historical_plot)

ggsave(
    file.path(dir$figs, 'final', 'cost_historical.pdf'),
    cost_historical_plot, height = 4, width = 12, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'final', 'cost_historical.png'),
    cost_historical_plot, height = 4, width = 12)




# Cumulative savings ----

# Annual costs
cost_2f_range_ann <- rbind(
    cost$us_2f_range_cum %>%
        group_by(year) %>%
        summarise(
            annSavings = sum(cost_diff_bil), 
            annSavings_lb = sum(cost_diff_bil_lb),
            annSavings_ub = sum(cost_diff_bil_ub)) %>% 
        mutate(country = 'USA'),
    cost$germany_2f_range_cum %>%
        group_by(year) %>%
        select(
            annSavings = cost_diff_bil, 
            annSavings_lb = cost_diff_bil_lb,
            annSavings_ub = cost_diff_bil_ub) %>% 
        mutate(country = 'Germany'),
    cost$china_2f_range_cum %>%
        group_by(year) %>%
        select(
            annSavings = cost_diff_bil, 
            annSavings_lb = cost_diff_bil_lb,
            annSavings_ub = cost_diff_bil_ub) %>% 
        mutate(country = 'China'))

# Cumulative costs
cost_2f_range_cum <- rbind(
    cost$us_2f_range_cum %>%
        group_by(year) %>%
        summarise(
            cumSavings = sum(cost_diff_cum), 
            cumSavings_lb = sum(cost_diff_cum_lb),
            cumSavings_ub = sum(cost_diff_cum_ub)) %>% 
        mutate(country = 'USA'),
    cost$germany_2f_range_cum %>%
        group_by(year) %>%
        select(
            cumSavings = cost_diff_cum, 
            cumSavings_lb = cost_diff_cum_lb,
            cumSavings_ub = cost_diff_cum_ub) %>% 
        mutate(country = 'Germany'),
    cost$china_2f_range_cum %>%
        group_by(year) %>%
        select(
            cumSavings = cost_diff_cum, 
            cumSavings_lb = cost_diff_cum_lb,
            cumSavings_ub = cost_diff_cum_ub) %>% 
        mutate(country = 'China'))

# Cost data frames -- Projections BAU
cost_us2030 <- cost$us2030_2f_range %>%
    filter(model == "2factor", installType == 'Utility') %>%
    mutate(scenario = str_to_upper(scenario))

cum_savings_historical_plot <- cost_2f_range_cum %>%
    mutate(country = fct_relevel(country, c("Germany", "USA", "China"))) %>%
    ggplot() +
    geom_area(aes(x = year, y = cumSavings, fill = country)) +
    scale_fill_manual(values = c("#E5C61A", "#1A9FE5", "#E5601A")) +
    scale_x_continuous(breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        labels = dollar,
        breaks = seq(0, 150, 50),
        expand = expansion(mult = c(0, 0.05))) +
    theme_minimal_hgrid(font_family = "Fira Sans Condensed") +
    theme(
        plot.title.position = "plot",
        legend.position = "none"
    ) +
    # Add country labels
    geom_text(
        data = data.frame(
            x = c(2016, 2016, 2015.5), 
            y = c(25, 67, 130), 
            label = c("China", "USA", "Germany")), 
        aes(x = x, y = y, label = label, color = label), 
        size = 6, family = "Fira Sans Condensed"
    ) +
    annotate(
        "segment", x = 2016.4, xend = 2017, y = 128, yend = 121,
        colour = "black") +
    scale_color_manual(values = c("white", "black", "white")) + 
    labs(
        title = "Cumulative module cost savings from global vs. national learning",
        x = NULL,
        y = "Cumulative savings (Billion 2018 $USD)",
        fill = "Country")

# colorblindr::cvd_grid(cum_savings_historical)

ggsave(
    file.path(dir$figs, 'final', 'cum_savings_historical.pdf'),
    cum_savings_historical_plot, height = 4, width = 6.5, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'final', 'cum_savings_historical.png'),
    cum_savings_historical_plot, height = 4, width = 6.5)

# Annual savings ----
cum_savings_labels <- cost_2f_range_ann %>% 
    group_by(country) %>% 
    summarise(
        mean = scales::dollar(round(sum(annSavings))), 
        lb = scales::dollar(round(sum(annSavings_lb))),
        ub = scales::dollar(round(sum(annSavings_ub)))) %>% 
    mutate(
        x = 2009, 
        y = 22,
        label = paste0(
            "Cumulative savings:\n", mean, " (", lb, " - ", ub, ") billion"))
ann_savings_historical_plot <- cost_2f_range_ann %>%
    filter(year >= 2009) %>% 
    mutate(country = fct_relevel(country, c("Germany", "USA", "China"))) %>%
    ggplot() + 
    geom_col(aes(x = year, y = annSavings, fill = country)) + 
    geom_errorbar(
        aes(x = year, ymin = annSavings_lb, ymax = annSavings_ub), 
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
        size = 4.5, family = "Fira Sans Condensed", hjust = 0
    ) 

ggsave(
    file.path(dir$figs, 'final', 'ann_savings_historical_plot.pdf'),
    ann_savings_historical_plot, height = 4, width = 12, device = cairo_pdf)
ggsave(
    file.path(dir$figs, 'final', 'ann_savings_historical_plot.png'),
    ann_savings_historical_plot, height = 4, width = 12)

# 
# 
# # 2030 Projections -----
# 
# bau_s1_us2030 <- cost_us2030 %>%
#      ggplot(aes(x = year, y = cost_per_kw, color = component)) +
#      geom_line(aes(linetype = scenario), size = 1) +
#      geom_point(aes(shape = scenario),
#                 fill = 'white', size = 1.7) +
#      facet_wrap(vars(component), nrow = 1) +
#      scale_x_continuous(
#          limits = c(2018, 2030),
#          breaks = seq(2018, 2030, 2)) +
#      # scale_y_continuous(
#      #     limits = c(0, 5500),
#      #     labels = scales::comma) +
#      scale_shape_manual(values = c(21, 16)) +
#      guides(color = FALSE, shape = FALSE) +
#      theme_minimal_grid() +
#      panel_border() +
#      theme(
#          legend.position = "bottom",
#          strip.background = element_rect(fill = "grey"))+
#      labs(title = 'Comparison of cost per kW (USA - Utility Only)',
#           y = "Cost per kW (2018 $USD)",
#           linetype = "Scenario:")
# 
# ggsave(
#     file.path(dir$figs, 'cost_scenarios', 'bau_s1_us2030.pdf'),
#     bau_s1_us2030, height = 4, width = 10)


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

ggsave(here::here(dir$figs, 'final', "pvProduction.pdf"),
  pvProduction, width = 8, height = 6, device = cairo_pdf)
ggsave(here::here(dir$figs, 'final', "pvProduction.png"),
  pvProduction, width = 8, height = 6, dpi = 300)

