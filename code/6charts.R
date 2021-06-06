# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load cost scenario data
cost <- readRDS(dir$cost_scenarios)


# Cost Scenarios  --------------------------------------------------------

# **All of these are using the 2 factor models**

# Cost data frames -- BAU, S1
cost_us <- cost$us_2f_range %>%
    filter(
        model == "2factor", installType == 'Utility') %>%
    mutate(scenario = str_to_upper(scenario))

cost_germany <- cost$germany_2f_range %>%
    filter(model == "2factor") %>%
    mutate(scenario = str_to_upper(scenario))

cost_china <- cost$china_2f_range %>%
    filter(model == "2factor") %>%
    mutate(scenario = str_to_upper(scenario))

cost_all <- rbind(
    mutate(cost_us, country = "USA"),
    mutate(cost_germany, country = "Germany"),
    mutate(cost_china, country = "China"))

# Cost data frames -- BAU, S1, S2
cost_us_s2 <- cost$us_2f_range_s2 %>%
    filter(
        model == "2factor", installType == 'Utility') %>%
    mutate(scenario = str_to_upper(scenario))

cost_germany_s2 <- cost$germany_2f_range_s2 %>%
    filter(model == "2factor") %>%
    mutate(scenario = str_to_upper(scenario))

cost_china_s2 <- cost$china_2f_range_s2 %>%
    filter(model == "2factor") %>%
    mutate(scenario = str_to_upper(scenario))

cost_all_s2 <- rbind(
    mutate(cost_us_s2, country = "USA"),
    mutate(cost_germany_s2, country = "Germany"),
    mutate(cost_china_s2, country = "China"))

# Cost data frames -- Projections BAU
cost_us2030 <- cost$us2030_2f_range %>%
    filter(
        model == "2factor", installType == 'Utility') %>%
    mutate(scenario = str_to_upper(scenario))


# Historical cost per KW -----------------------------------------------------

# US ---

costPerKw_us_Nrel <- data$usNrel %>%
    filter(year <= 2018) %>%
    mutate(installType = fct_relevel(installType,
        c('Residential', 'Commercial', 'Utility'))) %>%
    ggplot(aes(x = year, y = costPerKw,
               color = installType, group = installType)) +
    geom_line(size = 0.8) +
    geom_point(fill = 'white', shape = 21, size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    theme_bw() +
    theme(
        legend.position = 'bottom',
        plot.title.position = "plot"
    ) +
    scale_x_continuous(limits = c(2010, 2018)) +
    labs(x = 'Year', y = 'Cost / kW (USD 2018)', color = 'Type',
         title = 'U.S. Cost of Solar PV by Component (2010 - 2018)',
         caption = 'Data source: NREL')

ggsave(file.path(dir$figs, 'historical', 'costPerKw_us_Nrel.pdf'),
       costPerKw_us_Nrel, width = 12, height = 4)

costPerKw_us_seiaLbnl <- data$usSeiaLbnl %>%
    mutate(installType = fct_relevel(installType,
        c('Residential', 'Commercial', 'Utility'))) %>%
    ggplot(aes(x = year, y = costPerKw,
               color = installType, group = installType)) +
    geom_line(size = 0.8) +
    geom_point(fill = 'white', shape = 21, size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    theme_bw() +
    theme(
        legend.position = 'bottom',
        plot.title.position = "plot"
    ) +
    labs(x = 'Year', y = 'Cost / kW (USD 2018)', color = 'Type',
         title = 'U.S. Cost of Solar PV by Component (2000 - 2018)',
         caption = 'Data source: SEIA (capacity); LBNL (Cost)')

ggsave(file.path(dir$figs, 'historical', 'costPerKw_us_seiaLbnl.pdf'),
       costPerKw_us_seiaLbnl, width = 8, height = 4)

# Germany ---

costPerKw_germany <- data$germany %>%
    ggplot(aes(x = year, y = costPerKw)) +
    geom_line(size = 0.8) +
    geom_point(fill = 'white', shape = 21, size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    theme_bw() +
    theme(plot.title.position = "plot") +
    labs(x = 'Year', y = 'Cost / kW (USD 2018)',
         title = 'Germany Cost of Solar PV by Component (2006 - 2018)',
         caption = 'Data source: BSW Solar')

ggsave(file.path(dir$figs, 'historical', 'costPerKw_germany.pdf'),
       costPerKw_germany, width = 6, height = 3.5)

# China ---

costPerKw_china <- data$china %>%
    ggplot(aes(x = year, y = costPerKw)) +
    geom_line(size = 0.8) +
    geom_point(fill = 'white', shape = 21, size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    theme_bw() +
    theme(plot.title.position = "plot") +
    labs(x = 'Year', y = 'Cost / kW (USD 2018)',
         title = 'China Cost of Solar PV by Component (2007 - 2020)',
         caption = 'Data source: BSW Solar')

ggsave(file.path(dir$figs, 'historical', 'costPerKw_china.pdf'),
       costPerKw_china, width = 6, height = 3.5)

# US-Germany-China Module cost ---

# Merge historical data
data_historical <- rbind(
    data$usSeiaLbnl %>%
        filter(
            year >= 2008, year <= 2018,
            component == "Module", installType == "Utility") %>%         mutate(country = "USA"),
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

costPerKw_module <- data_historical %>%
    mutate(label = ifelse(year == max(year), country, NA)) %>%
    ggplot(aes(x = year, y = costPerKw, color = country)) +
    geom_line(size = 0.8) +
    geom_point(fill = 'white', shape = 21, size = 1.7) +
    geom_text_repel(aes(label = label),
        hjust = 0, nudge_x = 1, size = 5,
        direction = "y") +
    scale_x_continuous(
        limits = c(2008, 2020),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(limits = c(0, 4500)) +
    theme_bw() +
    theme(legend.position = 'none',
          plot.margin = margin(0.1, 2.7, 0.1, 0.1, "cm")) +
    coord_cartesian(clip = 'off') +
    labs(
        x = 'Year', y = 'Cost / kW (USD 2018)',
        color = 'Type',
        title = 'Cost of Solar PV Modules (2008 - 2018)')

ggsave(file.path(dir$figs, 'historical', 'costPerKw_module.pdf'),
       costPerKw_module, width = 7, height = 3.5)

# Demand curves --------------------------------------------------------

# USA - Module

data_us_modules <- data$usSeiaLbnl %>%
    filter(component == "Module") %>% 
    arrange(installType, year) %>% 
    group_by(installType) %>% 
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008, year <= 2018) %>% 
    select(year, installType, costPerKw, annCapacityKw) %>% 
    arrange(installType, year)

modules_us <- data_us_modules %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    facet_wrap(vars(installType)) + 
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for modules (US), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'modules_us.pdf'),
       modules_us, width = 10, height = 3.5)

# USA - Total

data_us_total <- data$usSeiaLbnl %>%
    arrange(installType, component, year) %>%
    group_by(installType, year) %>% 
    summarize(costPerKw = sum(costPerKw), cumCapacityKw = mean(cumCapacityKw)) %>%
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008, year <= 2018) %>% 
    select(year, installType, costPerKw, annCapacityKw) %>% 
    arrange(installType, year)

total_us <- data_us_total %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    facet_wrap(vars(installType)) + 
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for installations (US), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'total_us.pdf'),
       total_us, width = 10, height = 3.5)

# USA - Total post-tax credit

data_us_total_posttax <- data_us_total %>%
    merge(data$usITC) %>%
    mutate(costPerKwPosttax = costPerKw*(1-ITC))

total_us_posttax <- data_us_total_posttax %>% 
    ggplot(aes(x = costPerKwPosttax, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    facet_wrap(vars(installType)) + 
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw post-tax)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for installations (US), post-tax credit, log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'total_us_posttax.pdf'),
       total_us_posttax, width = 10, height = 3.5)

# Germany - Module

data_germany_modules <- data$germany %>%
    filter(component == "Module") %>% 
    arrange(year) %>% 
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008, year <= 2018) %>% 
    select(year, costPerKw, annCapacityKw) %>% 
    arrange(year)

modules_germany <- data_germany_modules %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for modules (Germany), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'modules_germany.pdf'),
       modules_germany, width = 6.5, height = 4.5)


# Germany - Total

data_germany_total <- data$germany %>%
    arrange(component, year) %>%
    group_by(year) %>% 
    summarize(costPerKw = sum(costPerKw), cumCapacityKw = mean(cumCapacityKw)) %>%
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008, year <= 2018) %>% 
    select(year, costPerKw, annCapacityKw) %>% 
    arrange(year)

total_germany <- data_germany_total %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for installations (Germany), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'total_germany.pdf'),
       total_germany, width = 6.5, height = 4.5)



# China - Module

data_china_modules <- data$china %>%
    arrange(year) %>% 
    filter(component == "Module") %>% 
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008, year <= 2018) %>% 
    select(year, costPerKw, annCapacityKw) %>% 
    arrange(year)

modules_china <- data_china_modules %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for modules (China), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'modules_china.pdf'),
       modules_china, width = 6, height = 4.5)

# China - Total

data_china_total <- data$china %>%
    arrange(component, year) %>%
    group_by(year) %>% 
    summarize(costPerKw = sum(costPerKw), cumCapacityKw = mean(cumCapacityKw)) %>%
    mutate(
        annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1), 
        lnAnnCap = log(annCapacityKw)) %>% 
    ungroup() %>% 
    filter(year >= 2008) %>% 
    select(year, costPerKw, annCapacityKw) %>% 
    arrange(year)

total_china <- data_china_total %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for installations (China), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'total_china.pdf'),
       total_china, width = 6, height = 4.5)


# Combine all countries on log scale

data_modules <- rbind(
    data_us_modules %>% 
        filter(installType == "Utility") %>% 
        select(-installType) %>% 
        mutate(country = "USA"),
    mutate(data_germany_modules, country = "Germany"), 
    mutate(data_china_modules, country = "China")
)

modules_all_facets <- data_modules %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    facet_wrap(vars(country), nrow = 1) +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for modules, log-scales")

ggsave(
    file.path(dir$figs, 'demand_models', 'modules_all_facets.pdf'),
    modules_all_facets, width = 10, height = 3.5)

modules_all <- data_modules %>% 
    group_by(year) %>%
    summarize(
        costPerKw = mean(costPerKw), 
        annCapacityKw = sum(annCapacityKw)) %>% 
    ggplot(aes(x = costPerKw, y = annCapacityKw)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_text_repel(
        aes(label = year), 
        size = 3, color = "grey") +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10() +
    theme_minimal_grid() + 
    panel_border() + 
    theme(strip.background = element_rect(fill = "grey")) +
    labs(
        x = "log(Cost per Kw)", 
        y = "log(Annual installed capacity)", 
        title = "Demand curve for modules (global), log-scales")

ggsave(file.path(dir$figs, 'demand_models', 'modules_all.pdf'),
       modules_all, width = 6, height = 4.5)


# BAU and S1 costs ----

(bau_s1_us <- cost_us %>%
    ggplot(aes(x = year, y = cost_per_kw, color = component)) +
    geom_line(aes(linetype = scenario), size = 1) +
    geom_point(aes(shape = scenario),
        fill = 'white', size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    scale_x_continuous(
        limits = c(2008, 2018),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        limits = c(0, 5500),
        labels = scales::comma) +
    scale_shape_manual(values = c(21, 16)) +
    guides(color = FALSE, shape = FALSE) +
    theme_minimal_grid() +
    panel_border() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey"))+
    labs(title = 'Comparison of cost per kW (USA - Utility Only)',
         y = "Cost per kW (2018 $USD)",
         linetype = "Scenario:"))

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_us.pdf'),
    bau_s1_us, height = 4, width = 10)

bau_s1_germany <- cost_germany %>%
    ggplot(aes(x = year, y = cost_per_kw, color = component)) +
    geom_line(aes(linetype = scenario), size = 1) +
    geom_point(aes(shape = scenario),
        fill = 'white', size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    scale_x_continuous(
        limits = c(2008, 2018),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        limits = c(0, 4500),
        labels = scales::comma) +
    scale_shape_manual(values = c(21, 16)) +
    guides(color = FALSE, shape = FALSE) +
    theme_minimal_grid() +
    panel_border() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey"))+
    labs(title = 'Comparison of cost per kW (Germany)',
         y = "Cost per kW (2018 $USD)",
         linetype = "Scenario:")

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_germany.pdf'),
    bau_s1_germany, height = 4, width = 8)

bau_s1_china <- cost_china %>%
    ggplot(aes(x = year, y = cost_per_kw, color = component)) +
    geom_line(aes(linetype = scenario), size = 1) +
    geom_point(aes(shape = scenario),
        fill = 'white', size = 1.7) +
    facet_wrap(vars(component), nrow = 1) +
    scale_x_continuous(
        limits = c(2008, 2018),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        limits = c(0, 6000),
        labels = scales::comma) +
    scale_shape_manual(values = c(21, 16)) +
    guides(color = FALSE, shape = FALSE) +
    theme_minimal_grid() +
    panel_border() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey"))+
    labs(title = 'Comparison of cost per kW (China)',
         y = "Cost per kW (2018 $USD)",
         linetype = "Scenario:")

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_china.pdf'),
    bau_s1_china, height = 4, width = 10)

bau_s1_module <- cost_all %>%
    filter(component == "Module") %>%
    ggplot(aes(x = year, y = cost_per_kw, color = country)) +
    geom_line(aes(linetype = scenario), size = 1) +
    geom_point(aes(shape = scenario),
        fill = 'white', size = 1.7) +
    facet_wrap(vars(country), nrow = 1) +
    scale_x_continuous(
        limits = c(2008, 2018),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        limits = c(0, 4500),
        labels = scales::comma) +
    scale_shape_manual(values = c(21, 16)) +
    guides(color = FALSE, shape = FALSE) +
    theme_minimal_grid() +
    panel_border() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey"))+
    labs(
        title = 'Module Cost per kW between BAU and S1 Scenarios',
        y = "Cost per kW (2018 $USD)",
        linetype = "Scenario:")

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_module.pdf'),
    bau_s1_module, height = 4, width = 10)

# BAU, S1 and S2 costs ----

bau_s2_module <- cost_all_s2 %>%
    filter(component == "Module") %>%
    ggplot(aes(x = year, y = cost_per_kw, color = country)) +
    geom_line(aes(linetype = scenario), size = 1) +
    geom_point(aes(shape = scenario),
               fill = 'white', size = 1.7) +
    facet_wrap(vars(country), nrow = 1) +
    scale_x_continuous(
        limits = c(2008, 2018),
        breaks = seq(2008, 2018, 2)) +
    scale_y_continuous(
        limits = c(0, 4500),
        labels = scales::comma) +
    scale_shape_manual(values = c(21, 16, 16)) +
    guides(color = FALSE, shape = FALSE) +
    theme_minimal_grid() +
    panel_border() +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey"))+
    labs(
        title = 'Module Cost per kW between BAU and S1 Scenarios',
        y = "Cost per kW (2018 $USD)",
        linetype = "Scenario:")

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_s2_module.pdf'),
    bau_s2_module, height = 4, width = 10)


# BAU and S1 costs against historical costs ----

bau_s1_module_historical <- bau_s1_module +
    geom_line(
        data = data_historical %>%
            filter(component == "Module"),
        aes(x = year, y = costPerKw, col = country),
        alpha = 0.4, size = 1) +
    labs(caption = 'Shaded line = Historical')

ggsave(
    file.path(dir$figs, 'cost_scenarios',
              'bau_s1_module_historical.pdf'),
    bau_s1_module_historical, height = 4, width = 10)


# 2030 Projections -----


(bau_s1_us2030 <- cost_us2030 %>%
     ggplot(aes(x = year, y = cost_per_kw, color = component)) +
     geom_line(aes(linetype = scenario), size = 1) +
     geom_point(aes(shape = scenario),
                fill = 'white', size = 1.7) +
     facet_wrap(vars(component), nrow = 1) +
     scale_x_continuous(
         limits = c(2018, 2030),
         breaks = seq(2018, 2030, 2)) +
     # scale_y_continuous(
     #     limits = c(0, 5500),
     #     labels = scales::comma) +
     scale_shape_manual(values = c(21, 16)) +
     guides(color = FALSE, shape = FALSE) +
     theme_minimal_grid() +
     panel_border() +
     theme(
         legend.position = "bottom",
         strip.background = element_rect(fill = "grey"))+
     labs(title = 'Comparison of cost per kW (USA - Utility Only)',
          y = "Cost per kW (2018 $USD)",
          linetype = "Scenario:"))

ggsave(
    file.path(dir$figs, 'cost_scenarios', 'bau_s1_us2030.pdf'),
    bau_s1_us2030, height = 4, width = 10)

