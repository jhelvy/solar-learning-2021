# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Create cumulative production capacity data for each nation
production <- data$pvProduction %>% 
    group_by(year) %>% 
    mutate(global = sum(production_gw)) %>% 
    ungroup() %>% 
    mutate(
        production = production_gw * 10^3, 
        global = global * 10^3
    ) %>% 
    select(-production_gw)
p0 <- production %>% 
    filter(year == 2010, country == 'china') %>% 
    pull(global)
production <- production %>% 
    group_by(country) %>% 
    mutate(cum_production = p0 + cumsum(production)) %>% 
    select(year, country, cum_production)
q0 <- data$hist_us %>% 
    filter(year == 2010) %>% 
    pull(cumCapKw_world)
capacity <- rbind(
    data$hist_us %>% mutate(country = "us"),
    data$hist_china %>% mutate(country = "china"),
    data$hist_germany %>% mutate(country = "europe")) %>% 
    filter(year >= 2010) %>% 
    group_by(country) %>% 
    mutate(cum_capacity = q0 + cumsum(annCapKw_nation)) %>% 
    select(country, year, cum_capacity, price_si, costPerKw) %>% 
    ungroup() %>% 
    mutate(cum_capacity = cum_capacity / 10^3)
df <- capacity %>% 
    left_join(production, by = c('year', 'country')) %>% 
    filter(!is.na(cum_production)) %>% 
    group_by(country) %>% 
    mutate(
        rho = round(cor(cum_capacity, cum_production), 2), 
        label = paste0("Corr: ", rho))
    
# Visualize correlation between cumulative production and installed capacity
ggplot(df) + 
    geom_point(aes(x = cum_capacity, y = cum_production)) + 
    geom_text(
        data = df %>% filter(year == max(year)),
        aes(label = label,
            x = c(50000, 80000, 55000),
            y = cum_production)
    ) +
    geom_smooth(aes(x = cum_capacity, y = cum_production), 
                se = FALSE, method = 'lm', color = 'blue') + 
    facet_wrap(vars(country), scales = 'free') + 
    theme_bw() + 
    labs(
        x = "Cumulative installed capacity (MW)",
        y = "Cumulative production capacity (MW)"
    )

# Estimate LR models for each country including cumulative production term

run_model2 <- function(df) {
    temp <- df %>%
        mutate(
            log_q = log(cum_capacity),
            log_p = log(cum_production),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_p + log_s, data = temp))
}

# Run historical models
model_us <- run_model2(df %>% filter(country == "us"))
model_china <- run_model2(df %>% filter(country == "china"))
model_europe <- run_model2(df %>% filter(country == "europe"))

summary(model_us)
summary(model_china)
summary(model_europe)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_europe <- 1 - 2^coef(model_europe)["log_q"]

lr_us
lr_china
lr_europe
