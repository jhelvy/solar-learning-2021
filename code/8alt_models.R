# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Model with production ----

# Create cumulative production capacity data for each nation

production <- data$pvProduction %>% 
    group_by(country) %>% 
    mutate(cum_prod_gw = cumsum(production_gw)) %>% 
    select(year, country, cum_prod_gw) %>% 
    filter(country %in% c("us", "china", "europe")) %>% 
    mutate(country = ifelse(country == "europe", "germany", country))

# Join on capacity and compute correlation

capacity_world <- data$hist_us %>% 
    filter(year >= 2010) %>% 
    mutate(cum_cap_gw = cumCapKw_world / 10^6) %>%
    select(year, cum_cap_gw)
df <- production %>% 
    left_join(capacity_world, by = c('year')) %>% 
    filter(year <= 2019) %>% 
    group_by(country) %>% 
    mutate(
        rho = round(cor(cum_cap_gw, cum_prod_gw), 3), 
        label = paste0("Corr: ", rho))

# Visualize correlation between cumulative production and installed capacity
corr_plot <- ggplot(df, aes(x = cum_cap_gw, y = cum_prod_gw)) + 
    geom_point() + 
    geom_text(
        data = df %>% filter(year == max(year)),
        aes(label = label, x = rep(100, 3), y = c(420, 18.5, 11)),
        size = 5
    ) +
    geom_smooth(se = FALSE, method = 'lm', color = 'blue') +
    facet_wrap(vars(country), scales = 'free', nrow = 1) + 
    theme_bw(base_size = 18) + 
    labs(
        x = "Cumulative global installed capacity (GW)",
        y = "Cumulative national\nproduction capacity (GW)"
    )

ggsave(file.path("corr_plot.pdf"), corr_plot, width = 15, height = 5, device = cairo_pdf)

# Estimate LR models for each country including cumulative production term

# Join on price_si and costPerKw variables

df <- df %>% 
    left_join(
    rbind(
        data$hist_us %>% 
            filter(year >= 2010) %>% 
            mutate(country = "us") %>% 
            select(year, country, price_si, costPerKw),
        data$hist_china %>% 
            filter(year >= 2010) %>% 
            mutate(country = "china") %>% 
            select(year, country, price_si, costPerKw),
        data$hist_germany %>% 
            filter(year >= 2010) %>% 
            mutate(country = "germany") %>% 
            select(year, country, price_si, costPerKw)
    ),
    by = c("year", "country")
) %>% 
    mutate(
        cum_prod_kw = cum_prod_gw*10^6, 
        cum_cap_kw = cum_cap_gw*10^6
    )

# Run models

run_model_prod <- function(df) {
    temp <- df %>%
        mutate(
            log_q = log(cum_cap_kw),
            log_p = log(cum_prod_kw),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_p + log_s, data = temp))
}

model_us <- run_model_prod(df %>% filter(country == "us"))
model_china <- run_model_prod(df %>% filter(country == "china"))
model_germany <- run_model_prod(df %>% filter(country == "germany"))

# View results
summary(model_us)
summary(model_china)
summary(model_germany)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_germany <- 1 - 2^coef(model_germany)["log_q"]

lr_us
lr_china
lr_germany


# Model with country capacity ----

run_model_qi <- function(df) {
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            log_qi = log(q0 + cumsum(annCapKw_nation)),
            log_q = log(cumCapKw_world),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_qi + log_s, data = temp))
}

model_us <- run_model_qi(data$hist_us)
model_china <- run_model_qi(data$hist_china)
model_germany <- run_model_qi(data$hist_germany)

# View results
summary(model_us)
summary(model_china)
summary(model_germany)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_germany <- 1 - 2^coef(model_germany)["log_q"]

lr_us
lr_china
lr_germany


# Combined model with country capacity ----

run_model_q <- function(df) {
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            log_qi = log(q0 + cumsum(annCapKw_nation)),
            log_q = log(cumCapKw_world),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + country*log_q + log_qi + log_s, data = temp))
}

data_combined <- rbind(
    data$hist_china %>% cbind(country='China'),
    data$hist_germany %>% cbind(country='Germany'),
    data$hist_us %>% cbind(country='US')
) %>% mutate(country=factor(country))

model_combined <- run_model_q(data_combined)

# View results
summary(model_combined)

# Compute learning rates
coefs <- coef(model_combined)
coef_china <- coefs["log_q"]
coef_us <- coef_china + coefs["log_q:countryUS"]
coef_germany <- coef_china + coefs["log_q:countryGermany"]
lr_china <- 1 - 2^coef_china
lr_us <- 1 - 2^coef_us
lr_germany <- 1 - 2^coef_germany

lr_us
lr_china
lr_germany


# Model with shipment capacity ----

run_model_ship <- function(df) {
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            log_ship = log(shipment_mw*10^3),
            log_q = log(cumCapKw_world),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_s + log_ship, data = temp))
}

df_us <- data$hist_us %>% 
    left_join(data$shipments %>% select(year, shipment_mw), by = "year") %>% 
    filter(!is.na(shipment_mw))
df_china <- data$hist_china %>% 
    left_join(data$shipments %>% select(year, shipment_mw), by = "year") %>% 
    filter(!is.na(shipment_mw))
df_germany <- data$hist_germany %>% 
    left_join(data$shipments %>% select(year, shipment_mw), by = "year") %>% 
    filter(!is.na(shipment_mw))

model_us <- run_model_ship(df_us)
model_china <- run_model_ship(df_china)
model_germany <- run_model_ship(df_germany)

# View results
summary(model_us)
summary(model_china)
summary(model_germany)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_germany <- 1 - 2^coef(model_germany)["log_q"]

100*round(lr_us, 3)
100*round(lr_china, 3)
100*round(lr_germany, 3)

# Model with plant size ----

run_model_plant <- function(df) {
    temp <- df %>%
        mutate(
            log_plantsize = log(weighted_average_gw*10^6),
            log_q = log(cumCapKw_world),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_s + log_plantsize, data = temp))
}

df_us <- data$hist_us %>% 
    left_join(data$plantsize, by = "year") %>% 
    filter(!is.na(average_gw))
df_china <- data$hist_china %>% 
    left_join(data$plantsize, by = "year") %>% 
    filter(!is.na(average_gw))
df_germany <- data$hist_germany %>% 
    left_join(data$plantsize, by = "year") %>% 
    filter(!is.na(average_gw))

model_us <- run_model_plant(df_us)
model_china <- run_model_plant(df_china)
model_germany <- run_model_plant(df_germany)

# View results
summary(model_us)
summary(model_china)
summary(model_germany)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_germany <- 1 - 2^coef(model_germany)["log_q"]

100*round(lr_us, 3)
100*round(lr_china, 3)
100*round(lr_germany, 3)
