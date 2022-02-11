# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Select data
df <- data$hist_us
df <- data$hist_china
df <- data$hist_germany

df_proj <- data$proj_nat_trends_us
# df_sus_dev <- data$proj_sus_dev_us
# df_proj <- data$proj_nat_trends_china
# df_proj <- data$proj_sus_dev_china
# df_proj <- data$proj_nat_trends_germany
# df_proj <- data$proj_sus_dev_germany

df <- df %>% 
    left_join(
        filter(data$pvProduction, country == "china"),
        by = "year"
    ) %>% 
    filter(!is.na(production_gw)) %>% 
    mutate(cumProdKw = cumsum(production_gw*10^6))

lambda <- 0
q0 <- df$cumCapKw_world[1]
temp <- df %>%
    mutate(
        q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
        log_q = log(q),
        log_p = log(cumProdKw),
        log_c = log(costPerKw),
        log_s = log(price_si)
    )
model <- lm(formula = log_c ~ log_q + log_p + log_s, data = temp)
    
summary(model)

1 - 2^coef(model)["log_q"] # Learning rate
