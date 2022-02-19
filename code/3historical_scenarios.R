# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Merge together true historical cost per kW
cost_historical_true <- rbind(
    data$hist_us %>% mutate(country = "U.S."),
    data$hist_china %>% mutate(country = "China"),
    data$hist_germany %>% mutate(country = "Germany"))

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Set global baseline "delay" variable
# Controls how many years until 100% of national capacity is 
# domestically-supplied
delay <- 10

# Set starting and ending lambda values for national market scenarios
lambda_start <- 0
lambda_end <- 1

# Set common data frames for data and params
df_us <- data$hist_us
df_china <- data$hist_china
df_germany <- data$hist_germany
params_us <- lr$params_us
params_china <- lr$params_china
params_germany <- lr$params_germany

# Set lambda values for national markets scenario
lambda_nat_us <- make_lambda_national(lambda_start, lambda_end, df_us)
lambda_nat_china <- make_lambda_national(lambda_start, lambda_end, df_china)
lambda_nat_germany <- make_lambda_national(lambda_start, lambda_end, df_germany)

# Compute GLOBAL cost scenarios by country
cost_global_us <- predict_cost(
    params = params_us,
    df     = df_us,
    lambda = 0)

cost_global_china <- predict_cost(
    params = params_china,
    df     = df_china,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

cost_global_germany <- predict_cost(
    params = params_germany,
    df     = df_germany,
    lambda = 0) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

# Compute NATIONAL cost scenarios by country
cost_national_us <- predict_cost(
    params = params_us,
    df     = df_us,
    lambda = lambda_nat_us)

cost_national_china <- predict_cost(
    params = params_china,
    df     = df_china,
    lambda = lambda_nat_china) %>% 
    convertToUsd(data$exchangeRatesRMB) # Currency conversion

cost_national_germany <- predict_cost(
    params = params_germany,
    df     = df_germany,
    lambda = lambda_nat_germany) %>% 
    convertToUsd(data$exchangeRatesEUR) # Currency conversion

# Combine Cost Scenarios ----
cost <- rbind(
    mutate(cost_global_us, learning = "global", country = "U.S."),
    mutate(cost_national_us, learning = "national", country = "U.S."),
    mutate(cost_global_china, learning = "global", country = "China"),
    mutate(cost_national_china, learning = "national", country = "China"),
    mutate(cost_global_germany, learning = "global", country = "Germany"),
    mutate(cost_national_germany, learning = "national", country = "Germany")
)

# Preview results
cost %>%
    ggplot() +
    facet_wrap(vars(country)) +
    geom_line(
        aes(
            x = year, 
            y = cost_per_kw, 
            color = learning
        )
    ) +
    geom_ribbon(
        aes(
            x = year, 
            ymin = cost_per_kw_lb, 
            ymax = cost_per_kw_ub,
            fill = learning
        ), 
        alpha = 0.22
    ) +
    geom_point(aes(x = year, y = cost_per_kw_hist)) +
    theme_bw() +
    scale_y_log10()

# ggsave("cost_historical.png", width = 15, height = 5)

# Calculate savings between national and global learning scenarios

# First, compute the cost difference CIs for each country
cost_diff_us <- compute_cost_diff(
    params     = params_us,
    df         = df_us,
    lambda_nat = lambda_nat_us,
    ci         = 0.95) %>%
    mutate(country = "U.S.")

cost_diff_china <- compute_cost_diff(
    params     = params_china,
    df         = df_china,
    lambda_nat = lambda_nat_china,
    ci         = 0.95) %>%
    mutate(country = "China")

cost_diff_germany <- compute_cost_diff(
    params     = params_germany,
    df         = df_germany,
    lambda_nat = lambda_nat_germany,
    ci         = 0.95) %>%
    mutate(country = "Germany")

cost_diffs <- rbind(cost_diff_us, cost_diff_china, cost_diff_germany) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

# Compute the additional capacity in each country in each year
cap_additions <- cost_historical_true %>% 
    select(year, country, annCapKw_nation) %>%
    filter(year >= year_savings_min, year <= year_savings_max)

# Compute savings
savings <- cost_diffs %>%
    left_join(cap_additions, by = c("year", "country")) %>%
    mutate(
        ann_savings_bil = (annCapKw_nation * cost_per_kw) / 10^9,
        ann_savings_bil_lb = (annCapKw_nation * cost_per_kw_lb) / 10^9,
        ann_savings_bil_ub = (annCapKw_nation * cost_per_kw_ub) / 10^9
    ) %>%
    select(
        year, country, ann_savings_bil, ann_savings_bil_lb,
        ann_savings_bil_ub) %>%
    group_by(country) %>%
    mutate(
        cum_savings_bil = cumsum(ann_savings_bil),
        cum_savings_bil_lb = cumsum(ann_savings_bil_lb),
        cum_savings_bil_ub = cumsum(ann_savings_bil_ub)) %>%
    ungroup()

# Save outputs ----

saveRDS(list(
    cost_historical_true = cost_historical_true,
    cost = cost,
    savings = savings),
    dir$scenarios_hist
)
