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

# Set exchange rates
er_us <- 1
er_china <- data$exchangeRatesRMB
er_germany <- data$exchangeRatesEUR

# Compute GLOBAL cost scenarios by country
cost_global_us <- predict_cost(
    params = params_us,
    df     = df_us,
    lambda = 0,
    exchange_rate = er_us)

cost_global_china <- predict_cost(
    params = params_china,
    df     = df_china,
    lambda = 0,
    exchange_rate = er_china)

cost_global_germany <- predict_cost(
    params = params_germany,
    df     = df_germany,
    lambda = 0,
    exchange_rate = er_germany)

# Compute NATIONAL cost scenarios by country
cost_national_us <- predict_cost(
    params = params_us,
    df     = df_us,
    lambda = lambda_nat_us,
    exchange_rate = er_us)

cost_national_china <- predict_cost(
    params = params_china,
    df     = df_china,
    lambda = lambda_nat_china,
    exchange_rate = er_china)

cost_national_germany <- predict_cost(
    params = params_germany,
    df     = df_germany,
    lambda = lambda_nat_germany,
    exchange_rate = er_germany)

# Preview results

cost <- combine(
    global_us        = cost_global_us,
    national_us      = cost_national_us,
    global_china     = cost_global_china,
    national_china   = cost_national_china,
    global_germany   = cost_global_germany,
    national_germany = cost_national_germany
)

make_historical_plot(cost)
make_historical_plot(cost, log_scale = TRUE)

# ggsave("cost_historical.png", width = 15, height = 5)

# Calculate savings between national and global learning scenarios

# First, compute the cost difference CIs for each country
cost_diff_us <- compute_cost_diff(
    params     = params_us,
    df         = df_us,
    lambda_nat = lambda_nat_us,
    exchange_rate = er_us) %>%
    mutate(country = "U.S.")

cost_diff_china <- compute_cost_diff(
    params     = params_china,
    df         = df_china,
    lambda_nat = lambda_nat_china,
    exchange_rate = er_china) %>%
    mutate(country = "China") 

cost_diff_germany <- compute_cost_diff(
    params     = params_germany,
    df         = df_germany,
    lambda_nat = lambda_nat_germany,
    exchange_rate = er_germany) %>% 
    mutate(country = "Germany")

cost_diffs <- rbind(cost_diff_us, cost_diff_china, cost_diff_germany) %>% 
    filter(year >= year_savings_min, year <= year_savings_max)

# Compute savings

savings <- compute_savings(cost_diffs, cost_historical_true)

# Save outputs ----

saveRDS(list(
    cost    = cost,
    savings = savings),
    dir$scenarios_hist
)
