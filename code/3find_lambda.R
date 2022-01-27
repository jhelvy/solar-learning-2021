# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Set beginning values
us_beg <- lr$data_us %>%
    filter(year == year_model_us_min)
china_beg <- lr$data_china %>%
    filter(year == year_model_china_min)
germany_beg <- lr$data_germany %>%
    filter(year == year_model_germany_min)
world_beg <- data$world %>%
    filter(year == year_model_world_min)

# Merge together true historical cost per kW
cost_historical_true <- rbind(
    lr$data_us %>% mutate(country = "U.S."),
    lr$data_china %>% mutate(country = "China"),
    lr$data_germany %>% mutate(country = "Germany"))

# Learning rates based on local cumulative capacity and local installed costs
# Note: Since world data does not break down installation type
#       (Commercial, Residential, Utility),
#       we replicate capacities across all types
#       (assuming in effect that learning is shared across installation type)

# Define country capacity data
data_us <- data$us %>%
    filter(year >= year_model_us_min, year <= year_model_us_max)
data_china <- data$china %>%
    filter(component == "Module") %>%
    filter(year <= year_model_china_max) %>%
    select(year, cumCapacityKw) %>% 
    left_join(
        lr$data_china %>% 
            select(year, costPerKw), by = "year")
data_germany <- data$germany %>%
    select(year, cumCapacityKw) %>%
    filter(year <= year_model_germany_max) %>% 
    left_join(
        lr$data_germany %>% 
            select(year, costPerKw), by = "year")

# For each country, compute the cost curve using different values of lambda

lambda_us <- find_lambda(
    data_nation = data_us, 
    data_world = data$world, 
    year_beg = year_model_us_min, 
    lr_model = lr$model_us, 
    beg = us_beg
)

lambda_china <- find_lambda(
    data_nation = data_china, 
    data_world = data$world, 
    year_beg = year_model_china_min, 
    lr_model = lr$model_china, 
    beg = china_beg
)

lambda_germany <- find_lambda(
    data_nation = data_germany, 
    data_world = data$world, 
    year_beg = year_model_germany_min, 
    lr_model = lr$model_germany, 
    beg = germany_beg
)

# Visualize how sse changes with lambda for each country
df <- rbind(
    mutate(lambda_us, country = "U.S."), 
    mutate(lambda_china, country = "China"),
    mutate(lambda_germany, country = "Germany"))

ggplot(df) + 
    geom_point(
        aes(
            x = lambda, 
            y = sse
        )
    ) + 
    geom_vline(
        data = df %>% 
            group_by(country) %>% 
            arrange(sse) %>% 
            slice(1), 
        aes(xintercept = lambda), 
        color = "red"
    ) +
    facet_wrap(vars(country)) + 
    theme_bw()

# ggsave("lambda.png", width = 15, height = 5)

# Optimal lambda values: 
lambda_us <- lambda_us %>% 
    slice(1) %>% 
    pull(lambda)
lambda_china <- lambda_china %>% 
    slice(1) %>% 
    pull(lambda)
lambda_germany <- lambda_germany %>% 
    slice(1) %>% 
    pull(lambda)

lambda_us
lambda_china
lambda_germany
