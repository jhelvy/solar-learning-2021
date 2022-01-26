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

find_lambda <- function(
    data_nation, data_world, year_beg, lr_model, beg
) {
    lambda <- seq(0, 1, 0.01)
    error_us <- list()
    for (i in 1:length(lambda)) {
        data_global <- makeGlobalCapData(
            data_nation = data_nation,
            data_world  = data_world,
            year_beg    = year_beg,
            lambda      = lambda[i])
        cost_global <- predict_cost(
            model    = lr_model,
            data     = data_global,
            cost_beg = beg$costPerKw,
            cap_beg  = beg$cumCapacityKw,
            si_beg   = beg$price_si,
            year_beg = year_beg,
            ci       = 0.95)
        error_us[[i]] <- cost_global %>% 
            select(year, cost_per_kw) %>% 
            left_join(
                data_nation %>% 
                    select(year, cost_per_kw_true = costPerKw), 
                by = "year"
            ) %>% 
            mutate(err_sq = log(abs(cost_per_kw - cost_per_kw_true))^2) %>% 
            filter(err_sq != Inf)
    }
    result <- data.frame(
        sse = unlist(lapply(error_us, function(x) sum(x$err_sq))), 
        lambda = lambda) %>% 
        arrange(sse)
    return(result)
}


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

ggsave("lambda.png", width = 15, height = 5)

    