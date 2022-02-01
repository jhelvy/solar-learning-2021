# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))
library(rstan)
options(mc.cores = parallel::detectCores())

# Load formatted data
data <- readRDS(dir$data_formatted)

# Functions for formatting data
formatCapData <- function(data_nation, data_world, year_beg, year_max) {
    cap_data_nation <- getAnnCapData(data_nation, year_beg)
    cap_data_world <- getAnnCapData(data_world, year_beg)
    result <- cap_data_nation %>% 
        select(year, annCapKw_nation = annCapKw) %>% 
        left_join(
            cap_data_world %>% 
                select(
                    year, cumCapKw_world = cumCapKw, annCapKw_world = annCapKw
                ), 
            by = "year"
        ) %>% 
        mutate(
            annCapKw_other = annCapKw_world - annCapKw_nation,
            cumCapKw_other = cumsum(annCapKw_other)
        ) %>% 
        filter(year <= year_max) %>% 
        select(year, cumCapKw_world, cumCapKw_other)    
    return(result)
}

getAnnCapData <- function(df, year_beg) {
    result <- df %>%
        filter(year >= year_beg) %>%
        select(year, cumCapKw = cumCapacityKw) %>%
        mutate(
            annCapKw = cumCapKw - lag(cumCapKw, 1),
            annCapKw = ifelse(is.na(annCapKw), 0, annCapKw))
    return(result)
}

# Setup US data
data_us <- formatCapData(
    data_nation = data$us, 
    data_world = data$world, 
    year_beg = year_model_us_min, 
    year_max = year_model_us_max
) %>% 
    # Add price data
    left_join(select(data$us, year, costPerKw), by = "year") %>% 
    left_join(select(data$world, year, price_si), by = "year")

# Setup data for stan
x1 <- data_us$cumCapKw_world
x2 <- log(data_us$price_si)
y <- log(data_us$costPerKw)
data <- list(
	N = length(x1),
	x1 = x1,
	x2 = x2,
	y = y
)

# Fit the data
fit <- stan(file = here::here('stan', 'model3.stan'), data = data)
print(fit)

# Extract the best fit parameters and visualize on the data
plot(log(x1), y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * log(x1) + beta2 * x2
lines(log(x1), y_fit, col = "red")

# Posterior intervals
plot(log(x1), y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * log(x1) + beta2 * x2
lines(log(x1), y_fit, col = "blue")
yCI <- matrix(0, ncol = 2, nrow = length(x1))
for (i in 1:length(x1)) {
   yCI[i,] <- quantile(
       params$alpha + params$beta1*log(x1[i]) + params$beta2*x2[i], 
       probs = c(0.05, 0.95)
    )
}
# 95% quantiles 
lines(log(x1), yCI[,1], col = 'red')
lines(log(x1), yCI[,2], col = 'red')
