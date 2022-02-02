# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))
library(rstan)
options(mc.cores = parallel::detectCores())

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load diagnostics functions
util <- new.env()
source(file.path('stan', 'stan_utility.R'), local = util)

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

# Setup data
# USA
df <- data$us
year_min <- year_model_us_min
year_max <- year_model_us_max

# # China
# df <- data$china %>% filter(component == "Module")
# year_min <- year_model_china_min
# year_max <- year_model_china_max

data_model <- formatCapData(
    data_nation = df,
    data_world  = data$world, 
    year_beg    = year_min, 
    year_max    = year_max
) %>% 
    # Add price data
    left_join(select(df, year, costPerKw), by = "year") %>% 
    left_join(select(data$world, year, price_si), by = "year")

# Setup data for stan
x1 <- data_model$cumCapKw_world
x2 <- data_model$cumCapKw_other
x3 <- log(data_model$price_si)
y  <- log(data_model$costPerKw)
data_list <- list(
	N  = length(x1),
	x1 = x1,
	x2 = x2,
	x3 = x3,
	y  = y
)

# Fit the data
fit <- stan(
    file = here::here('stan', 'model4.stan'), 
    data = data_list, 
    control = list(max_treedepth = 15, adapt_delta = 0.99))

# Check diagnostics one by one
util$check_n_eff(fit)
util$check_rhat(fit)
util$check_div(fit)
util$check_treedepth(fit)
util$check_energy(fit)

# Or all at once
util$check_all_diagnostics(fit)

# Extract the best fit parameters
params <- extract(fit)
quantile(params$alpha, c(0.05, 0.5, 0.95))
quantile(params$beta1, c(0.05, 0.5, 0.95))
quantile(params$beta2, c(0.05, 0.5, 0.95))
quantile(params$lambda, c(0.05, 0.5, 0.95))

# Visualize
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
lambda <- mean(params$lambda)
plot(log(x1), y)
y_fit <- alpha + beta1 * log(x1 - (lambda * x2)) + beta2 * x3
lines(log(x1), y_fit, col = "red")

# Learning rate
1 - (2^beta1)
lambda

# Add Posterior intervals
plot(log(x1), y)
params <- extract(fit)
y_fit <- alpha + beta1 * log(x1 - (lambda * x2)) + beta2 * x3
lines(log(x1), y_fit, col = "blue")
yCI <- matrix(0, ncol = 2, nrow = length(x1))
for (i in 1:length(x1)) {
   yCI[i,] <- quantile(
       params$alpha + params$beta1 * log(x1[i] - (params$lambda * x2[i])) + params$beta2 * x3[i],
       probs = c(0.05, 0.95)
    )
}
# 95% quantiles 
lines(log(x1), yCI[,1], col = 'red')
lines(log(x1), yCI[,2], col = 'red')

# ggplot visualization
y_sim <- matrix(0, ncol = 3, nrow = length(x1))
for (i in 1:length(x1)) {
    sim <- params$alpha + params$beta1 * log(x1[i] - (params$lambda * x2[i])) + params$beta2 * x3[i]
   y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
}
y_sim <- as.data.frame(exp(y_sim))
names(y_sim) <- c("mean", "lower", "upper")
y_sim <- cbind(y_sim, cumCapKw_world = data_model$cumCapKw_world)
data_model %>% 
    ggplot() + 
    geom_line(
        data = y_sim,
        mapping = aes(x = cumCapKw_world, y = mean), 
        color = "red"
    ) +
    geom_ribbon(
        data = y_sim,
        mapping = aes(x = cumCapKw_world, ymin = lower, ymax = upper), 
        fill = "red", 
        alpha = 0.2
    ) +
    geom_point(aes(x = cumCapKw_world, y = costPerKw)) + 
    scale_x_log10() + 
    scale_y_log10() + 
    theme_bw()

