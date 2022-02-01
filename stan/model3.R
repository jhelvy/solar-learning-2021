# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))
library(rstan)
options(mc.cores = parallel::detectCores())

# Load formatted data
data <- readRDS(dir$data_formatted)

# Setup US data
data_us <- data$us %>%
    filter(
        year >= year_model_us_min,
        year <= year_model_us_max
    ) %>%
    select(year, costPerKw) %>%
    left_join(
        data$world %>%
            select(year, price_si, cumCapacityKw),
        by = "year") %>% 
    filter(!is.na(price_si), !is.na(cumCapacityKw))

# Setup data for stan
x1 <- data_us$cumCapacityKw
x2 <- log(data_us$price_si)
y <- log(data_us$costPerKw)
data <- list(
	N = length(x),
	x1 = x1,
	x2 = x2,
	y = y
)

# Fit the data
fit <- stan(file = here::here('stan', 'model3.stan'), data = data)
print(fit)

# Extract the best fit parameters and visualize on the data
plot(log(x), y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * log(x1) + beta2 * x2
lines(log(x), y_fit, col = "red")

# Posterior intervals
plot(log(x), y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * log(x1) + beta2 * x2
lines(log(x), y_fit, col = "blue")
yCI <- matrix(0, ncol = 2, nrow = length(x))
for (i in 1:length(x1)) {
   yCI[i,] <- quantile(
       params$alpha + params$beta1*log(x1[i]) + params$beta2*x2[i], 
       probs = c(0.05, 0.95)
    )
}
# 95% quantiles 
lines(log(x1), yCI[,1], col = 'red')
lines(log(x1), yCI[,2], col = 'red')
