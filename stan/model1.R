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
x <- log(data_us$cumCapacityKw)
y <- log(data_us$costPerKw)
data <- list(
	N = length(x),
	x = x,
	y = y
)

# Fit the data
fit <- stan(file = here::here('stan', 'model1.stan'), data = data)
print(fit)

# Extract the best fit parameters and visualize on the data
plot(x, y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta <- mean(params$beta)
abline(a = alpha, b = beta)

# Posterior intervals
plot(x, y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta <- mean(params$beta)
abline(a = alpha, b = beta)
xr <- seq(15, 21, 0.1)
yCI <- sapply(
    xr, 
    function(x) quantile(
        params$beta*x + params$alpha, 
        probs = c(0.05, 0.95))
) # 95% quantiles 
lines(xr, yCI[1,], col='red')
lines(xr, yCI[2,], col='red')

# Simulated data
# Simulated data from the posterior can be used as a sanity check
plot(density(y), xlim = c(4, 11), ylim = c(0, 0.8))
for(i in 1:10) {
    lines(density(params$y_sim[i,]), col = 'red')
}

# Using the simulated data, we should recover similar parameters to the real data.
# To check this we can re-run the model with a simulated dataset.
y_new <- params$y_sim[20, ] # 20th simulated dataset
data_new <- list(
	N = length(x),
	x = x,
	y = y_new
)
fit_new <- stan(file = here::here('stan', 'model1.stan'), data = data_new)

# Plot the posteriors on the parameters of the simulated and real data
par(mfrow = c(1, 2))
params_new <- extract(fit_new)
plot(density(params$alpha))
lines(density(params_new$alpha), col = 'red')
plot(density(params$beta))
lines(density(params_new$beta), col = 'red')

