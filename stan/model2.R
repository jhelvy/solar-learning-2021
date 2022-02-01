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
x1 <- log(data_us$cumCapacityKw)
x2 <- log(data_us$price_si)
y <- log(data_us$costPerKw)
data <- list(
	N = length(x),
	x1 = x1,
	x2 = x2,
	y = y
)

# Fit the data
fit <- stan(file = here::here('stan', 'model2.stan'), data = data)
print(fit)

# Extract the best fit parameters and visualize on the data
plot(x, y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * x1 + beta2 * x2
lines(x, y_fit, col = "red")

# Posterior intervals
plot(x, y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta1 <- mean(params$beta1)
beta2 <- mean(params$beta2)
y_fit <- alpha + beta1 * x1 + beta2 * x2
lines(x, y_fit, col = "blue")
yCI <- matrix(0, ncol = 2, nrow = length(x))
for (i in 1:length(x1)) {
   yCI[i,] <- quantile(
       params$alpha + params$beta1*x1[i] + params$beta2*x2[i], 
       probs = c(0.05, 0.95)
    )
}
# 95% quantiles 
lines(x1, yCI[,1], col = 'red')
lines(x1, yCI[,2], col = 'red')

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
	N = length(x1),
	x1 = x1,
	x2 = x2,
	y = y_new
)
fit_new <- stan(file = here::here('stan', 'model2.stan'), data = data_new)

# Plot the posteriors on the parameters of the simulated and real data
par(mfrow = c(1, 3))
params_new <- extract(fit_new)
plot(density(params$alpha))
lines(density(params_new$alpha), col = 'red')
plot(density(params$beta1))
lines(density(params_new$beta1), col = 'red')
plot(density(params$beta2))
lines(density(params_new$beta2), col = 'red')
