# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Setup data
df_us <- formatCapData(
    data_nation = data$us,
    data_world  = data$world, 
    year_beg    = year_model_us_min, 
    year_max    = year_model_us_max
) 

df_china <- formatCapData(
    data_nation = data$china %>% filter(component == "Module"),
    data_world  = data$world, 
    year_beg    = year_model_china_min, 
    year_max    = year_model_china_max
) 

df_germany <- formatCapData(
    data_nation = data$germany,
    data_world  = data$world, 
    year_beg    = year_model_germany_min, 
    year_max    = year_model_germany_max
) 

# Setup data for stan
data_us <- make_stan_data(df_us)
data_china <- make_stan_data(df_china)
data_germany <- make_stan_data(df_germany)

# Fit the data for each country
fit_us <- stan(
    file = here::here('code', 'lrmodel_lambda.stan'),
    data = data_us,
    iter = 4000,
    control = list(max_treedepth = 10, adapt_delta = 0.9)
)

fit_china <- stan(
    file = here::here('code', 'lrmodel_lambda.stan'),
    data = data_china,
    iter = 4000,
    control = list(max_treedepth = 10, adapt_delta = 0.9)
)

fit_germany <- stan(
    file = here::here('code', 'lrmodel_lambda.stan'),
    data = data_germany,
    iter = 4000,
    control = list(max_treedepth = 11, adapt_delta = 0.90)
)

# Set country to view results
fit <- fit_us
data <- data_us

fit <- fit_china
data <- data_china

fit <- fit_germany
data <- data_germany

# Diagnostics
check_all_diagnostics(fit)

# Extract the best fit parameters
params <- extract(fit)
alpha <- mean(params$alpha)
beta <- mean(params$beta)
gamma <- mean(params$gamma)
lambda <- mean(params$lambda)

# Learning rate & lambda
round(1 - (2^ci(params$beta, alpha = 0.05)), 2)
round(ci(params$lambda, alpha = 0.05), 2)

# Visualize
nobs <- data$N
y_sim <- matrix(0, ncol = 3, nrow = nobs)
for (i in 1:nobs) {
    sim <- params$alpha + params$beta * log(data$qw[i] - (params$lambda * data$qj[i])) + params$gamma * log(data$p[i])
   y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
}
y_sim <- as.data.frame(exp(y_sim))
names(y_sim) <- c("mean", "lower", "upper")
y_sim <- cbind(y_sim, cumCapKw_world = data$qw, costPerKw = exp(data$logc))
y_sim %>% 
    ggplot() + 
    geom_line(
        mapping = aes(x = cumCapKw_world, y = mean), 
        color = "red"
    ) +
    geom_ribbon(
        mapping = aes(x = cumCapKw_world, ymin = lower, ymax = upper), 
        fill = "red", 
        alpha = 0.2
    ) +
    geom_point(aes(x = cumCapKw_world, y = costPerKw)) + 
    scale_x_log10() + 
    scale_y_log10() + 
    theme_bw() + 
    labs(
        x = "log(Cumulative Global Installed Capacity, kW)",
        y = "log(Cost per kW, $USD)"
    )

ggsave("fit_germany.png", width = 6, height = 4)

# Save fits 
saveRDS(list(
    fit_us = fit_us,
    data_us = data_us,
    fit_china = fit_china,
    data_china = data_china,
    fit_germany = fit_germany,
    data_germany = data_germany),
    dir$lr_models_stan
)
