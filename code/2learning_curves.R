# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Run historical models
model_us <- run_model(data$hist_us, lambda = 0)
model_china <- run_model(data$hist_china, lambda = 0)
model_germany <- run_model(data$hist_germany, lambda = 0)

# Compute learning rates
lr_us <- 1 - 2^coef(model_us)["log_q"]
lr_china <- 1 - 2^coef(model_china)["log_q"]
lr_germany <- 1 - 2^coef(model_germany)["log_q"]

# Get draws of the parameters for each model
params_us <- as.data.frame(
    MASS::mvrnorm(10^4, coef(model_us), vcov(model_us))
)
params_china <- as.data.frame(
    MASS::mvrnorm(10^4, coef(model_china), vcov(model_china))
)
params_germany <- as.data.frame(
    MASS::mvrnorm(10^4, coef(model_germany), vcov(model_germany))
)
names(params_us) <- c("alpha", "beta", "gamma")
names(params_china) <- c("alpha", "beta", "gamma")
names(params_us) <- c("alpha", "beta", "gamma")

# Save output 
saveRDS(list(
    model_us       = model_us,
    model_china    = model_china,
    model_germany  = model_germany,
    lr_us          = lr_us,
    lr_china       = lr_china,
    lr_germany     = lr_germany,
    params_us      = params_us,
    params_china   = params_china,
    params_germany = params_germany),
    dir$lr_models
)
