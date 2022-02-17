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

# Save output 

saveRDS(list(
    model_us      = model_us,
    lr_us         = lr_us,
    model_china   = model_china,
    lr_china      = lr_china,
    model_germany = model_germany,
    lr_germany    = lr_germany),
    dir$lr_models
)
