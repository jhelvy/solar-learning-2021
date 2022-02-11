# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

df_list <- list(
    "U.S." = data$hist_us,
    "China" = data$hist_china,
    "Germany" = data$hist_germany
)
df_proj_nt_list <- list(
    "U.S." = data$proj_nat_trends_us,
    "China" = data$proj_nat_trends_china,
    "Germany" = data$proj_nat_trends_germany
)
df_proj_sd_list <- list(
    "U.S." = data$proj_sus_dev_us,
    "China" = data$proj_sus_dev_china,
    "Germany" = data$proj_sus_dev_germany
)

# Run historical models
model_us <- run_model(df_list[["U.S."]], lambda = 0)
model_china <- run_model(df_list[["China"]], lambda = 0)
model_germany <- run_model(df_list[["Germany"]], lambda = 0)

model_list <- list(
    "U.S." = model_us,
    "China" = model_china,
    "Germany" = model_germany
)
lr_list <- list(
    "U.S." = percent(1 - 2^coef(model_us)["log_q"]),
    "China" = percent(1 - 2^coef(model_china)["log_q"]),
    "Germany" = percent(1 - 2^coef(model_germany)["log_q"])
)
