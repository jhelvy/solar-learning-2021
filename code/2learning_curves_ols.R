# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Select data
df <- data$hist_us
# df <- data$hist_china
# df <- data$hist_germany

df_proj <- data$proj_nat_trends_us
# df_sus_dev <- data$proj_sus_dev_us
# df_proj <- data$proj_nat_trends_china
# df_proj <- data$proj_sus_dev_china
# df_proj <- data$proj_nat_trends_germany
# df_proj <- data$proj_sus_dev_germany

# Set parameters
delay_hist <- 10
delay_proj <- 10
lambda_end_hist <- 0.9
lambda_end_proj <- 1

# Fit historical model
results <- run_model(df)
model <- results$model
lambda <- results$lambda
lambda <- 0.1
1 - 2^coef(model)["log_q"] # Learning rate

# Predict cost - historical, global
cost_global <- predict_cost(model, df, lambda)

# Predict cost - historical, national
lambda_nat <- seq(lambda, lambda_end_hist, length.out = delay_hist + 1)
lambda_nat <- c(lambda_nat, rep(lambda_end_hist, nrow(df) - length(lambda_nat)))
cost_national <- predict_cost(model, df, lambda_nat)    

# Project cost - global
proj_global <- project_cost(model, df_proj, lambda)

# Project cost - national
lambda_nat <- seq(lambda, lambda_end_proj, length.out = delay_proj + 1)
lambda_nat <- c(lambda_nat, rep(lambda_end_proj, nrow(df_proj) - length(lambda_nat)))
proj_national <- project_cost(model, df_proj, lambda_nat)

# Visualize
make_historical_plot(cost_national, cost_global)
make_projection_plot(proj_national, proj_global)
