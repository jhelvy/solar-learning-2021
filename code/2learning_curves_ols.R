# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

run_model <- function(df) {
    # For every value of lambda, run the linear model
    lambdas <- seq(0, 1, by = 0.005)
    models <- list()
    errs <- list()
    for (i in 1:length(lambdas)) {
        temp <- df %>% 
            mutate(
                log_q = log(cumCapKw_world - (lambdas[i]*cumCapKw_other)),
                log_c = log(costPerKw),
                log_p = log(price_si)
            )
        model <- lm(formula = log_c ~ log_q + log_p, data = temp)
        models[[i]] <- model
        errs[[i]] <- sum(model$residuals^2)
    }
    # Get the best model based on the lowest error
    err <- unlist(errs)
    # plot(lambdas, err)
    index_best <- which(err == min(err))
    model <- models[[index_best]]

    # Best lambda
    lambda <- lambdas[index_best]
    return(list(model = model, lambda = lambda))
}

predict_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>% 
        mutate(
            log_q = log(cumCapKw_world - (lambda*cumCapKw_other)),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(
        y_sim, 
        cumCapKw_world = df$cumCapKw_world, 
        costPerKw = df$costPerKw)
    return(y_sim)
}

project_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>% 
        mutate(
            log_q = log(cumCapKw_world - (lambda*cumCapKw_other)),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(y_sim, cumCapKw_world = df$cumCapKw_world)
    return(y_sim)
}

# Load formatted data
data <- readRDS(dir$data_formatted)

# Select data
df <- data$hist_us
df <- data$hist_china
df <- data$hist_germany

df_proj <- data$proj_nat_trends_us
# df_sus_dev <- data$proj_sus_dev_us
df_proj <- data$proj_nat_trends_china
df_proj <- data$proj_nat_trends_germany

# Set parameters
delay_hist <- 6
delay_proj <- 10
lambda_end_hist <- 0.9
lambda_end_proj <- 0.9

# Fit model
results <- run_model(df)
model <- results$model
lambda <- results$lambda
# lambda <- 0.8
1 - 2^coef(model)["log_q"] # Learning rate

# Predict cost - historical, global
cost_global <- predict_cost(model, df, lambda)

# Predict cost - historical, national
lambda_nat <- seq(lambda, lambda_end_hist, length.out = delay_hist + 1)
lambda_nat <- c(lambda_nat, rep(lambda_end_hist, nrow(df) - length(lambda_nat)))
cost_national <- predict_cost(model, df, lambda_nat)    

# Visualize
rbind(
    cost_global %>% mutate(scenario = "global"),
    cost_national %>% mutate(scenario = "national")) %>% 
    ggplot() + 
    geom_line(
        mapping = aes(x = cumCapKw_world, y = mean, color = scenario), 
    ) +
    geom_ribbon(
        mapping = aes(
            x = cumCapKw_world, ymin = lower, ymax = upper, fill = scenario),
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

# Projections 

# Project cost - global
proj_global <- project_cost(model, df_proj, lambda)

# Project cost - national
lambda_nat <- seq(lambda, lambda_end_proj, length.out = delay_proj + 1)
lambda_nat <- c(lambda_nat, rep(lambda_end_proj, nrow(df_proj) - length(lambda_nat)))
proj_national <- project_cost(model, df_proj, lambda_nat)

# Visualize
rbind(
    proj_global %>% mutate(scenario = "global"),
    proj_national %>% mutate(scenario = "national")) %>% 
    ggplot() + 
    geom_line(
        mapping = aes(x = cumCapKw_world, y = mean, color = scenario), 
    ) +
    geom_ribbon(
        mapping = aes(
            x = cumCapKw_world, ymin = lower, ymax = upper, fill = scenario),
        alpha = 0.2
    ) +
    # scale_x_log10() + 
    # scale_y_log10() + 
    theme_bw() + 
    labs(
        x = "log(Cumulative Global Installed Capacity, kW)",
        y = "log(Cost per kW, $USD)"
    )
