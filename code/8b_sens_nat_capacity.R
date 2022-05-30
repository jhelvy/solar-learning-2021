# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Two altnerative models:

# 1) Separate installed capacity between country i and all other countries j 
# 2) Separate installed capacity between country i and global capacity

run_model1 <- function(df) {
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            log_qi = log(q0 + cumsum(annCapKw_nation)),
            log_qj = log(q0 + cumsum(annCapKw_other)),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_qi + log_qj + log_s, data = temp))
}

run_model2 <- function(df) {
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            log_qi = log(q0 + cumsum(annCapKw_nation)),
            log_q = log(cumCapKw_world),
            log_c = log(costPerKw),
            log_s = log(price_si)
        )
    return(lm(formula = log_c ~ log_qi + log_q + log_s, data = temp))
}


model_us1 <- run_model1(data$hist_us)
model_china1 <- run_model1(data$hist_china)
model_germany1 <- run_model1(data$hist_germany)
model_us2 <- run_model2(data$hist_us)
model_china2 <- run_model2(data$hist_china)
model_germany2 <- run_model2(data$hist_germany)

summary(model_us1)
summary(model_china1)
summary(model_germany1)
summary(model_us2)
summary(model_china2)
summary(model_germany2)

# Compute learning rates
lr_us1 <- 1 - 2^coef(model_us1)["log_qj"]
lr_china1 <- 1 - 2^coef(model_china1)["log_qj"]
lr_germany1 <- 1 - 2^coef(model_germany1)["log_qj"]

lr_us2 <- 1 - 2^coef(model_us2)["log_q"]
lr_china2 <- 1 - 2^coef(model_china2)["log_q"]
lr_germany2 <- 1 - 2^coef(model_germany2)["log_q"]
