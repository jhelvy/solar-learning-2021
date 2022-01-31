# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Define range of lambda values 
lambda <- seq(0, 1, 0.01)

# Define country capacity data

cap_data_china <- data$china %>%
    filter(component == "Module") %>%
    filter(year <= year_model_china_max) %>%
    select(year, cumCapacityKw)
cap_data_germany <- data$germany %>%
    select(year, cumCapacityKw) %>%
    filter(year <= year_model_germany_max)

# Notes -----------------------------------------------------------------------

# Basic learning curve model: Y_x = A*x^b
# where
#   Y = the cost of unit x (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   b = a constant representing the slope (slope = 2^b)

# Log transformation: ln(Y) = ln(A) + b*ln(x)
#           Re-write: Y'    = int   + b*x'

# To convert log-space estimated coefficients back to original model:
# A = exp(int)
# b = b
# Learning curve slope = 2^b
# Learning Rate        = 1 - slope

# Approximation of the cumulative total cost of producing N units:
# C = (A*N^(b + 1)) / (b + 1)

# Two factor learning curve model: Y = A * x^b * p^c
# where
#   Y = the cost of unit x at silicon price p (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   p = silicon price
#   b = lnCap_estimate = learning coefficient on capacity
#   c = lnSi_estimate = coefficient on silicon price

# U.S. ----------------------------------------------------------------------
# World capacity data: IRENA
# US cost data: LBNL

models_us <- list()
for (i in 1:length(lambda)) {
    models_us[[i]] <- 
        # Prep data
        makeGlobalCapData(
        data_nation = data$us,
        data_world  = data$world,
        year_beg    = year_model_us_min,
        lambda      = lambda[i]) %>% 
        select(year, cumCapacityKw, price_si) %>% 
        left_join(select(data$us, year, costPerKw), by = "year") %>% 
        filter(year <= year_model_us_max) %>% 
        run_model() # Run model
}

fit_vals <- unlist(lapply(models_us, function(x) summary(x)$r.squared))
fit_vals <- unlist(lapply(models_us, function(x) summary(x)$adj.r.squared))
index_best <- which(fit_vals == max(fit_vals))

# Best model: 
model_us <- models_us[[index_best]]
summary(model_us)

# Learning rate
1 - 2^coef(model_us)["log(cumCapacityKw)"]

# Lambda:
lambda[index_best]

# China ----------------------------------------------------------------------

china_data <- filter(data$china, component == 'Module')
models_china <- list()
for (i in 1:length(lambda)) {
    models_china[[i]] <- 
        # Prep data
        makeGlobalCapData(
        data_nation = china_data,
        data_world  = data$world,
        year_beg    = year_model_china_min,
        lambda      = lambda[i]) %>% 
        select(year, cumCapacityKw, price_si) %>% 
        left_join(select(china_data, year, costPerKw), by = "year") %>% 
        filter(year <= year_model_china_max) %>% 
        run_model() # Run model
}

fit_vals <- unlist(lapply(models_china, function(x) summary(x)$r.squared))
fit_vals <- unlist(lapply(models_china, function(x) summary(x)$adj.r.squared))
index_best <- which(fit_vals == max(fit_vals))

# Best model: 
model_china <- models_china[[index_best]]
summary(model_china)

# Learning rate
1 - 2^coef(model_china)["log(cumCapacityKw)"]

# Lambda:
lambda[index_best]


# Germany ---------------------------------------------------------------------

models_germany <- list()
for (i in 1:length(lambda)) {
    models_germany[[i]] <- 
        # Prep data
        makeGlobalCapData(
        data_nation = data$germany,
        data_world  = data$world,
        year_beg    = year_model_germany_min,
        lambda      = lambda[i]) %>% 
        select(year, cumCapacityKw, price_si) %>% 
        left_join(select(data$germany, year, costPerKw), by = "year") %>% 
        filter(year <= year_model_germany_max) %>% 
        run_model() # Run model
}

fit_vals <- unlist(lapply(models_germany, function(x) summary(x)$r.squared))
fit_vals <- unlist(lapply(models_germany, function(x) summary(x)$adj.r.squared))
index_best <- which(fit_vals == max(fit_vals))

# Best model: 
model_germany <- models_germany[[index_best]]
summary(model_germany)

# Learning rate
1 - 2^coef(model_germany)["log(cumCapacityKw)"]

# Lambda:
lambda[index_best]

# Output ----------------------------------------------------------------------

saveRDS(list(
    model_us      = model_us,
    data_us       = data_us,
    model_china   = model_china,
    data_china    = data_china,
    model_germany = model_germany,
    data_germany  = data_germany),
    dir$lr_models
)
