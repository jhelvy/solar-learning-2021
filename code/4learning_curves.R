# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

year_max <- 2018

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

# Prep data
data_us <- data$usSeiaLbnl %>%
    filter(
        year <= year_max,
        component == 'Module',
        installType == "Utility") %>%
    select(year, costPerKw) %>%
    left_join(
        data$world %>%
            select(year, price_si, cumCapacityKw),
        by = "year") %>% 
    filter(!is.na(price_si), !is.na(cumCapacityKw))

# Run model
model_us <- run_model(data_us)
summary(model_us)

 # China ----------------------------------------------------------------------

# Prep data
data_china <- data$china %>%
    filter(
        year <= year_max,
        component == 'Module') %>%
    select(year, costPerKw) %>%
    left_join(
        data$world %>%
            select(year, price_si, cumCapacityKw),
        by = "year")

# Run model
model_china <- run_model(data_china)
summary(model_china)

# Germany ---------------------------------------------------------------------

# Prep data
data_germany <- data$germany %>%
    filter(
        year <= year_max,
        component == 'Module') %>%
    select(year, costPerKw) %>%
    left_join(
        data$world %>%
            select(year, price_si, cumCapacityKw),
        by = "year")

# Run model
model_germany <- run_model(data_germany)
summary(model_germany)

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
