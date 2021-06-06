# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

year_min <- 2008
year_max <- 2018

# Notes -----------------------------------------------------------------------

## Basic learning curve model: Y_x = A*x^b
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
usLrData <- data$usSeiaLbnl %>%
    filter(
        component == 'Module',
        year >= year_min, year <= year_max,
        installType == "Utility") %>%
    select(year, lnCost) %>%
    left_join(
        data$world %>%
            select(year, price_si, lnCap),
        by = "year")

# Run model
usLrModel <- lm(lnCost ~ lnCap + log(price_si), data = usLrData)
summary(usLrModel)

 # China -----------------------------------------------------------------------

# Prep data
chinaLrData <- data$china %>%
    filter(
        component == 'Module',
        year >= year_min, year <= year_max) %>%
    select(year, lnCost) %>%
    left_join(
        data$world %>%
            select(year, price_si, lnCap),
        by = "year")

# Run model
chinaLrModel <- lm(lnCost ~ lnCap + log(price_si), data = chinaLrData)
summary(chinaLrModel)

# Germany ---------------------------------------------------------------------

# Prep data
germanyLrData <- data$germany %>%
    filter(
        component == 'Module',
        year >= year_min, year <= year_max) %>%
    select(year, lnCost) %>%
    left_join(
        data$world %>%
            select(year, price_si, lnCap),
        by = "year")

# Run model
germanyLrModel <- lm(lnCost ~ lnCap + log(price_si), data = germanyLrData)
summary(germanyLrModel)

# Output ----------------------------------------------------------------------

saveRDS(list(
    us      = usLrModel,
    china   = chinaLrModel,
    germany = germanyLrModel),
    dir$lr_models
)
