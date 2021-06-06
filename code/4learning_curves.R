# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))

# Load formatted data
data <- readRDS(dir$data_formatted)

# Notes -----------------------------------------------------------------------

## Basic learning curve model: Y_x = A*x^b
# where
#   Y_x = the cost of unit x (dependent variable)
#   A   = the theoretical cost of unit 1 (a.k.a. T1)
#   x   = the unit number (independent variable)
#   b   = a constant representing the slope (slope = 2^b)


# Log transformation: ln(Y_x) = ln(A) + b*ln(x)
#           Re-write: Y'      = int    + b*x'

# To convert log-space estimated coefficients back to original model:
# A = exp(int)
# b = b
# Learning curve slope = 2^b
# Learning Rate        = 1 - slope

# Approximation of the cumulative total cost of producing N units:
# C = (A*N^(b + 1)) / (b + 1)

## Two factor learning curve model: Y(x,p) = A * x^b * p^c
# where
#   Y(x,p) = the cost of unit x at silicon price p (dependent variable)
#   A   = the theoretical cost of unit 1 (a.k.a. T1)
#   x   = the unit number (independent variable)
#   p   = silicon price
#   b   = lnCap_estimate = learning coefficient on capacity
#   c   = lnSi_estimate = coefficient on silicon price

# Use two-factor model on these costs only (otherwise, one-factor model):
CostsTwoFactor <- c('Module')

## Two-level models:
#   World capacity for hard costs
#   Local capacity for soft costs


# # USA (NREL) -------------------------------------------------------------------------
# ####
# ## NOT USING NREL DATA, SKIP TO LBNL SECTION
# ####
#
# # Run linear models by component (module, BOS-Inverter) and installType
# # (residential, commercial, utility)
# usLR <- data$usNrel %>%
#     select(year, installType, component, lnCost, lnCap) %>%
#     nest(data = c(year, lnCost, lnCap)) %>%
#     fit_lr_model()
#
# usLR
#
# # USA with two-level learning rates: ------------------------------------------
# # 1) World hardware and 2) local soft costs
#
# # Format the US and world data to be merged
# usAndWorldData <- data$world %>%
#     select(year, lnCap_world = lnCap, price_si) %>%
#     right_join(data$usNrel, by = 'year') %>%
#     rename(lnCap_us = lnCap) %>%
#     select(year, lnCost, lnCap_world, lnCap_us, everything())
#
# # Determine which costs to be local vs. global
# usCostsLocal <- c('BOS', 'Labor', 'Other')
# usCostsWorld <- c('Inverter', 'Module')
#
# # Run linear models
# usLR_local <- usAndWorldData %>%
#     select(year, installType, component, lnCost, lnCap = lnCap_us) %>%
#     filter(component %in% usCostsLocal) %>%
#     nest(data = c(year, lnCost, lnCap)) %>%
#     fit_lr_model()
#
# usLR_world <- usAndWorldData %>%
#     select(year, installType, component, lnCost, lnCap = lnCap_world) %>%
#     # Run linear model by component and installType
#     filter(component %in% usCostsWorld) %>%
#     nest(data = c(year, lnCost, lnCap)) %>%
#     fit_lr_model()
#
# # Merge learning rate results from local and global
# usLR_twoLevel <- rbind(
#     usLR_local %>%
#         mutate(capData = 'local'),
#     usLR_world %>%
#         mutate(capData = 'world')) %>%
#     arrange(installType, component)
#
# # Run two-factor learning model
# usLR2f_world <- usAndWorldData %>%
#     select(
#         year, installType, component, lnCost, lnCap = lnCap_world,
#         price_si) %>%
#     filter(component %in% usCostsWorld) %>%
#     nest(data = c(year, lnCost, lnCap, price_si)) %>%
#     fit_2flr_model()


# USA (LBNL) -------------------------------------------------------------------------

# Run linear models by component (module, BOS-Inverter) and installType
# (residential, commercial, utility)
usLR <- data$usSeiaLbnl %>%
    select(year, installType, component, lnCost, lnCap) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model()

# USA with two-level learning rates: ------------------------------------------
# 1) World hardware and 2) local soft costs

# Format the US and world data to be merged
usAndWorldData <- data$world %>%
    select(year, lnCap_world = lnCap, price_si) %>%
    right_join(data$usSeiaLbnl, by = 'year') %>%
    rename(lnCap_us = lnCap) %>%
    select(year, lnCost, lnCap_world, lnCap_us, everything())

# Determine which costs to be local vs. global
usCostsLocal <- c('BOS', 'Labor', 'Other')
usCostsWorld <- c('Inverter', 'Module')

# Run linear models
usLR_local <- usAndWorldData %>%
    select(year, installType, component, lnCost, lnCap = lnCap_us) %>%
    filter(component %in% usCostsLocal) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model()

usLR_world <- usAndWorldData %>%
    select(year, installType, component, lnCost, lnCap = lnCap_world) %>%
    # Run linear model by component and installType
    filter(component %in% usCostsWorld) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model()

# Merge learning rate results from local and global
usLR_twoLevel <- rbind(
    usLR_local %>%
        mutate(capData = 'local'),
    usLR_world %>%
        mutate(capData = 'world')) %>%
    arrange(installType, component)

# Run two-factor learning model
usLR2f_world <- usAndWorldData %>%
    select(
        year, installType, component, lnCost, lnCap = lnCap_world,
        price_si) %>%
    filter(component %in% usCostsWorld, component %in% CostsTwoFactor) %>%
    nest(data = c(year, lnCost, lnCap, price_si)) %>%
    fit_2flr_model()

# China -----------------------------------------------------------------------

chinaLR <- data$china %>%
    select(year, component, lnCost, lnCap) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model() %>%
    mutate(installType = 'All')

# China with two-level learning rates: ----------------------------------------
# 1) World hardware and 2) local soft costs

# Format the China and world data to be merged
chinaAndWorldData <- data$world %>%
    select(year, lnCap_world = lnCap, price_si) %>%
    right_join(data$china, by = 'year') %>%
    rename(lnCap_china = lnCap) %>%
    select(year, lnCost, lnCap_world, lnCap_china, everything())

# Determine which costs to be local vs. global
# NOTE: No local BOS cost data in file
chinaCostsLocal <- c('BOS')
chinaCostsWorld <- c('Inverter','Module','System')

# Run linear models
chinaLR_local <- chinaAndWorldData %>%
    select(year, installType, component, lnCost, lnCap = lnCap_china) %>%
    filter(component %in% chinaCostsLocal) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model()

chinaLR_world <- chinaAndWorldData %>%
    select(year, installType, component, lnCost, lnCap = lnCap_world) %>%
    filter(component %in% chinaCostsWorld) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model()

# Merge learning rate results from local and global
chinaLR_twoLevel <- rbind(
    chinaLR_local %>%
        mutate(capData = 'local'),
    chinaLR_world %>%
        mutate(capData = 'world')) %>%
    arrange(installType, component)

# Run two-factor learning model
chinaLR2f_world <- chinaAndWorldData %>%
    select(
        year, installType, component, lnCost, lnCap = lnCap_world,
        price_si) %>%
    filter(
        component %in% chinaCostsWorld,
        component %in% CostsTwoFactor) %>%
    nest(data = c(year, lnCost, lnCap, price_si)) %>%
    fit_2flr_model()

# Germany ---------------------------------------------------------------------

# Run linear models by component
germanyLR <- data$germany %>%
    select(year, component, lnCost, lnCap) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model() %>%
    mutate(installType = 'All')

# Germany with two-level learning rates: ----------------------------------------
# 1) World hardware and 2) local soft costs

# Format the Germany and world data to be merged
germanyAndWorldData <- data$world %>%
    select(year, lnCap_world = lnCap, price_si) %>%
    right_join(data$germany, by = 'year') %>%
    rename(lnCap_germany = lnCap) %>%
    select(year, lnCost, lnCap_world, lnCap_germany, everything())

# Determine which costs to be local vs. global
germanyCostsLocal <- c('BOS_Inverter')
germanyCostsWorld <- c('Module')

# Run linear models
germanyLR_local <- germanyAndWorldData %>%
    select(year, component, lnCost, lnCap = lnCap_germany) %>%
    filter(component %in% germanyCostsLocal) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model() %>%
    mutate(installType = 'All')

germanyLR_world <- germanyAndWorldData %>%
    select(year, component, lnCost, lnCap = lnCap_world) %>%
    filter(component %in% germanyCostsWorld) %>%
    nest(data = c(year, lnCost, lnCap)) %>%
    fit_lr_model() %>%
    mutate(installType = 'All')

# Merge learning rate results from local and global
germanyLR_twoLevel <- rbind(
    germanyLR_local %>%
        mutate(capData = 'local'),
    germanyLR_world %>%
        mutate(capData = 'world')) %>%
    arrange(component)

# Run two-factor learning model
germanyLR2f_world <- germanyAndWorldData %>%
    select(
        year, installType, component, lnCost, lnCap = lnCap_world,
        price_si) %>%
    filter(
        component %in% germanyCostsWorld,
        component %in% CostsTwoFactor) %>%
    nest(data = c(year, lnCost, lnCap, price_si)) %>%
    fit_2flr_model()

# Combine country LRs ---------------------------------------------

moduleLR_twoLevel <- usLR_twoLevel %>%
    filter(component == 'Module') %>%
    mutate(country = 'US') %>%
    select(country, everything()) %>%
    rbind(
        chinaLR_twoLevel %>%
            filter(component == 'Module') %>%
            mutate(country = 'China') %>%
            select(installType, country, everything())) %>%
    rbind(
        germanyLR_twoLevel %>%
            filter(component == 'Module') %>%
            mutate(country = 'Germany') %>%
            select(installType, country, everything()))

softLR_twoLevel <- usLR_twoLevel %>%
    filter(component %in% usCostsLocal) %>%
    mutate(country = 'US') %>%
    select(country, everything()) %>%
    # rbind(
    #     chinaLR_twoLevel %>%
    #         filter(component %in% chinaCostsLocal) %>%
    #         mutate(country = 'China') %>%
    #         select(installType, country, everything())) %>%
    rbind(
        germanyLR_twoLevel %>%
            filter(component %in% germanyCostsLocal) %>%
            mutate(country = 'Germany') %>%
            select(installType, country, everything()))

# Print all -------------------------------------------------------------------

usLR
usLR_twoLevel

chinaLR
chinaLR_twoLevel

germanyLR
germanyLR_twoLevel

moduleLR_twoLevel
softLR_twoLevel

# Output ----------------------------------------------------------------------

saveRDS(list(
    us                      = usLR,
    us_twoLevel             = usLR_twoLevel,
    us_twoFactor            = usLR2f_world,
    china                   = chinaLR,
    china_twoLevel          = chinaLR_twoLevel,
    china_twoFactor         = chinaLR2f_world,
    germany                 = germanyLR,
    germany_twoLevel        = germanyLR_twoLevel,
    germany_twoFactor       = germanyLR2f_world,
    module_twoLevel         = moduleLR_twoLevel,
    soft_twoLevel           = softLR_twoLevel),
    dir$lr_models
)
