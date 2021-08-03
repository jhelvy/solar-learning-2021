# Load libraries
library(tidyverse)
library(scales)
library(ggrepel)
library(broom)
library(readxl)
library(janitor)
library(rjson)
library(here)
library(cowplot)
library(priceR)
library(ggtext)
options(dplyr.width = Inf)

# Load custom functions
source(here::here('code', '0functions.R'))

# Setup directories
dir <- list(
    data                 = here::here('data'),
    data_formatted       = here::here('data', 'formatted.Rds'),
    figs                 = here::here('figs'),
    output               = here::here('output'),
    lr_models            = here::here('output', 'lr_models.Rds'),
    historical_scenarios = here::here('output', 'historical_scenarios.Rds'),
    projection_scenarios = here::here('output', 'projection_scenarios.Rds')
)

# Set global "year" variables

# Set year for aligning all inflation adjustment 
year_inflation <- 2020

# Historical range for model estimation (limited by data)
year_model_china_min   <- 2007
year_model_china_max   <- 2020
year_model_us_min      <- 2006
year_model_us_max      <- 2020
year_model_germany_min <- 2006
year_model_germany_max <- 2020
year_model_world_min   <- 2006
year_model_world_max   <- 2020

# Historical savings - needs to be same for all countries, so using:
#   max of the country min values +1 for the year_savings_min
#   min of the country max values for the year_savings_max
year_savings_min <- 2008
year_savings_max <- 2020

# Projection range - same for all countries
year_proj_min <- 2020
year_proj_max <- 2030

# Projections Targets:
#
# Project out to 2030, based on achieving fixed capacity target
#
# National trends scenario is based on capacity at end of 2020 plus
# continuation of recent trends. Note that this lines up well with Germany's
# stated national target of 100GW, and is equivalent to China solar share in
# solar+wind reaching ~ 63% of the 1200GW target (currently at 47%). There are
# analyses that indicate China will need to exceed 1200 to reach its other
# targets, and I expect solar growth to increase relative to wind for the next decade. The U.S. scenario is also in line with other scenarios (e.g. EIA low-RE
# cost, NAM decarbonization study). World capacity is computed by taking the
# shares of the three countries in current world capacity (2019 from IRENA),
# which comes out to 54%, then scaling up capacity linearly to 2030.
#
# Sustainable development scenario is from IEA WEO 2020 (the net zero study
# does not provide a country breakdown), then splitting up EU into Germany via
# the 2019 shares of capacity (IRENA).
#
# Targets:
target_nat_trends_us      <- 295*1e6
target_nat_trends_china   <- 750*1e6
target_nat_trends_germany <- 103*1e6
target_nat_trends_world   <- 2115*1e6
target_sus_dev_us         <- 628*1e6
target_sus_dev_china      <- 1106*1e6
target_sus_dev_germany    <- 147*1e6
target_sus_dev_world      <- 3125*1e6
