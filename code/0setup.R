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

# Model range
year_min_model <- 2007
year_max_model <- 2019

# Historical range
year_min <- 2008
year_max <- 2018

# Projection range
year_min_proj <- year_max
year_max_proj <- 2030
