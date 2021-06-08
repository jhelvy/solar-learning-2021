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
options(dplyr.width = Inf)

# Load custom functions
source(here::here('code', '0functions.R'))

# Setup directories
dir <- list(
    data           = here::here('data'),
    data_formatted = here::here('data', 'formatted.Rds'),
    figs           = here::here('figs'),
    output         = here::here('output'),
    lr_models      = here::here('output', 'lr_models.Rds'),
    cost_scenarios = here::here('output', 'cost_scenarios.Rds')
)

# Set period dates

# Historical range
year_min <- 2008 
year_max <- 2018

# Projection range
year_min_proj <- 2018
year_max_proj <- 2030
num_years_proj <- year_max_proj - year_min_proj
