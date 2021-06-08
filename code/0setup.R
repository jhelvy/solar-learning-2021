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
    data                 = here::here('data'),
    data_formatted       = here::here('data', 'formatted.Rds'),
    figs                 = here::here('figs'),
    output               = here::here('output'),
    lr_models            = here::here('output', 'lr_models.Rds'),
    historical_scenarios = here::here('output', 'historical_scenarios.Rds'),
    projection_scenarios = here::here('output', 'projection_scenarios.Rds')
)
