
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![DOI](https://zenodo.org/badge/373634772.svg)](https://zenodo.org/badge/latestdoi/373634772)

This repository contains the data and code to reproduce results from our
study titled **“Quantifying the cost savings of global solar
photovoltaic supply chains”**. All code is written using the [R
programming language](https://www.r-project.org/).

**Authors**: John Paul Helveston, Gang He, & Michael R. Davidson.

**Abstract**: Achieving carbon neutrality requires deploying renewable
energy at unprecedented speed and scale, yet countries sometimes
implement policies that increase costs by restricting the free flow of
capital, talent, and innovation in favor of localizing benefits such as
economic growth, employment, and trade surpluses. Here we assess the
cost savings from a globalized solar photovoltaic (PV) module supply
chain. We develop a two-factor learning model using historical capacity,
component, and input material price data of solar PV deployment in the
U.S., Germany, and China. We estimate that the globalized PV module
market has saved PV installers in the U.S. $24 ($19 - $31) billion,
Germany $7 ($5 - $9) billion, and China $36 ($26 - $45) billion from
2008 to 2020 compared to a counterfactual scenario where domestic
manufacturers supply an increasing proportion of installed capacities
over a 10-year period. Projecting the same scenario forward from 2020
results in estimated solar module prices that are approximately 20% -
25% higher in 2030 compared to a future with globalized supply chains.
International climate policy benefits from a globalized low-carbon value
chain, and these results point to the need for complementary policies to
mitigate welfare distribution effects and potential impacts on
technological crowding-out.

# File organization

## data

Contains all of the “raw” data used in our analyses as well as a single
`formatted.Rds` file that when loaded into R is a list of formatted data
frames, which is generated by running the script at
`code/2format_data.R`.

## code

| file or folder       | description                                                                                                                                                                                      |
|----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `0a_run.R`           | A single file to reproduce all analyses.                                                                                                                                                         |
| `0functions.R`       | Custom functions used in our analyses.                                                                                                                                                           |
| `0setup.R`           | Loads libraries, creates `dir` object (a list of paths to folders and objects), and loads `0functions.R`.                                                                                        |
| `1format_data.R`     | Formats and harmonizes all raw data and saves the result as the list object at `data/formatted.Rds`.                                                                                             |
| `2learning_curves.R` | Estimates learning curve models and saves all results as the list object at `output/lr_models.Rds`.                                                                                              |
| `3scenarios_hist.R`  | Uses the estimates learning curves models to compute differences in historical costs under different scenarios. Results are saved as the list object at `output/historical_scenarios.Rds`.       |
| `4scenarios_proj.R`  | Uses the estimates learning curves models to compute differences in projected future costs under different scenarios. Results are saved as the list object at `output/projection_scenarios.Rds`. |
| `5charts.R`          | Code to reproduce all charts used in our analyses.                                                                                                                                               |
| `6summary.R`         | Code to print out a summary of all main results.                                                                                                                                                 |
| `7tables.Rmd`        | Generates all tables in the `output/tables.docx` file.                                                                                                                                           |
| `8alt_models.R`      | Code to reproduce several alternative models that we considered as sensitivity checks. These results are presented in Extended Data Tables 2 - 4.                                                |

## figs

All charts created in the `code/5charts.R` file are saved here in 3
formats: eps, pdf, and png.

## output

All model / scenario analyses outputs are saved here.

# Reproducing the analyses

## Installation setup

Reproducing the analyses requires the follow setup steps:

1.  Install [R](https://www.r-project.org/).
2.  Install
    [RStudio](https://www.rstudio.com/products/rstudio/download/)
    (optional, though recommended).
3.  If on a Mac, install [XQuartz](https://www.xquartz.org/) to enable
    Cairo graphics (for reproducing figures).
4.  Download the files in this repository.
5.  Open the `solar-learning-2021.Rproj` file, which sets the working
    directory to the root of the files in this repository. The
    repository root must be set as the working directory otherwise the
    code will error.
6.  Install additional required R packages by running the code in the
    `/code/0install.R` file (you’ll only need to install these packages
    once).

## Full reproduction

The `code/0a_run.R` file contains scripts to fully reproduce the entire
set of analyses in sequential order. This is purely for convenience to
quickly reproduce everything. Each file can be separately run if
desired.

## Setup

Most R files in the `/code` folder start with the following line to
execute the `/code/0setup.R` file, which loads all required libraries
and sets several global variables such as starting and stopping years to
bound the analyses:

``` r
source(here::here('code', '0setup.R'))
```

## Formatting the data

All of the raw data are stored in the `/data` folder. The script at
`/code/1format_data.R` formats all of this data and saves it as a list
stored at `/data/formatted.Rds`.

To load this list of formatted data, run this line after sourcing the
`/code/0setup.R` file:

``` r
data <- readRDS(dir$data_formatted)

# Names of the formatted data frames:
names(data)
```

    ##  [1] "pvProduction"            "silicon"                
    ##  [3] "shipments"               "plantsize"              
    ##  [5] "irenaCumCapacityMw"      "nrelCapacity"           
    ##  [7] "nrelCost"                "seiaCapacity"           
    ##  [9] "lbnlCost"                "usNrel"                 
    ## [11] "us"                      "china"                  
    ## [13] "germany"                 "world"                  
    ## [15] "rates"                   "hist_us"                
    ## [17] "hist_china"              "hist_germany"           
    ## [19] "proj_nat_trends_us"      "proj_sus_dev_us"        
    ## [21] "proj_nat_trends_china"   "proj_sus_dev_china"     
    ## [23] "proj_nat_trends_germany" "proj_sus_dev_germany"   
    ## [25] "exchangeRatesRMB"        "exchangeRatesEUR"

## Learning curve models

All of the learning curve models are estimated by running the
`/code/2learning_curves.R` file. Results are saved in a list of data
frames containing formatted model results stored in
`/output/lr_models.Rds`.

To load formatted results, run this line:

``` r
lr <- readRDS(dir$lr_models)

# Names of the LR models:
names(lr)
```

## Historical scenarios

All of the historical (2008 - 2018) cost scenario calculations are
computed by running the `/code/3historical_scenarios.R` file. Results
are saved in a list of data frame containing formatted results stored in
`/output/historical_scenarios.Rds`.

To load the results of the historical scenarios, run this line:

``` r
cost <- readRDS(dir$scenarios_hist)

# Names of the scenarios:
names(cost)
```

    ## [1] "cost"    "savings"

## Projected scenarios

All of the future projected (2018 - 2030) cost scenario calculations are
computed by running the `/code/4projection_scenarios.R` file. Results
are saved in a list of data frame containing formatted results stored in
`/output/projection_scenarios.Rds`.

To load the results of the projected scenarios, run this line:

``` r
proj <- readRDS(dir$scenarios_proj)

# Names of the scenarios:
names(proj)
```

    ## [1] "nat_trends"         "sus_dev"            "savings_nat_trends"
    ## [4] "savings_sus_dev"

## Charts

All of the charts are generated by running the `/code/5charts.R` file.
Results are saved as pdf, eps, and png files in the `/figs` folder.

## Summary

A full summary of all results can be seen by running the
`/code/6summary.R` file.

## Other

The `/code/7tables.Rmd` file is a simple template that we used to
generate a word-formatted summary table of the regression results.

The `8alt_models.R` file contains code to reproduce several alternative
models that we considered as sensitivity checks. These results are
presented in Extended Data Tables 2 - 4.
