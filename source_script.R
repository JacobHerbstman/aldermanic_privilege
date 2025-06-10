### this is a source script that should be run before any other scripts to set the root directory and load in packages

rm(list = ls())

root <- "/Users/jacobherbstman/Desktop/aldermanic_privilege/"

#remotes::install_github("ccao-data/ptaxsim")

library(ptaxsim)
library(ggplot2)
library(tidyverse)
library(DBI)
library(sf)
library(furrr)
library(nngeo)
library(fixest)
library(data.table)
library(tidycensus)
library(tigris)
library(zoo)
library(patchwork)
library(glue)
library(writexl)
library(arrow)
library(DBI)
library(duckdb)
## load census api key

##comment out since it is loaded permanently
#census_api_key("29d8cfa2c622f7074d57c85ac2a64eff9835820b", install = TRUE)
