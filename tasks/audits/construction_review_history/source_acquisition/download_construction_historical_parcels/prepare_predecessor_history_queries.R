source("../../shared/code/save_data.R")
# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

queries <- read_csv("../input/historical_project_parcel_coverage.csv",
  col_types = cols(component_pin = col_character(), coverage_status = col_character(), .default = col_skip())) |>
  filter(coverage_status == "missing") |>
  distinct(pin = component_pin) |>
  arrange(pin)
stopifnot(!anyNA(queries), all(grepl("^[0-9]{14}$", queries$pin)))
SaveData(queries, c("pin"), "../output/predecessor_history_queries_download.csv")
