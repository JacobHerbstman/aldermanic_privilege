source("../../shared/code/save_data.R")
# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

requests <- read_csv("../input/historical_project_parcel_requests.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character(), .default = col_skip())) |>
  distinct(target_year, pin10)
queried <- read_csv("../input/historical_project_parcel_queries_2026-07-27.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyNA(requests), !anyDuplicated(queried))
additional <- anti_join(requests, queried, by = c("target_year", "pin10")) |>
  arrange(target_year, pin10)
SaveData(additional, c("target_year", "pin10"), "../output/historical_parcel_queries_additional.csv")
