# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

requests <- read_csv("../input/preferred_project_geography_requests.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character(), .default = col_skip())) |> distinct()
preferred <- read_csv("../input/preferred_historical_parcel_source_queries.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
initial <- bind_rows(
  read_csv("../input/historical_project_parcel_queries_2026-07-27.csv", col_types = cols(target_year = col_integer(), pin10 = col_character())),
  read_csv("../input/historical_project_parcel_queries_2026-09-07.csv", col_types = cols(target_year = col_integer(), pin10 = col_character())))
stopifnot(!anyNA(requests), !anyDuplicated(preferred), !anyDuplicated(initial))
additional <- requests |> anti_join(preferred, by = c("target_year", "pin10")) |>
  anti_join(initial, by = c("target_year", "pin10")) |> arrange(target_year, pin10)
write_csv(additional, "../output/preferred_parcel_queries_additional.csv")
