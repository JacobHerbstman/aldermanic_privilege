# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

requests <- read_csv("../input/preferred_project_geography_requests.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character(), .default = col_skip())) |> distinct()
requests <- bind_rows(requests, read_csv("parcel_evidence_requests.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character(), .default = col_skip()))) |> distinct()
queried <- read_csv("../output/preferred_historical_parcel_source_queries.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyNA(requests), !anyDuplicated(queried))
additional <- requests |> anti_join(queried, by = c("target_year", "pin10")) |>
  arrange(target_year, pin10)
write_csv(additional, "../output/preferred_parcel_queries_additional.csv")
