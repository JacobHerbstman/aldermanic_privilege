# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

original <- read_csv("../input/preferred_historical_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
initial <- read_csv("../output/historical_project_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
additional <- read_csv("../input/preferred_parcel_supplement_queries_2026-09-07.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyDuplicated(original), !anyDuplicated(initial), !anyDuplicated(additional))
queries <- bind_rows(original, anti_join(initial, original, by = c("target_year", "pin10")), additional) |>
  arrange(target_year, pin10)
stopifnot(!anyNA(queries), !anyDuplicated(queries))
write_csv(queries, "../output/preferred_historical_parcel_source_queries.csv")
