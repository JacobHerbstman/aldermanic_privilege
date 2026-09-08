# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

original <- read_csv("../input/historical_project_parcel_queries_2026-07-27.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
additional <- read_csv("../input/historical_project_parcel_queries_2026-09-07.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
queries <- bind_rows(original, additional) |> arrange(target_year, pin10)
stopifnot(!anyNA(queries), !anyDuplicated(queries), all(grepl("^[0-9]{10}$", queries$pin10)))
write_csv(queries, "../output/historical_project_parcel_source_queries.csv")
