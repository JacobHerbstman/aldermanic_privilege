# setwd("tasks/download_construction_historical_parcels/code")
library(dplyr)
library(readr)
history <- bind_rows(
  read_csv("../input/geocoding_parcel_history_2026-09-07.csv", col_types = cols(.default = col_character())),
  read_csv("../output/reviewed_parcel_history.csv", col_types = cols(.default = col_character()))) |>
  arrange(pin, year, row_id)
stopifnot(!anyNA(history$row_id), !anyDuplicated(history$row_id), !anyDuplicated(history[c("pin", "year")]))
write_csv(history, "../output/geocoding_parcel_history.csv")
