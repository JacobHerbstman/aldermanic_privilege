# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)

queries <- read_csv("../input/preferred_address_geocode_requests.csv",
  col_types = cols(component_pin = col_character(), .default = col_skip())) |>
  distinct(pin = component_pin) |>
  arrange(pin)
stopifnot(!anyNA(queries), all(grepl("^[0-9]{14}$", queries$pin)))
write_csv(queries, "../output/geocoding_history_queries_download.csv")
