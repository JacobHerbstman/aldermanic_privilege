# setwd("tasks/download_construction_address_geocodes/code")

library(dplyr)
library(readr)

queries <- read_csv("../input/preferred_address_geocode_requests.csv",
  col_types = cols(selected_address = col_character(), address_query = col_character(), .default = col_skip())) |>
  filter(!is.na(selected_address), !is.na(address_query)) |>
  distinct() |> arrange(selected_address)
stopifnot(!anyDuplicated(queries$selected_address))
write_csv(queries, "../output/address_geocode_queries.csv")
