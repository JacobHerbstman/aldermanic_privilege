source("../../shared/code/save_data.R")
# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)
library(sf)

requests <- read_csv("../input/preferred_address_geocode_requests.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  distinct(component_pin, target_year)
history <- read_csv("../output/geocoding_parcel_history.csv",
  col_types = cols(pin = col_character(), .default = col_guess())) |>
  filter(is.finite(lon), is.finite(lat))
stopifnot(!anyDuplicated(history[c("pin", "year")]))
points <- st_as_sf(history, coords = c("lon", "lat"), crs = 4326) |> st_transform(3435)
history <- history |> mutate(reference_x_3435 = st_coordinates(points)[, 1],
  reference_y_3435 = st_coordinates(points)[, 2])

# Query every recorded exact-PIN location, without adopting a coordinate-year rule.
queries <- list()
for (year in sort(unique(requests$target_year))) {
  queries[[as.character(year)]] <- requests |> filter(target_year == year) |>
    inner_join(history, by = c("component_pin" = "pin"), relationship = "one-to-many") |>
    select(target_year, reference_x_3435, reference_y_3435) |> distinct()
}
queries <- bind_rows(queries) |> distinct() |>
  arrange(target_year, reference_x_3435, reference_y_3435)

# The earlier history extract also covers unresolved requests that never needed an address query.
earlier <- read_csv("../output/predecessor_parcel_history.csv",
  col_types = cols(pin = col_character(), .default = col_guess())) |>
  filter(is.finite(lon), is.finite(lat))
references <- read_csv("../input/preferred_predecessor_reference_points.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess()))
parcels <- st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE)
references$first_polygon_count <- 0L
for (year in sort(unique(references$target_year))) {
  rows <- which(references$target_year == year & is.finite(references$reference_x_3435) &
    is.finite(references$reference_y_3435))
  points <- st_as_sf(references[rows, ], coords = c("reference_x_3435", "reference_y_3435"), crs = 3435)
  references$first_polygon_count[rows] <- lengths(st_within(points, parcels[parcels$target_year == year, ]))
}
stopifnot(!anyDuplicated(earlier[c("pin", "year")]))
additional <- references |> filter(first_polygon_count == 0L) |>
  select(component_pin, target_year) |> distinct() |>
  inner_join(earlier |> mutate(target_year = year - 1L),
    by = c("component_pin" = "pin", "target_year"), relationship = "one-to-one")
points <- st_as_sf(additional, coords = c("lon", "lat"), crs = 4326) |> st_transform(3435)
queries <- bind_rows(queries, additional |> transmute(target_year,
  reference_x_3435 = st_coordinates(points)[, 1], reference_y_3435 = st_coordinates(points)[, 2])) |>
  distinct() |> arrange(target_year, reference_x_3435, reference_y_3435)
stopifnot(!anyNA(queries), !anyDuplicated(queries))
SaveData(queries, c("target_year", "reference_x_3435", "reference_y_3435"), "../output/history_reference_spatial_queries_download.csv")
