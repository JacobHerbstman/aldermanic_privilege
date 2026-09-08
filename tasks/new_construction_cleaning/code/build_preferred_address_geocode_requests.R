# setwd("tasks/new_construction_cleaning/code")

library(dplyr)
library(readr)
library(stringr)

coverage <- read_csv("../output/preferred_historical_parcel_coverage.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(), .default = col_guess())) |>
  filter(coverage_status %in% c("missing_pin10", "ambiguous_pin10")) |>
  mutate(request_id = paste(project_id, component_pin, target_year, sep = "|"))
historical <- read_csv("../output/density_historical_coordinates.csv",
  col_types = cols(pin = col_character(), construction_year = col_double(),
    longitude = col_double(), latitude = col_double(), .default = col_skip())) |>
  filter(is.finite(longitude), is.finite(latitude)) |>
  select(component_pin = pin, target_year = construction_year)
current <- data.table::fread("../input/parcel_universe_2025_city.csv",
  select = c("pin", "centroid_x_crs_3435", "centroid_y_crs_3435"), colClasses = c(pin = "character")) |>
  as_tibble() |>
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) |>
  select(component_pin = pin)
addresses <- read_csv("../output/density_parcel_address_selected_history.csv",
  col_types = cols(pin = col_character(), .default = col_guess())) |>
  filter(address_selection_status == "selected_nearest_year_address") |>
  select(component_pin = pin, target_year = construction_year, selected_address,
    selected_address_normalized, selected_address_year, selected_address_year_gap,
    nearest_address_count, address_selection_status)
stopifnot(!anyDuplicated(coverage$request_id), !anyDuplicated(historical),
          !anyDuplicated(current), !anyDuplicated(addresses[c("component_pin", "target_year")]))

# Address geocoding is needed only when both exact-parcel coordinate sources fail.
requests <- coverage |>
  anti_join(historical, by = c("component_pin", "target_year")) |>
  anti_join(current, by = "component_pin") |>
  left_join(addresses, by = c("component_pin", "target_year"), relationship = "many-to-one") |>
  mutate(address_query = if_else(!is.na(selected_address), paste(selected_address, "Chicago, IL", sep = ", "), NA_character_),
    query_house_number = str_extract(selected_address, "^[0-9]+")) |>
  arrange(target_year, source_family, project_id, component_pin)
stopifnot(!anyDuplicated(requests$request_id))
write_csv(requests, "../output/preferred_address_geocode_requests.csv")
