# setwd("tasks/new_construction_cleaning/code")
# maximum_distance_ft <- 1500

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(maximum_distance_ft)
if (length(args) != 1L) stop("Expected the maximum boundary distance in feet.")
maximum_distance_ft <- as.numeric(args[1])
if (!is.finite(maximum_distance_ft) || maximum_distance_ft <= 0) {
  stop("The maximum boundary distance must be positive.")
}

buildings <- readr::read_csv(
  "../output/density_historical_building_universe.csv",
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
historical <- readr::read_csv(
  "../input/density_historical_parcel_records.csv",
  col_types = readr::cols(pin = readr::col_character(), parcel_class = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
address_matches <- readr::read_csv(
  "../adjudication/historical_address_matches.csv",
  col_types = readr::cols(pin = readr::col_character(), matched_pin = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
current <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  col_types = readr::cols(pin = readr::col_character(), longitude = readr::col_double(), latitude = readr::col_double(), .default = readr::col_skip()),
  show_col_types = FALSE
)
if (anyDuplicated(buildings$pin) || anyDuplicated(historical[c("pin", "year")]) ||
    anyDuplicated(address_matches$pin) || anyDuplicated(current$pin)) {
  stop("Historical coordinate inputs must have unique building, PIN-year, and address-decision keys.")
}

parcel_coordinates <- buildings |>
  dplyr::anti_join(
    current |>
      dplyr::filter(is.finite(longitude), is.finite(latitude)),
    by = "pin"
  ) |>
  dplyr::inner_join(historical, by = c("pin", "construction_year" = "year"), relationship = "one-to-one") |>
  dplyr::filter(is.finite(longitude), is.finite(latitude)) |>
  dplyr::transmute(pin, construction_year, longitude, latitude, coordinate_source = "historical_parcel")

if (nrow(dplyr::anti_join(address_matches, buildings, by = c("pin", "construction_year"))) > 0L ||
    any(address_matches$pin %in% parcel_coordinates$pin)) {
  stop("An address decision is outside the building universe or overlaps an exact historical coordinate.")
}
address_coordinates <- address_matches |>
  dplyr::left_join(current, by = c("matched_pin" = "pin"), relationship = "many-to-one") |>
  dplyr::transmute(pin, construction_year, longitude, latitude, coordinate_source = "historical_address")
if (any(!is.finite(address_coordinates$longitude) | !is.finite(address_coordinates$latitude))) {
  stop("A reviewed address match lacks coordinates for its source PIN.")
}

coordinates <- dplyr::bind_rows(parcel_coordinates, address_coordinates) |>
  dplyr::mutate(
    construction_date = as.Date(sprintf("%d-06-15", construction_year)),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )
points <- sf::st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(
  sf::st_read("../input/ward_panel.gpkg", quiet = TRUE),
  eras = sort(unique(coordinates$era))
)
boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = sort(unique(coordinates$era)))
assignments <- assign_points_to_boundaries(points, coordinates$era, ward_maps, boundaries, chunk_n = 2000L)

coordinates <- coordinates |>
  dplyr::mutate(distance_to_boundary_ft = assignments$dist_ft) |>
  dplyr::filter(is.finite(distance_to_boundary_ft), distance_to_boundary_ft <= maximum_distance_ft) |>
  dplyr::select(pin, construction_year, longitude, latitude, coordinate_source) |>
  dplyr::arrange(pin)
if (anyNA(coordinates$pin) || anyDuplicated(coordinates$pin)) {
  stop("Recovered coordinates must be unique by original building PIN.")
}
readr::write_csv(coordinates, "../output/density_historical_coordinate_candidates.csv")
