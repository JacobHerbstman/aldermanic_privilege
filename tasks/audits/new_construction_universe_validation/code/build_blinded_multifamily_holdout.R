# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

holdout <- readr::read_csv(
  "../output/multifamily_classification_mode_b_review_sample.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(
    sample_order = dplyr::row_number(),
    project_id
  )

inventory <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    construction_year,
    component_pins,
    source_addresses,
    current_property_addresses,
    addresses,
    x_3435,
    y_3435,
    exact_permit_numbers,
    exact_permit_addresses,
    exact_permit_descriptions,
    strong_spatial_permit_numbers,
    strong_spatial_permit_addresses,
    strong_spatial_permit_descriptions,
    exact_pin_permit_numbers,
    exact_pin_permit_addresses,
    exact_pin_positive_descriptions
  )

if (
  nrow(holdout) != 50L ||
    anyDuplicated(holdout$project_id) ||
    anyDuplicated(inventory$project_id)
) {
  stop("The held-out sample or project inventory failed validation.", call. = FALSE)
}

blinded <- holdout |>
  dplyr::left_join(
    inventory,
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  any(is.na(blinded$construction_year)) ||
    any(!is.finite(blinded$x_3435)) ||
    any(!is.finite(blinded$y_3435))
) {
  stop("Every held-out project must have a year and location.", call. = FALSE)
}

coordinates <- sf::st_as_sf(
  blinded,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
) |>
  sf::st_transform(4326) |>
  sf::st_coordinates()

blinded <- blinded |>
  dplyr::mutate(
    longitude = coordinates[, "X"],
    latitude = coordinates[, "Y"],
    primary_address = dplyr::coalesce(
      exact_pin_permit_addresses,
      exact_permit_addresses,
      current_property_addresses,
      addresses,
      source_addresses,
      strong_spatial_permit_addresses
    ),
    google_maps_url = sprintf(
      paste0(
        "https://www.google.com/maps/search/?api=1&query=",
        "%.7f,%.7f"
      ),
      latitude,
      longitude
    )
  ) |>
  dplyr::arrange(sample_order) |>
  dplyr::select(
    sample_order,
    project_id,
    construction_year,
    component_pins,
    primary_address,
    longitude,
    latitude,
    google_maps_url,
    exact_pin_permit_numbers,
    exact_pin_permit_addresses,
    exact_pin_positive_descriptions,
    exact_permit_numbers,
    exact_permit_addresses,
    exact_permit_descriptions,
    strong_spatial_permit_numbers,
    strong_spatial_permit_addresses,
    strong_spatial_permit_descriptions
  )

readr::write_csv(
  blinded,
  "../output/multifamily_classification_blinded_review.csv",
  na = ""
)
