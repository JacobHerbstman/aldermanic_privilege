# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
# Restore the selection used in the July 18, 2026 export. The comparison
# universe for lineage screening must precede historical-coordinate recovery.
projects <- readr::read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_key = readr::col_character(),
    member_pins = readr::col_character(),
    recommended_action = readr::col_character(),
    .default = readr::col_skip()
  )
)
addresses <- readr::read_csv(
  "../output/density_parcel_address_lineage_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_key = readr::col_character(),
    address_audit_recommendation = readr::col_character(),
    .default = readr::col_skip()
  )
)
candidates <- readr::read_csv(
  "../output/density_historical_coordinate_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
)
year_corrections <- readr::read_csv(
  "../input/historical_coordinate_year_corrections.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    matched_construction_year = readr::col_integer(),
    corrected_construction_year = readr::col_integer(),
    reason = readr::col_character()
  )
)
if (anyNA(projects$project_key) || anyDuplicated(projects$project_key) ||
    anyNA(addresses$project_key) || anyDuplicated(addresses$project_key) ||
    anyNA(candidates$pin) || anyDuplicated(candidates$pin) ||
    anyNA(year_corrections) || anyDuplicated(year_corrections$pin)) {
  stop("Historical-coordinate screening inputs have invalid or duplicate keys.")
}

selected <- projects |>
  dplyr::left_join(addresses, by = "project_key", relationship = "one-to-one") |>
  dplyr::filter(dplyr::coalesce(
    recommended_action == "candidate_for_recovery" &
      address_audit_recommendation != "exclude_address_confirmed_duplicate",
    FALSE
  )) |>
  dplyr::select(project_key, member_pins) |>
  tidyr::separate_longer_delim(member_pins, delim = ";") |>
  dplyr::rename(pin = member_pins)
if (anyNA(selected$pin) || anyDuplicated(selected$pin)) {
  stop("An accepted historical PIN belongs to multiple project groups or is missing.")
}

coordinates <- dplyr::bind_rows(
  candidates |>
    dplyr::filter(coordinate_source == "historical_parcel") |>
    dplyr::inner_join(selected, by = "pin", relationship = "one-to-one") |>
    dplyr::select(-project_key),
  candidates |>
    dplyr::filter(coordinate_source == "historical_address")
) |>
  dplyr::arrange(pin)
if (nrow(dplyr::anti_join(
  year_corrections,
  coordinates,
  by = c("pin", "matched_construction_year" = "construction_year")
)) > 0L) {
  stop("A recorded construction-year correction does not match its accepted historical record.")
}
coordinates <- coordinates |>
  dplyr::left_join(year_corrections, by = "pin", relationship = "one-to-one") |>
  dplyr::mutate(construction_year = dplyr::coalesce(
    corrected_construction_year, construction_year
  )) |>
  dplyr::select(pin, construction_year, longitude, latitude, coordinate_source)
if (anyDuplicated(coordinates$pin) ||
    any(!is.finite(coordinates$longitude) | !is.finite(coordinates$latitude))) {
  stop("Accepted historical coordinates are duplicated or incomplete.")
}
SaveData(coordinates, c("pin"), "../output/density_historical_coordinates.csv")
