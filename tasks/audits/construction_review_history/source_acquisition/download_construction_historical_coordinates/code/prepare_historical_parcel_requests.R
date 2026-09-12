# setwd("tasks/download_construction_historical_coordinates/code")

source("../../setup_environment/code/packages.R")
buildings <- readr::read_csv(
  "../input/density_historical_building_universe.csv", show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) |> dplyr::filter(!current_coordinates_complete) |> dplyr::select(pin, construction_year)
if (anyNA(buildings) || anyDuplicated(buildings$pin)) stop("Historical requests must have unique PINs and construction years.")
readr::write_csv(dplyr::arrange(buildings, pin), "../output/historical_parcel_requests.csv")
