# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

library(sf)

scope <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft)

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

zoning <- readr::read_csv(
  "../output/final_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_zone_group = readr::col_character(),
    zoning_assignment_source = readr::col_character(),
    .default = readr::col_skip()
  )
)

previous <- readr::read_csv(
  "../output/preferred_density_model_production_card_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (
  anyDuplicated(scope$project_id) ||
    anyDuplicated(ledger$project_id) ||
    anyDuplicated(zoning$project_id) ||
    anyDuplicated(previous$project_id)
) {
  stop("A final density input is not unique by project.", call. = FALSE)
}

retained <- previous |>
  dplyr::semi_join(
    scope |>
      dplyr::filter(ledger_action == "retain_existing"),
    by = "project_id"
  ) |>
  dplyr::mutate(ledger_action = "retain_existing")

additions <- scope |>
  dplyr::filter(ledger_action == "add_recovered_project") |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        project_id,
        source_addresses,
        allow_far,
        allow_dupac
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

addition_points <- sf::st_as_sf(
  additions,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(addition_points$era))
)
additions$segment_id <- assign_points_to_nearest_segments(
  addition_points,
  addition_points$era,
  addition_points$ward_pair,
  segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 1000L
)

# Attach construction evidence here; treatment and controls belong to the final export.
additions <- additions |>
  dplyr::left_join(zoning, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft,
    zone_group = construction_zone_group
  )

required_fields <- c("segment_id", "zone_group")
missing_addition_fields <- additions |>
  dplyr::filter(within_500ft) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_fields),
      ~ sum(is.na(.x) | as.character(.x) == "")
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_rows"
  )

if (any(missing_addition_fields$missing_rows > 0)) {
  print(missing_addition_fields)
  stop("A recovered 500-foot project lacks a segment or zoning assignment.", call. = FALSE)
}

model_columns <- c(
  "project_id",
  "source_family",
  "ledger_action",
  "project_kind",
  "construction_year",
  "construction_date",
  "ward",
  "neighbor_ward",
  "ward_pair",
  "distance_to_boundary_ft",
  "within_500ft",
  "within_1500ft",
  "allow_far",
  "allow_dupac",
  "segment_id",
  "dwelling_units",
  "building_sqft",
  "land_sqft",
  "density_far",
  "density_dupac",
  "zone_group"
)

retained <- retained |>
  dplyr::select(dplyr::all_of(model_columns))
additions <- additions |>
  dplyr::select(dplyr::all_of(model_columns))

final <- dplyr::bind_rows(retained, additions)

if (
  anyDuplicated(final$project_id) ||
    nrow(additions) != 35L ||
    any(!additions$allow_far) ||
    any(!additions$allow_dupac)
) {
  stop("Final density model input failed validation.", call. = FALSE)
}

readr::write_csv(final, "../output/final_density_model_input.csv", na = "")
