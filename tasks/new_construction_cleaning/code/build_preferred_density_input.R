# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

library(sf)

scope <- readr::read_csv(
  "../output/preferred_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft)

ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE
)

zoning <- readr::read_csv(
  "../output/preferred_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character()),
  col_select = c(
    project_id,
    construction_zone_group,
    zoning_assignment_source
  )
)

points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    scope,
    by = c("project_id", "source_family"),
    relationship = "one-to-one"
  )

if (sf::st_crs(points)$epsg != 3435) {
  stop("Preferred project centroids must use EPSG:3435.", call. = FALSE)
}

segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(points$era))
)
points$segment_id <- assign_points_to_nearest_segments(
  points,
  points$era,
  points$ward_pair,
  segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 5000L
)

preferred <- points |>
  sf::st_drop_geometry() |>
  dplyr::inner_join(
    ledger |>
      dplyr::select(
        project_id,
        project_kind,
        dwelling_units,
        building_sqft,
        land_sqft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::inner_join(
    zoning,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    construction_date = as.Date(construction_date),
    dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
  )

# Treatment and controls are assigned after project verification in the final exporter.

required_preferred <- c("segment_id", "construction_zone_group")
missing_preferred <- preferred |>
  dplyr::filter(within_500ft) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_preferred),
      ~ sum(is.na(.x) | as.character(.x) == "")
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_rows"
  )

if (any(missing_preferred$missing_rows > 0)) {
  print(missing_preferred)
  stop("Preferred 500-foot model input has missing segment or zoning assignments.", call. = FALSE)
}
if (anyDuplicated(preferred$project_id)) {
  stop("Preferred model input is not unique by project.", call. = FALSE)
}

preferred <- preferred |> dplyr::mutate(zone_group = construction_zone_group)

production_multicard_pins <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_skip()),
  col_select = "pin"
) |>
  dplyr::distinct(pin) |>
  dplyr::pull(pin)

production_multicard_values <- readr::read_csv(
  "../output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(pin %in% production_multicard_pins) |>
  dplyr::mutate(
    is_single_family =
      (
        !is.na(single_v_multi_family) &
          stringr::str_detect(
            single_v_multi_family,
            stringr::regex("^single", ignore_case = TRUE)
          )
      ) |
      (
        !is.na(type_of_residence) &
          type_of_residence %in% c(
            "1 Story",
            "1.5 Story",
            "2 Story",
            "3 Story +",
            "Split Level"
          )
      ),
    production_units = dplyr::if_else(
      is_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) |>
  dplyr::transmute(
    pin,
    production_year = as.integer(year_built),
    production_units,
    production_building_sqft = as.numeric(building_sqft),
    production_land_sqft = as.numeric(land_sqft)
  )

preferred_multicard_values <- ledger |>
  dplyr::filter(
    source_family == "residential",
    project_kind == "same_pin_multiple_cards"
  ) |>
  dplyr::transmute(
    project_id,
    pin = component_pins,
    preferred_year = as.integer(construction_year)
  ) |>
  dplyr::inner_join(
    production_multicard_values,
    by = "pin",
    relationship = "one-to-one"
  )

preferred_multicard_projects <- preferred |>
  dplyr::filter(project_kind == "same_pin_multiple_cards") |>
  dplyr::pull(project_id)
preferred_multicard_values <- preferred_multicard_values |>
  dplyr::filter(project_id %in% preferred_multicard_projects)
if (!setequal(preferred_multicard_values$project_id, preferred_multicard_projects)) {
  stop("A preferred same-PIN multicard project lacks its production-selected card.", call. = FALSE)
}

multicard_year_mismatches <- preferred_multicard_values |>
  dplyr::filter(preferred_year != production_year)

preferred_production_card_rule <- preferred |>
  dplyr::left_join(
    preferred_multicard_values |>
      dplyr::select(
        project_id,
        production_units,
        production_building_sqft,
        production_land_sqft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    replace_multicard = project_id %in% preferred_multicard_values$project_id,
    dwelling_units = dplyr::if_else(
      replace_multicard,
      production_units,
      dwelling_units
    ),
    building_sqft = dplyr::if_else(
      replace_multicard,
      production_building_sqft,
      building_sqft
    ),
    land_sqft = dplyr::if_else(
      replace_multicard,
      production_land_sqft,
      land_sqft
    ),
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft,
    allow_far = dplyr::if_else(
      replace_multicard,
      land_sqft > 1 & building_sqft > 1,
      allow_far
    ),
    allow_dupac = dplyr::if_else(
      replace_multicard,
      land_sqft > 1 & building_sqft > 1,
      allow_dupac
    )
  ) |>
  dplyr::filter(!project_id %in% multicard_year_mismatches$project_id) |>
  dplyr::select(
    -production_units,
    -production_building_sqft,
    -production_land_sqft,
    -replace_multicard
  )

readr::write_csv(preferred_production_card_rule, "../output/preferred_density_model_production_card_input.csv", na = "")
