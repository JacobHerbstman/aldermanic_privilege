# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

eligibility <- readr::read_csv(
  "../output/eligibility_rule_validation.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    source_family,
    construction_year,
    within_500ft,
    within_1500ft,
    eligibility_rule
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_family = readr::col_character(),
    construction_year = readr::col_double(),
    within_500ft = readr::col_logical(),
    within_1500ft = readr::col_logical(),
    eligibility_rule = readr::col_character()
  )
)

classification <- readr::read_csv(
  "../output/multifamily_classification_decisions.csv",
  show_col_types = FALSE,
  col_select = c(project_id, proposed_multifamily),
  col_types = readr::cols(
    project_id = readr::col_character(),
    proposed_multifamily = readr::col_logical()
  )
)

scope <- eligibility |>
  dplyr::inner_join(
    classification,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    source_family == "residential",
    construction_year <= 2015,
    within_1500ft,
    proposed_multifamily,
    eligibility_rule ==
      "retain_assessor_report_without_contradictory_evidence"
  )

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    scope |>
      dplyr::select(
        project_id,
        construction_year,
        within_500ft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    geometry_year_gap = target_year - construction_year,
    geometry_method = "historical_parcel_polygon"
  )

if (
  anyDuplicated(project_polygons$project_id) ||
    any(!sf::st_is_valid(project_polygons))
) {
  stop("Historical project polygons are duplicated or invalid.")
}

project_points <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE,
  col_select = c(project_id, x_3435, y_3435),
  col_types = readr::cols(
    project_id = readr::col_character(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double()
  )
) |>
  dplyr::inner_join(
    scope |>
      dplyr::anti_join(
        sf::st_drop_geometry(project_polygons) |>
          dplyr::select(project_id),
        by = "project_id"
      ) |>
      dplyr::select(
        project_id,
        construction_year,
        within_500ft
      ),
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  nrow(project_polygons) + nrow(project_points) != nrow(scope) ||
    any(!is.finite(project_points$x_3435)) ||
    any(!is.finite(project_points$y_3435))
) {
  stop("Weak-project geometry is incomplete.")
}

project_points <- project_points |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = TRUE
  ) |>
  dplyr::mutate(
    target_year = construction_year,
    geometry_year_gap = 0,
    geometry_method = "audited_project_point"
  )

archive <- paste0(
  "/vsizip/",
  normalizePath("../input/chicago_building_footprints_2015.zip"),
  "/buildings.shp"
)

footprints <- sf::st_read(
  archive,
  query = paste(
    "SELECT",
    "BLDG_ID, HARRIS_STR, YEAR_BUILT, BLDG_SQ_FO, NO_OF_UNIT",
    "FROM buildings",
    "WHERE YEAR_BUILT BETWEEN 2004 AND 2015"
  ),
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::mutate(
    city_building_id = as.character(BLDG_ID),
    city_pin = stringr::str_remove_all(
      dplyr::coalesce(as.character(HARRIS_STR), ""),
      "[^0-9]"
    ),
    city_year_built = as.integer(YEAR_BUILT),
    city_building_sqft = as.numeric(BLDG_SQ_FO),
    city_dwelling_units = as.numeric(NO_OF_UNIT)
  ) |>
  dplyr::select(
    city_building_id,
    city_pin,
    city_year_built,
    city_building_sqft,
    city_dwelling_units
  )

footprint_points <- suppressWarnings(sf::st_point_on_surface(footprints))

polygon_matches <- sf::st_join(
  footprint_points,
  project_polygons |>
    dplyr::select(
      project_id,
      target_year = construction_year,
      within_500ft,
      geometry_year_gap,
      geometry_method
  ),
  join = sf::st_within,
  left = FALSE
)

point_matches <- sf::st_join(
  project_points,
  footprints,
  join = sf::st_within,
  left = FALSE
) |>
  dplyr::select(
    city_building_id,
    city_pin,
    city_year_built,
    city_building_sqft,
    city_dwelling_units,
    project_id,
    target_year,
    within_500ft,
    geometry_year_gap,
    geometry_method
  )

matches <- dplyr::bind_rows(
  polygon_matches,
  point_matches
) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    year_gap = city_year_built - target_year,
    near_reported_year = abs(year_gap) <= 1
  )

evidence <- scope |>
  dplyr::left_join(
    matches |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        matched_city_footprints = dplyr::n_distinct(city_building_id),
        near_year_city_footprints =
          dplyr::n_distinct(city_building_id[near_reported_year]),
        city_building_ids = paste(
          sort(unique(city_building_id)),
          collapse = "/"
        ),
        city_year_built_values = paste(
          sort(unique(city_year_built)),
          collapse = "/"
        ),
        city_building_sqft_values = paste(
          sort(unique(city_building_sqft[is.finite(city_building_sqft)])),
          collapse = "/"
        ),
        city_dwelling_unit_values = paste(
          sort(unique(city_dwelling_units[is.finite(city_dwelling_units)])),
          collapse = "/"
        ),
        geometry_method = paste(
          sort(unique(geometry_method)),
          collapse = "/"
        ),
        geometry_year_gap = paste(
          sort(unique(geometry_year_gap)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    matched_city_footprints = dplyr::coalesce(
      matched_city_footprints,
      0L
    ),
    near_year_city_footprints = dplyr::coalesce(
      near_year_city_footprints,
      0L
    ),
    city_footprint_support =
      near_year_city_footprints > 0
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    construction_year,
    project_id
  )

summary <- evidence |>
  dplyr::count(
    construction_year,
    within_500ft,
    city_footprint_support,
    name = "projects"
  ) |>
  dplyr::arrange(construction_year, within_500ft, city_footprint_support)

readr::write_csv(
  evidence,
  "../output/city_2015_footprint_evidence.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/city_2015_footprint_evidence_summary.csv",
  na = ""
)
