# setwd("tasks/new_construction_cleaning/code")
# exact_match_ft <- 10
# local_match_ft <- 150
# max_building_gap <- 0.10

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(exact_match_ft, local_match_ft, max_building_gap)
if (length(args) != 3L) stop("Expected exact distance, local distance, and building-area gap.")
exact_match_ft <- as.numeric(args[1])
local_match_ft <- as.numeric(args[2])
max_building_gap <- as.numeric(args[3])
if (any(!is.finite(c(exact_match_ft, local_match_ft, max_building_gap))) ||
    exact_match_ft < 0 || local_match_ft <= exact_match_ft || max_building_gap < 0) {
  stop("Invalid historical project matching thresholds.")
}

buildings <- readr::read_csv(
  "../output/density_historical_building_universe.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(), project_key = readr::col_character(),
    current_subdivision_id = readr::col_character(), .default = readr::col_guess()
  )
)
historical <- readr::read_csv(
  "../input/density_historical_parcel_records.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(), subdivision_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyNA(buildings$pin) || anyDuplicated(buildings$pin) ||
    anyNA(buildings$project_key) || anyDuplicated(historical[c("pin", "year")])) {
  stop("Discovery buildings and historical PIN-year records must have unique keys.")
}

# Screen missing locations against the current-only universe. Adding recovered
# locations before this comparison would make them match themselves.
lineage <- buildings |>
  dplyr::filter(!current_coordinates_complete) |>
  dplyr::left_join(
    historical |>
      dplyr::select(pin, year, centroid_x_crs_3435, centroid_y_crs_3435, subdivision_id),
    by = c("pin", "construction_year" = "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(project_key, pin) |>
  dplyr::group_by(project_key) |>
  dplyr::summarise(
    source = dplyr::first(source),
    member_pins = paste(sort(unique(pin)), collapse = ";"),
    member_pin_count = dplyr::n_distinct(pin),
    reported_construction_year = min(construction_year),
    distinct_construction_years = dplyr::n_distinct(construction_year),
    distinct_unit_counts = dplyr::n_distinct(unitscount),
    distinct_building_areas = dplyr::n_distinct(areabuilding),
    unitscount = max(unitscount),
    areabuilding = max(areabuilding),
    exact_coordinate_pin_count = sum(is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)),
    historical_x = {
      located <- is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
      if (any(located)) weighted.mean(centroid_x_crs_3435[located], coordinate_weight[located]) else NA_real_
    },
    historical_y = {
      located <- is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
      if (any(located)) weighted.mean(centroid_y_crs_3435[located], coordinate_weight[located]) else NA_real_
    },
    historical_subdivision_id = {
      values <- sort(unique(stats::na.omit(subdivision_id)))
      if (length(values) == 1L) values else NA_character_
    },
    .groups = "drop"
  ) |>
  dplyr::mutate(
    historical_pin_prefix_8 = substr(member_pins, 1, 8),
    source_group_conflict = distinct_construction_years > 1 |
      distinct_unit_counts > 1 | distinct_building_areas > 1,
    lineage_row = dplyr::row_number()
  )

current <- buildings |>
  dplyr::filter(current_coordinates_complete) |>
  sf::st_as_sf(coords = c("current_longitude", "current_latitude"), crs = 4326) |>
  sf::st_transform(3435)
current_xy <- sf::st_coordinates(current)
current <- current |>
  sf::st_drop_geometry() |>
  dplyr::mutate(current_x = current_xy[, "X"], current_y = current_xy[, "Y"]) |>
  dplyr::arrange(project_key, pin) |>
  dplyr::group_by(project_key) |>
  dplyr::summarise(
    current_member_pins = paste(sort(unique(pin)), collapse = ";"),
    current_construction_year = min(construction_year),
    current_unitscount = max(unitscount),
    current_areabuilding = max(areabuilding),
    current_x = weighted.mean(current_x, coordinate_weight),
    current_y = weighted.mean(current_y, coordinate_weight),
    current_subdivision_id = {
      values <- sort(unique(stats::na.omit(current_subdivision_id)))
      if (length(values) == 1L) values else NA_character_
    },
    current_source_group_conflict = dplyr::n_distinct(construction_year) > 1 |
      dplyr::n_distinct(unitscount) > 1 | dplyr::n_distinct(areabuilding) > 1,
    .groups = "drop"
  ) |>
  dplyr::rename(current_project_key = project_key) |>
  dplyr::mutate(current_pin_prefix_8 = substr(current_member_pins, 1, 8))

same_year_matches <- list()
for (construction_year_i in sort(unique(lineage$reported_construction_year))) {
  historical_rows <- which(
    lineage$reported_construction_year == construction_year_i &
      is.finite(lineage$historical_x) & is.finite(lineage$historical_y)
  )
  current_rows <- which(current$current_construction_year == construction_year_i)
  if (length(historical_rows) == 0L || length(current_rows) == 0L) next
  nearest <- nabor::knn(
    data = as.matrix(current[current_rows, c("current_x", "current_y")]),
    query = as.matrix(lineage[historical_rows, c("historical_x", "historical_y")]),
    k = 1
  )
  same_year_matches[[as.character(construction_year_i)]] <-
    current[current_rows[nearest$nn.idx[, 1]], ] |>
    dplyr::transmute(
      lineage_row = historical_rows,
      same_year_current_member_pins = current_member_pins,
      same_year_current_unitscount = current_unitscount,
      same_year_current_areabuilding = current_areabuilding,
      same_year_current_source_group_conflict = current_source_group_conflict,
      same_year_current_subdivision_id = current_subdivision_id,
      same_year_current_pin_prefix_8 = current_pin_prefix_8,
      same_year_project_distance_ft = nearest$nn.dists[, 1]
    )
}
lineage <- lineage |>
  dplyr::left_join(dplyr::bind_rows(same_year_matches), by = "lineage_row", relationship = "one-to-one")

coordinate_rows <- which(is.finite(lineage$historical_x) & is.finite(lineage$historical_y))
nearest <- nabor::knn(
  data = as.matrix(current[c("current_x", "current_y")]),
  query = as.matrix(lineage[coordinate_rows, c("historical_x", "historical_y")]),
  k = 1
)
nearest_projects <- current[nearest$nn.idx[, 1], ] |>
  dplyr::transmute(
    lineage_row = coordinate_rows,
    nearest_current_project_pins = current_member_pins,
    nearest_current_project_year = current_construction_year,
    nearest_current_project_units = current_unitscount,
    nearest_current_project_building_area = current_areabuilding,
    nearest_current_project_distance_ft = nearest$nn.dists[, 1]
  )

lineage <- lineage |>
  dplyr::left_join(nearest_projects, by = "lineage_row", relationship = "one-to-one") |>
  dplyr::mutate(
    same_year_unit_gap = abs(unitscount - same_year_current_unitscount),
    same_year_building_gap = abs(areabuilding - same_year_current_areabuilding) /
      pmax(areabuilding, same_year_current_areabuilding),
    same_year_same_prefix_8 = historical_pin_prefix_8 == same_year_current_pin_prefix_8,
    same_year_same_subdivision = !is.na(historical_subdivision_id) &
      historical_subdivision_id != "" & historical_subdivision_id == same_year_current_subdivision_id,
    successor_match_reason = dplyr::case_when(
      same_year_current_source_group_conflict ~ NA_character_,
      same_year_project_distance_ft <= 1 ~ "same_year_exact_centroid",
      same_year_project_distance_ft <= exact_match_ft & same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap ~ "same_year_exact_location_and_attributes",
      same_year_project_distance_ft <= 50 & same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~ "same_year_local_identity_and_attributes",
      same_year_project_distance_ft <= local_match_ft & same_year_building_gap <= 0.02 &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~ "same_year_local_identity_and_building_area",
      TRUE ~ NA_character_
    ),
    high_confidence_successor = !is.na(successor_match_reason),
    probable_successor = !high_confidence_successor & !same_year_current_source_group_conflict &
      same_year_project_distance_ft <= local_match_ft & same_year_building_gap <= max_building_gap &
      (same_year_unit_gap == 0 | same_year_same_prefix_8 | same_year_same_subdivision),
    nearby_different_year_project = nearest_current_project_distance_ft <= 50 &
      abs(reported_construction_year - nearest_current_project_year) <= 2,
    lineage_status = dplyr::case_when(
      exact_coordinate_pin_count == 0 ~ "unresolved_no_exact_historical_coordinate",
      source_group_conflict ~ "unresolved_source_group_conflict",
      high_confidence_successor ~ "duplicate_high_confidence",
      probable_successor ~ "duplicate_probable",
      same_year_project_distance_ft <= local_match_ft ~ "unresolved_nearby_same_year_project",
      nearby_different_year_project ~ "unresolved_nearby_different_year_project",
      TRUE ~ "candidate_unique_historical_project"
    ),
    recommended_action = dplyr::case_when(
      lineage_status == "duplicate_high_confidence" ~ "exclude_as_already_represented",
      lineage_status == "candidate_unique_historical_project" ~ "candidate_for_recovery",
      TRUE ~ "hold_out_pending_resolution"
    )
  ) |>
  dplyr::select(-lineage_row) |>
  dplyr::arrange(project_key)
SaveData(lineage, character(), "../output/density_project_lineage.csv")
