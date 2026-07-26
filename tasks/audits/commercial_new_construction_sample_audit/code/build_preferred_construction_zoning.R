# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

library(sf)

zone_group <- function(zone_code) {
  zone_code <- stringr::str_to_upper(as.character(zone_code))
  dplyr::case_when(
    stringr::str_detect(zone_code, "^RS-?") ~ "Single-Family Residential",
    stringr::str_detect(zone_code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    stringr::str_detect(zone_code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    stringr::str_detect(zone_code, "^C-?[1-7]-") ~ "Commercial",
    stringr::str_detect(zone_code, "^M-?[1-7]-") ~ "Industrial",
    stringr::str_detect(zone_code, "^(DX|DR|DS|DC)-") ~ "Downtown",
    stringr::str_starts(zone_code, "PD") ~ "Planned Development",
    stringr::str_starts(zone_code, "PMD") ~ "Planned Manufacturing",
    stringr::str_starts(zone_code, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

projects <- readr::read_csv(
  "../output/preferred_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft) |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    construction_date,
    within_500ft
  )

components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    projects,
    by = c("project_id", "source_family"),
    relationship = "one-to-one"
  )

if (sf::st_crs(points)$epsg != 3435) {
  stop("Preferred project centroids must use EPSG:3435.", call. = FALSE)
}
if (anyDuplicated(points$project_id)) {
  stop("Preferred project centroids are not unique by project.", call. = FALSE)
}

validated <- readr::read_csv(
  "../adjudication/historical_zoning_project_construction_year.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character())
) |>
  dplyr::transmute(
    component_pin = pin,
    validated_year = as.integer(construction_year),
    validated_group = construction_zone_group,
    validated_status = construction_zoning_status,
    longitude,
    latitude
  )

if (anyDuplicated(validated$component_pin)) {
  stop("Validated construction-year zoning is not unique by PIN.", call. = FALSE)
}

component_matches <- components |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    validated,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    exact_year = construction_year == validated_year,
    adjacent_year = abs(construction_year - validated_year) <= 1
  ) |>
  dplyr::summarise(
    component_count = dplyr::n(),
    matched_component_count = sum(!is.na(validated_group)),
    exact_component_count = sum(exact_year, na.rm = TRUE),
    adjacent_component_count = sum(adjacent_year, na.rm = TRUE),
    exact_groups = paste(sort(unique(validated_group[exact_year])), collapse = ";"),
    adjacent_groups = paste(sort(unique(validated_group[adjacent_year])), collapse = ";"),
    exact_group_count = dplyr::n_distinct(validated_group[exact_year], na.rm = TRUE),
    adjacent_group_count = dplyr::n_distinct(validated_group[adjacent_year], na.rm = TRUE),
    .by = project_id
  )

zoning_2006 <- sf::st_read(
  "../adjudication/historical_zoning_2006_candidate.gpkg",
  quiet = TRUE
) |>
  dplyr::select(zone_group_2006 = candidate_zone_group_2006)
zoning_2012 <- sf::st_read(
  "/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2012 = ZONE_CLASS)
zoning_2014 <- sf::st_read(
  "/vsizip/../input/zoning_sep2014.zip/Zoning.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2014 = ZONE_CLASS)
zoning_2016 <- sf::st_read(
  "/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2016 = ZONE_CLASS)
zoning_2025 <- sf::st_read(
  "../input/zoning_sep2025.geojson",
  quiet = TRUE
) |>
  dplyr::select(
    zone_code_2025 = zone_class,
    ordinance_number_2025 = ordinance,
    ordinance_date_2025 = ordinance_1,
    clerk_document_2025 = clerk_docn
  )

for (object_name in c(
  "zoning_2006",
  "zoning_2012",
  "zoning_2014",
  "zoning_2016",
  "zoning_2025"
)) {
  object <- get(object_name)
  if (sf::st_crs(object) != sf::st_crs(points)) {
    object <- sf::st_transform(object, sf::st_crs(points))
  }
  assign(object_name, object)
}

project_count <- nrow(points)
points <- sf::st_join(points, zoning_2006, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2012, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2014, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2016, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2025, left = TRUE, largest = TRUE)
if (nrow(points) != project_count) {
  stop("Historical zoning joins changed the preferred-project row count.", call. = FALSE)
}

points <- points |>
  dplyr::mutate(
    zone_group_2012 = zone_group(zone_code_2012),
    zone_group_2014 = zone_group(zone_code_2014),
    zone_group_2016 = zone_group(zone_code_2016),
    zone_group_2025 = zone_group(zone_code_2025),
    construction_date = as.Date(construction_date),
    ordinance_date_2025 = as.Date(ordinance_date_2025),
    current_last_event_preconstruction = !is.na(ordinance_date_2025) &
      ordinance_date_2025 <= construction_date,
    preceding_snapshot_group = dplyr::case_when(
      construction_year <= 2012 ~ zone_group_2006,
      construction_year <= 2014 ~ zone_group_2012,
      construction_year == 2015 ~ zone_group_2014,
      construction_year >= 2016 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    early_missing_2006_fallback = dplyr::if_else(
      construction_year <= 2012 &
        is.na(preceding_snapshot_group) &
        zone_group_2012 == zone_group_2014 &
        zone_group_2014 == zone_group_2016 &
        (is.na(ordinance_date_2025) | ordinance_date_2025 > construction_date),
      zone_group_2012,
      NA_character_
    )
  ) |>
  dplyr::left_join(
    component_matches,
    by = "project_id",
    relationship = "one-to-one"
  )

validated_points <- sf::st_as_sf(
  validated,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) |>
  sf::st_transform(3435)
nearest_index <- sf::st_nearest_feature(points, validated_points)
points$nearest_validated_pin <- validated_points$component_pin[nearest_index]
points$nearest_validated_year <- validated_points$validated_year[nearest_index]
points$nearest_validated_group <- validated_points$validated_group[nearest_index]
points$nearest_validated_distance_ft <- as.numeric(sf::st_distance(
  points,
  validated_points[nearest_index, ],
  by_element = TRUE
))

points <- points |>
  dplyr::mutate(
    stable_interval_group = dplyr::case_when(
      construction_year == 2006 ~ zone_group_2006,
      construction_year <= 2012 & zone_group_2006 == zone_group_2012 ~ zone_group_2006,
      construction_year <= 2014 & zone_group_2012 == zone_group_2014 ~ zone_group_2012,
      construction_year == 2015 & zone_group_2014 == zone_group_2016 ~ zone_group_2014,
      construction_year >= 2016 & zone_group_2016 == zone_group_2025 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    coincident_validated_group = dplyr::if_else(
      nearest_validated_distance_ft <= 1 &
        abs(construction_year - nearest_validated_year) <= 1,
      nearest_validated_group,
      NA_character_
    ),
    construction_zone_group = dplyr::case_when(
      exact_group_count == 1 ~ exact_groups,
      exact_group_count == 0 & adjacent_group_count == 1 ~ adjacent_groups,
      !is.na(coincident_validated_group) ~ coincident_validated_group,
      !is.na(stable_interval_group) ~ stable_interval_group,
      current_last_event_preconstruction ~ zone_group_2025,
      !is.na(preceding_snapshot_group) ~ preceding_snapshot_group,
      !is.na(early_missing_2006_fallback) ~ early_missing_2006_fallback,
      TRUE ~ NA_character_
    ),
    zoning_assignment_source = dplyr::case_when(
      exact_group_count == 1 ~ "validated_component_exact_year",
      exact_group_count == 0 & adjacent_group_count == 1 ~
        "validated_component_adjacent_year",
      !is.na(coincident_validated_group) ~ "coincident_validated_project",
      !is.na(stable_interval_group) ~ "stable_official_snapshot_interval",
      current_last_event_preconstruction ~
        "current_polygon_last_event_preconstruction",
      !is.na(preceding_snapshot_group) ~ "preceding_official_snapshot",
      !is.na(early_missing_2006_fallback) ~
        "stable_later_snapshots_missing_2006_polygon",
      TRUE ~ "unresolved_snapshot_change"
    )
  )

flat <- points |>
  sf::st_drop_geometry() |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    within_500ft,
    construction_zone_group,
    zoning_assignment_source,
    component_count,
    matched_component_count,
    exact_component_count,
    adjacent_component_count,
    exact_groups,
    adjacent_groups,
    zone_group_2006,
    zone_group_2012,
    zone_group_2014,
    zone_group_2016,
    zone_group_2025,
    ordinance_number_2025,
    ordinance_date_2025,
    clerk_document_2025,
    nearest_validated_pin,
    nearest_validated_year,
    nearest_validated_group,
    nearest_validated_distance_ft
  )

if (nrow(flat) != nrow(projects) || anyDuplicated(flat$project_id)) {
  stop("Preferred construction-zoning output is not one row per project.", call. = FALSE)
}

readr::write_csv(
  flat,
  "../output/preferred_new_construction_zoning.csv",
  na = ""
)
readr::write_csv(
  flat |>
    dplyr::filter(is.na(construction_zone_group)) |>
    dplyr::arrange(dplyr::desc(within_500ft), construction_year, source_family, project_id),
  "../output/preferred_new_construction_zoning_unresolved.csv",
  na = ""
)
readr::write_csv(
  flat |>
    dplyr::count(
      within_500ft,
      source_family,
      zoning_assignment_source,
      name = "projects"
    ) |>
    dplyr::arrange(dplyr::desc(within_500ft), source_family, zoning_assignment_source),
  "../output/preferred_new_construction_zoning_summary.csv",
  na = ""
)
