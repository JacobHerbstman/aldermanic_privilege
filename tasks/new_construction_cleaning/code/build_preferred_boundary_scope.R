# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

project_coverage <- readr::read_csv(
  "../output/preferred_project_year_geometry_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(project_coverage[c("source_family", "project_id", "target_year")]) > 0) {
  stop("Preferred project-year coverage is not unique.", call. = FALSE)
}

project_centroids <- sf::st_read(
  "../output/preferred_project_year_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  mutate(
    construction_date = as.Date(paste0(target_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(project_centroids$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(project_centroids$era))

# The shared assignment helper must not silently choose between overlapping wards.
for (era_value in unique(project_centroids$era)) {
  stopifnot(sf::st_crs(ward_maps[[era_value]])$epsg == 3435,
    sf::st_crs(boundary_lines[[era_value]])$epsg == 3435,
    !anyDuplicated(boundary_lines[[era_value]]$ward_pair_id),
    all(lengths(sf::st_within(project_centroids[project_centroids$era == era_value, ],
      ward_maps[[era_value]])) == 1L))
}

boundary_assignment <- assign_points_to_boundaries(
  points_sf = project_centroids,
  era_values = project_centroids$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

located_projects <- bind_cols(
  sf::st_drop_geometry(project_centroids),
  boundary_assignment
) %>%
  transmute(
    source_family,
    project_id,
    target_year,
    construction_date,
    boundary_year,
    era,
    ward,
    neighbor_ward,
    ward_pair = ward_pair_id,
    distance_to_boundary_ft = dist_ft,
    within_1500ft = is.finite(dist_ft) & dist_ft <= 1500,
    within_500ft = is.finite(dist_ft) & dist_ft <= 500,
    project_land_area_sqft,
    location_source,
    location_reference_year,
    location_reference_row_ids,
    location_reference_pin
  )

if (anyDuplicated(located_projects[c("source_family", "project_id", "target_year")]) > 0) {
  stop("Located preferred projects are not unique.", call. = FALSE)
}
if (any(!is.finite(located_projects$distance_to_boundary_ft)) ||
    any(is.na(located_projects$ward)) ||
    any(is.na(located_projects$ward_pair))) {
  stop("A complete project geometry lacks a ward-boundary assignment.", call. = FALSE)
}

project_scope <- project_coverage %>%
  left_join(
    located_projects,
    by = c("source_family", "project_id", "target_year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    geography_status = case_when(
      complete_project_geometry ~ "complete_construction_year_geometry",
      location_source == "reviewed_completed_permit_point" ~ "reviewed_permit_location",
      TRUE ~ "unresolved_construction_year_geometry"
    )
  ) %>%
  arrange(target_year, source_family, project_id)

if (any(project_scope$complete_project_geometry & is.na(project_scope$ward))) {
  stop("A complete project disappeared during boundary assignment.", call. = FALSE)
}
if (any(!project_scope$complete_project_geometry & !is.na(project_scope$ward) &
    project_scope$location_source != "reviewed_completed_permit_point")) {
  stop("An incomplete project received a boundary assignment.", call. = FALSE)
}

review_projects <- bind_rows(
  readr::read_csv(
    "../output/residential_adjudication_queue.csv",
    show_col_types = FALSE,
    col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
  ) %>%
    transmute(source_family = "residential", project_id),
  readr::read_csv(
    "../output/commercial_adjudication_queue.csv",
    show_col_types = FALSE,
    col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
  ) %>%
    transmute(source_family, project_id)
) %>%
  distinct()

if (anyDuplicated(review_projects[c("source_family", "project_id")]) > 0) {
  stop("Adjudication queue project IDs are not unique within source family.", call. = FALSE)
}

adjudication_scope <- review_projects %>%
  left_join(
    project_scope %>%
      select(
        source_family,
        project_id,
        project_kind,
        target_year,
        geography_status,
        unresolved_components,
        collapsed_components,
        distance_to_boundary_ft,
        within_1500ft,
        within_500ft
      ),
    by = c("source_family", "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    review_scope = case_when(
      geography_status == "unresolved_construction_year_geometry" ~ "review_geography_unresolved",
      within_1500ft ~ "review_within_1500ft",
      !is.na(within_1500ft) ~ "mechanical_rule_outside_1500ft",
      TRUE ~ "review_year_or_geography_unresolved"
    )
  ) %>%
  arrange(source_family, review_scope, project_id)

if (any(is.na(adjudication_scope$review_scope))) {
  stop("Every queued project must receive a review scope.", call. = FALSE)
}

readr::write_csv(
  project_scope,
  "../output/preferred_project_boundary_scope.csv"
)
readr::write_csv(
  adjudication_scope,
  "../output/preferred_adjudication_scope.csv"
)
