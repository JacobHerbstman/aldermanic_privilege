# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

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
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

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
    project_land_area_sqft
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
    geography_status = if_else(
      complete_project_geometry,
      "complete_construction_year_geometry",
      "unresolved_construction_year_geometry"
    )
  ) %>%
  arrange(target_year, source_family, project_id)

if (any(project_scope$complete_project_geometry & is.na(project_scope$ward))) {
  stop("A complete project disappeared during boundary assignment.", call. = FALSE)
}
if (any(!project_scope$complete_project_geometry & !is.na(project_scope$ward))) {
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

summary <- bind_rows(
  project_scope %>%
    count(source_family, geography_status, within_1500ft, name = "value") %>%
    transmute(
      metric = paste(
        source_family,
        geography_status,
        coalesce(as.character(within_1500ft), "unknown"),
        sep = ":"
      ),
      value
    ),
  adjudication_scope %>%
    count(source_family, review_scope, name = "value") %>%
    transmute(metric = paste(source_family, review_scope, sep = ":"), value)
)

readr::write_csv(
  project_scope,
  "../output/preferred_project_boundary_scope.csv"
)
readr::write_csv(
  adjudication_scope,
  "../output/preferred_adjudication_scope.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_boundary_scope_summary.csv"
)
