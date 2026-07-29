# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

ledger <- readr::read_csv(
  "../output/preferred_residential_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

components <- readr::read_csv(
  "../output/preferred_residential_project_components_final.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

adjudication_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "residential")

review_dispositions <- readr::read_csv(
  "../output/residential_review_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

review_projects <- readr::read_csv(
  "../output/residential_review_resolution_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

tail_dispositions <- readr::read_csv(
  "../adjudication/residential_unresolved_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

tail_projects <- readr::read_csv(
  "../adjudication/residential_unresolved_final_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(final_project_id = readr::col_character(), .default = readr::col_guess())
)

suppressions <- readr::read_csv(
  "../adjudication/residential_candidate_suppressions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

additional_decisions <- readr::read_csv(
  "../adjudication/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

mechanical_expected <- candidates %>%
  filter(candidate_status == "retain_mechanical") %>%
  anti_join(suppressions, by = c("project_id" = "candidate_project_id")) %>%
  anti_join(
    additional_decisions %>%
      filter(decision != "retain_with_override") %>%
      select(candidate_project_id),
    by = c("project_id" = "candidate_project_id")
  ) %>%
  select(project_id)

mechanical_observed <- ledger %>%
  filter(decision_source %in% c(
    "mechanical_rule",
    "additional_adjudication_ledger"
  )) %>%
  select(project_id)

within_review_scope <- adjudication_scope %>%
  filter(review_scope == "review_within_1500ft")

outside_review_scope <- adjudication_scope %>%
  filter(review_scope == "mechanical_rule_outside_1500ft")

unresolved_review_scope <- adjudication_scope %>%
  filter(review_scope %in% c(
    "review_geography_unresolved",
    "review_year_or_geography_unresolved"
  ))

unscoped_deferred <- candidates %>%
  filter(candidate_status == "defer_to_commercial_reconciliation") %>%
  anti_join(adjudication_scope, by = "project_id")

replacement_ids <- additional_decisions %>%
  filter(decision == "suppress_duplicate") %>%
  select(replacement_project_ids) %>%
  tidyr::separate_longer_delim(replacement_project_ids, delim = "/") %>%
  pull(replacement_project_ids)

commercial_replacement_ids <- additional_decisions %>%
  filter(decision == "replace_by_commercial") %>%
  pull(replacement_project_ids)

required_text_columns <- c(
  "year_source",
  "units_source",
  "building_source",
  "land_source",
  "membership_source",
  "evidence_ids",
  "decision_reason",
  "confidence",
  "decision_source",
  "geometry_source",
  "geometry_evidence"
)

if (anyDuplicated(ledger$project_id) > 0 ||
    anyDuplicated(components$component_pin) > 0 ||
    anyDuplicated(components[c("project_id", "component_pin")]) > 0 ||
    !setequal(ledger$project_id, components$project_id) ||
    !setequal(mechanical_expected$project_id, mechanical_observed$project_id) ||
    !setequal(review_projects$project_id, ledger$project_id[
      ledger$decision_source %in% c(
        "class297_resolution",
        "tieback_episode_resolution",
        "tieback_no_snapshot_resolution",
        "remaining_case_resolution",
        "overlap_resolution"
      )
    ]) ||
    !setequal(tail_projects$final_project_id, ledger$project_id[
      ledger$decision_source == "adjudication_ledger"
    ]) ||
    nrow(within_review_scope) != 184 ||
    !setequal(within_review_scope$project_id, review_dispositions$source_project_id) ||
    nrow(unresolved_review_scope) != 44 ||
    !setequal(unresolved_review_scope$project_id, tail_dispositions$source_project_id) ||
    nrow(outside_review_scope) != 61 ||
    nrow(unscoped_deferred) != 20 ||
    any(!unscoped_deferred$construction_year < 2006) ||
    any(!replacement_ids %in% ledger$project_id) ||
    any(!commercial_replacement_ids %in% commercial_projects$project_id) ||
    any(suppressions$candidate_project_id %in% ledger$project_id) ||
    any(additional_decisions$candidate_project_id[
      additional_decisions$decision != "retain_with_override"
    ] %in% ledger$project_id) ||
    any(!components$project_id %in% ledger$project_id)) {
  stop("Residential project membership or source dispositions do not reconcile.", call. = FALSE)
}

if (any(!between(ledger$construction_year, 2006L, 2022L)) ||
    any(!is.finite(ledger$dwelling_units) | ledger$dwelling_units <= 0) ||
    any(!is.finite(ledger$land_sqft) | ledger$land_sqft <= 0) ||
    any(!is.finite(ledger$x_3435) | !is.finite(ledger$y_3435)) ||
    any(ledger$allow_far != (
      is.finite(ledger$building_sqft) &
        ledger$building_sqft > 0 &
        ledger$land_sqft > 0
    )) ||
    any(ledger$allow_dupac != (
      ledger$dwelling_units > 0 &
        ledger$land_sqft > 0
    )) ||
    any(is.na(ledger[required_text_columns])) ||
    any(ledger[required_text_columns] == "")) {
  stop("Residential fields, provenance, or sample flags violate the final contract.", call. = FALSE)
}

prohibited_columns <- regex(
  "strictness|uncertainty_score|treatment|more_stringent|coefficient|influence|residual",
  ignore_case = TRUE
)

adjudication_files <- c(
  "../adjudication/residential_unresolved_source_dispositions.csv",
  "../adjudication/residential_unresolved_final_projects.csv",
  "../adjudication/residential_candidate_suppressions.csv",
  "../adjudication/residential_additional_candidate_decisions.csv",
  "../adjudication/residential_class297_component_overrides.csv"
)

adjudication_headers <- purrr::map_chr(
  adjudication_files,
  ~ paste(names(readr::read_csv(.x, n_max = 0, show_col_types = FALSE)), collapse = "/")
)

if (any(str_detect(adjudication_headers, prohibited_columns))) {
  stop("An adjudication ledger contains prohibited treatment or outcome fields.", call. = FALSE)
}

centroids <- sf::st_read(
  "../output/preferred_residential_project_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(centroids)[names(centroids) == attr(centroids, "sf_column")] <- "geometry"
sf::st_geometry(centroids) <- "geometry"

adjudicated_geometry <- sf::st_read(
  "../output/residential_adjudicated_project_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

if (sf::st_crs(centroids)$epsg != 3435 ||
    sf::st_crs(adjudicated_geometry)$epsg != 3435 ||
    nrow(centroids) != nrow(ledger) ||
    anyDuplicated(centroids$project_id) > 0 ||
    !setequal(centroids$project_id, ledger$project_id) ||
    nrow(adjudicated_geometry) != nrow(review_projects) + nrow(tail_projects) ||
    any(sf::st_is_empty(centroids)) ||
    any(sf::st_is_empty(adjudicated_geometry)) ||
    any(!sf::st_is_valid(adjudicated_geometry))) {
  stop("Final residential geometries are incomplete, duplicated, or invalid.", call. = FALSE)
}

centroid_coordinates <- sf::st_coordinates(centroids)
coordinate_check <- centroids %>%
  sf::st_drop_geometry() %>%
  transmute(
    project_id,
    geometry_x = centroid_coordinates[, "X"],
    geometry_y = centroid_coordinates[, "Y"]
  ) %>%
  inner_join(
    ledger %>% select(project_id, x_3435, y_3435),
    by = "project_id",
    relationship = "one-to-one"
  )

if (any(abs(coordinate_check$geometry_x - coordinate_check$x_3435) > 1e-6) ||
    any(abs(coordinate_check$geometry_y - coordinate_check$y_3435) > 1e-6)) {
  stop("Flat ledger coordinates do not match the final centroid geometry.", call. = FALSE)
}

centroids <- centroids %>%
  left_join(
    ledger %>% select(project_id, construction_year, allow_far, allow_dupac),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

boundary_assignment <- assign_points_to_boundaries(
  points_sf = centroids,
  era_values = centroids$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

boundary_scope <- bind_cols(
  sf::st_drop_geometry(centroids),
  boundary_assignment
) %>%
  transmute(
    project_id,
    construction_year,
    construction_date,
    boundary_year,
    era,
    ward,
    neighbor_ward,
    ward_pair = ward_pair_id,
    distance_to_boundary_ft = dist_ft,
    within_1500ft = is.finite(dist_ft) & dist_ft <= 1500,
    within_500ft = is.finite(dist_ft) & dist_ft <= 500,
    allow_far,
    allow_dupac
  ) %>%
  arrange(project_id)

if (nrow(boundary_scope) != nrow(ledger) ||
    anyDuplicated(boundary_scope$project_id) > 0 ||
    any(!is.finite(boundary_scope$distance_to_boundary_ft)) ||
    any(is.na(boundary_scope$ward)) ||
    any(is.na(boundary_scope$ward_pair))) {
  stop("A final residential project lacks a canonical boundary assignment.", call. = FALSE)
}

shared_location_groups <- ledger %>%
  count(x_3435, y_3435, name = "projects_at_location") %>%
  filter(projects_at_location > 1)

shared_location_projects <- ledger %>%
  inner_join(shared_location_groups, by = c("x_3435", "y_3435")) %>%
  left_join(
    boundary_scope %>% select(project_id, era, within_500ft),
    by = "project_id",
    relationship = "one-to-one"
  )

shared_site_projects_near_boundary <- shared_location_projects %>%
  filter(
    within_500ft,
    geometry_source == "construction_year_parcel_centroid"
  ) %>%
  select(project_id, era)

shared_site_geometries <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  inner_join(shared_site_projects_near_boundary, by = "project_id")

shared_site_boundary_intersections <- purrr::map_dfr(
  unique(shared_site_geometries$era),
  function(era_value) {
    era_sites <- shared_site_geometries[
      shared_site_geometries$era == era_value,
    ]
    tibble::tibble(
      project_id = era_sites$project_id,
      boundary_intersections = lengths(
        sf::st_intersects(era_sites, boundary_lines[[era_value]])
      )
    )
  }
)

if (nrow(shared_site_geometries) != nrow(shared_site_projects_near_boundary) ||
    any(shared_site_boundary_intersections$boundary_intersections > 0)) {
  stop(
    "A shared-centroid historical site inside 500 feet intersects a ward boundary.",
    call. = FALSE
  )
}

plausibility <- ledger %>%
  mutate(
    far = building_sqft / land_sqft,
    dupac = dwelling_units * 43560 / land_sqft,
    review_reason = case_when(
      !allow_far ~ "building_area_unavailable",
      far > 4 ~ "far_above_4",
      dupac > 100 ~ "dupac_above_100",
      land_sqft < 500 ~ "land_below_500_sqft",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(review_reason)) %>%
  arrange(review_reason, desc(far), desc(dupac), project_id)

if (max(ledger$building_sqft / ledger$land_sqft, na.rm = TRUE) > 10 ||
    max(ledger$dwelling_units * 43560 / ledger$land_sqft, na.rm = TRUE) > 250 ||
    min(ledger$land_sqft, na.rm = TRUE) < 400) {
  stop("A final residential density field exceeds the conservative plausibility bound.", call. = FALSE)
}

boundary_summary <- tibble::tibble(
  section = "boundary_scope",
  metric = c(
    "citywide",
    "within_1500ft",
    "within_500ft",
    "far_within_1500ft",
    "dupac_within_1500ft",
    "far_within_500ft",
    "dupac_within_500ft"
  ),
  value = c(
    nrow(boundary_scope),
    sum(boundary_scope$within_1500ft),
    sum(boundary_scope$within_500ft),
    sum(boundary_scope$within_1500ft & boundary_scope$allow_far),
    sum(boundary_scope$within_1500ft & boundary_scope$allow_dupac),
    sum(boundary_scope$within_500ft & boundary_scope$allow_far),
    sum(boundary_scope$within_500ft & boundary_scope$allow_dupac)
  )
)

if (boundary_summary$value[
      boundary_summary$metric == "far_within_1500ft"
    ] > boundary_summary$value[boundary_summary$metric == "within_1500ft"] ||
    boundary_summary$value[
      boundary_summary$metric == "dupac_within_1500ft"
    ] > boundary_summary$value[boundary_summary$metric == "within_1500ft"] ||
    boundary_summary$value[
      boundary_summary$metric == "far_within_500ft"
    ] > boundary_summary$value[boundary_summary$metric == "within_500ft"] ||
    boundary_summary$value[
      boundary_summary$metric == "dupac_within_500ft"
    ] > boundary_summary$value[boundary_summary$metric == "within_500ft"]) {
  stop("Boundary eligibility counts exceed their geographic samples.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    section = "final_ledger",
    metric = c(
      "projects",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "component_pins",
      "resolved_review_projects",
      "tail_adjudicated_projects",
      "documented_missing_building_area"
    ),
    value = c(
      nrow(ledger),
      sum(ledger$allow_far),
      sum(ledger$allow_dupac),
      nrow(components),
      nrow(review_projects),
      nrow(tail_projects),
      sum(!ledger$allow_far)
    )
  ),
  boundary_summary,
  tibble::tibble(
    section = "shared_locations",
    metric = c(
      "duplicate_coordinate_groups",
      "projects_at_duplicate_coordinates",
      "projects_at_duplicate_coordinates_within_500ft",
      "shared_site_centroid_projects_within_500ft",
      "shared_site_polygons_intersecting_boundary"
    ),
    value = c(
      nrow(shared_location_groups),
      nrow(shared_location_projects),
      sum(shared_location_projects$within_500ft),
      nrow(shared_site_projects_near_boundary),
      sum(shared_site_boundary_intersections$boundary_intersections > 0)
    )
  ),
  ledger %>%
    count(geometry_source, name = "value") %>%
    transmute(section = "geometry_source", metric = geometry_source, value)
)

readr::write_csv(
  boundary_scope,
  "../output/preferred_residential_boundary_scope.csv"
)
readr::write_csv(
  plausibility,
  "../output/preferred_residential_plausibility_review.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_residential_validation_summary.csv"
)
