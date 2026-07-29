# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

candidate_components <- readr::read_csv(
  "../output/preferred_residential_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

dispositions <- readr::read_csv(
  "../adjudication/residential_unresolved_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

manual_projects <- readr::read_csv(
  "../adjudication/residential_unresolved_final_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    final_project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    geometry_source_project_id = readr::col_character(),
    geometry_component_pins = readr::col_character(),
    class_values = readr::col_character(),
    .default = readr::col_guess()
  )
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
    location_evidence_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

resolved_review_projects <- readr::read_csv(
  "../output/residential_review_resolution_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    geometry_source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    class_values = readr::col_character(),
    .default = readr::col_guess()
  )
)

resolved_review_components <- readr::read_csv(
  "../output/residential_review_resolution_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

review_source_dispositions <- readr::read_csv(
  "../output/residential_review_source_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

unresolved_inventory <- readr::read_csv(
  "../output/residential_unresolved_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

if (anyDuplicated(candidates$project_id) > 0 ||
    anyDuplicated(manual_projects$final_project_id) > 0 ||
    anyDuplicated(dispositions$source_project_id) > 0 ||
    anyDuplicated(suppressions$candidate_project_id) > 0 ||
    anyDuplicated(additional_decisions$candidate_project_id) > 0 ||
    anyDuplicated(resolved_review_projects$project_id) > 0 ||
    anyDuplicated(resolved_review_components$component_pin) > 0 ||
    anyDuplicated(review_source_dispositions$source_project_id) > 0) {
  stop("Residential candidate or adjudication IDs are not unique.", call. = FALSE)
}
if (!setequal(unresolved_inventory$project_id, dispositions$source_project_id)) {
  stop("The source disposition ledger does not cover the 44-project tail exactly.", call. = FALSE)
}

referenced_final_projects <- dispositions %>%
  filter(!is.na(final_project_ids), final_project_ids != "") %>%
  select(final_project_ids) %>%
  tidyr::separate_longer_delim(final_project_ids, delim = "/") %>%
  pull(final_project_ids) %>%
  unique()

if (!setequal(referenced_final_projects, manual_projects$final_project_id)) {
  stop("Source dispositions and adjudicated final projects do not agree.", call. = FALSE)
}
if (!all(suppressions$candidate_project_id %in% candidates$project_id) ||
    !all(suppressions$replacement_project_id %in% manual_projects$final_project_id)) {
  stop("A suppression references an unknown candidate or replacement project.", call. = FALSE)
}
if (any(
  candidates$candidate_status[match(suppressions$candidate_project_id, candidates$project_id)] !=
    "retain_mechanical"
)) {
  stop("Only mechanically retained candidates may be suppressed.", call. = FALSE)
}
if (!all(additional_decisions$candidate_project_id %in% candidates$project_id) ||
    !setequal(
      unique(additional_decisions$decision),
      c(
        "suppress_duplicate",
        "retain_with_override",
        "exclude_invalid_land_area",
        "replace_by_commercial"
      )
    )) {
  stop("The additional residential decisions violate their candidate or action contract.", call. = FALSE)
}
if (any(
  candidates$candidate_status[
    match(additional_decisions$candidate_project_id, candidates$project_id)
  ] != "retain_mechanical"
)) {
  stop("Additional residential decisions may only modify retained candidates.", call. = FALSE)
}

commercial_replacements <- additional_decisions %>%
  filter(decision == "replace_by_commercial")

if (any(is.na(commercial_replacements$replacement_project_ids)) ||
    !all(commercial_replacements$replacement_project_ids %in% commercial_projects$project_id)) {
  stop("A cross-family replacement does not name a retained commercial project.", call. = FALSE)
}

additional_replacements <- additional_decisions %>%
  filter(decision == "suppress_duplicate") %>%
  select(candidate_project_id, replacement_project_ids) %>%
  tidyr::separate_longer_delim(replacement_project_ids, delim = "/")

if (any(is.na(additional_replacements$replacement_project_ids)) ||
    !all(additional_replacements$replacement_project_ids %in% candidates$project_id) ||
    any(additional_replacements$candidate_project_id ==
      additional_replacements$replacement_project_ids)) {
  stop("A duplicate suppression lacks a valid distinct replacement candidate.", call. = FALSE)
}

additional_overrides <- additional_decisions %>%
  filter(decision == "retain_with_override")

if (any(!is.finite(additional_overrides$final_dwelling_units)) ||
    any(additional_overrides$final_dwelling_units <= 0) ||
    any(!is.finite(additional_overrides$final_land_sqft)) ||
    any(additional_overrides$final_land_sqft <= 0) ||
    any(!additional_overrides$location_mode %in% c(
      "completed_permit_point",
      "adjacent_year_exact_parcel_centroid",
      "preferred_project_centroid"
    ))) {
  stop("A retained residential override lacks valid fields or a location rule.", call. = FALSE)
}

direct_components <- sf::st_read(
  "../output/residential_unresolved_episode_component_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(direct_components)[names(direct_components) == attr(direct_components, "sf_column")] <- "geometry"
sf::st_geometry(direct_components) <- "geometry"

component_geometry <- direct_components %>%
  group_by(target_year, component_pin) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

accepted_episodes <- sf::st_read(
  "../output/residential_unresolved_accepted_episode_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(accepted_episodes)[names(accepted_episodes) == attr(accepted_episodes, "sf_column")] <- "geometry"
sf::st_geometry(accepted_episodes) <- "geometry"

reference_points <- readr::read_csv(
  "../output/residential_unresolved_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    reference_status == "reference_point_available",
    is.finite(x_3435),
    is.finite(y_3435)
  )

component_request_keys <- manual_projects %>%
  filter(geometry_mode == "component_union") %>%
  select(final_project_id, geometry_year, geometry_component_pins) %>%
  tidyr::separate_longer_delim(geometry_component_pins, delim = "/") %>%
  rename(component_pin = geometry_component_pins)

component_requests <- component_geometry %>%
  inner_join(
    component_request_keys,
    by = c("target_year" = "geometry_year", "component_pin"),
    relationship = "one-to-one"
  )

if (nrow(component_requests) != nrow(component_request_keys) ||
    any(sf::st_is_empty(component_requests)) ||
    any(is.na(sf::st_geometry(component_requests)))) {
  stop("An adjudicated component-union project lacks a requested polygon.", call. = FALSE)
}

component_project_geometry <- component_requests %>%
  group_by(final_project_id) %>%
  summarise(
    geometry_evidence = paste(sort(unique(component_pin)), collapse = "/"),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(geometry_source = "construction_year_component_union")

episode_request_keys <- manual_projects %>%
  filter(geometry_mode == "episode_union") %>%
  select(final_project_id, geometry_source_project_id, geometry_year)

episode_project_geometry <- accepted_episodes %>%
  select(project_id, target_year, geometry) %>%
  inner_join(
    episode_request_keys,
    by = c(
      "project_id" = "geometry_source_project_id",
      "target_year" = "geometry_year"
    ),
    relationship = "one-to-one"
  ) %>%
  transmute(
    final_project_id,
    geometry_evidence = paste0(project_id, "|", target_year),
    geometry_source = "accepted_episode_union",
    geometry
  )

if (nrow(episode_project_geometry) != nrow(episode_request_keys) ||
    any(sf::st_is_empty(episode_project_geometry)) ||
    any(is.na(sf::st_geometry(episode_project_geometry)))) {
  stop("An adjudicated episode-union project lacks its accepted geometry.", call. = FALSE)
}

reference_requests <- manual_projects %>%
  filter(geometry_mode == "reference_point") %>%
  select(final_project_id, geometry_source_project_id, geometry_year) %>%
  left_join(
    reference_points %>%
      group_by(project_id, target_year) %>%
      summarise(
        x_3435 = mean(x_3435),
        y_3435 = mean(y_3435),
        reference_point_ids = paste(sort(unique(point_id)), collapse = "/"),
        .groups = "drop"
      ),
    by = c(
      "geometry_source_project_id" = "project_id",
      "geometry_year" = "target_year"
    ),
    relationship = "one-to-one"
  )

if (any(!is.finite(reference_requests$x_3435)) ||
    any(!is.finite(reference_requests$y_3435))) {
  stop("An adjudicated reference-point project lacks coordinates.", call. = FALSE)
}

reference_project_geometry <- sf::st_as_sf(
  reference_requests,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  transmute(
    final_project_id,
    geometry_evidence = reference_point_ids,
    geometry_source = "accepted_reference_point",
    geometry
  )

manual_geometry <- rbind(
  component_project_geometry,
  episode_project_geometry,
  reference_project_geometry
) %>%
  arrange(final_project_id)

if (nrow(manual_geometry) != nrow(manual_projects) ||
    anyDuplicated(manual_geometry$final_project_id) > 0 ||
    any(!sf::st_is_valid(manual_geometry)) ||
    any(sf::st_is_empty(manual_geometry))) {
  stop("Adjudicated residential geometry does not cover every final project once.", call. = FALSE)
}

manual_centroids <- manual_geometry %>%
  mutate(geometry = sf::st_centroid(geometry))
manual_coordinates <- sf::st_coordinates(manual_centroids)
manual_location <- manual_centroids %>%
  sf::st_drop_geometry() %>%
  mutate(
    x_3435 = manual_coordinates[, "X"],
    y_3435 = manual_coordinates[, "Y"]
  )

review_component_geometry <- sf::st_read(
  "../output/preferred_project_component_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(source_family == "residential")
names(review_component_geometry)[
  names(review_component_geometry) == attr(review_component_geometry, "sf_column")
] <- "geometry"
sf::st_geometry(review_component_geometry) <- "geometry"

review_geometry_keys <- resolved_review_projects %>%
  select(
    final_project_id = project_id,
    geometry_source_project_ids,
    component_pins
  ) %>%
  tidyr::separate_longer_delim(geometry_source_project_ids, delim = "/") %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(
    source_project_id = geometry_source_project_ids,
    component_pin = component_pins
  ) %>%
  distinct(final_project_id, source_project_id, component_pin)

review_geometry_matches <- review_component_geometry %>%
  select(source_project_id = project_id, component_pin, target_year, geometry) %>%
  inner_join(
    review_geometry_keys,
    by = c("source_project_id", "component_pin"),
    relationship = "one-to-one"
  )

review_project_geometry <- review_geometry_matches %>%
  group_by(final_project_id) %>%
  summarise(
    geometry_evidence = paste(
      sort(unique(paste0(source_project_id, ":", component_pin, "@", target_year))),
      collapse = "/"
    ),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(geometry_source = "resolved_construction_site_union") %>%
  select(final_project_id, geometry_source, geometry_evidence, geometry)

if (nrow(review_project_geometry) != nrow(resolved_review_projects) ||
    anyDuplicated(review_project_geometry$final_project_id) > 0 ||
    any(sf::st_is_empty(review_project_geometry)) ||
    any(!sf::st_is_valid(review_project_geometry))) {
  stop("Resolved review projects do not have one valid site geometry.", call. = FALSE)
}

review_centroids <- review_project_geometry %>%
  mutate(geometry = sf::st_centroid(geometry))
review_coordinates <- sf::st_coordinates(review_centroids)
review_location <- review_centroids %>%
  sf::st_drop_geometry() %>%
  mutate(
    x_3435 = review_coordinates[, "X"],
    y_3435 = review_coordinates[, "Y"]
  )

retained_mechanical <- candidates %>%
  filter(candidate_status == "retain_mechanical") %>%
  anti_join(suppressions, by = c("project_id" = "candidate_project_id")) %>%
  anti_join(
    additional_decisions %>%
      filter(decision != "retain_with_override") %>%
      select(candidate_project_id),
    by = c("project_id" = "candidate_project_id")
  ) %>%
  left_join(
    additional_overrides %>%
      transmute(
        project_id = candidate_project_id,
        override_dwelling_units = final_dwelling_units,
        override_building_sqft = final_building_sqft,
        override_land_sqft = final_land_sqft,
        override_location_mode = location_mode,
        override_location_evidence_id = location_evidence_id,
        override_evidence_ids = evidence_ids,
        override_decision_reason = decision_reason,
        override_confidence = confidence
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    has_override = !is.na(override_location_mode),
    dwelling_units = if_else(
      has_override,
      as.numeric(override_dwelling_units),
      as.numeric(dwelling_units)
    ),
    building_sqft = if_else(
      has_override,
      as.numeric(override_building_sqft),
      as.numeric(building_sqft)
    ),
    land_sqft = if_else(
      has_override,
      as.numeric(override_land_sqft),
      as.numeric(land_sqft)
    ),
    units_source = if_else(
      has_override,
      paste0("adjudication:", override_evidence_ids),
      units_source
    ),
    building_source = if_else(
      has_override & is.na(override_building_sqft),
      "unavailable_after_adjudication",
      if_else(
        has_override,
        paste0("adjudication:", override_evidence_ids),
        building_source
      )
    ),
    land_source = if_else(
      has_override,
      paste0("adjudication:", override_evidence_ids),
      land_source
    ),
    decision_reason = if_else(
      has_override,
      override_decision_reason,
      decision_reason
    )
  )

preferred_centroids <- sf::st_read(
  "../output/preferred_project_year_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(source_family == "residential")

names(preferred_centroids)[
  names(preferred_centroids) == attr(preferred_centroids, "sf_column")
] <- "geometry"
sf::st_geometry(preferred_centroids) <- "geometry"

preferred_mechanical_centroids <- preferred_centroids %>%
  select(project_id, target_year, geometry) %>%
  inner_join(
    retained_mechanical %>% select(project_id, construction_year),
    by = c("project_id", "target_year" = "construction_year"),
    relationship = "one-to-one"
  ) %>%
  transmute(
    project_id,
    geometry_source = "construction_year_parcel_centroid",
    geometry_evidence = paste0(project_id, "|", target_year),
    geometry
  )

if (anyDuplicated(preferred_mechanical_centroids$project_id) > 0 ||
    any(is.na(sf::st_geometry(preferred_mechanical_centroids))) ||
    any(sf::st_is_empty(preferred_mechanical_centroids))) {
  stop("Preferred residential centroids are not unique and complete where present.", call. = FALSE)
}

missing_after_preferred <- retained_mechanical %>%
  select(project_id, construction_year) %>%
  anti_join(
    sf::st_drop_geometry(preferred_mechanical_centroids) %>% select(project_id),
    by = "project_id"
  )

permit_location_requests <- additional_overrides %>%
  filter(location_mode == "completed_permit_point") %>%
  transmute(
    project_id = candidate_project_id,
    permit = location_evidence_id
  )

permit_points <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(permit %in% permit_location_requests$permit)
names(permit_points)[
  names(permit_points) == attr(permit_points, "sf_column")
] <- "geometry"
sf::st_geometry(permit_points) <- "geometry"

permit_mechanical_centroids <- permit_points %>%
  select(permit, geometry) %>%
  inner_join(
    permit_location_requests,
    by = "permit",
    relationship = "one-to-one"
  ) %>%
  semi_join(missing_after_preferred, by = "project_id") %>%
  transmute(
    project_id,
    geometry_source = "completed_permit_point",
    geometry_evidence = paste0("permit_", permit),
    geometry
  )

if (nrow(permit_mechanical_centroids) != nrow(permit_location_requests) ||
    anyDuplicated(permit_mechanical_centroids$project_id) > 0 ||
    any(sf::st_is_empty(permit_mechanical_centroids))) {
  stop("A completed-permit location override is missing or duplicated.", call. = FALSE)
}

missing_after_permits <- missing_after_preferred %>%
  anti_join(
    sf::st_drop_geometry(permit_mechanical_centroids) %>% select(project_id),
    by = "project_id"
  )

accepted_reference_points <- readr::read_csv(
  "../output/preferred_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "residential",
    reference_status == "reference_point_available",
    is.finite(reference_x_3435),
    is.finite(reference_y_3435)
  ) %>%
  inner_join(
    missing_after_permits,
    by = c("project_id", "target_year" = "construction_year"),
    relationship = "many-to-one"
  ) %>%
  group_by(project_id) %>%
  summarise(
    x_3435 = first(reference_x_3435),
    y_3435 = first(reference_y_3435),
    distinct_reference_points = n_distinct(
      paste(round(reference_x_3435, 6), round(reference_y_3435, 6))
    ),
    geometry_evidence = paste(
      sort(unique(paste0(component_pin, ":", reference_source))),
      collapse = "/"
    ),
    .groups = "drop"
  )

if (any(accepted_reference_points$distinct_reference_points != 1)) {
  stop("A fallback project has more than one accepted reference point.", call. = FALSE)
}

reference_mechanical_centroids <- sf::st_as_sf(
  accepted_reference_points,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  transmute(
    project_id,
    geometry_source = "accepted_project_reference_point",
    geometry_evidence,
    geometry
  )

missing_after_references <- missing_after_permits %>%
  anti_join(
    sf::st_drop_geometry(reference_mechanical_centroids) %>% select(project_id),
    by = "project_id"
  )

adjacent_year_parcels <- sf::st_read(
  "../output/residential_unresolved_adjacent_year_parcels.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(
    match_status == "exact_pin14",
    project_id %in% missing_after_references$project_id
  )
names(adjacent_year_parcels)[
  names(adjacent_year_parcels) == attr(adjacent_year_parcels, "sf_column")
] <- "geometry"
sf::st_geometry(adjacent_year_parcels) <- "geometry"

adjacent_mechanical_centroids <- adjacent_year_parcels %>%
  group_by(project_id) %>%
  summarise(
    geometry_evidence = paste(
      sort(unique(paste0(pin14, "_", query_year))),
      collapse = "/"
    ),
    geometry = sf::st_centroid(sf::st_union(geometry)),
    .groups = "drop"
  ) %>%
  mutate(geometry_source = "adjacent_year_exact_parcel_centroid") %>%
  select(project_id, geometry_source, geometry_evidence, geometry)

missing_after_adjacent <- missing_after_references %>%
  anti_join(
    sf::st_drop_geometry(adjacent_mechanical_centroids) %>% select(project_id),
    by = "project_id"
  )

if (nrow(missing_after_adjacent) > 0) {
  stop(
    paste(
      "Retained residential projects still lack locations:",
      paste(missing_after_adjacent$project_id, collapse = ", ")
    ),
    call. = FALSE
  )
}

base_mechanical_centroids <- rbind(
  preferred_mechanical_centroids,
  permit_mechanical_centroids,
  reference_mechanical_centroids,
  adjacent_mechanical_centroids
)

shared_site_locations <- readr::read_csv(
  "../output/residential_shared_site_location_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(shared_site_locations$project_id) > 0 ||
    any(!shared_site_locations$project_id %in% retained_mechanical$project_id) ||
    any(!shared_site_locations$reference_point_in_selected_site)) {
  stop("Shared-site location refinements violate the retained-project contract.", call. = FALSE)
}

shared_site_centroids <- sf::st_as_sf(
  shared_site_locations,
  coords = c("reference_x_3435", "reference_y_3435"),
  crs = 3435,
  remove = FALSE
) %>%
  transmute(
    project_id,
    geometry_source = if_else(
      reference_source == "parcel_universe_2025_exact_pin",
      "successor_exact_parcel_centroid_within_shared_site",
      "accepted_address_point_within_shared_site"
    ),
    geometry_evidence = paste0(
      component_pin,
      ":",
      reference_source,
      "|historical_site_",
      parcel_object_ids
    ),
    geometry
  )

mechanical_centroids <- rbind(
  base_mechanical_centroids %>%
    filter(!project_id %in% shared_site_centroids$project_id),
  shared_site_centroids
) %>%
  arrange(project_id)

if (nrow(mechanical_centroids) != nrow(retained_mechanical) ||
    anyDuplicated(mechanical_centroids$project_id) > 0 ||
    any(is.na(sf::st_geometry(mechanical_centroids))) ||
    any(sf::st_is_empty(mechanical_centroids))) {
  stop("The location hierarchy does not cover retained residential projects once.", call. = FALSE)
}

mechanical_coordinates <- sf::st_coordinates(mechanical_centroids)
mechanical_location <- mechanical_centroids %>%
  sf::st_drop_geometry() %>%
  mutate(
    x_3435 = mechanical_coordinates[, "X"],
    y_3435 = mechanical_coordinates[, "Y"]
  )

mechanical_ledger <- retained_mechanical %>%
  transmute(
    project_id,
    source_project_ids = project_id,
    component_pins,
    project_kind,
    construction_year = as.integer(construction_year),
    dwelling_units = as.numeric(dwelling_units),
    building_sqft = as.numeric(building_sqft),
    land_sqft = as.numeric(land_sqft),
    class_values,
    year_source,
    units_source,
    building_source,
    land_source,
    membership_source = "mechanical_candidate_rule",
    evidence_ids = if_else(
      has_override,
      paste0(source_row_ids, "/", override_evidence_ids),
      source_row_ids
    ),
    decision_reason,
    confidence = if_else(
      has_override,
      override_confidence,
      "mechanical"
    ),
    decision_source = if_else(
      has_override,
      "additional_adjudication_ledger",
      "mechanical_rule"
    )
  ) %>%
  left_join(mechanical_location, by = "project_id", relationship = "one-to-one")

manual_ledger <- manual_projects %>%
  transmute(
    project_id = final_project_id,
    source_project_ids,
    component_pins,
    project_kind,
    construction_year = as.integer(construction_year),
    dwelling_units = as.numeric(dwelling_units),
    building_sqft = as.numeric(building_sqft),
    land_sqft = as.numeric(land_sqft),
    class_values,
    year_source,
    units_source,
    building_source,
    land_source,
    membership_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "adjudication_ledger"
  ) %>%
  mutate(
    building_source = if_else(
      !is.finite(building_sqft) &
        (is.na(building_source) | building_source == ""),
      "unavailable_after_adjudication",
      building_source
    )
  ) %>%
  left_join(
    manual_location %>%
      select(final_project_id, geometry_source, geometry_evidence, x_3435, y_3435),
    by = c("project_id" = "final_project_id"),
    relationship = "one-to-one"
  )

review_ledger <- resolved_review_projects %>%
  transmute(
    project_id,
    source_project_ids,
    component_pins,
    project_kind,
    construction_year = as.integer(construction_year),
    dwelling_units = as.numeric(dwelling_units),
    building_sqft = as.numeric(building_sqft),
    land_sqft = as.numeric(land_sqft),
    class_values,
    year_source,
    units_source,
    building_source,
    land_source,
    membership_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source
  ) %>%
  left_join(
    review_location %>%
      select(final_project_id, geometry_source, geometry_evidence, x_3435, y_3435),
    by = c("project_id" = "final_project_id"),
    relationship = "one-to-one"
  )

ledger <- bind_rows(mechanical_ledger, review_ledger, manual_ledger) %>%
  mutate(
    allow_far = is.finite(building_sqft) & building_sqft > 0 & land_sqft > 0,
    allow_dupac = dwelling_units > 0 & land_sqft > 0
  ) %>%
  arrange(project_id)

final_components <- bind_rows(
  candidate_components %>%
    semi_join(retained_mechanical %>% select(project_id), by = "project_id"),
  resolved_review_components,
  manual_projects %>%
    select(project_id = final_project_id, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/") %>%
    rename(component_pin = component_pins)
) %>%
  arrange(project_id, component_pin)

if (anyDuplicated(ledger$project_id) > 0 ||
    anyDuplicated(final_components$component_pin) > 0 ||
    any(!is.finite(ledger$x_3435)) ||
    any(!is.finite(ledger$y_3435)) ||
    any(ledger$dwelling_units <= 0) ||
    any(ledger$land_sqft <= 0) ||
    any(!between(ledger$construction_year, 2006L, 2022L))) {
  stop("The preferred residential ledger violates its final data contract.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "candidate_projects",
    "mechanically_retained_before_suppression",
    "suppressed_mechanical_projects",
    "additional_duplicate_suppressions",
    "cross_family_replacements",
    "additional_invalid_land_exclusions",
    "mechanically_retained_final",
    "resolved_review_projects",
    "adjudicated_final_projects",
    "preferred_residential_projects",
    "preferred_far_projects",
    "preferred_dupac_projects",
    "preferred_component_pins"
  ),
  value = c(
    nrow(candidates),
    sum(candidates$candidate_status == "retain_mechanical"),
    nrow(suppressions),
    sum(additional_decisions$decision == "suppress_duplicate"),
    sum(additional_decisions$decision == "replace_by_commercial"),
    sum(additional_decisions$decision == "exclude_invalid_land_area"),
    nrow(mechanical_ledger),
    nrow(review_ledger),
    nrow(manual_ledger),
    nrow(ledger),
    sum(ledger$allow_far),
    sum(ledger$allow_dupac),
    nrow(final_components)
  )
)

readr::write_csv(
  ledger,
  "../output/preferred_residential_project_ledger.csv"
)
readr::write_csv(
  final_components,
  "../output/preferred_residential_project_components_final.csv"
)
sf::st_write(
  rbind(manual_geometry, review_project_geometry),
  "../output/residential_adjudicated_project_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  rbind(
    mechanical_centroids %>% select(project_id, geometry),
    review_centroids %>% rename(project_id = final_project_id) %>% select(project_id, geometry),
    manual_centroids %>% rename(project_id = final_project_id) %>% select(project_id, geometry)
  ),
  "../output/preferred_residential_project_centroids.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  summary,
  "../output/preferred_residential_project_ledger_summary.csv"
)
