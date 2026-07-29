# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

projects <- readr::read_csv(
  "../output/preferred_commercial_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

requests <- projects %>%
  select(project_id, target_year = construction_year, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  transmute(
    project_id,
    target_year = as.integer(target_year),
    component_pin = component_pins,
    pin10 = str_sub(component_pin, 1, 10)
  ) %>%
  arrange(project_id, component_pin)

coverage <- readr::read_csv(
  "../output/preferred_commercial_final_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(projects$project_id) > 0 ||
    anyDuplicated(requests[c("project_id", "component_pin")]) > 0 ||
    !setequal(
      paste(requests$project_id, requests$component_pin, requests$target_year),
      paste(coverage$project_id, coverage$component_pin, coverage$target_year)
    )) {
  stop("Final commercial projects and parcel coverage do not reconcile.", call. = FALSE)
}

historical_parcels <- sf::st_read(
  "../output/preferred_commercial_final_historical_parcels.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(historical_parcels)[
  names(historical_parcels) == attr(historical_parcels, "sf_column")
] <- "geometry"
sf::st_geometry(historical_parcels) <- "geometry"

historical_parcels_by_pin <- historical_parcels %>%
  group_by(target_year, pin14, pin10) %>%
  summarise(
    parcel_object_ids = paste(sort(unique(object_id)), collapse = "/"),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  )

if (anyDuplicated(historical_parcels_by_pin[c("target_year", "pin14")]) > 0) {
  stop("A historical PIN14 has more than one selected geometry.", call. = FALSE)
}

exact_components <- coverage %>%
  filter(coverage_status == "exact_pin14") %>%
  inner_join(
    historical_parcels_by_pin,
    by = c("parcel_year" = "target_year", "component_pin" = "pin14", "pin10"),
    relationship = "many-to-one"
  ) %>%
  transmute(
    project_id,
    component_pin,
    target_year,
    parcel_year,
    location_source = "exact_construction_year_parcel",
    location_evidence = paste0(parcel_year, ":", parcel_object_ids),
    geometry
  )

unique_pin10_components <- coverage %>%
  filter(coverage_status == "unique_pin10_predecessor") %>%
  inner_join(
    historical_parcels_by_pin,
    by = c("parcel_year" = "target_year", "pin10"),
    relationship = "many-to-one"
  ) %>%
  transmute(
    project_id,
    component_pin,
    target_year,
    parcel_year,
    location_source = "unique_construction_year_pin10_parcel",
    location_evidence = paste0(parcel_year, ":", parcel_object_ids),
    geometry
  )

component_polygons <- bind_rows(exact_components, unique_pin10_components) %>%
  sf::st_as_sf(sf_column_name = "geometry", crs = 3435) %>%
  arrange(project_id, component_pin)

current_coordinates <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  filter(
    is.finite(centroid_x_crs_3435),
    is.finite(centroid_y_crs_3435)
  ) %>%
  transmute(
    component_pin = pin,
    x_3435 = centroid_x_crs_3435,
    y_3435 = centroid_y_crs_3435
  )

current_components <- coverage %>%
  filter(coverage_status == "missing_pin10") %>%
  inner_join(current_coordinates, by = "component_pin", relationship = "many-to-one") %>%
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  ) %>%
  transmute(
    project_id,
    component_pin,
    target_year,
    parcel_year,
    location_source = "current_exact_pin_centroid_fallback",
    location_evidence = paste0("parcel_universe_2025:", component_pin),
    geometry
  )

polygon_centroids <- component_polygons %>%
  mutate(geometry = sf::st_centroid(geometry))

component_locations <- rbind(polygon_centroids, current_components) %>%
  arrange(project_id, component_pin)

if (anyDuplicated(component_locations[c("project_id", "component_pin")]) > 0 ||
    any(sf::st_is_empty(component_locations)) ||
    any(!sf::st_is_valid(component_locations))) {
  stop("Commercial component locations are duplicated, empty, or invalid.", call. = FALSE)
}

component_coverage <- requests %>%
  left_join(
    sf::st_drop_geometry(component_locations) %>%
      select(project_id, component_pin, parcel_year, location_source, location_evidence),
    by = c("project_id", "component_pin"),
    relationship = "one-to-one"
  ) %>%
  add_count(project_id, name = "project_component_count") %>%
  group_by(project_id) %>%
  mutate(located_component_count = sum(!is.na(location_source))) %>%
  ungroup() %>%
  arrange(project_id, component_pin)

project_coverage <- component_coverage %>%
  group_by(project_id) %>%
  summarise(
    project_component_count = first(project_component_count),
    located_component_count = first(located_component_count),
    missing_component_pins = paste(component_pin[is.na(location_source)], collapse = "/"),
    .groups = "drop"
  )

if (any(project_coverage$located_component_count == 0) ||
    sum(project_coverage$located_component_count < project_coverage$project_component_count) != 1 ||
    sum(project_coverage$project_component_count - project_coverage$located_component_count) != 1) {
  stop("Unexpected commercial component-location coverage.", call. = FALSE)
}

complete_polygon_projects <- component_coverage %>%
  group_by(project_id) %>%
  summarise(
    all_components_have_polygons = all(
      location_source %in% c(
        "exact_construction_year_parcel",
        "unique_construction_year_pin10_parcel"
      )
    ),
    .groups = "drop"
  ) %>%
  filter(all_components_have_polygons) %>%
  select(project_id)

polygon_project_geometry <- component_polygons %>%
  semi_join(complete_polygon_projects, by = "project_id") %>%
  group_by(project_id) %>%
  summarise(
    project_geometry_source = "construction_year_component_union",
    project_geometry_evidence = paste(sort(unique(location_evidence)), collapse = "/"),
    project_polygon_area_sqft = as.numeric(sf::st_area(sf::st_union(geometry))),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(geometry = sf::st_centroid(geometry))

mixed_project_points <- component_locations %>%
  filter(!project_id %in% complete_polygon_projects$project_id)
mixed_coordinates <- sf::st_coordinates(mixed_project_points)
mixed_project_points <- mixed_project_points %>%
  sf::st_drop_geometry() %>%
  mutate(
    component_x_3435 = mixed_coordinates[, "X"],
    component_y_3435 = mixed_coordinates[, "Y"]
  ) %>%
  group_by(project_id) %>%
  summarise(
    project_geometry_source = case_when(
      all(location_source == "current_exact_pin_centroid_fallback") ~
        "current_exact_component_centroid_fallback",
      TRUE ~ "construction_year_and_current_component_centroids"
    ),
    project_geometry_evidence = paste(sort(unique(location_evidence)), collapse = "/"),
    project_polygon_area_sqft = NA_real_,
    x_3435 = mean(component_x_3435),
    y_3435 = mean(component_y_3435),
    .groups = "drop"
  ) %>%
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  ) %>%
  select(
    project_id,
    project_geometry_source,
    project_geometry_evidence,
    project_polygon_area_sqft,
    geometry
  )

project_centroids <- rbind(polygon_project_geometry, mixed_project_points) %>%
  arrange(project_id)

if (nrow(project_centroids) != nrow(projects) ||
    anyDuplicated(project_centroids$project_id) > 0 ||
    !setequal(project_centroids$project_id, projects$project_id) ||
    any(sf::st_is_empty(project_centroids)) ||
    any(!sf::st_is_valid(project_centroids))) {
  stop("Final commercial project locations are incomplete or invalid.", call. = FALSE)
}

centroid_coordinates <- sf::st_coordinates(project_centroids)
commercial_ledger <- projects %>%
  left_join(
    project_centroids %>%
      sf::st_drop_geometry() %>%
      mutate(
        x_3435 = centroid_coordinates[, "X"],
        y_3435 = centroid_coordinates[, "Y"]
      ),
    by = "project_id",
    relationship = "one-to-one"
  )

commercial_points <- project_centroids %>%
  left_join(
    projects %>% select(project_id, construction_year, allow_far, allow_dupac),
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
  points_sf = commercial_points,
  era_values = commercial_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

boundary_scope <- bind_cols(
  sf::st_drop_geometry(commercial_points),
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

if (any(!is.finite(boundary_scope$distance_to_boundary_ft)) ||
    any(is.na(boundary_scope$ward)) ||
    any(is.na(boundary_scope$ward_pair))) {
  stop("A final commercial project lacks a canonical boundary assignment.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    section = "final_ledger",
    metric = c(
      "projects",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "component_pins",
      "projects_with_incomplete_component_geography"
    ),
    value = c(
      nrow(commercial_ledger),
      sum(commercial_ledger$allow_far),
      sum(commercial_ledger$allow_dupac),
      nrow(requests),
      sum(project_coverage$located_component_count <
        project_coverage$project_component_count)
    )
  ),
  tibble::tibble(
    section = "boundary_scope",
    metric = c(
      "within_1500ft",
      "within_500ft",
      "far_within_500ft",
      "dupac_within_500ft"
    ),
    value = c(
      sum(boundary_scope$within_1500ft),
      sum(boundary_scope$within_500ft),
      sum(boundary_scope$within_500ft & boundary_scope$allow_far),
      sum(boundary_scope$within_500ft & boundary_scope$allow_dupac)
    )
  ),
  commercial_ledger %>%
    count(project_geometry_source, name = "value") %>%
    transmute(section = "geometry_source", metric = project_geometry_source, value)
)

readr::write_csv(
  commercial_ledger,
  "../output/preferred_commercial_project_ledger.csv"
)
readr::write_csv(
  component_coverage,
  "../output/preferred_commercial_project_component_locations.csv"
)
sf::st_write(
  project_centroids,
  "../output/preferred_commercial_project_centroids.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  boundary_scope,
  "../output/preferred_commercial_boundary_scope.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_commercial_final_geography_summary.csv"
)
