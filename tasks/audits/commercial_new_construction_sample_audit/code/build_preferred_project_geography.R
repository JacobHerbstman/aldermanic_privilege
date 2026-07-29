# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

coverage <- readr::read_csv(
  "../output/preferred_historical_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  mutate(request_id = paste(project_id, component_pin, target_year, sep = "|"))

if (anyDuplicated(coverage$request_id) > 0) {
  stop("Preferred project geography requests are not unique.", call. = FALSE)
}

direct_parcels <- sf::st_read(
  "../output/preferred_historical_parcels.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(direct_parcels)[
  names(direct_parcels) == attr(direct_parcels, "sf_column")
] <- "geometry"
sf::st_geometry(direct_parcels) <- "geometry"

if (anyDuplicated(direct_parcels[c("target_year", "object_id")]) > 0) {
  stop("Direct historical parcels are not unique by year and object ID.", call. = FALSE)
}
if (any(!sf::st_is_valid(direct_parcels)) || any(sf::st_is_empty(direct_parcels))) {
  stop("Direct historical parcel geometries must be valid and nonempty.", call. = FALSE)
}

direct_parcels_by_pin <- direct_parcels %>%
  group_by(target_year, layer_id, pin14, pin10) %>%
  summarise(
    parcel_object_ids = paste(sort(unique(object_id)), collapse = "/"),
    object_id = min(object_id),
    .groups = "drop"
  )

if (anyDuplicated(direct_parcels_by_pin[c("target_year", "pin14")]) > 0) {
  stop("An annual PIN14 maps to more than one direct parcel record.", call. = FALSE)
}

direct_exact <- direct_parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "exact_pin14"),
    by = c("target_year", "pin14" = "component_pin"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin = pin14,
    requested_pin10 = pin10.y,
    target_year,
    match_method = "exact_construction_year_pin14",
    layer_id,
    object_id,
    parcel_object_ids,
    parcel_pin14 = pin14,
    parcel_pin10 = pin10.x,
    geometry
  )

direct_pin10 <- direct_parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "unique_pin10_predecessor"),
    by = c("target_year", "pin10"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin,
    requested_pin10 = pin10,
    target_year,
    match_method = "unique_construction_year_pin10",
    layer_id,
    object_id,
    parcel_object_ids,
    parcel_pin14 = pin14,
    parcel_pin10 = pin10,
    geometry
  )

predecessor_parcels <- sf::st_read(
  "../output/preferred_historical_predecessor_selected.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(predecessor_parcels)[
  names(predecessor_parcels) == attr(predecessor_parcels, "sf_column")
] <- "geometry"
sf::st_geometry(predecessor_parcels) <- "geometry"

if (anyDuplicated(predecessor_parcels$request_id) > 0) {
  stop("Selected predecessor parcels are not unique by request ID.", call. = FALSE)
}

predecessor <- predecessor_parcels %>%
  transmute(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin,
    requested_pin10 = pin10,
    target_year,
    match_method = "point_in_construction_year_predecessor",
    layer_id = NA_integer_,
    object_id = NA_integer_,
    parcel_object_ids = predecessor_object_ids,
    parcel_pin14 = predecessor_pin14s,
    parcel_pin10 = predecessor_pin10s,
    geometry
  )

component_geometry <- rbind(direct_exact, direct_pin10, predecessor) %>%
  arrange(target_year, source_family, project_id, component_pin)

if (anyDuplicated(component_geometry$request_id) > 0) {
  stop("A component-year resolves to more than one accepted parcel polygon.", call. = FALSE)
}

requested_counts <- coverage %>%
  group_by(
    source_family,
    project_id,
    project_kind,
    candidate_status,
    target_year
  ) %>%
  summarise(requested_components = n_distinct(component_pin), .groups = "drop")

resolved_counts <- component_geometry %>%
  sf::st_drop_geometry() %>%
  group_by(
    source_family,
    project_id,
    project_kind,
    candidate_status,
    target_year
  ) %>%
  summarise(
    resolved_components = n_distinct(component_pin),
    distinct_parcel_polygons = n_distinct(paste(target_year, parcel_object_ids)),
    component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    parcel_pins = paste(sort(unique(parcel_pin14)), collapse = "/"),
    match_methods = paste(sort(unique(match_method)), collapse = "/"),
    .groups = "drop"
  )

project_year_coverage <- requested_counts %>%
  left_join(
    resolved_counts,
    by = c(
      "source_family",
      "project_id",
      "project_kind",
      "candidate_status",
      "target_year"
    ),
    relationship = "one-to-one"
  ) %>%
  mutate(
    resolved_components = coalesce(resolved_components, 0L),
    distinct_parcel_polygons = coalesce(distinct_parcel_polygons, 0L),
    unresolved_components = requested_components - resolved_components,
    collapsed_components = resolved_components - distinct_parcel_polygons,
    complete_project_geometry = unresolved_components == 0
  ) %>%
  arrange(target_year, source_family, project_id)

complete_projects <- project_year_coverage %>%
  filter(complete_project_geometry) %>%
  select(source_family, project_id, target_year)

project_geometry <- component_geometry %>%
  inner_join(
    complete_projects,
    by = c("source_family", "project_id", "target_year"),
    relationship = "many-to-one"
  ) %>%
  group_by(source_family, project_id, target_year) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  mutate(
    project_polygon_valid = sf::st_is_valid(geometry),
    project_land_area_sqft = as.numeric(sf::st_area(geometry))
  )

if (any(!project_geometry$project_polygon_valid) || any(sf::st_is_empty(project_geometry))) {
  stop("Preferred project geometries must be valid and nonempty.", call. = FALSE)
}

project_centroids <- sf::st_centroid(project_geometry) %>%
  select(
    source_family,
    project_id,
    target_year,
    project_polygon_valid,
    project_land_area_sqft,
    geometry
  )

summary <- bind_rows(
  project_year_coverage %>%
    count(source_family, complete_project_geometry, name = "value") %>%
    transmute(
      metric = paste0(
        source_family,
        "_project_year_geometry_",
        if_else(complete_project_geometry, "complete", "unresolved")
      ),
      value
    ),
  tibble::tibble(
    metric = c(
      "requested_project_component_years",
      "resolved_project_component_years",
      "complete_project_year_geometries",
      "projects_with_component_polygon_collapse"
    ),
    value = c(
      nrow(coverage),
      nrow(component_geometry),
      nrow(project_geometry),
      sum(project_year_coverage$collapsed_components > 0)
    )
  )
)

sf::st_write(
  component_geometry,
  "../output/preferred_project_component_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  project_geometry,
  "../output/preferred_project_year_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  project_centroids,
  "../output/preferred_project_year_centroids.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  project_year_coverage,
  "../output/preferred_project_year_geometry_coverage.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_geography_summary.csv"
)
