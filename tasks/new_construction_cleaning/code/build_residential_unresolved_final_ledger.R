# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

# Measurements are final. This script attaches locations without correcting them.
projects <- readr::read_csv("../output/residential_selected_buildings.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", class_values = "c",
    .default = readr::col_guess()))
# Density and physical measurements are complete before final geography starts.
measurements <- readr::read_csv("../output/new_construction_measurements.csv",
  col_types = readr::cols(.default = readr::col_guess())) %>% filter(source_family == "residential")
stopifnot(!anyDuplicated(measurements$project_id), setequal(projects$project_id, measurements$project_id))
measurements <- measurements[match(projects$project_id, measurements$project_id), ]
for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft", "allow_far", "allow_dupac")) {
  stopifnot(isTRUE(all.equal(projects[[field]], measurements[[field]], check.attributes = FALSE)))
}

review <- projects %>% filter(decision_source != "residential_candidate")
components <- projects %>% select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>% rename(component_pin = component_pins)
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(components$component_pin))

# Reuse an accepted source location only for the identical construction year.
# Condo replacements preserve a verified one-building source identity; their
# reported land replaces the measurement, not the location or ward-map vintage.
points <- sf::st_read("../output/preferred_project_year_centroids.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>% filter(location_source != "former_parcel_centroid_unresolved_individual")
stopifnot(!anyDuplicated(points$project_id))
centroids <- points %>% rename(geometry_source_project_ids = project_id) %>%
  inner_join(projects %>% select(project_id, geometry_source_project_ids, construction_year),
    by = c("geometry_source_project_ids", "target_year" = "construction_year"), relationship = "one-to-one") %>%
  select(project_id, project_geometry_source = location_source, location_reference_year,
    location_reference_row_ids, location_reference_pin) %>% arrange(project_id)
stopifnot(!anyDuplicated(centroids$project_id), !any(sf::st_is_empty(centroids)),
  all(sf::st_is_valid(centroids)))
xy <- sf::st_coordinates(centroids)
ledger <- projects %>% left_join(sf::st_drop_geometry(centroids) %>%
  mutate(x_3435 = xy[, 1], y_3435 = xy[, 2]), by = "project_id", relationship = "one-to-one") %>%
  mutate(location_resolved = is.finite(x_3435) & is.finite(y_3435),
    allow_far = allow_far & location_resolved, allow_dupac = allow_dupac & location_resolved)
polygons <- sf::st_read("../output/preferred_project_year_geometry.gpkg", quiet = TRUE) %>%
  rename(geometry_source_project_ids = project_id) %>%
  inner_join(review %>% select(project_id, geometry_source_project_ids, construction_year),
    by = c("geometry_source_project_ids", "target_year" = "construction_year"), relationship = "one-to-one") %>%
  select(project_id)
readr::write_csv(ledger, "../output/preferred_residential_project_ledger.csv")
readr::write_csv(arrange(components, project_id, component_pin), "../output/preferred_residential_project_components_final.csv")
sf::st_write(centroids, "../output/preferred_residential_project_centroids.gpkg", delete_dsn = TRUE, quiet = TRUE)
sf::st_write(polygons, "../output/residential_adjudicated_project_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)
