# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

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

points <- centroids %>% inner_join(ledger %>% select(project_id, construction_year),
    by = "project_id", relationship = "one-to-one") %>%
  mutate(construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year))
stopifnot(!anyDuplicated(ledger$project_id), !anyDuplicated(points$project_id),
  setequal(points$project_id, ledger$project_id[ledger$location_resolved]))
ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>% sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
for (era_value in unique(points$era)) stopifnot(all(lengths(sf::st_within(
  points[points$era == era_value, ], ward_maps[[era_value]])) == 1L))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)
located <- bind_cols(sf::st_drop_geometry(points), assignment) %>%
  transmute(project_id, construction_year, construction_date, boundary_year, era,
    ward, neighbor_ward, ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= 1500, within_500ft = dist_ft <= 500)
stopifnot(all(is.finite(located$distance_to_boundary_ft)), !anyNA(located$ward_pair))
boundary_scope <- ledger %>% select(project_id, construction_year, allow_far, allow_dupac, location_resolved) %>%
  left_join(located, by = c("project_id", "construction_year"), relationship = "one-to-one")
SaveData(boundary_scope, c("project_id"), "../output/preferred_residential_boundary_scope.csv")

SaveData(ledger, c("project_id"), "../output/preferred_residential_project_ledger.csv")
SaveData(arrange(components, project_id, component_pin), c("project_id", "component_pin"), "../output/preferred_residential_project_components_final.csv")
SaveData(centroids, c("project_id"), "../output/preferred_residential_project_centroids.gpkg", delete_dsn = TRUE, quiet = TRUE)
SaveData(polygons, c("project_id"), "../output/residential_adjudicated_project_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)
