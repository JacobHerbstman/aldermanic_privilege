# setwd("tasks/construction_boundary_distances/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")


# Build residential final geography

# Measurements are final. This script attaches locations without correcting them.
projects <- readr::read_csv("../input/residential_selected_buildings.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", class_values = "c",
    .default = readr::col_guess()))
# Density and physical measurements are complete before final geography starts.
measurements <- readr::read_csv("../input/new_construction_measurements.csv",
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
points <- sf::st_read("../input/preferred_project_year_centroids.gpkg", quiet = TRUE) %>%
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
polygons <- sf::st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE) %>%
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

# Build preferred commercial final geography

projects <- readr::read_csv("../input/preferred_commercial_projects.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(), .default = readr::col_guess()))
stopifnot(!anyDuplicated(projects$project_id))
# Density and physical measurements are complete before final geography starts.
measurements <- readr::read_csv("../input/new_construction_measurements.csv",
  col_types = readr::cols(.default = readr::col_guess())) %>% filter(source_family == "commercial")
stopifnot(!anyDuplicated(measurements$project_id), setequal(projects$project_id, measurements$project_id))
measurements <- measurements[match(projects$project_id, measurements$project_id), ]
for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft", "allow_far", "allow_dupac")) {
  stopifnot(isTRUE(all.equal(projects[[field]], measurements[[field]], check.attributes = FALSE)))
}

requests <- projects %>% select(project_id, target_year = construction_year, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>% rename(component_pin = component_pins)
stopifnot(!anyDuplicated(requests[c("project_id", "component_pin")]))
# Only the selected construction year and selected parcel set define the site.
queried <- readr::read_csv("../input/preferred_historical_parcel_source_queries.csv",
  col_types = readr::cols(target_year = "i", pin10 = "c"))
stopifnot(!anyDuplicated(queried[c("target_year", "pin10")]))
missing_queries <- requests %>%
  semi_join(projects %>% filter(allow_far | allow_dupac), by = "project_id") %>% mutate(pin10 = substr(component_pin, 1, 10)) %>%
  anti_join(queried, by = c("target_year", "pin10"))

parcels <- sf::st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
names(parcels)[names(parcels) == attr(parcels, "sf_column")] <- "geometry"
sf::st_geometry(parcels) <- "geometry"
parcels <- parcels %>% group_by(target_year, pin14) %>%
  summarise(location_evidence = paste(sort(unique(object_id)), collapse = "/"), .groups = "drop")
stopifnot(!anyDuplicated(sf::st_drop_geometry(parcels)[c("target_year", "pin14")]))
components <- parcels %>% inner_join(requests,
  by = c("target_year", "pin14" = "component_pin"), relationship = "one-to-many") %>%
  rename(component_pin = pin14)
# The next year's exact parcel can identify a unique construction-year parcel.
# Its polygon supplies location only; density continues to use reported land.
future <- parcels %>% mutate(target_year = target_year - 1L) %>%
  inner_join(requests, by = c("target_year", "pin14" = "component_pin"), relationship = "one-to-many") %>%
  rename(component_pin = pin14) %>%
  anti_join(sf::st_drop_geometry(components) %>% select(project_id, component_pin),
    by = c("project_id", "component_pin")) %>% sf::st_centroid()
predecessors <- sf::st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
for (i in seq_len(nrow(future))) {
  historical <- predecessors %>% filter(target_year == future$target_year[i])
  containing <- sf::st_within(future[i, ], historical)[[1]]
  if (length(containing) != 1L) next
  old <- historical[containing, ]
  components <- bind_rows(components, sf::st_sf(target_year = future$target_year[i],
    location_evidence = paste0("next_year_exact_parcel:", future$location_evidence[i],
      ";construction_year_parcel:", old$object_id), project_id = future$project_id[i],
    component_pin = future$component_pin[i], geometry = sf::st_geometry(old)))
}
component_coverage <- requests %>% left_join(sf::st_drop_geometry(components) %>%
  select(project_id, component_pin, location_evidence),
  by = c("project_id", "component_pin"), relationship = "one-to-one") %>%
  mutate(location_source = if_else(!is.na(location_evidence), if_else(str_detect(location_evidence, "^next_year_exact_parcel:"),
    "next_year_exact_parcel_identifies_construction_year_polygon", "exact_construction_year_parcel"), NA_character_))
complete <- component_coverage %>% group_by(project_id) %>%
  summarise(complete = all(!is.na(location_source)), .groups = "drop") %>% filter(complete)
centroids <- components %>% semi_join(complete, by = "project_id") %>%
  group_by(project_id) %>% summarise(
    project_geometry_source = "construction_year_component_union",
    project_geometry_evidence = paste(sort(unique(location_evidence)), collapse = "/"),
    project_polygon_area_sqft = as.numeric(sf::st_area(sf::st_union(geometry))), .groups = "drop") %>%
  sf::st_centroid()
# Reuse a reviewed candidate location only when both its year and parcel set agree.
candidates <- readr::read_csv("../input/preferred_commercial_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(), .default = readr::col_guess()))
candidate_points <- sf::st_read("../input/preferred_project_year_centroids.gpkg", quiet = TRUE) %>%
  filter(source_family == "commercial")
names(candidate_points)[names(candidate_points) == attr(candidate_points, "sf_column")] <- "geometry"
sf::st_geometry(candidate_points) <- "geometry"
unchanged <- projects %>% select(project_id, construction_year, component_pins) %>%
  inner_join(candidates %>% select(project_id, construction_year, component_pins),
    by = c("project_id", "construction_year", "component_pins"), relationship = "one-to-one")
reused <- candidate_points %>% inner_join(unchanged,
  by = c("project_id", "target_year" = "construction_year"), relationship = "one-to-one") %>%
  filter(!project_id %in% centroids$project_id) %>%
  transmute(project_id, project_geometry_source = location_source,
    project_geometry_evidence = paste0("candidate_year:", target_year, ";parcels:", component_pins),
    project_polygon_area_sqft = project_land_area_sqft)
centroids <- bind_rows(centroids, reused)

# A completed new-building permit already cited by the selected decision can
# locate that project when every cited building permit identifies the same point.
permit_links <- projects %>% filter(allow_far | allow_dupac) %>%
  anti_join(sf::st_drop_geometry(centroids) %>% select(project_id), by = "project_id") %>%
  transmute(project_id, permit = str_extract_all(coalesce(evidence_ids, ""), "(?<![0-9])1[0-9]{8}(?![0-9])")) %>%
  tidyr::unnest_longer(permit) %>% distinct(project_id, permit)
location_permits <- sf::st_read("../input/building_permits_for_verification.gpkg",
  query = "SELECT * FROM building_permits_clean WHERE permit_status = 'COMPLETE' AND permit_type = 'PERMIT - NEW CONSTRUCTION'",
  quiet = TRUE) %>% sf::st_transform(3435) %>%
  filter(str_detect(work_description, regex("RESIDENTIAL (BUILDING|TOWER)|APARTMENT (BUILDING|TOWER)|FULL BUILDING|FULL PERMIT|CONSTRUCT NEW", ignore_case = TRUE)))
stopifnot(!anyDuplicated(location_permits$permit))
permit_points <- location_permits %>% inner_join(permit_links, by = "permit", relationship = "one-to-many")
for (id in unique(permit_points$project_id)) {
  points <- permit_points %>% filter(project_id == .env$id)
  xy <- sf::st_coordinates(points)
  if (nrow(unique(as.data.frame(xy))) != 1L || any(!is.finite(xy))) next
  centroids <- bind_rows(centroids, sf::st_sf(project_id = id,
    project_geometry_source = "completed_permits_in_recorded_building_decision",
    project_geometry_evidence = paste(sort(unique(points$permit)), collapse = "/"),
    project_polygon_area_sqft = NA_real_, geometry = sf::st_geometry(points[1, ])))
}
reviews <- readr::read_csv("../input/commercial_reviewed_locations.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(reviews$project_id), all(reviews$project_id %in% projects$project_id))
permits <- sf::st_read("../input/building_permits_for_verification.gpkg",
  query = paste0("SELECT * FROM building_permits_clean WHERE permit IN ('",
    paste(reviews$source_id[reviews$source == "completed_permit"], collapse = "','"), "')"), quiet = TRUE) %>% sf::st_transform(3435)
current <- readr::read_csv("../input/parcel_universe_2025_city.csv",
  col_types = readr::cols(pin = readr::col_character(), longitude = readr::col_double(),
    latitude = readr::col_double(), .default = readr::col_skip()))
for (j in seq_len(nrow(reviews))) {
  r <- reviews[j, ]
  stopifnot(projects$construction_year[match(r$project_id, projects$project_id)] == as.integer(r$construction_year))
  if (r$source == "completed_permit") {
    point <- permits %>% filter(permit == r$source_id)
    stopifnot(nrow(point) == 1L, point$permit_status == "COMPLETE",
      point$permit_type == "PERMIT - NEW CONSTRUCTION",
      point$street_number == as.numeric(r$street_number), point$street_direction == r$street_direction,
      point$street_name == r$street_name)
  } else {
    stopifnot(r$source == "reviewed_current_exact_pin",
      r$source_id %in% strsplit(projects$component_pins[match(r$project_id, projects$project_id)], "/", fixed = TRUE)[[1]])
    point <- current %>% filter(pin == r$source_id)
    stopifnot(nrow(point) == 1L, is.finite(point$longitude), is.finite(point$latitude))
    point <- sf::st_transform(sf::st_as_sf(point, coords = c("longitude", "latitude"), crs = 4326), 3435)
  }
  centroids <- centroids %>% filter(project_id != r$project_id)
  centroids <- bind_rows(centroids, sf::st_sf(project_id = r$project_id,
    project_geometry_source = r$source, project_geometry_evidence = r$source_id,
    project_polygon_area_sqft = NA_real_, geometry = sf::st_geometry(point)))
}
stopifnot(!anyDuplicated(centroids$project_id), !any(sf::st_is_empty(centroids)), all(sf::st_is_valid(centroids)))
centroids <- centroids %>% arrange(project_id)
if (any(missing_queries$project_id %in% setdiff(projects$project_id, centroids$project_id))) {
  stop("Unlocated commercial projects have unqueried construction-year parcels: ",
    paste(unique(missing_queries$project_id[missing_queries$project_id %in%
      setdiff(projects$project_id, centroids$project_id)]), collapse = ", "))
}

points <- centroids %>% inner_join(projects %>% select(project_id, construction_year),
  by = "project_id", relationship = "one-to-one") %>%
  mutate(construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date), era = canonical_era_from_boundary_year(boundary_year))
ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>% sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
for (era_value in unique(points$era)) stopifnot(all(lengths(sf::st_within(points[points$era == era_value, ], ward_maps[[era_value]])) == 1L))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)
located <- bind_cols(sf::st_drop_geometry(points), assignment) %>%
  transmute(project_id, construction_year, construction_date, boundary_year, era,
    ward, neighbor_ward, ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= 1500, within_500ft = dist_ft <= 500)
stopifnot(all(is.finite(located$distance_to_boundary_ft)), !anyNA(located$ward_pair))
boundary_scope <- projects %>% select(project_id, construction_year, allow_far, allow_dupac) %>%
  left_join(located, by = c("project_id", "construction_year"), relationship = "one-to-one") %>%
  mutate(location_resolved = !is.na(distance_to_boundary_ft),
    allow_far = allow_far & location_resolved, allow_dupac = allow_dupac & location_resolved)
xy <- sf::st_coordinates(centroids)
commercial_ledger <- projects %>% left_join(sf::st_drop_geometry(centroids) %>%
  mutate(x_3435 = xy[,1], y_3435 = xy[,2]), by = "project_id", relationship = "one-to-one") %>%
  mutate(location_resolved = !is.na(x_3435), allow_far = allow_far & location_resolved,
    allow_dupac = allow_dupac & location_resolved)
SaveData(commercial_ledger, c("project_id"), "../output/preferred_commercial_project_ledger.csv")
SaveData(component_coverage, c("project_id", "component_pin"), "../output/preferred_commercial_project_component_locations.csv")
SaveData(boundary_scope, c("project_id"), "../output/preferred_commercial_boundary_scope.csv")
SaveData(centroids, c("project_id"), "../output/preferred_commercial_project_centroids.gpkg", delete_dsn = TRUE, quiet = TRUE)
