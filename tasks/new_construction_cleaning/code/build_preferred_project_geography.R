# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/assessor_classification.R")

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

if (anyDuplicated(sf::st_drop_geometry(direct_parcels)[c("target_year", "object_id")]) > 0) {
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

if (anyDuplicated(sf::st_drop_geometry(direct_parcels_by_pin)[c("target_year", "pin14")]) > 0) {
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
stopifnot(nrow(anti_join(sf::st_drop_geometry(component_geometry), coverage,
  by = c("request_id", "source_family", "project_id", "target_year"))) == 0L)

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

if (any(!project_geometry$project_polygon_valid) || any(sf::st_is_empty(project_geometry)) ||
    any(!is.finite(project_geometry$project_land_area_sqft) | project_geometry$project_land_area_sqft <= 0)) {
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

# A former development parcel can contain many separately recorded homes.
# Locate each individual home from its own contemporaneous parcel point when
# available, while retaining the historical polygon as coverage evidence.
history_points <- bind_rows(
  readr::read_csv("../input/predecessor_parcel_history.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), lon = readr::col_double(),
    lat = readr::col_double(), row_id = readr::col_character(), .default = readr::col_skip())),
  readr::read_csv("../input/geocoding_parcel_history.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), lon = readr::col_double(),
    lat = readr::col_double(), row_id = readr::col_character(), .default = readr::col_skip())),
  readr::read_csv("../input/density_historical_parcel_records.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), longitude = readr::col_double(),
    latitude = readr::col_double(), row_id = readr::col_character(), .default = readr::col_skip())) %>%
    rename(lon = longitude, lat = latitude)
) %>% filter(is.finite(lon), is.finite(lat)) %>% distinct()
# A verified parcel-number replacement can carry the same home's older point.
# Keep the source PIN so this is never presented as an exact match to the new PIN.
history_points$point_source_pin <- history_points$pin
replacements <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(.default = readr::col_character())) %>%
  filter(replacement_check == "same_home_consecutive_parcel_numbers",
    candidate_status == "exclude_source_duplicate_keep_successors") %>%
  transmute(old_pin = component_pins, new_pin = str_remove(replacement_project_ids, "^residential_"))
stopifnot(!anyDuplicated(replacements$old_pin), !anyDuplicated(replacements$new_pin))
previous_points <- history_points %>% inner_join(replacements,
  by = c("pin" = "old_pin"), relationship = "many-to-one") %>%
  mutate(pin = new_pin) %>% select(-new_pin)
history_points <- bind_rows(history_points, previous_points)
individual_parents <- project_year_coverage %>%
  filter(complete_project_geometry, project_kind == "single_pin_single_card",
    requested_components == 1L, component_pins != parcel_pins)
project_centroids$location_source <- "historical_parcel_centroid"
project_centroids$location_reference_year <- project_centroids$target_year
project_centroids$location_reference_row_ids <- NA_character_
project_centroids$location_reference_pin <- NA_character_
for (j in seq_len(nrow(individual_parents))) {
  site <- individual_parents[j, ]
  i <- which(project_centroids$project_id == site$project_id & project_centroids$target_year == site$target_year)
  stopifnot(length(i) == 1L)
  project_centroids$location_source[i] <- "former_parcel_centroid_unresolved_individual"
  points <- history_points %>% filter(pin == site$component_pins,
    year %in% c(site$target_year, site$target_year + 1L))
  if (!nrow(points)) next
  points <- points %>% filter(year == min(year))
  if (any(points$point_source_pin == site$component_pins))
    points <- points %>% filter(point_source_pin == site$component_pins)
  if (nrow(distinct(points, lon, lat)) != 1L) next
  point <- sf::st_transform(sf::st_as_sf(points[1, ], coords = c("lon", "lat"), crs = 4326), 3435)
  if (length(sf::st_within(point, project_geometry[i, ])[[1]]) != 1L) next
  sf::st_geometry(project_centroids)[i] <- sf::st_geometry(point)
  project_centroids$location_source[i] <- if (points$year[1] == site$target_year)
    "exact_pin_construction_year_point" else "exact_pin_next_year_point"
  if (points$point_source_pin[1] != site$component_pins)
    project_centroids$location_source[i] <- "verified_previous_pin_historical_point"
  project_centroids$location_reference_pin[i] <- points$point_source_pin[1]
  project_centroids$location_reference_year[i] <- points$year[1]
  project_centroids$location_reference_row_ids[i] <- paste(sort(unique(points$row_id)), collapse = "/")
}

# Reviewed home identities permit an exact parcel point when the old map covers
# several homes, or when overlapping old polygons prevent a unique map match.
reviewed_locations <- readr::read_csv("../adjudication/residential_reviewed_parcel_locations.csv",
  col_types = readr::cols(project_id = readr::col_character(), pin = readr::col_character(),
    reference_year = readr::col_integer(), target_year = readr::col_integer(), .default = readr::col_character()))
current_points <- readr::read_csv("../input/parcel_universe_2025_city.csv",
  col_types = readr::cols(pin = readr::col_character(), tax_year = readr::col_integer(),
    longitude = readr::col_double(), latitude = readr::col_double(),
    row_id = readr::col_character(), .default = readr::col_skip())) %>%
  transmute(pin, year = tax_year, lon = longitude, lat = latitude, row_id,
    point_source_pin = pin, source = "parcel_universe_2025")
# A later point can locate the same building and lot. Require unchanged reported
# measurements and construction year; retain the construction-year ward map.
unchanged <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", construction_year = "d",
    dwelling_units = "d", building_sqft = "d", land_sqft = "d", .default = readr::col_skip())) %>%
  semi_join(project_centroids %>% sf::st_drop_geometry() %>%
    filter(source_family == "residential",
      location_source == "former_parcel_centroid_unresolved_individual"), by = "project_id") %>%
  rename(pin = component_pins)
stopifnot(!anyDuplicated(unchanged$pin), !anyDuplicated(current_points$pin))
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "location_pins", unchanged %>% select(pin))
assessment <- DBI::dbGetQuery(con, "SELECT h.pin, h.tax_year, h.card_num, h.class, h.year_built,
  h.building_sqft, h.land_sqft, h.num_apartments
  FROM read_parquet('../input/residential_assessor_history.parquet') h
  INNER JOIN location_pins p ON h.pin = p.pin
  WHERE h.building_sqft > 1")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(assessment[c("pin", "tax_year", "card_num")]))
assessment <- assessment %>% add_count(pin, tax_year) %>% filter(n == 1L) %>%
  transmute(pin, year = tax_year, construction_year = year_built, building_sqft, land_sqft,
    dwelling_units = if_else(class %in% single_family_assessor_classes, 1, num_apartments))
year_decisions <- readr::read_csv("../adjudication/residential_reviewed_construction_years.csv",
  col_types = readr::cols(project_id = "c", reported_year = "d", construction_year = "d",
    .default = readr::col_skip()))
stopifnot(!anyDuplicated(year_decisions$project_id))
unchanged <- unchanged %>% left_join(year_decisions,
  by = c("project_id", "construction_year"), relationship = "one-to-one") %>%
  mutate(match_year = coalesce(reported_year, construction_year)) %>%
  inner_join(assessment,
  by = c("pin", "match_year" = "construction_year", "building_sqft", "land_sqft", "dwelling_units"),
  relationship = "one-to-many", na_matches = "never")
location_evidence <- bind_rows(history_points, current_points %>% select(-source)) %>%
  filter(pin == point_source_pin, is.finite(lon), is.finite(lat)) %>%
  group_by(pin, year) %>%
  filter(n_distinct(paste(lon, lat)) == 1L) %>%
  summarise(lon = first(lon), lat = first(lat),
    row_id = paste(sort(unique(row_id)), collapse = "/"), .groups = "drop")
unchanged <- unchanged %>%
  inner_join(location_evidence, by = c("pin", "year"), relationship = "many-to-one") %>%
  filter(year >= construction_year) %>% group_by(project_id) %>%
  slice_max(year, n = 1L, with_ties = FALSE) %>% ungroup()
points <- sf::st_transform(sf::st_as_sf(unchanged, coords = c("lon", "lat"), crs = 4326), 3435)
polygon <- match(points$project_id, project_geometry$project_id)
stopifnot(!anyNA(polygon), !anyDuplicated(points$project_id))
# One thousandth of a foot accommodates coordinate rounding at the lot edge.
inside <- as.numeric(sf::st_distance(points, project_geometry[polygon, ], by_element = TRUE)) <= 0.001
points <- points[inside, ]
i <- match(points$project_id, project_centroids$project_id)
sf::st_geometry(project_centroids)[i] <- sf::st_geometry(points)
project_centroids$location_source[i] <- "same_property_later_exact_parcel_point"
project_centroids$location_reference_pin[i] <- points$pin
project_centroids$location_reference_year[i] <- points$year
project_centroids$location_reference_row_ids[i] <- points$row_id

# Keep the exact Chicago address point already verified upstream, rather than
# replacing it with the center of the larger parcel found around that address.
address_points <- readr::read_csv("../output/preferred_chicago_address_geocodes.csv",
  col_types = readr::cols(project_id = "c", request_id = "c", component_pin = "c",
    chicago_status = "c", chicago_x_3435 = "d", chicago_y_3435 = "d",
    selected_address_year = "d", .default = readr::col_skip())) %>%
  filter(chicago_status == "accepted_reference_point") %>%
  semi_join(project_centroids %>% sf::st_drop_geometry() %>%
    filter(source_family == "residential",
      location_source == "former_parcel_centroid_unresolved_individual"), by = "project_id") %>%
  sf::st_as_sf(coords = c("chicago_x_3435", "chicago_y_3435"), crs = 3435)
stopifnot(!anyDuplicated(address_points$project_id))
polygon <- match(address_points$project_id, project_geometry$project_id)
stopifnot(!anyNA(polygon))
inside <- as.numeric(sf::st_distance(address_points, project_geometry[polygon, ], by_element = TRUE)) <= 0.001
address_points <- address_points[inside, ]
i <- match(address_points$project_id, project_centroids$project_id)
sf::st_geometry(project_centroids)[i] <- sf::st_geometry(address_points)
project_centroids$location_source[i] <- "verified_chicago_individual_address_point"
project_centroids$location_reference_pin[i] <- address_points$component_pin
project_centroids$location_reference_year[i] <- address_points$selected_address_year
project_centroids$location_reference_row_ids[i] <- address_points$request_id

location_evidence <- bind_rows(history_points %>% mutate(source = "historical_parcel_history"), current_points)
stopifnot(!anyDuplicated(reviewed_locations$project_id),
  all(reviewed_locations$project_id %in% project_year_coverage$project_id))
for (j in seq_len(nrow(reviewed_locations))) {
  review <- reviewed_locations[j, ]
  stopifnot(any(coverage$project_id == review$project_id & coverage$component_pin == review$pin &
    coverage$target_year == review$target_year))
  points <- location_evidence %>% filter(pin == review$pin, point_source_pin == review$pin,
    year == review$reference_year, source == review$source)
  stopifnot(nrow(points) > 0L, nrow(distinct(points, lon, lat)) == 1L,
    all(is.finite(points$lon)), all(is.finite(points$lat)))
  point <- sf::st_transform(sf::st_as_sf(points[1, ], coords = c("lon", "lat"), crs = 4326), 3435)
  old <- which(project_centroids$project_id == review$project_id)
  polygon <- which(project_geometry$project_id == review$project_id)
  if (length(polygon)) stopifnot(length(sf::st_within(point, project_geometry[polygon, ])[[1]]) == 1L)
  row <- sf::st_sf(source_family = "residential", project_id = review$project_id,
    target_year = review$target_year, project_polygon_valid = NA,
    project_land_area_sqft = NA_real_, location_source = "reviewed_exact_parcel_point",
    location_reference_year = review$reference_year,
    location_reference_row_ids = paste(sort(unique(points$row_id)), collapse = "/"),
    location_reference_pin = review$pin, geometry = sf::st_geometry(point))
  if (length(old)) {
    stopifnot(length(old) == 1L, project_centroids$target_year[old] == review$target_year)
    row$project_polygon_valid <- project_centroids$project_polygon_valid[old]
    row$project_land_area_sqft <- project_centroids$project_land_area_sqft[old]
    project_centroids <- project_centroids[-old, ]
  }
  project_centroids <- bind_rows(project_centroids, row)
}

# Some individually measured homes share a development-wide tax polygon.
# Use a reviewed completed-permit point without claiming an individual parcel map.
permit_locations <- readr::read_csv("../adjudication/residential_reviewed_permit_locations.csv",
  col_types = readr::cols(project_id = readr::col_character(), permit = readr::col_character(),
    street_number = readr::col_double(), target_year = readr::col_integer(), .default = readr::col_character()))
stopifnot(!anyDuplicated(permit_locations$project_id), !anyDuplicated(permit_locations$permit),
  all(str_detect(permit_locations$permit, "^[0-9]+$")),
  !any(permit_locations$project_id %in% project_centroids$project_id))
permit_points <- sf::st_read("../output/building_permits_for_verification.gpkg",
  query = paste0("SELECT * FROM building_permits_clean WHERE permit IN ('",
    paste(permit_locations$permit, collapse = "','"), "')"), quiet = TRUE) %>%
  sf::st_transform(3435)
stopifnot(nrow(permit_points) == nrow(permit_locations), !anyDuplicated(permit_points$permit),
  all(permit_points$permit_status == "COMPLETE"),
  all(permit_points$permit_type == "PERMIT - NEW CONSTRUCTION"),
  !any(sf::st_is_empty(permit_points)))
i <- match(permit_locations$permit, permit_points$permit)
stopifnot(all(permit_points$street_number[i] == permit_locations$street_number),
  all(permit_points$street_direction[i] == permit_locations$street_direction),
  all(permit_points$street_name[i] == permit_locations$street_name))
permit_locations <- permit_locations %>% inner_join(
  project_year_coverage %>% select(project_id, target_year, source_family, complete_project_geometry),
  by = c("project_id", "target_year"), relationship = "one-to-one")
stopifnot(nrow(permit_locations) == nrow(permit_points), !any(permit_locations$complete_project_geometry))
i <- match(permit_locations$permit, permit_points$permit)
reviewed_points <- sf::st_sf(permit_locations %>% transmute(source_family, project_id, target_year,
  project_polygon_valid = NA, project_land_area_sqft = NA_real_,
  location_source = "reviewed_completed_permit_point",
  location_reference_year = as.integer(format(as.Date(permit_points$issue_date[i]), "%Y")),
  location_reference_row_ids = permit, location_reference_pin = NA_character_),
  geometry = sf::st_geometry(permit_points)[i])
project_centroids <- bind_rows(project_centroids, reviewed_points) %>% arrange(target_year, source_family, project_id)
stopifnot(!anyDuplicated(project_centroids$project_id))

sf::st_write(
  project_geometry,
  "../output/preferred_project_year_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  component_geometry,
  "../output/preferred_project_component_geometry.gpkg",
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
