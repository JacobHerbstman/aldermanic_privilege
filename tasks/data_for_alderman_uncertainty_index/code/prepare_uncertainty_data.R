# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/data_for_alderman_uncertainty_index/code")

source("../../setup_environment/code/packages.R")

assert_unique_key <- function(df, key_cols, label) {
  data <- if (inherits(df, "sf")) st_drop_geometry(df) else df
  missing_key_cols <- setdiff(key_cols, names(data))
  if (length(missing_key_cols) > 0) {
    stop(
      sprintf("%s is missing required key columns: %s", label, paste(missing_key_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  missing_keys <- data %>%
    filter(if_any(all_of(key_cols), is.na))
  if (nrow(missing_keys) > 0) {
    stop(sprintf("%s has %s rows with missing key values.", label, nrow(missing_keys)), call. = FALSE)
  }

  duplicate_keys <- data %>%
    count(across(all_of(key_cols)), name = "n") %>%
    filter(n > 1)
  if (nrow(duplicate_keys) > 0) {
    stop(sprintf("%s has duplicate key rows for %s.", label, paste(key_cols, collapse = ", ")), call. = FALSE)
  }
}

assert_expected_crs <- function(layer, expected_epsg, label) {
  layer_crs <- st_crs(layer)
  if (is.na(layer_crs)) {
    stop(sprintf("%s has missing CRS.", label), call. = FALSE)
  }
  if (!identical(as.integer(layer_crs$epsg), as.integer(expected_epsg))) {
    stop(
      sprintf("%s must use EPSG:%s; found EPSG:%s.", label, expected_epsg, layer_crs$epsg),
      call. = FALSE
    )
  }
}

assign_wards_for_era <- function(permits_era, ward_geoms, era_label) {
  if (nrow(permits_era) == 0) {
    return(permits_era %>% st_drop_geometry())
  }

  joined <- st_join(permits_era, ward_geoms, join = st_within, left = TRUE)
  joined_data <- st_drop_geometry(joined)

  ward_hits <- joined_data %>%
    group_by(id) %>%
    summarise(
      n_ward_hits = sum(!is.na(ward.y)),
      .groups = "drop"
    )

  if (any(ward_hits$n_ward_hits > 1L)) {
    stop(sprintf(
      "%s ward spatial join produced multiple ward matches for %d permits.",
      era_label,
      sum(ward_hits$n_ward_hits > 1L)
    ), call. = FALSE)
  }

  joined_data %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward))
}

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  )

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

ward_controls <- read_csv("../input/ward_controls.csv", show_col_types = FALSE)

community_areas <- st_read("../input/community_areas.geojson", quiet = TRUE) %>%
  select(area_numbe, community) %>%
  rename(ca_id = area_numbe, ca_name = community)

cta_stations <- st_read("../input/cta_stations.geojson", quiet = TRUE)
water_osm <- st_read("../input/gis_osm_water_a_free_1.shp", quiet = TRUE)

assert_unique_key(permits, "id", "Building permits")
assert_unique_key(alderman_panel, c("ward", "month"), "Alderman panel")
assert_unique_key(ward_controls, c("ward", "year"), "Ward controls")
assert_unique_key(ward_panel, c("ward", "year"), "Ward panel")

missing_permit_geometry_n <- sum(st_is_empty(st_geometry(permits)) | is.na(st_geometry(permits)))
missing_ward_geometry_n <- sum(st_is_empty(st_geometry(ward_panel)) | is.na(st_geometry(ward_panel)))
if (missing_permit_geometry_n > 0) {
  stop(sprintf("Building permits has %s rows with missing geometry.", missing_permit_geometry_n), call. = FALSE)
}
if (missing_ward_geometry_n > 0) {
  stop(sprintf("Ward panel has %s rows with missing geometry.", missing_ward_geometry_n), call. = FALSE)
}

assert_expected_crs(ward_panel, 3435, "Ward panel")

if (st_crs(cta_stations) != st_crs(ward_panel)) {
  cta_stations <- st_transform(cta_stations, st_crs(ward_panel))
}
if (st_crs(water_osm) != st_crs(ward_panel)) {
  water_osm <- st_transform(water_osm, st_crs(ward_panel))
}

if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}
assert_expected_crs(permits, 3435, "Building permits")

permits_high_discretion <- permits %>%
  filter(high_discretion == 1)

ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 1L)

ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 2L)

ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 3L)

if (any(c(
  n_distinct(ward_geoms_map1$ward),
  n_distinct(ward_geoms_map2$ward),
  n_distinct(ward_geoms_map3$ward)
) != 50)) {
  stop("Expected 50 wards in each uncertainty-index map-year geometry.", call. = FALSE)
}

permits_pre2015 <- permits_high_discretion %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))
permits_2015_2023 <- permits_high_discretion %>%
  filter(
    application_start_date_ym >= as.yearmon("2015-05") &
      application_start_date_ym < as.yearmon("2023-05")
  )
permits_post2023 <- permits_high_discretion %>%
  filter(application_start_date_ym >= as.yearmon("2023-05"))

ward_pre2015 <- assign_wards_for_era(permits_pre2015, ward_geoms_map1, "pre_2015")
ward_2015_2023 <- assign_wards_for_era(permits_2015_2023, ward_geoms_map2, "2015_2023")
ward_post2023 <- assign_wards_for_era(permits_post2023, ward_geoms_map3, "post_2023")

permits_ward_data <- bind_rows(
  ward_pre2015,
  ward_2015_2023,
  ward_post2023
)

assert_unique_key(permits_ward_data, "id", "Permits after ward spatial join")

permits_ward_data <- permits_ward_data %>%
  mutate(
    map_version = coalesce(
      as.integer(map_version),
      case_when(
        application_start_date_ym < as.yearmon("2015-05") ~ 1L,
        application_start_date_ym < as.yearmon("2023-05") ~ 2L,
        TRUE ~ 3L
      )
    )
  )

community_area_joined <- permits_high_discretion %>%
  select(id) %>%
  st_transform(st_crs(community_areas)) %>%
  st_join(community_areas, join = st_within, left = TRUE) %>%
  st_drop_geometry()

community_area_hits <- community_area_joined %>%
  group_by(id) %>%
  summarise(
    n_ca_hits = sum(!is.na(ca_id)),
    .groups = "drop"
  )

if (any(community_area_hits$n_ca_hits > 1L)) {
  stop(sprintf(
    "Community-area spatial join produced multiple matches for %d permits.",
    sum(community_area_hits$n_ca_hits > 1L)
  ), call. = FALSE)
}

permits_with_ca <- community_area_joined %>%
  filter(!is.na(ca_id)) %>%
  select(id, ca_id, ca_name)
assert_unique_key(permits_with_ca, "id", "Permit community-area assignment")

# Merge back to ward data
permits_ward_data <- permits_ward_data %>%
  left_join(permits_with_ca, by = "id", relationship = "many-to-one")

if (any(is.na(permits_ward_data$ca_id))) {
  stop("Permits after ward spatial join include rows without a community-area assignment.", call. = FALSE)
}

permits_with_alderman_all <- permits_ward_data %>%
  left_join(
    alderman_panel,
    by = c("ward", "application_start_date_ym" = "month"),
    relationship = "many-to-one"
  )

permits_with_alderman <- permits_with_alderman_all %>%
  filter(!is.na(alderman))
assert_unique_key(permits_with_alderman, "id", "Permits after alderman join")

max_permit_year <- max(permits_with_alderman$application_year, na.rm = TRUE)
max_control_year <- max(ward_controls$year, na.rm = TRUE)

if (max_permit_year > max_control_year) {
  filled_data <- ward_controls %>%
    filter(year == max_control_year) %>%
    select(-year) %>%
    tidyr::crossing(year = (max_control_year + 1):max_permit_year)

  ward_controls <- bind_rows(ward_controls, filled_data)
}
assert_unique_key(ward_controls, c("ward", "year"), "Ward controls after forward fill")

permits_with_controls <- permits_with_alderman %>%
  left_join(
    ward_controls,
    by = c("ward", "application_year" = "year"),
    relationship = "many-to-one"
  )
assert_unique_key(permits_with_controls, "id", "Permits after ward-controls join")

if (any(is.na(permits_with_controls$homeownership_rate))) {
  stop("Permits after alderman join include rows without ward controls.", call. = FALSE)
}

permit_points <- permits_high_discretion %>%
  select(id) %>%
  semi_join(permits_with_controls %>% select(id), by = "id")

metric_crs <- 26916
permit_points_m <- st_transform(permit_points, metric_crs)
cta_stations_m <- st_transform(cta_stations, metric_crs)
water_osm_m <- st_transform(water_osm, metric_crs)
assert_expected_crs(permit_points_m, metric_crs, "Permit points for place controls")
assert_expected_crs(cta_stations_m, metric_crs, "CTA stations for place controls")
assert_expected_crs(water_osm_m, metric_crs, "OSM water for place controls")

# CBD reference point (downtown Chicago)
cbd_m <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(metric_crs)

dist_cbd_km <- as.numeric(units::set_units(st_distance(permit_points_m, cbd_m), "m")) / 1000

n_rail_stations_800m <- lengths(st_is_within_distance(permit_points_m, cta_stations_m, dist = 800))

lake_michigan_features <- water_osm_m %>%
  filter(!is.na(name) & tolower(name) == "lake michigan") %>%
  st_make_valid()
if (nrow(lake_michigan_features) == 0) {
  stop("Could not construct Lake Michigan geometry for distance calculation.", call. = FALSE)
}
lake_michigan_m <- st_union(lake_michigan_features)

dist_lake_km <- as.numeric(units::set_units(st_distance(permit_points_m, lake_michigan_m), "m")) / 1000

permit_place_controls <- tibble(
  id = permit_points$id,
  dist_cbd_km = dist_cbd_km,
  dist_lake_km = dist_lake_km,
  n_rail_stations_800m = n_rail_stations_800m
)
assert_unique_key(permit_place_controls, "id", "Permit place controls")

permits_with_controls <- permits_with_controls %>%
  left_join(permit_place_controls, by = "id", relationship = "many-to-one")

missing_dist_cbd <- sum(is.na(permits_with_controls$dist_cbd_km))
missing_dist_lake <- sum(is.na(permits_with_controls$dist_lake_km))
missing_cta <- sum(is.na(permits_with_controls$n_rail_stations_800m))

if (missing_dist_cbd > 0 || missing_dist_lake > 0 || missing_cta > 0) {
  stop(
    paste0(
      "Missing parcel-level place controls after join. ",
      "dist_cbd_km missing=", missing_dist_cbd, ", ",
      "dist_lake_km missing=", missing_dist_lake, ", ",
      "n_rail_stations_800m missing=", missing_cta
    ),
    call. = FALSE
  )
}

bad_dist_cbd <- sum(!is.na(permits_with_controls$dist_cbd_km) & permits_with_controls$dist_cbd_km < 0)
bad_dist_lake <- sum(!is.na(permits_with_controls$dist_lake_km) & permits_with_controls$dist_lake_km < 0)
bad_cta <- sum(!is.na(permits_with_controls$n_rail_stations_800m) & permits_with_controls$n_rail_stations_800m < 0)

if (bad_dist_cbd > 0 || bad_dist_lake > 0 || bad_cta > 0) {
  stop(
    paste0(
      "Invalid parcel-level control values found. ",
      "dist_cbd_km<0: ", bad_dist_cbd, ", ",
      "dist_lake_km<0: ", bad_dist_lake, ", ",
      "n_rail_stations_800m<0: ", bad_cta
    ),
    call. = FALSE
  )
}

plausibly_far_cbd <- sum(permits_with_controls$dist_cbd_km > 80, na.rm = TRUE)
plausibly_far_lake <- sum(permits_with_controls$dist_lake_km > 80, na.rm = TRUE)

if (plausibly_far_cbd > 0 || plausibly_far_lake > 0) {
  stop(
    paste0(
      "Plausibility check failed: distances too large for Chicago permits. ",
      "dist_cbd_km>80: ", plausibly_far_cbd, ", ",
      "dist_lake_km>80: ", plausibly_far_lake
    ),
    call. = FALSE
  )
}

permits_analysis <- permits_with_controls %>%
  mutate(
    month = application_start_date_ym,
    year = application_year,
    log_processing_time = log(if_else(processing_time > 0, processing_time, NA_real_)),
    log_reported_cost   = log(if_else(reported_cost   > 0, reported_cost,   NA_real_)),
    permit_type_clean = case_when(
      grepl("NEW CONSTRUCTION", permit_type) ~ "new_construction",
      grepl("RENOVATION|ALTERATION", permit_type) ~ "renovation",
      grepl("WRECKING|DEMOLITION", permit_type) ~ "demolition",
      grepl("PORCH", permit_type) ~ "porch",
      grepl("REINSTATE", permit_type) ~ "reinstate",
      TRUE ~ "other"
    ),
    is_porch = grepl("PORCH", permit_type),
    review_type_clean = if_else(is.na(review_type), "unknown", review_type)
  ) %>%
  filter(
    !is.na(alderman),
    !is.na(log_processing_time),
    !is.na(ward),
    !is.na(month)
  )

output_data <- permits_analysis %>%
  select(
    id, pin, ward, alderman, month, year, ca_id, ca_name,
    processing_time, log_processing_time,
    permit_type, permit_type_clean, review_type, review_type_clean,
    reported_cost, log_reported_cost, is_porch,
    pop_total, median_hh_income, share_black, share_hisp, share_white,
    homeownership_rate, share_bach_plus,
    dist_cbd_km, dist_lake_km, n_rail_stations_800m,
    map_version
  )
assert_unique_key(output_data, "id", "Uncertainty-index permit output")

write_csv(output_data, "../output/permits_for_uncertainty_index.csv")
