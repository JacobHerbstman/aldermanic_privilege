source("../../setup_environment/code/packages.R")

# Interactive run:
# setwd("tasks/data_for_alderman_uncertainty_index/code")

message("=== Starting data preparation for uncertainty index ===")

drop_geometry_if_needed <- function(df) {
  if (inherits(df, "sf")) {
    return(st_drop_geometry(df))
  }
  df
}

missing_geometry_n <- function(df) {
  if (!inherits(df, "sf")) {
    return(NA_integer_)
  }
  geom <- st_geometry(df)
  sum(st_is_empty(geom) | is.na(geom))
}

assert_unique_key <- function(df, key_cols, label) {
  data <- drop_geometry_if_needed(df)
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
    print(head(duplicate_keys, 20))
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

clean_permit_type <- function(permit_type) {
  case_when(
    grepl("NEW CONSTRUCTION", permit_type) ~ "new_construction",
    grepl("RENOVATION|ALTERATION", permit_type) ~ "renovation",
    grepl("WRECKING|DEMOLITION", permit_type) ~ "demolition",
    grepl("PORCH", permit_type) ~ "porch",
    grepl("REINSTATE", permit_type) ~ "reinstate",
    TRUE ~ "other"
  )
}

assign_wards_for_era <- function(permits_era, ward_geoms, map_version_value, era_label) {
  if (nrow(permits_era) == 0) {
    return(
      list(
        data = permits_era %>% st_drop_geometry(),
        diagnostics = tibble(
          id = character(),
          application_month = character(),
          application_year = integer(),
          map_version = integer(),
          era = character(),
          n_ward_hits = integer(),
          assigned_ward = numeric(),
          reason = character()
        )
      )
    )
  }

  joined <- st_join(permits_era, ward_geoms, join = st_within, left = TRUE)
  joined_data <- st_drop_geometry(joined)

  diagnostics <- joined_data %>%
    mutate(ward_match = ward.y) %>%
    group_by(id) %>%
    summarise(
      application_month = as.character(first(application_start_date_ym)),
      application_year = first(application_year),
      map_version = map_version_value,
      era = era_label,
      n_ward_hits = sum(!is.na(ward_match)),
      assigned_ward = {
        ward_matches <- ward_match[!is.na(ward_match)]
        if (length(ward_matches) == 1L) ward_matches[[1]] else NA_real_
      },
      reason = case_when(
        n_ward_hits == 0L ~ "no_ward_match",
        n_ward_hits > 1L ~ "multiple_ward_matches",
        TRUE ~ "matched"
      ),
      .groups = "drop"
    )

  multiple_matches <- diagnostics %>% filter(n_ward_hits > 1)
  if (nrow(multiple_matches) > 0) {
    print(head(multiple_matches, 20))
    stop(sprintf("%s ward spatial join produced multiple ward matches.", era_label), call. = FALSE)
  }

  data <- joined_data %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward))

  list(data = data, diagnostics = diagnostics)
}

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

message("Loading input data...")

# Ward geometries (for spatial join)
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

# Building permits
permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  )

# Alderman-ward-month crosswalk
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

# Ward-level demographics
ward_controls <- read_csv("../input/ward_controls.csv", show_col_types = FALSE)

# Community areas (for CA fixed effects)
community_areas <- st_read("../input/community_areas.geojson", quiet = TRUE) %>%
  select(area_numbe, community) %>%
  rename(ca_id = area_numbe, ca_name = community)

message("Community areas loaded: ", nrow(community_areas), " areas")

# Place-control layers used in the active stringency pipeline
cta_stations <- st_read("../input/cta_stations.geojson", quiet = TRUE)
water_osm <- st_read("../input/gis_osm_water_a_free_1.shp", quiet = TRUE)

assert_unique_key(permits, "id", "Building permits")
assert_unique_key(alderman_panel, c("ward", "month"), "Alderman panel")
assert_unique_key(ward_controls, c("ward", "year"), "Ward controls")
assert_unique_key(ward_panel, c("ward", "year"), "Ward panel")

if (missing_geometry_n(permits) > 0) {
  stop(sprintf("Building permits has %s rows with missing geometry.", missing_geometry_n(permits)), call. = FALSE)
}
if (missing_geometry_n(ward_panel) > 0) {
  stop(sprintf("Ward panel has %s rows with missing geometry.", missing_geometry_n(ward_panel)), call. = FALSE)
}

assert_expected_crs(ward_panel, 3435, "Ward panel")

if (st_crs(cta_stations) != st_crs(ward_panel)) {
  cta_stations <- st_transform(cta_stations, st_crs(ward_panel))
}
if (st_crs(water_osm) != st_crs(ward_panel)) {
  water_osm <- st_transform(water_osm, st_crs(ward_panel))
}

message("Place-control layers loaded (CTA, water).")

# Ensure CRS alignment
if (st_crs(permits) != st_crs(ward_panel)) {
  message("Transforming permits CRS to match ward panel")
  permits <- st_transform(permits, st_crs(ward_panel))
}
assert_expected_crs(permits, 3435, "Building permits")

# -----------------------------------------------------------------------------
# 2. FILTER TO HIGH DISCRETION PERMITS
# -----------------------------------------------------------------------------

permits_high_discretion <- permits %>%
  filter(high_discretion == 1)

message("High discretion permits: ", nrow(permits_high_discretion))

# -----------------------------------------------------------------------------
# 3. SPATIAL JOIN PERMITS TO WARDS (BY MAP ERA)
# -----------------------------------------------------------------------------

message("Performing spatial join to wards by map era...")

# Map 1: Pre-May 2015 (Use 2014 shape)
ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 1L)

# Map 2: May 2015 to April 2023 (Use 2016 shape)
ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 2L)

# Map 3: May 2023 onwards (Use latest shape)
ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 3L)

ward_map_counts <- tibble(
  map_version = c(1L, 2L, 3L),
  map_year = c(2014L, 2016L, max(ward_panel$year)),
  n_wards = c(
    n_distinct(ward_geoms_map1$ward),
    n_distinct(ward_geoms_map2$ward),
    n_distinct(ward_geoms_map3$ward)
  )
)
if (any(ward_map_counts$n_wards != 50)) {
  print(ward_map_counts)
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

ward_pre2015 <- assign_wards_for_era(permits_pre2015, ward_geoms_map1, 1L, "pre_2015")
ward_2015_2023 <- assign_wards_for_era(permits_2015_2023, ward_geoms_map2, 2L, "2015_2023")
ward_post2023 <- assign_wards_for_era(permits_post2023, ward_geoms_map3, 3L, "post_2023")

permits_ward_data <- bind_rows(
  ward_pre2015$data,
  ward_2015_2023$data,
  ward_post2023$data
)
ward_spatial_join_diagnostics <- bind_rows(
  ward_pre2015$diagnostics,
  ward_2015_2023$diagnostics,
  ward_post2023$diagnostics
)

assert_unique_key(permits_ward_data, "id", "Permits after ward spatial join")
message("Permits after spatial join: ", nrow(permits_ward_data))

# Ensure map_version is always populated by month-era rule (legacy behavior)
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
message("Permits with map_version: ",
        sum(!is.na(permits_ward_data$map_version)),
        " (", round(mean(!is.na(permits_ward_data$map_version)) * 100, 1), "%)")

# -----------------------------------------------------------------------------
# 3b. SPATIAL JOIN PERMITS TO COMMUNITY AREAS
# -----------------------------------------------------------------------------

message("Assigning community areas...")

community_area_joined <- permits_high_discretion %>%
  select(id) %>%
  st_transform(st_crs(community_areas)) %>%
  st_join(community_areas, join = st_within, left = TRUE) %>%
  st_drop_geometry()

community_area_join_diagnostics <- community_area_joined %>%
  group_by(id) %>%
  summarise(
    n_ca_hits = sum(!is.na(ca_id)),
    assigned_ca_id = {
      ca_matches <- ca_id[!is.na(ca_id)]
      if (length(ca_matches) == 1L) as.character(ca_matches[[1]]) else NA_character_
    },
    reason = case_when(
      n_ca_hits == 0L ~ "no_community_area_match",
      n_ca_hits > 1L ~ "multiple_community_area_matches",
      TRUE ~ "matched"
    ),
    .groups = "drop"
  )

multiple_community_area_matches <- community_area_join_diagnostics %>%
  filter(n_ca_hits > 1)
if (nrow(multiple_community_area_matches) > 0) {
  print(head(multiple_community_area_matches, 20))
  stop("Community-area spatial join produced multiple matches for at least one permit.", call. = FALSE)
}

permits_with_ca <- community_area_joined %>%
  filter(!is.na(ca_id)) %>%
  select(id, ca_id, ca_name)
assert_unique_key(permits_with_ca, "id", "Permit community-area assignment")

# Merge back to ward data
permits_ward_data <- permits_ward_data %>%
  left_join(permits_with_ca, by = "id", relationship = "many-to-one")

message("Permits with community area: ", sum(!is.na(permits_ward_data$ca_id)),
        " (", round(mean(!is.na(permits_ward_data$ca_id)) * 100, 1), "%)")
if (any(is.na(permits_ward_data$ca_id))) {
  stop("Permits after ward spatial join include rows without a community-area assignment.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 4. MERGE ALDERMAN INFORMATION
# -----------------------------------------------------------------------------

message("Merging alderman information...")

permits_with_alderman_all <- permits_ward_data %>%
  left_join(
    alderman_panel,
    by = c("ward", "application_start_date_ym" = "month"),
    relationship = "many-to-one"
  )

permits_with_alderman <- permits_with_alderman_all %>%
  filter(!is.na(alderman))
assert_unique_key(permits_with_alderman, "id", "Permits after alderman join")

message("Permits with alderman: ", nrow(permits_with_alderman))
message("Unique aldermen: ", n_distinct(permits_with_alderman$alderman))

# -----------------------------------------------------------------------------
# 5. MERGE WARD CONTROLS (WITH FORWARD FILL)
# -----------------------------------------------------------------------------

message("Merging ward controls...")

# Extend controls to cover permit years
max_permit_year <- max(permits_with_alderman$application_year, na.rm = TRUE)
max_control_year <- max(ward_controls$year, na.rm = TRUE)

if (max_permit_year > max_control_year) {
  message("Extending ward controls from ", max_control_year, " to ", max_permit_year)
  
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

message("Permits with ward controls: ", sum(!is.na(permits_with_controls$homeownership_rate)))
if (any(is.na(permits_with_controls$homeownership_rate))) {
  stop("Permits after alderman join include rows without ward controls.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 6. BUILD + MERGE PLACE CONTROLS (PARCEL / PERMIT POINT LEVEL)
# -----------------------------------------------------------------------------

message("Building parcel-level place controls from permit-point geometries...")

# Bring back permit point geometry via id and keep only permits that made it into sample
permit_points <- permits_high_discretion %>%
  select(id) %>%
  semi_join(permits_with_controls %>% select(id), by = "id")

# Use a meter-based CRS for consistent distance/buffer calculations
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

# Distance to CBD (km)
dist_cbd_km <- as.numeric(units::set_units(st_distance(permit_points_m, cbd_m), "m")) / 1000

# Number of CTA stations within 800 meters of permit point
n_rail_stations_800m <- lengths(st_is_within_distance(permit_points_m, cta_stations_m, dist = 800))

# Distance to Lake Michigan shoreline / polygon (km)
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

# Strict QC: parcel-level controls should be complete and reasonable
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

# Broad plausibility checks for Chicago-area geometry
plausibly_far_cbd <- sum(permits_with_controls$dist_cbd_km > 80, na.rm = TRUE)
plausibly_far_lake <- sum(permits_with_controls$dist_lake_km > 80, na.rm = TRUE)

if (plausibly_far_cbd > 0 || plausibly_far_lake > 0) {
  offending_distance_rows <- permit_place_controls %>%
    filter(dist_cbd_km > 80 | dist_lake_km > 80) %>%
    left_join(
      permits_high_discretion %>%
        st_drop_geometry() %>%
        select(any_of(c("id", "pin", "application_start_date_ym", "ward"))),
      by = "id",
      relationship = "one-to-one"
    ) %>%
    arrange(desc(dist_cbd_km), desc(dist_lake_km))

  message("Offending permit distance rows:")
  print(offending_distance_rows)

  stop(
    paste0(
      "Plausibility check failed: distances too large for Chicago permits. ",
      "dist_cbd_km>80: ", plausibly_far_cbd, ", ",
      "dist_lake_km>80: ", plausibly_far_lake
    ),
    call. = FALSE
  )
}

message("Permits with dist_cbd_km: ",
        sum(!is.na(permits_with_controls$dist_cbd_km)),
        " (", round(mean(!is.na(permits_with_controls$dist_cbd_km)) * 100, 1), "%)")
message("Permits with dist_lake_km: ",
        sum(!is.na(permits_with_controls$dist_lake_km)),
        " (", round(mean(!is.na(permits_with_controls$dist_lake_km)) * 100, 1), "%)")
message("Permits with n_rail_stations_800m: ",
        sum(!is.na(permits_with_controls$n_rail_stations_800m)),
        " (", round(mean(!is.na(permits_with_controls$n_rail_stations_800m)) * 100, 1), "%)")

# -----------------------------------------------------------------------------
# 7. CREATE ANALYSIS VARIABLES
# -----------------------------------------------------------------------------

message("Creating analysis variables...")

permits_analysis <- permits_with_controls %>%
  mutate(
    # Time variables
    month = application_start_date_ym,
    year = application_year,
    
    # Log transforms (NA for zero/negative; downstream is.finite() filter handles removal)
    log_processing_time = log(if_else(processing_time > 0, processing_time, NA_real_)),
    log_reported_cost   = log(if_else(reported_cost   > 0, reported_cost,   NA_real_)),
    
    # Clean permit type for FE
    permit_type_clean = clean_permit_type(permit_type),
    
    # Flag for porch permits (for optional filtering)
    is_porch = grepl("PORCH", permit_type),
    
    # Clean review type for FE (handle potential NAs)
    review_type_clean = if_else(is.na(review_type), "unknown", review_type)
  ) %>%
  # Filter to valid observations
  filter(
    !is.na(alderman),
    !is.na(log_processing_time),
    !is.na(ward),
    !is.na(month)
  )

message("Final analysis dataset: ", nrow(permits_analysis), " permits")

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUT
# -----------------------------------------------------------------------------

message("Saving output...")

# Select columns for output
output_data <- permits_analysis %>%
  select(
    # Identifiers
    id, pin, ward, alderman, month, year, ca_id, ca_name,
    
    # Outcome
    processing_time, log_processing_time,
    
    # Permit characteristics
    permit_type, permit_type_clean, review_type, review_type_clean,
    reported_cost, log_reported_cost, is_porch,
    
    # Ward demographics
    pop_total, median_hh_income, share_black, share_hisp, share_white,
    homeownership_rate, share_bach_plus,
    
    # Place controls (permit-point level)
    dist_cbd_km, dist_lake_km, n_rail_stations_800m,
    
    # Metadata
    map_version
  )
assert_unique_key(output_data, "id", "Uncertainty-index permit output")

assert_unique_key(ward_spatial_join_diagnostics, "id", "Ward spatial-join diagnostics")
assert_unique_key(community_area_join_diagnostics, "id", "Community-area join diagnostics")

write_csv(output_data, "../output/permits_for_uncertainty_index.csv")

message("=== Data preparation complete ===")
message("Output: ../output/permits_for_uncertainty_index.csv")
message("Rows: ", nrow(output_data))
message("Columns: ", ncol(output_data))
