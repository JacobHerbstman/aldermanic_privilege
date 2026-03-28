## Prepare permit-level dataset for alderman uncertainty index
## This script merges permits with ward, alderman, demographics, and
## parcel-level place controls from permit-point geometry.

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/data_for_alderman_uncertainty_index/code")

message("=== Starting data preparation for uncertainty index ===")

ward_panel_path <- Sys.getenv("WARD_PANEL_PATH", "../input/ward_panel.gpkg")
building_permits_path <- Sys.getenv("BUILDING_PERMITS_INPUT_PATH", "../input/building_permits_clean.gpkg")
alderman_panel_path <- Sys.getenv("ALDERMAN_PANEL_PATH", "../input/chicago_alderman_panel.csv")
ward_controls_path <- Sys.getenv("WARD_CONTROLS_PATH", "../input/ward_controls.csv")
community_areas_path <- Sys.getenv("COMMUNITY_AREAS_PATH", "../input/community_areas.geojson")
cta_stations_path <- Sys.getenv("CTA_STATIONS_PATH", "../input/cta_stations.geojson")
city_boundary_path <- Sys.getenv("CITY_BOUNDARY_PATH", "../input/city_boundary.geojson")
water_osm_path <- Sys.getenv("WATER_OSM_PATH", "../input/gis_osm_water_a_free_1.shp")
permits_output_path <- Sys.getenv("OUTPUT_PERMITS_FOR_UNCERTAINTY_PATH", "../output/permits_for_uncertainty_index.csv")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

message("Loading input data...")

# Ward geometries (for spatial join)
ward_panel <- st_read(ward_panel_path, quiet = TRUE)

# Building permits
permits <- st_read(building_permits_path, quiet = TRUE) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  )

# Alderman-ward-month crosswalk
alderman_panel <- read_csv(alderman_panel_path, show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

# Ward-level demographics
ward_controls <- read_csv(ward_controls_path, show_col_types = FALSE)

# Community areas (for CA fixed effects)
community_areas <- st_read(community_areas_path, quiet = TRUE) %>%
  select(area_numbe, community) %>%
  rename(ca_id = area_numbe, ca_name = community)

message("Community areas loaded: ", nrow(community_areas), " areas")

# Place-control layers used in the active stringency pipeline
cta_stations <- st_read(cta_stations_path, quiet = TRUE)
city_boundary <- st_read(city_boundary_path, quiet = TRUE)
water_osm <- st_read(water_osm_path, quiet = TRUE)

if (st_crs(cta_stations) != st_crs(ward_panel)) {
  cta_stations <- st_transform(cta_stations, st_crs(ward_panel))
}
if (st_crs(city_boundary) != st_crs(ward_panel)) {
  city_boundary <- st_transform(city_boundary, st_crs(ward_panel))
}
if (st_crs(water_osm) != st_crs(ward_panel)) {
  water_osm <- st_transform(water_osm, st_crs(ward_panel))
}

message("Place-control layers loaded (CTA, city boundary, water).")

# Ensure CRS alignment
if (st_crs(permits) != st_crs(ward_panel)) {
  message("Transforming permits CRS to match ward panel")
  permits <- st_transform(permits, st_crs(ward_panel))
}

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

permits_pre2015 <- permits_high_discretion %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))
permits_2015_2023 <- permits_high_discretion %>%
  filter(
    application_start_date_ym >= as.yearmon("2015-05") &
      application_start_date_ym < as.yearmon("2023-05")
  )
permits_post2023 <- permits_high_discretion %>%
  filter(application_start_date_ym >= as.yearmon("2023-05"))

permits_ward_pre2015 <- if (nrow(permits_pre2015) == 0) {
  permits_pre2015 %>% st_drop_geometry()
} else {
  st_join(permits_pre2015, ward_geoms_map1, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}

permits_ward_2015_2023 <- if (nrow(permits_2015_2023) == 0) {
  permits_2015_2023 %>% st_drop_geometry()
} else {
  st_join(permits_2015_2023, ward_geoms_map2, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}

permits_ward_post2023 <- if (nrow(permits_post2023) == 0) {
  permits_post2023 %>% st_drop_geometry()
} else {
  st_join(permits_post2023, ward_geoms_map3, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}

permits_ward_data <- bind_rows(
  permits_ward_pre2015,
  permits_ward_2015_2023,
  permits_ward_post2023
)
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

# Convert permits back to sf for spatial join (they were converted to data frame in do_join)
# Need to re-join with original geometry
permits_with_ca <- permits_high_discretion %>%
  select(id) %>%
  # Ensure CRS matches
  st_transform(st_crs(community_areas)) %>%
  st_join(community_areas, join = st_within) %>%
  st_drop_geometry() %>%
  select(id, ca_id, ca_name)

# Merge back to ward data
permits_ward_data <- permits_ward_data %>%
  left_join(permits_with_ca, by = "id")

message("Permits with community area: ", sum(!is.na(permits_ward_data$ca_id)),
        " (", round(mean(!is.na(permits_ward_data$ca_id)) * 100, 1), "%)")

# -----------------------------------------------------------------------------
# 4. MERGE ALDERMAN INFORMATION
# -----------------------------------------------------------------------------

message("Merging alderman information...")

permits_with_alderman <- permits_ward_data %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  filter(!is.na(alderman))

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

permits_with_controls <- permits_with_alderman %>%
  left_join(ward_controls, by = c("ward", "application_year" = "year"))

message("Permits with ward controls: ", sum(!is.na(permits_with_controls$homeownership_rate)))

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

# CBD reference point (downtown Chicago)
cbd_m <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(metric_crs)

# Distance to CBD (km)
dist_cbd_km <- as.numeric(units::set_units(st_distance(permit_points_m, cbd_m), "m")) / 1000

# Number of CTA stations within 800 meters of permit point
n_rail_stations_800m <- lengths(st_is_within_distance(permit_points_m, cta_stations_m, dist = 800))

# Distance to Lake Michigan shoreline / polygon (km)
lake_michigan_m <- water_osm_m %>%
  filter(!is.na(name) & tolower(name) == "lake michigan") %>%
  st_make_valid() %>%
  st_union()

if (length(lake_michigan_m) == 0) {
  stop("Could not construct Lake Michigan geometry for distance calculation.", call. = FALSE)
}

dist_lake_km <- as.numeric(units::set_units(st_distance(permit_points_m, lake_michigan_m), "m")) / 1000

permit_place_controls <- tibble(
  id = permit_points$id,
  dist_cbd_km = dist_cbd_km,
  dist_lake_km = dist_lake_km,
  n_rail_stations_800m = n_rail_stations_800m
)

permits_with_controls <- permits_with_controls %>%
  left_join(permit_place_controls, by = "id")

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
      by = "id"
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
    log_processing_time = if_else(processing_time > 0, log(processing_time), NA_real_),
    log_reported_cost   = if_else(reported_cost   > 0, log(reported_cost),   NA_real_),
    
    # Clean permit type for FE
    permit_type_clean = case_when(
      grepl("NEW CONSTRUCTION", permit_type) ~ "new_construction",
      grepl("RENOVATION|ALTERATION", permit_type) ~ "renovation",
      grepl("WRECKING|DEMOLITION", permit_type) ~ "demolition",
      grepl("PORCH", permit_type) ~ "porch",
      grepl("REINSTATE", permit_type) ~ "reinstate",
      TRUE ~ "other"
    ),
    
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

write_csv(output_data, permits_output_path)

message("=== Data preparation complete ===")
message("Output: ", permits_output_path)
message("Rows: ", nrow(output_data))
message("Columns: ", ncol(output_data))
