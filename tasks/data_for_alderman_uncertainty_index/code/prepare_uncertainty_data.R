## Prepare permit-level dataset for alderman uncertainty index
## This script merges permits with ward, alderman, demographic, and parcel proximity data

source("../../setup_environment/code/packages.R")

message("=== Starting data preparation for uncertainty index ===")

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

# Parcel proximity data (large file - read only needed columns)
message("Loading parcel proximity data (this may take a moment)...")
proximity_cols <- c(
  "pin10",
  # Transit
  "nearest_cta_stop_dist_ft", "nearest_metra_stop_dist_ft", "num_bus_stop_in_half_mile",
  # Amenities
  "lake_michigan_dist_ft", "nearest_park_dist_ft", "num_school_in_half_mile",
  # Disamenities
  "airport_dnl_total", "nearest_railroad_dist_ft", "num_foreclosure_in_half_mile_past_5_years",
  # Density
  "num_pin_in_half_mile"
)

parcel_proximity <- read_csv(
  "../input/parcel_proximity.csv",
  col_select = all_of(proximity_cols),
  show_col_types = FALSE
) %>%
  # Keep only latest year per PIN (deduplicate)
  group_by(pin10) %>%
  slice_tail(n = 1) %>%
  ungroup()

message("Parcel proximity: ", nrow(parcel_proximity), " unique PINs")

# Community areas (for CA fixed effects)
community_areas <- st_read("../input/community_areas.geojson", quiet = TRUE) %>%
  select(area_numbe, community) %>%
  rename(ca_id = area_numbe, ca_name = community)

message("Community areas loaded: ", nrow(community_areas), " areas")

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

# Function to assign ward based on 3 time intervals (May turnover)
assign_ward_by_month <- function(permits_data) {
  # Period 1: Before May 2015
  permits_p1 <- permits_data %>%
    filter(application_start_date_ym < as.yearmon("2015-05"))
  
  # Period 2: May 2015 to April 2023
  permits_p2 <- permits_data %>%
    filter(application_start_date_ym >= as.yearmon("2015-05") &
           application_start_date_ym < as.yearmon("2023-05"))
  
  # Period 3: May 2023 onwards
  permits_p3 <- permits_data %>%
    filter(application_start_date_ym >= as.yearmon("2023-05"))
  
  # Helper for joining
  do_join <- function(pts, polys) {
    if (nrow(pts) == 0) return(pts %>% st_drop_geometry())
    
    result <- st_join(pts, polys, join = st_within)
    
    # Handle potential duplicate ward columns
    if ("ward.x" %in% names(result) && "ward.y" %in% names(result)) {
      result <- result %>%
        mutate(ward = coalesce(ward.y, ward.x)) %>%
        select(-ward.x, -ward.y)
    }
    
    result %>%
      filter(!is.na(ward)) %>%
      st_drop_geometry()
  }
  
  bind_rows(
    do_join(permits_p1, ward_geoms_map1),
    do_join(permits_p2, ward_geoms_map2),
    do_join(permits_p3, ward_geoms_map3)
  )
}

permits_ward_data <- assign_ward_by_month(permits_high_discretion)
message("Permits after spatial join: ", nrow(permits_ward_data))

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
# 6. MERGE PARCEL PROXIMITY BY PIN
# -----------------------------------------------------------------------------

message("Merging parcel proximity by PIN...")

# Check PIN availability before merge
n_with_pin <- sum(!is.na(permits_with_controls$pin))
message("Permits with PIN before merge: ", n_with_pin, 
        " (", round(n_with_pin / nrow(permits_with_controls) * 100, 1), "%)")

# Left join to keep all permits (NAs for missing PINs)
permits_with_proximity <- permits_with_controls %>%
  left_join(parcel_proximity, by = c("pin" = "pin10"))

# Check merge success
n_with_proximity <- sum(!is.na(permits_with_proximity$nearest_cta_stop_dist_ft))
message("Permits with proximity data after merge: ", n_with_proximity,
        " (", round(n_with_proximity / nrow(permits_with_proximity) * 100, 1), "%)")

# -----------------------------------------------------------------------------
# 7. CREATE ANALYSIS VARIABLES
# -----------------------------------------------------------------------------

message("Creating analysis variables...")

permits_analysis <- permits_with_proximity %>%
  mutate(
    # Time variables
    month = application_start_date_ym,
    year = application_year,
    
    # Log transforms
    log_processing_time = log(processing_time),
    log_reported_cost = log(reported_cost),
    
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
    
    # Parcel proximity - Transit
    nearest_cta_stop_dist_ft, nearest_metra_stop_dist_ft, num_bus_stop_in_half_mile,
    
    # Parcel proximity - Amenities
    lake_michigan_dist_ft, nearest_park_dist_ft, num_school_in_half_mile,
    
    # Parcel proximity - Disamenities
    airport_dnl_total, nearest_railroad_dist_ft, num_foreclosure_in_half_mile_past_5_years,
    
    # Parcel proximity - Density
    num_pin_in_half_mile,
    
    # Metadata
    map_version
  )

write_csv(output_data, "../output/permits_for_uncertainty_index.csv")

message("=== Data preparation complete ===")
message("Output: ../output/permits_for_uncertainty_index.csv")
message("Rows: ", nrow(output_data))
message("Columns: ", ncol(output_data))
