# this code takes the parcels dataset from cook county assessor and merges in latitude and longitude for all pins i can find (residential and condo datasets)
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/geocode_ccao_data/code")
# approved_commercial_density_blocker_corrections_path <- "../input/approved_commercial_density_blocker_corrections.csv"
# geocoded_residential_data_path <- "../output/geocoded_residential_data.gpkg"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) > 0) {
  if (length(cli_args) != 2) {
    stop("Expected arguments: approved_commercial_density_blocker_corrections_path geocoded_residential_data_path.", call. = FALSE)
  }
  approved_commercial_density_blocker_corrections_path <- cli_args[1]
  geocoded_residential_data_path <- cli_args[2]
} else if (!interactive()) {
  stop("Expected arguments: approved_commercial_density_blocker_corrections_path geocoded_residential_data_path.", call. = FALSE)
}

chicago_lat_min <- 41
chicago_lat_max <- 43
chicago_lon_min <- -89
chicago_lon_max <- -87

# ---- Load ----
residential      <- read_csv("../input/residential_cross_section.csv",   show_col_types = FALSE)
parcels          <- read_csv("../input/parcels.csv",  show_col_types = FALSE)
parcel_proximity <- read_csv("../input/parcel_proximity.csv", show_col_types = FALSE)
multifamily      <- read_csv("../input/multifamily_data_cleaned.csv", show_col_types = FALSE)

manual_commercial_coords <- read_csv(
  approved_commercial_density_blocker_corrections_path,
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) %>%
  janitor::clean_names() %>%
  filter(
    source == "commercial_value_raw",
    status == "approved",
    !is.na(corrected_latitude),
    !is.na(corrected_longitude)
  ) %>%
  mutate(
    pin = as.character(pin),
    manual_latitude = suppressWarnings(as.numeric(corrected_latitude)),
    manual_longitude = suppressWarnings(as.numeric(corrected_longitude))
  ) %>%
  select(source_row_id, pin, manual_latitude, manual_longitude)

invalid_manual_coords <- manual_commercial_coords %>%
  filter(
    is.na(manual_latitude) |
      is.na(manual_longitude) |
      manual_latitude < chicago_lat_min |
      manual_latitude > chicago_lat_max |
      manual_longitude < chicago_lon_min |
      manual_longitude > chicago_lon_max
  )

if (nrow(invalid_manual_coords) > 0) {
  stop(
    sprintf("Manual commercial coordinate corrections contain %d invalid rows.", nrow(invalid_manual_coords)),
    call. = FALSE
  )
}

duplicate_manual_pin_coords <- manual_commercial_coords %>%
  group_by(pin) %>%
  summarise(
    n_coordinate_pairs = n_distinct(paste(manual_latitude, manual_longitude)),
    .groups = "drop"
  ) %>%
  filter(n_coordinate_pairs > 1)

if (nrow(duplicate_manual_pin_coords) > 0) {
  stop(
    sprintf("Manual commercial coordinate corrections contain %d PINs with conflicting coordinates.", nrow(duplicate_manual_pin_coords)),
    call. = FALSE
  )
}

manual_commercial_coords_by_pin <- manual_commercial_coords %>%
  group_by(pin) %>%
  summarise(
    pin_manual_latitude = first(manual_latitude),
    pin_manual_longitude = first(manual_longitude),
    .groups = "drop"
  )

manual_commercial_coords <- manual_commercial_coords %>%
  select(source_row_id, row_manual_latitude = manual_latitude, row_manual_longitude = manual_longitude)

# ---- Prep Parcels (Geo) ----
parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  filter(!is.na(latitude) & !is.na(longitude))

duplicate_parcel_pins <- parcels %>%
  count(pin, name = "n_rows") %>%
  filter(!is.na(pin), n_rows > 1)

if (nrow(duplicate_parcel_pins) > 0) {
  stop(
    sprintf(
      "Parcel input contains %d duplicate PINs with latitude/longitude. Fix upstream instead of silently deduplicating.",
      nrow(duplicate_parcel_pins)
    ),
    call. = FALSE
  )
}

out_of_bounds_parcels <- parcels %>%
  filter(
    latitude < chicago_lat_min |
      latitude > chicago_lat_max |
      longitude < chicago_lon_min |
      longitude > chicago_lon_max
  )

if (nrow(out_of_bounds_parcels) > 0) {
  stop(
    sprintf(
      "Parcel input contains %d rows with coordinates outside the broad Chicago bounding box.",
      nrow(out_of_bounds_parcels)
    ),
    call. = FALSE
  )
}

parcels <- parcels %>%
  select(pin, pin10, latitude, longitude, cmap_walkability_total_score, cmap_walkability_no_transit_score)

parcel_proximity <- parcel_proximity %>%
  select(pin10, nearest_cta_route_dist_ft, nearest_hospital_dist_ft, nearest_major_road_dist_ft,
         nearest_metra_stop_dist_ft, nearest_park_dist_ft, nearest_water_dist_ft, nearest_neighbor_1_dist_ft)

# ---- Process Residential (Class 2) ----
res_with_geo <- residential %>%
  mutate(pin        = as.character(pin),
         tax_year   = suppressWarnings(as.integer(tax_year)),
         card_num   = suppressWarnings(as.integer(card_num)),
         year_built = suppressWarnings(as.integer(year_built))) %>%
  left_join(parcels, by = "pin") %>%
  mutate(residential = TRUE) %>%
  # drop rows without coordinates
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  left_join(parcel_proximity, by = "pin10") %>% 
  relocate(pin, pin10)

# Harmonize RES types
res_with_geo <- res_with_geo %>%
  mutate(
    pin           = as.character(pin),
    pin10         = as.character(pin10),
    tax_year      = as.integer(tax_year),
    card_num      = as.integer(card_num),
    year_built    = as.integer(year_built),
    building_sqft = as.numeric(building_sqft),
    land_sqft     = as.numeric(land_sqft),
    num_apartments= as.integer(num_apartments),
    latitude      = as.numeric(latitude),
    longitude     = as.numeric(longitude),
    residential   = as.logical(residential)
  ) %>%
  # Standardize columns to match final schema
  rename(yearbuilt = year_built, 
         arealotsf = land_sqft,
         areabuilding = building_sqft,
         unitscount = num_apartments, 
         roomscount = num_rooms,
         bedroomscount = num_bedrooms,
         fullbathcount = num_full_baths,
         halfbathcount = num_half_baths) %>%
  mutate(
    storiescount = case_when(
      is.na(type_of_residence) ~ NA_real_,
      str_detect(type_of_residence, regex("^1\\s*Story$", ignore_case = TRUE))    ~ 1,
      str_detect(type_of_residence, regex("^1\\.5\\s*Story$", ignore_case = TRUE))~ 1.5,
      str_detect(type_of_residence, regex("^2\\s*Story$", ignore_case = TRUE))    ~ 2,
      str_detect(type_of_residence, regex("^3\\s*Story\\s*\\+$", ignore_case=TRUE)) ~ 3,   
      str_detect(type_of_residence, regex("Split\\s*Level", ignore_case = TRUE))  ~ 2,     
      TRUE ~ NA_real_
    ),
    is_sf = (!is.na(single_v_multi_family) & grepl("^single", single_v_multi_family, ignore.case=TRUE)) |
      (!is.na(type_of_residence) & type_of_residence %in% c("1 Story","1.5 Story","2 Story","3 Story +","Split Level")),
    unitscount = ifelse(is_sf & (is.na(unitscount) | unitscount == 0L), 1L, unitscount)
  ) %>%
  select(pin, pin10, yearbuilt, arealotsf, areabuilding, unitscount, storiescount, bedroomscount, latitude, longitude, residential)

# ---- Process Commercial (Multifamily) ----
multifamily_geo <- multifamily %>%
  # Create pin10 for joining proximity
  mutate(pin10 = str_sub(pin, 1, 10)) %>%
  
  # Join Geo Data
  left_join(parcels, by = "pin") %>%
  # Handle pin10 overlap from parcels join (keep the one we generated if needed, or coalesce)
  mutate(pin10 = coalesce(pin10.x, pin10.y)) %>%
  select(-pin10.x, -pin10.y) %>%
  left_join(manual_commercial_coords, by = "source_row_id") %>%
  left_join(manual_commercial_coords_by_pin, by = "pin") %>%
  mutate(
    manual_latitude = coalesce(row_manual_latitude, pin_manual_latitude),
    manual_longitude = coalesce(row_manual_longitude, pin_manual_longitude),
    latitude = if_else(is.na(latitude), manual_latitude, latitude),
    longitude = if_else(is.na(longitude), manual_longitude, longitude)
  ) %>%
  select(-row_manual_latitude, -row_manual_longitude, -pin_manual_latitude, -pin_manual_longitude, -manual_latitude, -manual_longitude) %>%
  
  # Filter for valid geo
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  
  # Join Proximity
  left_join(parcel_proximity, by = "pin10") %>%
  
  # Calculate total bedrooms from the unit mix columns (0*studios + 1*1br + 2*2br...)
  mutate(
    bedroomscount = (
      (0 * coalesce(studiounits, 0)) +
        (1 * coalesce(x1brunits, 0)) +
        (2 * coalesce(x2brunits, 0)) +
        (3 * coalesce(x3brunits, 0)) +
        (4 * coalesce(x4brunits, 0))
    ),
    # If bedroom calculation resulted in 0 but units > 0, it might just be missing mix data, so set to NA
    bedroomscount = ifelse(bedroomscount == 0 & tot_units > 0, NA_real_, bedroomscount)
  ) %>%
  
  # Map to standard schema
  mutate(
    arealotsf = landsf,
    areabuilding = bldgsf,
    unitscount = tot_units,
    residential = TRUE,
    is_sf = FALSE, # By definition, these are commercial multifamily
    storiescount = NA_real_ # We don't have reliable stories in this dataset
  ) %>%
  
  # Select matching columns
  select(pin, pin10, yearbuilt, arealotsf, areabuilding, unitscount, storiescount, bedroomscount, latitude, longitude, residential)

# ---- Combine and Save ----
# Bind rows
all_parcels <- bind_rows(res_with_geo, multifamily_geo) %>%
  # DEDUPLICATE: 
  # If a PIN exists in both Class 2 (Residential) and Class 3 (Commercial), 
  # prioritize Commercial if units > 6 (since Class 2 caps at 6).
  group_by(pin) %>%
  arrange(desc(unitscount)) %>% # Keeps the record with higher unit count if duplicates exist
  slice(1) %>%
  ungroup()

if (anyDuplicated(all_parcels$pin)) {
  stop("Combined geocoded parcel output still has duplicate PINs after prioritization.", call. = FALSE)
}

invalid_output_coords <- all_parcels %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    (
      latitude < chicago_lat_min |
        latitude > chicago_lat_max |
        longitude < chicago_lon_min |
        longitude > chicago_lon_max
    )
  )

if (nrow(invalid_output_coords) > 0) {
  stop(
    sprintf(
      "Combined geocoded parcel output contains %d rows with coordinates outside the broad Chicago bounding box.",
      nrow(invalid_output_coords)
    ),
    call. = FALSE
  )
}

# Write output
has_xy <- is.finite(all_parcels$longitude) & is.finite(all_parcels$latitude)
sf_pts <- st_as_sf(all_parcels[has_xy, ], coords = c("longitude", "latitude"), crs = 4326)
sf_chi <- st_transform(sf_pts, 3435)

message(sprintf(
  "Writing %d geocoded parcel records with latitude range %.6f to %.6f and longitude range %.6f to %.6f.",
  nrow(all_parcels),
  min(all_parcels$latitude, na.rm = TRUE),
  max(all_parcels$latitude, na.rm = TRUE),
  min(all_parcels$longitude, na.rm = TRUE),
  max(all_parcels$longitude, na.rm = TRUE)
))

st_write(sf_chi, geocoded_residential_data_path, delete_dsn = TRUE, quiet = TRUE)
