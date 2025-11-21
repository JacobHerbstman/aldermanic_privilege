# this code takes the parcels dataset from cook county assessor and merges in latitude and longitude for all pins i can find (residential and condo datasets)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# ---- Load ----
residential      <- read_csv("../input/residential_cross_section.csv",   show_col_types = FALSE)
parcels          <- read_csv("../input/parcels.csv",  show_col_types = FALSE)
parcel_proximity <- read_csv("../input/parcel_proximity.csv", show_col_types = FALSE)
multifamily      <- read_csv("../input/multifamily_data_cleaned.csv", show_col_types = FALSE)

# ---- Prep Parcels (Geo) ----
parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  distinct(pin, .keep_all = TRUE) %>% 
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

# Write output
has_xy <- is.finite(all_parcels$longitude) & is.finite(all_parcels$latitude)
sf_pts <- st_as_sf(all_parcels[has_xy, ], coords = c("longitude", "latitude"), crs = 4326)
sf_chi <- st_transform(sf_pts, 3435)

st_write(sf_chi, "../output/geocoded_residential_data.gpkg", delete_dsn = TRUE, quiet = TRUE)


