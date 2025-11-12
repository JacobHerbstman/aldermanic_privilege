# this code takes the parcels dataset from cook county assessor and merges in latitude and longitude for all pins i can find (residential and condo datasets)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")


# ---- Load ----
residential    <- read_csv("../input/residential_cross_section.csv",   show_col_types = FALSE)
parcels   <- read_csv("../input/parcels.csv",  show_col_types = FALSE)
parcel_proximity <- read_csv("../input/parcel_proximity.csv", show_col_types = FALSE)


parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  distinct(pin, .keep_all = TRUE) %>% 
  select(pin, pin10, latitude, longitude, cmap_walkability_total_score, cmap_walkability_no_transit_score)

parcel_proximity <- parcel_proximity %>%
  select(pin10, nearest_cta_route_dist_ft, nearest_hospital_dist_ft, nearest_major_road_dist_ft,
         nearest_metra_stop_dist_ft, nearest_park_dist_ft, nearest_water_dist_ft, nearest_neighbor_1_dist_ft)



res_with_geo <- residential %>%
  mutate(pin    = as.character(pin),
         tax_year   = suppressWarnings(as.integer(tax_year)),
         card_num   = suppressWarnings(as.integer(card_num)),
         year_built = suppressWarnings(as.integer(year_built))) %>%
  left_join(parcels, by = "pin") %>%
  mutate(residential = TRUE) %>%
  # drop rows without coordinates
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  left_join(parcel_proximity, by = "pin10") %>% 
  relocate(pin, pin10)

# 1) Harmonize RES types (keep residential names)
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
  )

# 4) Row bind
all_parcels <- res_with_geo %>%
  mutate(
    storiescount = case_when(
      is.na(type_of_residence) ~ NA_real_,
      str_detect(type_of_residence, regex("^1\\s*Story$", ignore_case = TRUE))    ~ 1,
      str_detect(type_of_residence, regex("^1\\.5\\s*Story$", ignore_case = TRUE))~ 1.5,
      str_detect(type_of_residence, regex("^2\\s*Story$", ignore_case = TRUE))    ~ 2,
      str_detect(type_of_residence, regex("^3\\s*Story\\s*\\+$", ignore_case=TRUE)) ~ 3,   # lower bound
      str_detect(type_of_residence, regex("Split\\s*Level", ignore_case = TRUE))  ~ 2,     # or NA_real_
      TRUE ~ NA_real_
    )
  ) %>% 
  rename(yearbuilt = year_built, 
         arealotsf = land_sqft,
         areabuilding = building_sqft,
         unitscount = num_apartments, 
         roomscount = num_rooms,
         bedroomscount = num_bedrooms,
         fullbathcount = num_full_baths,
         halfbathcount = num_half_baths) %>% 
  mutate(
    is_sf = (!is.na(single_v_multi_family) & grepl("^single", single_v_multi_family, ignore.case=TRUE)) |
      (!is.na(type_of_residence) & type_of_residence %in% c("1 Story","1.5 Story","2 Story","3 Story +","Split Level")),
    unitscount = ifelse(is_sf & (is.na(unitscount) | unitscount == 0L), 1L, unitscount)
  ) %>% 
  relocate(pin, pin10, unitscount, storiescount, is_sf, single_v_multi_family)
  

has_xy <- is.finite(all_parcels$longitude) & is.finite(all_parcels$latitude)
has_xy[is.na(has_xy)] <- FALSE
sf_pts <- st_as_sf(all_parcels[has_xy, ], coords = c("longitude", "latitude"), crs = 4326)
sf_chi <- st_transform(sf_pts, 3435)

st_write(sf_chi, "../output/geocoded_residential_data.gpkg", delete_dsn = TRUE, quiet = TRUE)


cat("Wrote:", "../output/geocoded_residential_data.gpkg", "\n",
    "Rows:", nrow(sf_pts), "\n")

    