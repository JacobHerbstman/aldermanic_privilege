# this code takes the parcels dataset from cook county assessor and merges in latitude and longitude for all pins i can find (residential and condo datasets)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")


# ---- Load ----
condos <- read_csv("../input/condos_cross_section.csv", show_col_types = FALSE)
residential    <- read_csv("../input/residential_cross_section.csv",   show_col_types = FALSE)
parcels   <- read_csv("../input/parcels.csv",  show_col_types = FALSE)

parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  distinct(pin, .keep_all = TRUE) %>% 
  select(pin, pin10, latitude, longitude, cmap_walkability_total_score, cmap_walkability_no_transit_score)

# ---- Residential: join by 14-digit PIN ----
res_with_geo <- residential %>%
  left_join(parcels, by = "pin") %>%
  mutate(residential = T) %>% 
  filter(!is.na(latitude) & !is.na(longitude)) 
  



# pin -> pin10 rollup from parcels, then median lat/lon per pin10
parc_pin10 <- parcels %>%
  mutate(pin10 = substr(pin, 1, 10)) %>%
  group_by(pin10) %>%
  summarise(
    latitude = if (all(is.na(latitude))) NA_real_ else median(latitude, na.rm = TRUE),
    longitude = if (all(is.na(longitude))) NA_real_ else median(longitude, na.rm = TRUE),
    .groups = "drop"
  )

condo_with_geo <- condos %>%
  left_join(parc_pin10, by = "pin10") %>%
  mutate(residential = F) %>% 
  filter(!is.na(latitude) & !is.na(longitude))


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

# 2) Rename + harmonize CONDO to residential naming
#    - built_year  -> year_built
#    - units       -> num_apartments
condo_h <- condo_with_geo %>%
  rename(
    year_built     = built_year,
    num_apartments = units
  ) %>%
  mutate(
    pin10         = as.character(pin10),
    tax_year      = as.integer(tax_year),
    year_built    = as.integer(year_built),
    building_sqft = as.numeric(building_sqft),
    land_sqft     = as.numeric(land_sqft),
    num_apartments= as.integer(num_apartments),
    latitude      = as.numeric(latitude),
    longitude     = as.numeric(longitude),
    residential   = as.logical(residential)
  )

# 3) Select a unified column order (residential names win)
cols_union <- union(names(res_with_geo), names(condo_h))
res_with_geo  <- res_with_geo %>% select(any_of(cols_union))
condo_h       <- condo_h      %>% select(any_of(cols_union))

# 4) Row bind
all_parcels <- data.table::rbindlist(list(res_with_geo, condo_h), fill = TRUE, use.names = TRUE)

has_xy <- is.finite(all_parcels$lon) & is.finite(all_parcels$lat)
sf_pts <- st_as_sf(all_parcels[has_xy, ], coords = c("longitude", "latitude"), crs = 4326)
sf_chi <- st_transform(sf_pts, 3435)

st_write(sf_chi, "../output/geocoded_residential_data.gpkg", delete_dsn = TRUE, quiet = TRUE)


cat("Wrote:", "../output/geocoded_residential_data.gpkg", "\n",
    "Rows:", nrow(sf_pts), "\n")

    