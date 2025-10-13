# --- Land/Building Values: clean, collapse to pin10, geocode, and save GeoParquet ---

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")
library(sf)        # already installed via setup; load explicitly for clarity
library(sfarrow)   # for st_write_parquet

# ---- Load ----
data    <- read_csv("../input/land_value_raw.csv", show_col_types = FALSE)
parcels <- read_csv("../input/parcels.csv",      show_col_types = FALSE)

# ---- Sanity check (non-invasive): ratio-of-sums == value-weighted mean of unit land_share
data_sanity <- data %>%
  dplyr::distinct(pin, tax_year, .keep_all = TRUE) %>%
  mutate(
    pin10     = substr(pin, 1, 10),
    tot_val   = certified_land + certified_bldg,
    land_share = dplyr::if_else(tot_val > 0,
                                certified_land / tot_val,
                                NA_real_)
  ) %>%
  group_by(pin10, tax_year) %>%
  summarise(
    ls_ratio = sum(certified_land, na.rm = TRUE) / sum(tot_val, na.rm = TRUE),
    ls_vw    = weighted.mean(land_share, w = tot_val, na.rm = TRUE),
    .groups  = "drop"
  )

max_diff <- max(abs(data_sanity$ls_ratio - data_sanity$ls_vw), na.rm = TRUE)
message(sprintf("Sanity check max |ratio - weighted| diff: %.3e", max_diff))

# ---- Clean unit-level data; build unit land_share ----
data <- data %>%
  select(pin, tax_year, class,
         certified_land, certified_bldg, certified_tot,
         board_land, board_bldg, board_tot) %>%
  mutate(pin10 = substr(pin, 1, 10)) %>%
  filter(!is.na(pin),
         !is.na(certified_land), !is.na(certified_bldg),
         (certified_land + certified_bldg) > 0) %>%
  mutate(
    land_share = certified_land / (certified_land + certified_bldg)
  )

# ---- Collapse to lot level: pin10 x tax_year (ratio-of-sums) ----
data_pin10 <- data %>%
  mutate(pin10 = substr(pin, 1, 10)) %>%
  group_by(pin10, tax_year) %>%
  summarise(
    land_sum = sum(certified_land, na.rm = TRUE),
    bldg_sum = sum(certified_bldg, na.rm = TRUE),
    n_units  = dplyr::n_distinct(pin),     # for condos: approx # of units
    .groups  = "drop"
  ) %>%
  mutate(land_share_pin10 = land_sum / (land_sum + bldg_sum)) %>%
  filter((land_sum + bldg_sum) > 0)

# ---- Prep parcels; reduce to one row per 14-digit PIN, then to one per pin10 ----
parcels <- parcels %>%
  mutate(pin = as.character(pin),
         pin10 = substr(pin, 1, 10)) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(pin, .keep_all = TRUE) %>%
  select(pin, pin10, latitude, longitude,
         cmap_walkability_total_score, cmap_walkability_no_transit_score)

parcels_by_pin10 <- parcels %>%
  group_by(pin10) %>%
  summarise(
    latitude_pin10  = dplyr::first(na.omit(latitude)),
    longitude_pin10 = dplyr::first(na.omit(longitude)),
    cmap_walkability_total_score_pin10 =
      dplyr::first(na.omit(cmap_walkability_total_score)),
    cmap_walkability_no_transit_score_pin10 =
      dplyr::first(na.omit(cmap_walkability_no_transit_score)),
    .groups = "drop"
  )

# ---- Merge lot-level values with lot-level coords ----
merged <- data_pin10 %>%
  left_join(parcels_by_pin10, by = "pin10")

# make lon/lat column names expected by st_as_sf (source columns end with _pin10)
merged <- merged %>%
  filter(!is.na(latitude_pin10), !is.na(longitude_pin10))  %>% 
  rename(
    latitude  = latitude_pin10,
    longitude = longitude_pin10
  )

# ---- Build sf geometry (WGS84 -> EPSG:3435) ----
merged_sf <- st_as_sf(
  merged,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
) %>%
  st_transform(3435)

# ---- Save as GeoParquet (preserves geometry) ----
sfarrow::st_write_parquet(
  merged_sf,
  "../output/land_values_geo.parquet",
  compression = "zstd"
)
