# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/event_study_exact_balance/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/amenity_distance_helpers.R")

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  mean(x)
}

message("Loading geocoded parcels...")
parcels <- st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  mutate(
    parcel_row_id = row_number(),
    pin = as.character(pin),
    yearbuilt = suppressWarnings(as.integer(yearbuilt))
  ) %>%
  filter(is.na(yearbuilt) | yearbuilt <= 2014) %>%
  st_transform(3435)

message("Loading zoning polygons...")
zoning <- st_read("../input/zoning_data_clean.gpkg", quiet = TRUE) %>%
  select(zone_code, floor_area_ratio) %>%
  st_transform(3435)

message("Joining parcels to zoning...")
parcels_before_zoning <- nrow(parcels)
parcels <- parcels %>%
  st_join(zoning, left = TRUE, largest = TRUE)
if (nrow(parcels) != parcels_before_zoning) {
  stop("Parcel-zoning join changed row count.", call. = FALSE)
}

message("Loading 2010 census blocks...")
blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

message("Assigning parcels to blocks...")
parcels <- st_join(parcels, blocks %>% select(block_id), join = st_within)
if (anyDuplicated(parcels$parcel_row_id) > 0) {
  stop("Parcel-block spatial join assigned at least one parcel to multiple blocks.", call. = FALSE)
}

message("Computing exact amenity distances on unique parcel coordinates...")
parcel_xy <- parcels %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(pin, x, y, block_id, floor_area_ratio)

coords_tbl <- parcel_xy %>%
  filter(!is.na(block_id), is.finite(x), is.finite(y)) %>%
  distinct(x, y)

coords_sf <- st_as_sf(coords_tbl, coords = c("x", "y"), crs = 3435, remove = FALSE)
schools <- read_amenity_layer("../input/schools_2015.gpkg")
parks <- read_amenity_layer("../input/parks.gpkg")
major_streets <- read_amenity_layer("../input/major_streets.gpkg")
lake <- lake_michigan_geom("../input/gis_osm_water_a_free_1.shp")
cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(3435)

coords_tbl$nearest_school_dist_m <- nearest_distance_m(coords_sf, schools, label = "parcel coordinates")
coords_tbl$nearest_park_dist_m <- nearest_distance_m(coords_sf, parks, label = "parcel coordinates")
coords_tbl$nearest_major_road_dist_m <- nearest_distance_m(coords_sf, major_streets, label = "parcel coordinates")
coords_tbl$lake_michigan_dist_m <- nearest_distance_m(coords_sf, lake, label = "parcel coordinates")
coords_tbl$dist_cbd_m <- as.numeric(st_distance(coords_sf, cbd)) * 0.3048

parcel_xy <- parcel_xy %>%
  left_join(coords_tbl, by = c("x", "y"), relationship = "many-to-one") %>%
  filter(!is.na(block_id))

message("Aggregating parcel baselines to 2010 census blocks...")
block_baselines <- parcel_xy %>%
  group_by(block_id) %>%
  summarise(
    n_parcels = n(),
    mean_zoned_far = safe_mean(floor_area_ratio),
    mean_dist_cbd_m = safe_mean(dist_cbd_m),
    mean_nearest_school_dist_m = safe_mean(nearest_school_dist_m),
    mean_nearest_park_dist_m = safe_mean(nearest_park_dist_m),
    mean_nearest_major_road_dist_m = safe_mean(nearest_major_road_dist_m),
    mean_lake_michigan_dist_m = safe_mean(lake_michigan_dist_m),
    .groups = "drop"
  )
if (anyDuplicated(block_baselines$block_id) > 0) {
  stop("Block parcel baselines must be unique by block_id.", call. = FALSE)
}

write_csv(block_baselines, "../output/block_parcel_baselines_2014.csv")
