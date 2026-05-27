# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))
source("../../_lib/amenity_distance_helpers.R")

chunk_n <- 100000L

message("Loading rental listings...")
rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>% as_tibble()
message(sprintf("Listings loaded: %s", format(nrow(rent), big.mark = ",")))

message("Building unique coordinate table...")
coords <- rent %>%
  transmute(longitude, latitude) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  distinct()
message(sprintf("Unique coordinates: %s", format(nrow(coords), big.mark = ",")))

message("Loading amenity layers...")
coords <- build_unique_coordinate_amenity_table(
  rent,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  chunk_n
)

rent_out <- append_amenity_distances(rent, coords, "longitude", "latitude")

write_parquet(as.data.frame(rent_out), "../temp/rent_with_ward_distances_amenities.parquet")

message("Saved: ../temp/rent_with_ward_distances_amenities.parquet")
