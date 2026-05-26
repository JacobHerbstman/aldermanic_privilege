source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")

chunk_n <- 100000L

message("Loading sales...")
sales <- read_parquet("../input/sales_with_hedonics.parquet") %>% as_tibble()
message(sprintf("Sales loaded: %s", format(nrow(sales), big.mark = ",")))

message("Building unique coordinate table...")
coords <- sales %>%
  transmute(longitude, latitude) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  distinct()
message(sprintf("Unique coordinates: %s", format(nrow(coords), big.mark = ",")))

message("Loading amenity layers...")
coords <- build_unique_coordinate_amenity_table(
  sales,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  chunk_n
)

sales_out <- append_amenity_distances(sales, coords, "longitude", "latitude")

write_parquet(as.data.frame(sales_out), "../output/sales_with_hedonics_amenities.parquet")

message("Saved: ../output/sales_with_hedonics_amenities.parquet")
