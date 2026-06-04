# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

chunk_n <- 100000L

sales <- read_parquet("../input/sales_with_hedonics.parquet") %>% as_tibble()

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
