source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_border_pair_fe/code")
# input_parquet <- "../input/sales_with_hedonics.parquet"
# schools_gpkg <- "../input/schools_2015.gpkg"
# parks_gpkg <- "../input/parks.gpkg"
# major_streets_gpkg <- "../input/major_streets.gpkg"
# water_shp <- "../input/gis_osm_water_a_free_1.shp"
# output_parquet <- "../output/sales_with_hedonics_amenities.parquet"
# output_diag_csv <- "../output/sales_amenity_distance_diagnostics.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input_parquet, schools_gpkg, parks_gpkg, major_streets_gpkg, water_shp, output_parquet, output_diag_csv)
}

if (length(cli_args) >= 7) {
  input_parquet <- cli_args[1]
  schools_gpkg <- cli_args[2]
  parks_gpkg <- cli_args[3]
  major_streets_gpkg <- cli_args[4]
  water_shp <- cli_args[5]
  output_parquet <- cli_args[6]
  output_diag_csv <- cli_args[7]
} else {
  stop(
    "FATAL: Script requires 7 args: <input_parquet> <schools_gpkg> <parks_gpkg> <major_streets_gpkg> <water_shp> <output_parquet> <output_diag_csv>",
    call. = FALSE
  )
}

chunk_n <- 100000L

message("Loading sales...")
sales <- read_parquet(input_parquet) %>% as_tibble()
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
  schools_gpkg,
  parks_gpkg,
  major_streets_gpkg,
  water_shp,
  chunk_n
)

sales_out <- append_amenity_distances(sales, coords, "longitude", "latitude")
diagnostics <- amenity_distance_diagnostics(sales_out, coords, "sales") %>%
  rename(n_sales = n_rows)

write_parquet(as.data.frame(sales_out), output_parquet)
write_csv(diagnostics, output_diag_csv)

message(sprintf("Saved: %s", output_parquet))
message(sprintf("Saved: %s", output_diag_csv))
