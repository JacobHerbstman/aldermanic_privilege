# setwd("tasks/audits/historical_zoning_validation/code")

library(dplyr)
library(readr)
library(sf)

projects <- read_csv(
  "../output/historical_zoning_project_comparison.csv",
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)

map_index <- st_read("../input/zoning_map_index.geojson", quiet = TRUE) %>%
  st_transform(3435) %>%
  select(zoning_map_grid = ZONE_MAP)

project_maps <- st_join(projects %>% select(pin), map_index, join = st_within, left = TRUE)
if (nrow(project_maps) != nrow(projects)) {
  stop("A density point falls within more than one zoning map grid.", call. = FALSE)
}
if (anyNA(project_maps$zoning_map_grid)) {
  stop("At least one density point is outside the zoning map grid.", call. = FALSE)
}
if (anyDuplicated(project_maps$pin)) {
  stop("Density project PINs are not unique in the zoning map-grid crosswalk.", call. = FALSE)
}

project_maps %>%
  st_drop_geometry() %>%
  arrange(pin) %>%
  write_csv("../output/historical_zoning_project_map_grids.csv")
