# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(sf)

original <- st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE)
additional <- st_read("../input/preferred_predecessor_parcels_2026-09-07.gpkg", quiet = TRUE) |>
  select(-geometry_valid)
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435)
parcels <- bind_rows(original, additional) |>
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
st_write(parcels, "../output/preferred_predecessor_parcel_source.gpkg",
  layer = "historical_project_predecessor_parcels", delete_dsn = TRUE, quiet = TRUE)
