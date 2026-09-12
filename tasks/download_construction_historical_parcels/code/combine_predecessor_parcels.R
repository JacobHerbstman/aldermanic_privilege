source("../../shared/code/save_data.R")
# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(sf)

original <- st_read("../input/historical_predecessor_parcels_2026-07-27.gpkg", quiet = TRUE)
additional <- st_read("../input/historical_predecessor_parcels_2026-09-07.gpkg", quiet = TRUE)
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435)
parcels <- bind_rows(original, additional) |>
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
SaveData(parcels, c("target_year", "object_id"), "../output/historical_predecessor_parcel_source.gpkg", layer = "historical_project_predecessor_parcels", delete_dsn = TRUE, quiet = TRUE)
