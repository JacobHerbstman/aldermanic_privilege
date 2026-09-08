# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)
library(sf)

original <- st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE)
latest <- st_read("../input/preferred_parcel_supplement_2026-09-08.gpkg", quiet = TRUE)
original_queries <- read_csv("../input/preferred_historical_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
initial <- st_read("../output/historical_project_parcel_source.gpkg", quiet = TRUE) |>
  anti_join(original_queries, by = c("target_year", "pin10"))
additional <- st_read("../input/preferred_parcel_supplement_2026-09-07.gpkg", quiet = TRUE)
queries <- read_csv("../output/preferred_historical_parcel_source_queries.csv", col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(st_crs(original)$epsg == 3435, st_crs(initial)$epsg == 3435, st_crs(additional)$epsg == 3435)
parcels <- bind_rows(original, initial, additional, latest) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)),
          nrow(anti_join(st_drop_geometry(parcels), queries, by = c("target_year", "pin10"))) == 0L)
st_write(parcels, "../output/preferred_historical_parcel_source.gpkg",
  layer = "historical_parcels", delete_dsn = TRUE, quiet = TRUE)
