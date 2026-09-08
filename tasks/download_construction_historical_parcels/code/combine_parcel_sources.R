# setwd("tasks/download_construction_historical_parcels/code")

library(dplyr)
library(readr)
library(sf)

original <- st_read("../input/historical_project_parcel_source_2026-07-27.gpkg", quiet = TRUE)
additional <- st_read("../input/historical_project_parcel_source_2026-09-07.gpkg", quiet = TRUE)
queries <- read_csv("../output/historical_project_parcel_source_queries.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(st_crs(original)$epsg == 3435, st_crs(additional)$epsg == 3435,
          nrow(inner_join(st_drop_geometry(original) |> distinct(target_year, pin10),
                         st_drop_geometry(additional) |> distinct(target_year, pin10),
                         by = c("target_year", "pin10"), relationship = "one-to-one")) == 0L)
parcels <- bind_rows(original, additional) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)),
          nrow(anti_join(st_drop_geometry(parcels), queries, by = c("target_year", "pin10"))) == 0L)
st_write(parcels, "../output/historical_project_parcel_source.gpkg",
         layer = "historical_project_parcels", delete_dsn = TRUE, quiet = TRUE)
