# setwd("tasks/download_construction_historical_parcels/code")

library(sf)
library(dplyr)

annual <- list()
for (year in 2006:2022) {
  annual[[as.character(year)]] <- st_read(
    paste0("../output/historical_parcels_additional_", year, ".gpkg"), quiet = TRUE)
}
parcels <- bind_rows(annual) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
st_write(parcels, "../output/historical_parcels_additional.gpkg",
         layer = "historical_parcels", delete_dsn = TRUE, quiet = TRUE)
