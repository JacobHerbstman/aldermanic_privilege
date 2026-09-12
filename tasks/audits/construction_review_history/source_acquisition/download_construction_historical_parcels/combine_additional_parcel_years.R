# setwd("tasks/download_construction_historical_parcels/code")
# scope <- "historical"

source("../../shared/code/save_data.R")

library(sf)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(scope)
stopifnot(length(args) == 1L, args[1] %in% c("historical", "preferred"))
scope <- args[1]
annual <- list()
for (year in 2006:2022) {
  annual[[as.character(year)]] <- st_read(
    paste0("../output/", scope, "_parcels_additional_", year, ".gpkg"), quiet = TRUE)
}
parcels <- bind_rows(annual) |> arrange(target_year, pin10, pin14, object_id)
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
st_write(parcels, paste0("../output/", scope, "_parcels_additional.gpkg"),
         layer = "historical_parcels", delete_dsn = TRUE, quiet = TRUE)

ReportData(paste0("../output/", scope, "_parcels_additional.gpkg"), c("target_year", "object_id"))
