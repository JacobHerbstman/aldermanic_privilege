# setwd("tasks/new_construction_cleaning/code")

library(dplyr)
library(readr)
library(sf)

requests <- read_csv("../output/historical_project_parcel_requests.csv",
  col_types = cols(target_year = col_integer(), .default = col_character()))
queries <- read_csv("../input/historical_project_parcel_source_queries.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyDuplicated(requests[c("source_family", "project_id", "component_pin", "target_year")]),
          !anyDuplicated(queries), !anyNA(queries))
unqueried <- requests |>
  distinct(target_year, pin10) |>
  anti_join(queries, by = c("target_year", "pin10"))
if (nrow(unqueried) > 0L) {
  stop(nrow(unqueried), " parcel-year queries are outside the pinned source; acquire and pin them before classifying coverage.")
}

parcels <- st_read("../input/historical_project_parcel_source.gpkg", quiet = TRUE)
stopifnot(st_crs(parcels)$epsg == 3435,
          !anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
parcels <- st_drop_geometry(parcels)
stopifnot(nrow(anti_join(parcels, queries, by = c("target_year", "pin10"))) == 0L)

counts <- count(parcels, target_year, pin10, name = "polygon_count_pin10")
exact <- parcels |>
  distinct(target_year, pin10, pin14) |>
  mutate(exact_pin14_available = TRUE)
coverage <- requests |>
  left_join(counts, by = c("target_year", "pin10"), relationship = "many-to-one") |>
  left_join(exact, by = c("target_year", "pin10", "component_pin" = "pin14"), relationship = "many-to-one") |>
  mutate(polygon_count_pin10 = coalesce(polygon_count_pin10, 0L),
         exact_pin14_available = coalesce(exact_pin14_available, FALSE),
         coverage_status = case_when(
           exact_pin14_available ~ "exact_pin14",
           polygon_count_pin10 == 1L ~ "unique_pin10_only",
           polygon_count_pin10 > 1L ~ "ambiguous_pin10",
           TRUE ~ "missing")) |>
  arrange(target_year, source_family, project_id, component_pin)
stopifnot(nrow(coverage) == nrow(requests))
write_csv(coverage, "../output/historical_project_parcel_coverage.csv")
