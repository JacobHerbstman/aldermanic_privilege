# setwd("tasks/new_construction_cleaning/code")

library(dplyr)
library(readr)
library(sf)
library(tidyr)

references <- read_csv("../output/missing_project_reference_points.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(),
    history_row_id = col_character(), .default = col_guess())) |>
  select(request_id, source_family, project_id, component_pin, pin10, target_year,
    reference_source, reference_x_3435, reference_y_3435, history_reference_year,
    history_reference_year_gap, history_row_id)
queries <- read_csv("../input/historical_predecessor_source_queries.csv", show_col_types = FALSE)
available <- references |> filter(is.finite(reference_x_3435), is.finite(reference_y_3435))
stopifnot(!anyDuplicated(references$request_id), !anyDuplicated(queries), !anyNA(queries))
if (nrow(anti_join(available, queries, by = c("target_year", "reference_x_3435", "reference_y_3435"))) > 0L) {
  stop("Predecessor points exceed the pinned spatial query scope; acquire and pin the additional queries.")
}
points <- st_as_sf(available, coords = c("reference_x_3435", "reference_y_3435"), crs = 3435)
parcels <- st_read("../input/historical_predecessor_parcel_source.gpkg", quiet = TRUE)
stopifnot(st_crs(parcels)$epsg == 3435,
          !anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))

matches <- list()
for (year in sort(unique(points$target_year))) {
  year_points <- filter(points, target_year == year)
  year_parcels <- filter(parcels, target_year == year)
  # Preserve boundary intersections as candidates; never choose among multiple hits.
  hits <- st_intersects(year_points, year_parcels)
  matches[[as.character(year)]] <- tibble(request_id = year_points$request_id,
    predecessor_polygon_count = lengths(hits), parcel_index = as.list(hits)) |>
    unnest_longer(parcel_index, keep_empty = TRUE) |>
    mutate(object_id = year_parcels$object_id[parcel_index],
      predecessor_pin14 = year_parcels$predecessor_pin14[parcel_index],
      predecessor_pin10 = year_parcels$predecessor_pin10[parcel_index]) |>
    select(-parcel_index)
}
matches <- bind_rows(tibble(request_id = integer(), predecessor_polygon_count = integer(),
  object_id = integer(), predecessor_pin14 = character(), predecessor_pin10 = character()), bind_rows(matches))
resolution <- references |>
  left_join(matches, by = "request_id", relationship = "one-to-many") |>
  mutate(predecessor_polygon_count = coalesce(predecessor_polygon_count, 0L),
    predecessor_status = case_when(
      reference_source == "unresolved" ~ "no_reference_point",
      predecessor_polygon_count == 1L ~ "unique_predecessor_polygon",
      predecessor_polygon_count > 1L ~ "multiple_predecessor_polygons",
      TRUE ~ "no_predecessor_polygon")) |>
  select(all_of(names(references)), object_id, predecessor_pin14, predecessor_pin10,
    predecessor_polygon_count, predecessor_status) |>
  arrange(target_year, source_family, project_id, component_pin, object_id)
stopifnot(n_distinct(resolution$request_id) == nrow(references),
          !anyDuplicated(resolution[c("request_id", "object_id")]))
write_csv(resolution, "../output/historical_project_predecessor_resolution.csv")
