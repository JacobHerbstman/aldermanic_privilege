# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)
library(sf)

queue <- read_csv("../output/candidate_geography_review_queue.csv", show_col_types = FALSE)
candidates <- bind_rows(
  read_csv("../input/preferred_residential_project_candidates.csv", show_col_types = FALSE),
  read_csv("../input/preferred_commercial_project_candidates.csv", show_col_types = FALSE))
scope <- read_csv("../input/preferred_project_boundary_scope.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(scope$project_id),
  all(scope$project_id[!scope$complete_project_geometry] %in% queue$project_id))
review <- candidates |> semi_join(queue, by = "project_id") |>
  select(project_id, source_family, component_pins, construction_year, dwelling_units,
    building_sqft, land_sqft, candidate_status, decision_reason) |>
  left_join(scope |> select(project_id, geography_status, project_land_area_sqft,
    distance_to_boundary_ft), by = "project_id", relationship = "one-to-one")
universe <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(
  pin = col_character(), longitude = col_double(), latitude = col_double(), .default = col_skip()))
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(
  pin = col_character(), prop_address_full = col_character(), .default = col_skip()))
polygons <- st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE)
stopifnot(!anyDuplicated(universe$pin), !anyDuplicated(addresses$pin),
  !anyDuplicated(polygons$project_id), st_crs(polygons)$epsg == 3435)
review$current_addresses <- NA_character_
review$current_point_count <- 0L
review$maximum_point_outside_historical_site_ft <- NA_real_
review$current_point_minimum_boundary_ft <- NA_real_
review$current_point_maximum_boundary_ft <- NA_real_
review$current_points_cross_500ft <- FALSE
review$current_point_disagrees_with_500ft <- FALSE

# Current parcel locations are independent comparison evidence, not adopted historical locations.
for (i in seq_len(nrow(review))) {
  pins <- strsplit(review$component_pins[i], "/", fixed = TRUE)[[1]]
  observed_addresses <- addresses$prop_address_full[match(pins, addresses$pin)]
  review$current_addresses[i] <- paste(sort(unique(na.omit(observed_addresses))), collapse = "; ")
  rows <- universe[match(pins, universe$pin), ] |>
    filter(is.finite(longitude), is.finite(latitude))
  review$current_point_count[i] <- nrow(rows)
  if (!nrow(rows)) next
  points <- st_as_sf(rows, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
  j <- match(review$project_id[i], polygons$project_id)
  if (!is.na(j)) review$maximum_point_outside_historical_site_ft[i] <-
    max(as.numeric(st_distance(points, polygons[j, ])))
  if (is.na(review$construction_year[i])) next
  era <- if (review$construction_year[i] >= 2015) "2015_2023" else "2003_2014"
  boundaries <- st_read("../input/ward_pair_boundaries.gpkg", layer = era, quiet = TRUE)
  stopifnot(st_crs(boundaries)$epsg == 3435)
  distances <- apply(as.matrix(st_distance(points, boundaries)), 1, min)
  review$current_point_minimum_boundary_ft[i] <- min(distances)
  review$current_point_maximum_boundary_ft[i] <- max(distances)
  review$current_points_cross_500ft[i] <- any(distances <= 500) && any(distances > 500)
  if (is.finite(review$distance_to_boundary_ft[i]))
    review$current_point_disagrees_with_500ft[i] <-
      any((distances <= 500) != (review$distance_to_boundary_ft[i] <= 500))
}
review <- review |> mutate(mapped_to_assessor_land_ratio = project_land_area_sqft / land_sqft) |>
  arrange(source_family, project_id)
stopifnot(nrow(review) == n_distinct(queue$project_id), !anyDuplicated(review$project_id))
write_csv(review, "../output/remaining_project_location_checks.csv")
