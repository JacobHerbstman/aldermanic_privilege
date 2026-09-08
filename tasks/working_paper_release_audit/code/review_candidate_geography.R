# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)
library(sf)

scope <- read_csv("../input/preferred_project_boundary_scope.csv", show_col_types = FALSE)
polygons <- st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE)
points <- st_read("../input/preferred_project_year_centroids.gpkg", quiet = TRUE)
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
stopifnot(st_crs(points)$epsg == 3435, st_crs(polygons)$epsg == 3435,
  st_crs(ward_panel)$epsg == 3435, !anyDuplicated(scope$project_id),
  !anyDuplicated(points$project_id), !anyDuplicated(polygons$project_id))
polygons <- polygons[match(points$project_id, polygons$project_id), ]
checks <- scope |> filter(complete_project_geometry)
checks <- checks[match(points$project_id, checks$project_id), ]
stopifnot(!anyNA(checks$project_id), identical(points$target_year, polygons$target_year),
  all(points$target_year == checks$target_year),
  setequal(checks$project_id, scope$project_id[scope$complete_project_geometry]))
checks <- checks |> select(source_family, project_id, target_year, boundary_year, era,
  ward, ward_pair, distance_to_boundary_ft, within_500ft, project_land_area_sqft) |>
  mutate(ward_polygon_hits = NA_integer_, independent_distance_ft = NA_real_,
    assigned_pair_distance_ft = NA_real_, centroid_outside_parcel_ft =
      as.numeric(st_distance(points, polygons, by_element = TRUE)),
    area_difference_sqft = project_land_area_sqft - as.numeric(st_area(polygons)),
    old_map_ward_2015 = NA_integer_, old_map_distance_ft_2015 = NA_real_)

# A concave parcel can have an exterior centroid without any location error.
stopifnot(all(st_geometry_type(polygons) == "MULTIPOLYGON"))
checks$polygon_parts <- lengths(st_geometry(polygons))
checks$maximum_part_separation_ft <- 0
for (i in which(checks$polygon_parts > 1L)) {
  parts <- suppressWarnings(st_cast(st_geometry(polygons[i, ]), "POLYGON"))
  checks$maximum_part_separation_ft[i] <- max(as.numeric(st_distance(parts)))
}

for (era_value in unique(checks$era)) {
  rows <- which(checks$era == era_value)
  map_year <- unique(checks$boundary_year[rows])
  stopifnot(length(map_year) == 1L)
  wards <- ward_panel |> filter(year == map_year) |> select(ward) |>
    group_by(ward) |> summarise(.groups = "drop")
  boundaries <- st_read("../input/ward_pair_boundaries.gpkg", layer = era_value, quiet = TRUE)
  stopifnot(st_crs(boundaries)$epsg == 3435, !anyDuplicated(boundaries$ward_pair_id))
  hits <- st_within(points[rows, ], wards)
  checks$ward_polygon_hits[rows] <- lengths(hits)
  stopifnot(all(lengths(hits) == 1L),
    all(wards$ward[unlist(hits)] == checks$ward[rows]))
  for (ward_value in unique(checks$ward[rows])) {
    selected <- rows[checks$ward[rows] == ward_value]
    edges <- boundaries[boundaries$ward_a == ward_value | boundaries$ward_b == ward_value, ]
    # Exhaustive distances to all boundaries of the ward, independent of the nearest-feature helper.
    distances <- st_distance(points[selected, ], edges)
    checks$independent_distance_ft[selected] <- apply(as.matrix(distances), 1, min)
    pair_column <- match(checks$ward_pair[selected], edges$ward_pair_id)
    stopifnot(!anyNA(pair_column))
    checks$assigned_pair_distance_ft[selected] <- as.numeric(distances[cbind(seq_along(selected), pair_column)])
  }
}

# The paper assigns June 15 when only a year is known. Measure sensitivity in the remap year.
rows <- which(checks$target_year == 2015L)
wards <- ward_panel |> filter(year == 2003L) |> select(ward) |>
  group_by(ward) |> summarise(.groups = "drop")
boundaries <- st_read("../input/ward_pair_boundaries.gpkg", layer = "2003_2014", quiet = TRUE)
hits <- st_within(points[rows, ], wards)
stopifnot(all(lengths(hits) == 1L))
checks$old_map_ward_2015[rows] <- wards$ward[unlist(hits)]
for (ward_value in unique(checks$old_map_ward_2015[rows])) {
  selected <- rows[checks$old_map_ward_2015[rows] == ward_value]
  edges <- boundaries[boundaries$ward_a == ward_value | boundaries$ward_b == ward_value, ]
  checks$old_map_distance_ft_2015[selected] <- apply(as.matrix(st_distance(points[selected, ], edges)), 1, min)
}
checks <- checks |> mutate(
  distance_difference_ft = distance_to_boundary_ft - independent_distance_ft,
  pair_distance_difference_ft = assigned_pair_distance_ft - independent_distance_ft,
  old_map_changes_ward_2015 = old_map_ward_2015 != ward,
  old_map_changes_500ft_2015 = (old_map_distance_ft_2015 <= 500) != within_500ft) |>
  arrange(target_year, source_family, project_id)
stopifnot(all(is.finite(checks$distance_difference_ft)),
  all(abs(checks$distance_difference_ft) < 1e-6),
  all(abs(checks$pair_distance_difference_ft) < 1e-6),
  all(abs(checks$area_difference_sqft) < 1e-6))
write_csv(checks, "../output/candidate_geography_checks.csv")
