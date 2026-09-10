# setwd("tasks/density_boundary_characteristics/code")
source("../../setup_environment/code/packages.R")
projects <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = cols(project_id = "c", ward_pair = "c", segment_id = "c", .default = col_guess()))
stopifnot(!anyDuplicated(projects$project_id), all(is.finite(projects$x_3435)), all(is.finite(projects$y_3435)))
points <- st_as_sf(projects, coords = c("x_3435", "y_3435"), crs = 3435)
boundaries <- do.call(rbind, lapply(c("2003_2014", "2015_2023"), function(era) {
  st_read("../input/ward_pair_boundaries.gpkg", layer = era, quiet = TRUE) |> st_transform(3435)
}))
stopifnot(!anyDuplicated(st_drop_geometry(boundaries)[c("era", "ward_pair_id")]))
points$reconstructed_distance_ft <- NA_real_
points$left_endpoint_distance_m <- NA_real_
points$right_endpoint_distance_m <- NA_real_

location_groups <- interaction(
  points$era,
  points$ward_pair,
  drop = TRUE,
  lex.order = TRUE
)

for (idx in split(seq_len(nrow(points)), location_groups)) {
  boundary_row <- boundaries |>
    dplyr::filter(
      era == points$era[idx[1]],
      ward_pair_id == points$ward_pair[idx[1]]
    )
  if (nrow(boundary_row) != 1L) {
    stop("Could not identify one ward-pair boundary for a price location.")
  }

  repeated_boundary <- sf::st_sfc(
    rep(
      list(sf::st_geometry(boundary_row)[[1]]),
      length(idx)
    ),
    crs = sf::st_crs(boundary_row)
  )
  nearest_lines <- sf::st_nearest_points(
    sf::st_geometry(points[idx, ]),
    repeated_boundary,
    pairwise = TRUE
  )

  point_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  boundary_xy <- matrix(NA_real_, nrow = length(idx), ncol = 2)
  for (j in seq_along(idx)) {
    nearest_coordinates <- sf::st_coordinates(nearest_lines[j])
    point_xy[j, ] <- nearest_coordinates[1, c("X", "Y")]
    boundary_xy[j, ] <- nearest_coordinates[
      nrow(nearest_coordinates),
      c("X", "Y")
    ]
  }

  normal_vector <- boundary_xy - point_xy
  normal_length <- sqrt(rowSums(normal_vector^2))
  if (any(!is.finite(normal_length) | normal_length <= 0)) {
    stop("A price observation lies directly on its assigned boundary.")
  }

  tangent_unit <- cbind(
    -normal_vector[, 2] / normal_length,
    normal_vector[, 1] / normal_length
  )
  half_line_ft <- 50 / 0.3048
  left_xy <- boundary_xy - half_line_ft * tangent_unit
  right_xy <- boundary_xy + half_line_ft * tangent_unit

  left_points <- sf::st_sfc(
    lapply(
      seq_len(nrow(left_xy)),
      function(j) sf::st_point(left_xy[j, ])
    ),
    crs = sf::st_crs(boundary_row)
  )
  right_points <- sf::st_sfc(
    lapply(
      seq_len(nrow(right_xy)),
      function(j) sf::st_point(right_xy[j, ])
    ),
    crs = sf::st_crs(boundary_row)
  )

  points$reconstructed_distance_ft[idx] <- normal_length
  points$left_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(left_points, sf::st_geometry(boundary_row))
  ) * 0.3048
  points$right_endpoint_distance_m[idx] <- as.numeric(
    sf::st_distance(right_points, sf::st_geometry(boundary_row))
  ) * 0.3048
}


points$straight_boundary <- points$left_endpoint_distance_m <= 15 & points$right_endpoint_distance_m <= 15
stopifnot(max(abs(points$reconstructed_distance_ft - points$distance_to_boundary_ft)) < 2)
cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) |> st_transform(3435)
points$distance_to_cbd_miles <- as.numeric(st_distance(points, cbd)) / 5280
schools <- st_read("../input/schools_2015.gpkg", quiet = TRUE) |> st_transform(3435) |> st_make_valid()
parks <- st_read("../input/parks.gpkg", quiet = TRUE) |> st_transform(3435) |> st_make_valid()
water <- st_read("../input/gis_osm_water_a_free_1.shp", quiet = TRUE) |> st_zm() |> st_transform(3435) |> st_make_valid()
lake <- water |> filter(tolower(name) == "lake michigan") |> st_union() |> st_boundary() |> st_simplify(dTolerance = 50)
stopifnot(length(lake) == 1, !st_is_empty(lake))
points$distance_to_lake_miles <- as.numeric(st_distance(points, lake)) / 5280
for (amenity in c("school", "park")) {
  target <- if (amenity == "school") schools else parks
  nearest <- st_nearest_feature(points, target)
  points[[paste0("distance_to_", amenity, "_miles")]] <- as.numeric(st_distance(points, target[nearest, ], by_element = TRUE)) / 5280
}
features <- read_csv("../output/boundary_feature_measurements.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(features$segment_id))
result <- st_drop_geometry(points) |> left_join(features, by = "segment_id", relationship = "many-to-one") |>
  mutate(expressway_share = expressway_overlap_ft / segment_length_ft,
    waterway_share = waterway_overlap_ft / segment_length_ft,
    physical_share = pmin(1, pmin(1, water_area_share + park_area_share + waterway_share) + cemetery_area_share),
    simple_overlap_keep = pmax(expressway_share, waterway_share, water_area_share) < 0.50,
    share_based_keep = physical_share < 0.50 & expressway_share < 0.40 & major_overlap_arterial_ft / segment_length_ft < 0.75) |>
  select(project_id, straight_boundary, simple_overlap_keep, share_based_keep,
    distance_to_cbd_miles, distance_to_school_miles, distance_to_park_miles, distance_to_lake_miles)
stopifnot(!anyNA(result))
write_csv(result, "../output/density_boundary_characteristics.csv")
