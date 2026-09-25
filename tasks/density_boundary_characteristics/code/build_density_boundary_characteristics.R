# setwd("tasks/density_boundary_characteristics/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")
# A boundary segment is dropped from the simple restriction when an expressway, waterway or water area overlaps at
# least half of it, and from the share-based restriction when parks, water and cemeteries cover half of it, an
# expressway 40 percent or an arterial 75 percent.
simple_overlap_max <- 0.50
physical_share_max <- 0.50
expressway_share_max <- 0.40
arterial_share_max <- 0.75
cbd_lon_lat <- c(-87.6313, 41.8837)
lake_simplify_ft <- 50

buildings <- read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = cols(building_id = "c", ward_pair = "c", segment_id = "c", .default = col_guess()))
stopifnot(!anyDuplicated(buildings$building_id), all(is.finite(buildings$x_3435)), all(is.finite(buildings$y_3435)))
points <- st_as_sf(buildings, coords = c("x_3435", "y_3435"), crs = 3435)
boundaries <- do.call(rbind, lapply(c("2003_2014", "2015_2023"), function(era) {
  st_read("../input/ward_pair_boundaries.gpkg", layer = era, quiet = TRUE) |> st_transform(3435)
}))
stopifnot(!anyDuplicated(st_drop_geometry(boundaries)[c("era", "ward_pair_id")]))
straightness <- boundary_straightness(points, boundaries)
points$straight_boundary <- straightness$straight_boundary
stopifnot(max(abs(straightness$reconstructed_distance_ft - points$distance_to_boundary_ft)) < 2)
cbd <- st_sfc(st_point(cbd_lon_lat), crs = 4326) |> st_transform(3435)
points$distance_to_cbd_miles <- as.numeric(st_distance(points, cbd)) / 5280
schools <- st_read("../input/schools_2015.gpkg", quiet = TRUE) |> st_transform(3435) |> st_make_valid()
parks <- st_read("../input/parks.gpkg", quiet = TRUE) |> st_transform(3435) |> st_make_valid()
water <- st_read("../input/gis_osm_water_a_free_1.shp", quiet = TRUE) |> st_zm() |> st_transform(3435) |> st_make_valid()
lake <- water |> filter(tolower(name) == "lake michigan") |> st_union() |> st_boundary() |> st_simplify(dTolerance = lake_simplify_ft)
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
    simple_overlap_keep = pmax(expressway_share, waterway_share, water_area_share) < simple_overlap_max,
    share_based_keep = physical_share < physical_share_max & expressway_share < expressway_share_max &
      major_overlap_arterial_ft / segment_length_ft < arterial_share_max) |>
  select(building_id, straight_boundary, simple_overlap_keep, share_based_keep,
    distance_to_cbd_miles, distance_to_school_miles, distance_to_park_miles, distance_to_lake_miles)
stopifnot(!anyNA(result))
SaveData(result, c("building_id"), "../output/density_boundary_characteristics.csv")
