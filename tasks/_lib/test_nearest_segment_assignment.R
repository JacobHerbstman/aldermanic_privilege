library(sf)
library(units)

source("canonical_geometry_helpers.R")

sf_use_s2(FALSE)

era <- "2003_2014"
pair_id <- "1-2"
crs_chicago <- 3435
m_to_usft <- function(x) as.numeric(set_units(set_units(x, "m"), "US_survey_foot"))

segment_line <- function(x_m, y0_m = -100, y1_m = 100) {
  st_linestring(matrix(
    c(m_to_usft(x_m), m_to_usft(y0_m), m_to_usft(x_m), m_to_usft(y1_m)),
    ncol = 2,
    byrow = TRUE
  ))
}

point_xy <- function(x_m, y_m = 0) {
  st_point(c(m_to_usft(x_m), m_to_usft(y_m)))
}

segments_threshold <- st_sf(
  segment_id = "seg_a",
  ward_pair_id = "1_2",
  geometry = st_sfc(segment_line(0), crs = crs_chicago)
)
segments_threshold$pair_dash <- normalize_pair_dash(segments_threshold$ward_pair_id)

pts_threshold <- st_sf(
  geometry = st_sfc(point_xy(249), point_xy(251), crs = crs_chicago)
)
threshold_result <- assign_points_to_nearest_segments(
  pts_threshold,
  rep(era, 2),
  rep(pair_id, 2),
  setNames(list(segments_threshold), era),
  max_distance = set_units(250, "m"),
  chunk_n = 1
)
stopifnot(identical(threshold_result, c("seg_a", NA_character_)))

segments_tie <- st_sf(
  segment_id = c("seg_b", "seg_a"),
  ward_pair_id = c("1_2", "1_2"),
  geometry = st_sfc(segment_line(10), segment_line(-10), crs = crs_chicago)
)
segments_tie$pair_dash <- normalize_pair_dash(segments_tie$ward_pair_id)
pts_tie <- st_sf(
  geometry = st_sfc(point_xy(0), point_xy(-9), point_xy(9), crs = crs_chicago)
)

tie_layers_forward <- setNames(list(segments_tie), era)
tie_layers_reverse <- setNames(list(segments_tie[2:1, ]), era)
tie_forward <- assign_points_to_nearest_segments(
  pts_tie,
  rep(era, 3),
  rep(pair_id, 3),
  tie_layers_forward,
  max_distance = set_units(50, "m"),
  chunk_n = 1
)
tie_reverse <- assign_points_to_nearest_segments(
  pts_tie,
  rep(era, 3),
  rep(pair_id, 3),
  tie_layers_reverse,
  max_distance = set_units(50, "m"),
  chunk_n = 2
)
stopifnot(identical(tie_forward, c("seg_a", "seg_a", "seg_b")))
stopifnot(identical(tie_forward, tie_reverse))

tie_chunk_17 <- assign_points_to_nearest_segments(
  pts_tie,
  rep(era, 3),
  rep(pair_id, 3),
  tie_layers_reverse,
  max_distance = set_units(50, "m"),
  chunk_n = 17
)
tie_chunk_large <- assign_points_to_nearest_segments(
  pts_tie,
  rep(era, 3),
  rep(pair_id, 3),
  tie_layers_reverse,
  max_distance = set_units(50, "m"),
  chunk_n = 50000
)
stopifnot(identical(tie_forward, tie_chunk_17))
stopifnot(identical(tie_forward, tie_chunk_large))

segments_global <- st_sf(
  segment_id = c("seg_other", "seg_left", "seg_right"),
  ward_pair_id = c("1_3", "1_2", "1_2"),
  geometry = st_sfc(segment_line(0), segment_line(-10), segment_line(10), crs = crs_chicago)
)
segments_global$pair_dash <- normalize_pair_dash(segments_global$ward_pair_id)
global_layers <- setNames(list(segments_global), era)
pts_global <- st_sf(
  geometry = st_sfc(point_xy(0), point_xy(0), crs = crs_chicago)
)

pair_constrained_result <- assign_points_to_nearest_segments(
  pts_global,
  rep(era, 2),
  c("1-2", "1-3"),
  global_layers,
  max_distance = set_units(50, "m"),
  chunk_n = 1
)
stopifnot(identical(pair_constrained_result, c("seg_left", "seg_other")))

missing_pair_result <- assign_points_to_nearest_segments(
  pts_global,
  rep(era, 2),
  c(NA_character_, "1-3"),
  global_layers,
  max_distance = set_units(50, "m"),
  chunk_n = 1
)
stopifnot(identical(missing_pair_result, c(NA_character_, "seg_other")))

missing_segment_pair_result <- assign_points_to_nearest_segments(
  pts_global,
  rep(era, 2),
  c("9-10", "1-3"),
  global_layers,
  max_distance = set_units(50, "m"),
  chunk_n = 1
)
stopifnot(identical(missing_segment_pair_result, c(NA_character_, "seg_other")))

pair_audit <- audit_nearest_segment_pair_constraints(
  pts_global,
  rep(era, 2),
  c("1-2", "1-3"),
  global_layers,
  constrained_segment_id = pair_constrained_result,
  max_distance = set_units(50, "m"),
  chunk_n = 1
)
stopifnot(identical(pair_audit$constrained_segment_id, c("seg_left", "seg_other")))
stopifnot(identical(pair_audit$unconstrained_segment_id, c("seg_other", "seg_other")))
stopifnot(identical(pair_audit$constrained_pair_matches_input, c(TRUE, TRUE)))
stopifnot(identical(pair_audit$unconstrained_pair_matches_input, c(FALSE, TRUE)))
stopifnot(identical(pair_audit$unconstrained_matches_constrained_segment, c(FALSE, TRUE)))
stopifnot(isTRUE(abs(pair_audit$constrained_extra_dist_m[1] - 10) < 1e-6))
stopifnot(isTRUE(abs(pair_audit$constrained_extra_dist_m[2]) < 1e-6))

bare_distance_error <- inherits(
  try(assign_points_to_nearest_segments(
    pts_tie,
    rep(era, 3),
    rep(pair_id, 3),
    tie_layers_forward,
    max_distance = 50
  ), silent = TRUE),
  "try-error"
)
stopifnot(bare_distance_error)

point_geometry_error <- inherits(
  try(assign_points_to_nearest_segments(
    st_sf(
      geometry = st_sfc(st_polygon(list(matrix(
        c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
        ncol = 2,
        byrow = TRUE
      ))), crs = crs_chicago)
    ),
    era,
    pair_id,
    tie_layers_forward,
    max_distance = set_units(50, "m")
  ), silent = TRUE),
  "try-error"
)
stopifnot(point_geometry_error)

deprecated_error <- inherits(
  try(assign_points_to_segments(
    pts_tie,
    rep(era, 3),
    rep(pair_id, 3),
    tie_layers_forward
  ), silent = TRUE),
  "try-error"
)
stopifnot(deprecated_error)

tmp_lines <- tempfile(fileext = ".gpkg")
st_write(segments_tie, tmp_lines, layer = era, quiet = TRUE)
loaded_lines <- load_segment_line_layers(tmp_lines, eras = era)
stopifnot(identical(loaded_lines[[era]]$segment_id, c("seg_a", "seg_b")))

tmp_dupes <- tempfile(fileext = ".gpkg")
segments_dupes <- segments_tie
segments_dupes$segment_id <- "seg_a"
st_write(segments_dupes, tmp_dupes, layer = era, quiet = TRUE)
dupe_error <- inherits(
  try(load_segment_line_layers(tmp_dupes, eras = era), silent = TRUE),
  "try-error"
)
stopifnot(dupe_error)

tmp_poly <- tempfile(fileext = ".gpkg")
poly_layer <- st_sf(
  segment_id = "seg_a",
  ward_pair_id = "1_2",
  geometry = st_sfc(st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  ))), crs = crs_chicago)
)
st_write(poly_layer, tmp_poly, layer = era, quiet = TRUE)
polygon_error <- inherits(
  try(load_segment_line_layers(tmp_poly, eras = era), silent = TRUE),
  "try-error"
)
stopifnot(polygon_error)

cat("Nearest segment assignment tests passed.\n")
