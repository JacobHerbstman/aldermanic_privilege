source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids/code")
# segment_length_ft <- 1320
# segment_buffer_m <- 250

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_length_ft, segment_buffer_m)
}

if (length(cli_args) != 2) {
  stop(
    "FATAL: Script requires 2 args: <segment_length_ft> <segment_buffer_m>",
    call. = FALSE
  )
}

segment_length_ft <- suppressWarnings(as.integer(cli_args[1]))
segment_buffer_m <- as.numeric(cli_args[2])

if (!is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("segment_length_ft must be a positive integer.", call. = FALSE)
}
if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}

pre <- fread("../input/parcels_pre_scores.csv", colClasses = c(pin = "character"))
required_pre_cols <- c("pin", "boundary_year", "ward_pair", "dist_to_boundary_m")
missing_pre_cols <- setdiff(required_pre_cols, names(pre))
if (length(missing_pre_cols) > 0) {
  stop(sprintf("parcels_pre_scores missing required columns: %s", paste(missing_pre_cols, collapse = ", ")), call. = FALSE)
}

pre <- pre[, .(
  pin = as.character(pin),
  boundary_year = as.integer(boundary_year),
  ward_pair = as.character(ward_pair),
  construction_year = if ("construction_year" %in% names(pre)) as.integer(construction_year) else NA_integer_,
  dist_to_boundary_m = as.numeric(dist_to_boundary_m)
)]

if (anyDuplicated(pre$pin) > 0) {
  stop("parcels_pre_scores has duplicate pin values; expected one row per pin.", call. = FALSE)
}

geom_sf <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE)
if (!("pin" %in% names(geom_sf))) {
  stop("parcels_with_geometry.gpkg is missing pin column.", call. = FALSE)
}

geom_sf$pin <- as.character(geom_sf$pin)
if (anyDuplicated(geom_sf$pin) > 0) {
  stop("parcels_with_geometry.gpkg has duplicate pin values; expected one row per pin.", call. = FALSE)
}
geom_sf <- geom_sf[, c("pin"), drop = FALSE]

joined <- geom_sf %>%
  right_join(pre, by = "pin") %>%
  mutate(
    pair_dash = normalize_pair_dash(ward_pair),
    era = canonical_era_from_boundary_year(boundary_year)
  )

joined_missing_geometry <- assert_point_geometries(joined, "joined parcel geometry")
joined$segment_reason <- case_when(
  is.na(joined$boundary_year) | is.na(joined$era) ~ "missing_boundary_year_or_era",
  is.na(joined$pair_dash) ~ "missing_or_invalid_ward_pair",
  joined_missing_geometry ~ "missing_geometry",
  TRUE ~ "pending"
)

needed_eras <- unique(na.omit(joined$era))
segments_by_era <- load_segment_line_layers(sprintf("../input/boundary_segments_%dft.gpkg", segment_length_ft), eras = needed_eras)
segment_metadata <- segment_metadata_from_layers(segments_by_era)
segment_metadata_key <- paste(segment_metadata$era, segment_metadata$segment_id, sep = "\r")
segment_id_by_row <- assign_points_to_nearest_segments(
  joined,
  joined$era,
  joined$pair_dash,
  segments_by_era,
  max_distance = units::set_units(segment_buffer_m, "m"),
  chunk_n = 50000L
)
joined$segment_id <- segment_id_by_row

pending_idx <- which(joined$segment_reason == "pending")
if (length(pending_idx) > 0) {
  joined$segment_reason[pending_idx] <- ifelse(
    !is.na(joined$segment_id[pending_idx]) & joined$segment_id[pending_idx] != "",
    "matched",
    "no_nearest_segment_within_radius"
  )
}

pair_audit <- as.data.table(audit_nearest_segment_pair_constraints(
  joined,
  joined$era,
  joined$pair_dash,
  segments_by_era,
  constrained_segment_id = joined$segment_id,
  max_distance = units::set_units(segment_buffer_m, "m"),
  chunk_n = 50000L
))
pair_audit <- cbind(
  data.table(
    pin = as.character(joined$pin),
    boundary_year = joined$boundary_year,
    construction_year = joined$construction_year,
    dist_to_boundary_m = joined$dist_to_boundary_m,
    era = joined$era,
    input_pair_dash = joined$pair_dash,
    segment_reason = joined$segment_reason
  ),
  pair_audit
)

assigned_segment <- !is.na(pair_audit$constrained_segment_id) &
  pair_audit$constrained_segment_id != ""
pair_mismatch <- assigned_segment & !pair_audit$constrained_pair_matches_input
if (any(pair_mismatch, na.rm = TRUE)) {
  stop(sprintf(
    "Production segment assignment is not in the input ward pair for %d rows.",
    sum(pair_mismatch, na.rm = TRUE)
  ), call. = FALSE)
}

distance_tolerance_m <- 0.05
missing_segment_distance <- assigned_segment & !is.finite(pair_audit$constrained_segment_dist_m)
if (any(missing_segment_distance, na.rm = TRUE)) {
  stop(sprintf(
    "Production segment assignment has missing segment distance for %d rows.",
    sum(missing_segment_distance, na.rm = TRUE)
  ), call. = FALSE)
}

outside_buffer <- assigned_segment &
  pair_audit$constrained_segment_dist_m > segment_buffer_m + distance_tolerance_m
if (any(outside_buffer, na.rm = TRUE)) {
  stop(sprintf(
    "Production segment assignment is outside the %.0fm radius for %d rows.",
    segment_buffer_m,
    sum(outside_buffer, na.rm = TRUE)
  ), call. = FALSE)
}

distance_mismatch <- assigned_segment &
  is.finite(pair_audit$dist_to_boundary_m) &
  abs(pair_audit$constrained_segment_dist_m - pair_audit$dist_to_boundary_m) > distance_tolerance_m
if (any(distance_mismatch, na.rm = TRUE)) {
  stop(sprintf(
    "Production segment distance differs from nearest boundary distance for %d rows.",
    sum(distance_mismatch, na.rm = TRUE)
  ), call. = FALSE)
}

lookup <- data.table(
  pin = as.character(joined$pin),
  segment_id = joined$segment_id,
  dist_to_segment_m = ifelse(
    !is.na(joined$segment_id) & joined$segment_id != "",
    pair_audit$constrained_segment_dist_m,
    NA_real_
  ),
  segment_reason = joined$segment_reason
)
lookup_segment_idx <- match(paste(joined$era, lookup$segment_id, sep = "\r"), segment_metadata_key)
lookup[, analysis_segment_id := segment_metadata$analysis_segment_id[lookup_segment_idx]]
lookup[, valid_segment := segment_metadata$valid_segment[lookup_segment_idx]]
lookup[, invalid_reason := segment_metadata$invalid_reason[lookup_segment_idx]]
lookup[, segment_length_ft := segment_metadata$segment_length_ft[lookup_segment_idx]]
lookup[, segment_lt500ft := segment_metadata$segment_lt500ft[lookup_segment_idx]]
lookup[, segment_lt1000ft := segment_metadata$segment_lt1000ft[lookup_segment_idx]]

if (nrow(lookup) != nrow(pre)) {
  stop(sprintf("Lookup row mismatch: expected %d rows, got %d.", nrow(pre), nrow(lookup)), call. = FALSE)
}
if (anyDuplicated(lookup$pin) > 0) {
  stop("Lookup has duplicate pin values; expected one row per pin.", call. = FALSE)
}

fwrite(lookup, "../output/parcel_segment_ids.csv", na = "NA")
