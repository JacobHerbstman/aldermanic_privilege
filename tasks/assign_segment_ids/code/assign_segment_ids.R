source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids/code")
# in_pre_scores <- "../input/parcels_pre_scores.csv"
# in_geom <- "../input/parcels_with_geometry.gpkg"
# in_segments <- "../input/boundary_segments_400m.gpkg"
# out_lookup <- "../output/parcel_segment_ids.csv"
# out_coverage <- "../output/parcel_segment_ids_coverage.csv"
# out_reason <- "../output/parcel_segment_ids_reason_summary.csv"
# out_pair_audit <- "../output/parcel_segment_pair_constraint_audit.csv"
# out_pair_audit_summary <- "../output/parcel_segment_pair_constraint_audit_summary.csv"
# segment_buffer_m <- 250
# coverage_bandwidths_m <- "100,250"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_pre_scores, in_geom, in_segments,
    out_lookup, out_coverage, out_reason,
    out_pair_audit, out_pair_audit_summary,
    segment_buffer_m, coverage_bandwidths_m
  )
}

if (length(cli_args) != 10) {
  stop(
    "FATAL: Script requires 10 args: <parcels_pre_scores_csv> <parcels_with_geometry_gpkg> <segment_gpkg> <out_lookup_csv> <out_coverage_csv> <out_reason_csv> <out_pair_audit_csv> <out_pair_audit_summary_csv> <segment_buffer_m> <coverage_bandwidths_m>",
    call. = FALSE
  )
}

in_pre_scores <- cli_args[1]
in_geom <- cli_args[2]
in_segments <- cli_args[3]
out_lookup <- cli_args[4]
out_coverage <- cli_args[5]
out_reason <- cli_args[6]
out_pair_audit <- cli_args[7]
out_pair_audit_summary <- cli_args[8]
segment_buffer_m <- as.numeric(cli_args[9])
coverage_bandwidths_m <- scan(text = gsub(",", " ", cli_args[10], fixed = TRUE), quiet = TRUE)

if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}
if (length(coverage_bandwidths_m) == 0 || any(!is.finite(coverage_bandwidths_m)) || any(coverage_bandwidths_m <= 0)) {
  stop("coverage_bandwidths_m must contain positive numeric bandwidths.", call. = FALSE)
}
coverage_bandwidths_m <- sort(unique(as.numeric(coverage_bandwidths_m)))

stopifnot(
  file.exists(in_pre_scores),
  file.exists(in_geom),
  file.exists(in_segments)
)

coverage_row <- function(scope, era, dt) {
  matched <- !is.na(dt$segment_id) & dt$segment_id != ""
  n_total <- nrow(dt)
  n_matched <- sum(matched)
  data.table(
    scope = scope,
    era = era,
    n_obs = n_total,
    n_matched = n_matched,
    coverage_rate = ifelse(n_total > 0, n_matched / n_total, NA_real_)
  )
}

coverage_block <- function(dt, scope) {
  out <- list(
    coverage_row(scope, "all", dt)
  )
  era_vals <- sort(unique(na.omit(dt$era)))
  if (length(era_vals) > 0) {
    out <- c(
      out,
      lapply(era_vals, function(ei) coverage_row(scope, ei, dt[era == ei]))
    )
  }
  rbindlist(out, fill = TRUE)
}

audit_summary_row <- function(scope, era, dt) {
  n_total <- nrow(dt)
  constrained_assigned <- !is.na(dt$constrained_segment_id) & dt$constrained_segment_id != ""
  unconstrained_assigned <- !is.na(dt$unconstrained_segment_id) & dt$unconstrained_segment_id != ""
  both_assigned <- constrained_assigned & unconstrained_assigned
  unconstrained_pair_diff <- both_assigned & !dt$unconstrained_pair_matches_input
  segment_diff <- both_assigned & !dt$unconstrained_matches_constrained_segment
  extra_dist <- dt$constrained_extra_dist_m[both_assigned & is.finite(dt$constrained_extra_dist_m)]

  data.table(
    scope = scope,
    era = era,
    n_obs = n_total,
    n_constrained_assigned = sum(constrained_assigned),
    n_unconstrained_assigned = sum(unconstrained_assigned),
    n_unconstrained_within_radius = sum(dt$unconstrained_within_radius, na.rm = TRUE),
    n_unconstrained_pair_diff = sum(unconstrained_pair_diff, na.rm = TRUE),
    unconstrained_pair_diff_share = ifelse(sum(both_assigned) > 0, sum(unconstrained_pair_diff, na.rm = TRUE) / sum(both_assigned), NA_real_),
    n_segment_diff = sum(segment_diff, na.rm = TRUE),
    segment_diff_share = ifelse(sum(both_assigned) > 0, sum(segment_diff, na.rm = TRUE) / sum(both_assigned), NA_real_),
    mean_extra_dist_m = ifelse(length(extra_dist) > 0, mean(extra_dist), NA_real_),
    p95_extra_dist_m = ifelse(length(extra_dist) > 0, as.numeric(quantile(extra_dist, probs = 0.95, names = FALSE)), NA_real_),
    max_extra_dist_m = ifelse(length(extra_dist) > 0, max(extra_dist), NA_real_)
  )
}

audit_summary_block <- function(dt, scope) {
  out <- list(audit_summary_row(scope, "all", dt))
  era_vals <- sort(unique(na.omit(dt$era)))
  if (length(era_vals) > 0) {
    out <- c(
      out,
      lapply(era_vals, function(ei) audit_summary_row(scope, ei, dt[era == ei]))
    )
  }
  rbindlist(out, fill = TRUE)
}

cat("=== Assign Segment IDs to Parcel PINs ===\n")
cat("Pre-scores:", in_pre_scores, "\n")
cat("Geometry:", in_geom, "\n")
cat("Segments:", in_segments, "\n")
cat("Output lookup:", out_lookup, "\n")
cat("Output coverage:", out_coverage, "\n")
cat("Output reasons:", out_reason, "\n")
cat("Output pair audit:", out_pair_audit, "\n")
cat("Output pair audit summary:", out_pair_audit_summary, "\n")
cat("Segment buffer:", segment_buffer_m, "m\n")
cat("Coverage bandwidths:", paste0(coverage_bandwidths_m, "m", collapse = ", "), "\n")

pre <- fread(in_pre_scores, colClasses = c(pin = "character"))
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

geom_sf <- st_read(in_geom, quiet = TRUE)
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
  is.na(joined$pair_dash) ~ "missing_pair",
  joined_missing_geometry ~ "missing_geometry",
  TRUE ~ "pending"
)

needed_eras <- unique(na.omit(joined$era))
segments_by_era <- load_segment_line_layers(in_segments, eras = needed_eras)
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
  pair_available <- logical(length(pending_idx))
  for (era_i in unique(joined$era[pending_idx])) {
    seg_era <- segments_by_era[[era_i]]
    idx_era <- which(joined$era[pending_idx] == era_i)
    if (length(idx_era) == 0) next
    if (is.null(seg_era) || nrow(seg_era) == 0) next
    pair_available[idx_era] <- joined$pair_dash[pending_idx[idx_era]] %in% unique(seg_era$pair_dash)
  }

  joined$segment_reason[pending_idx] <- ifelse(
    !is.na(joined$segment_id[pending_idx]) & joined$segment_id[pending_idx] != "",
    "matched",
    ifelse(pair_available, "no_nearest_segment_within_radius", "pair_not_in_segment_layer")
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

if (nrow(lookup) != nrow(pre)) {
  stop(sprintf("Lookup row mismatch: expected %d rows, got %d.", nrow(pre), nrow(lookup)), call. = FALSE)
}
if (anyDuplicated(lookup$pin) > 0) {
  stop("Lookup has duplicate pin values; expected one row per pin.", call. = FALSE)
}

fwrite(lookup, out_lookup, na = "NA")
fwrite(pair_audit, out_pair_audit, na = "NA")

diag_dt <- merge(
  copy(pre)[, .(pin, boundary_year, construction_year, dist_to_boundary_m)],
  lookup,
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
diag_dt[, era := canonical_era_from_boundary_year(boundary_year)]

coverage_parts <- c(
  list(
    coverage_block(diag_dt, "all"),
    coverage_block(diag_dt[construction_year >= 2006], "regression_base")
  ),
  lapply(coverage_bandwidths_m, function(bw_m_i) {
    coverage_block(
      diag_dt[construction_year >= 2006 & dist_to_boundary_m <= bw_m_i],
      sprintf("regression_bw%.0fm", bw_m_i)
    )
  })
)

coverage <- rbindlist(coverage_parts, fill = TRUE)
coverage <- coverage[!is.na(scope)]
setorder(coverage, scope, era)
fwrite(coverage, out_coverage)

reason_summary <- diag_dt[, .(n_obs = .N), by = .(era, segment_reason)]
setorder(reason_summary, era, segment_reason)
fwrite(reason_summary, out_reason)

pair_audit_summary_parts <- c(
  list(
    audit_summary_block(pair_audit, "all"),
    audit_summary_block(pair_audit[construction_year >= 2006], "regression_base")
  ),
  lapply(coverage_bandwidths_m, function(bw_m_i) {
    audit_summary_block(
      pair_audit[construction_year >= 2006 & dist_to_boundary_m <= bw_m_i],
      sprintf("regression_bw%.0fm", bw_m_i)
    )
  })
)
pair_audit_summary <- rbindlist(pair_audit_summary_parts, fill = TRUE)
pair_audit_summary <- pair_audit_summary[!is.na(scope)]
setorder(pair_audit_summary, scope, era)
fwrite(pair_audit_summary, out_pair_audit_summary, na = "NA")

cat("\nCoverage diagnostics:\n")
print(coverage[scope %in% c("all", sprintf("regression_bw%.0fm", coverage_bandwidths_m))])

cat("\nConstrained vs. unconstrained segment audit:\n")
print(pair_audit_summary[scope %in% c("all", sprintf("regression_bw%.0fm", coverage_bandwidths_m))])

cat("\nSaved:\n")
cat(" -", out_lookup, "\n")
cat(" -", out_coverage, "\n")
cat(" -", out_reason, "\n")
cat(" -", out_pair_audit, "\n")
cat(" -", out_pair_audit_summary, "\n")
