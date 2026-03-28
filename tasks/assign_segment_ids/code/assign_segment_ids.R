source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids/code")
# in_pre_scores <- "../input/parcels_pre_scores.csv"
# in_geom <- "../input/parcels_with_geometry.gpkg"
# in_segments <- "../input/boundary_segments_1320ft.gpkg"
# out_lookup <- "../output/parcel_segment_ids.csv"
# out_coverage <- "../output/parcel_segment_ids_coverage.csv"
# out_reason <- "../output/parcel_segment_ids_reason_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(in_pre_scores, in_geom, in_segments, out_lookup, out_coverage, out_reason)
}

if (length(args) >= 6) {
  in_pre_scores <- args[1]
  in_geom <- args[2]
  in_segments <- args[3]
  out_lookup <- args[4]
  out_coverage <- args[5]
  out_reason <- args[6]
} else if (length(args) >= 4) {
  in_pre_scores <- args[1]
  in_geom <- args[2]
  in_segments <- args[3]
  out_lookup <- args[4]
  out_coverage <- "../output/parcel_segment_ids_coverage.csv"
  out_reason <- "../output/parcel_segment_ids_reason_summary.csv"
} else {
  if (!exists("in_pre_scores") || !exists("in_geom") || !exists("in_segments") || !exists("out_lookup")) {
    stop("FATAL: Script requires args: <parcels_pre_scores_csv> <parcels_with_geometry_gpkg> <segment_gpkg> <out_lookup_csv> [<out_coverage_csv>] [<out_reason_csv>]", call. = FALSE)
  }
  if (!exists("out_coverage")) {
    out_coverage <- "../output/parcel_segment_ids_coverage.csv"
  }
  if (!exists("out_reason")) {
    out_reason <- "../output/parcel_segment_ids_reason_summary.csv"
  }
}

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

cat("=== Assign Segment IDs to Parcel PINs ===\n")
cat("Pre-scores:", in_pre_scores, "\n")
cat("Geometry:", in_geom, "\n")
cat("Segments:", in_segments, "\n")
cat("Output lookup:", out_lookup, "\n")
cat("Output coverage:", out_coverage, "\n")
cat("Output reasons:", out_reason, "\n")

pre <- fread(in_pre_scores)
required_pre_cols <- c("pin", "boundary_year", "ward_pair")
missing_pre_cols <- setdiff(required_pre_cols, names(pre))
if (length(missing_pre_cols) > 0) {
  stop(sprintf("parcels_pre_scores missing required columns: %s", paste(missing_pre_cols, collapse = ", ")), call. = FALSE)
}

pre <- pre[, .(
  pin = as.character(pin),
  boundary_year = as.integer(boundary_year),
  ward_pair = as.character(ward_pair),
  construction_year = if ("construction_year" %in% names(pre)) as.integer(construction_year) else NA_integer_,
  dist_to_boundary = if ("dist_to_boundary" %in% names(pre)) as.numeric(dist_to_boundary) else NA_real_
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

joined$segment_reason <- case_when(
  is.na(joined$boundary_year) | is.na(joined$era) ~ "missing_boundary_year_or_era",
  is.na(joined$pair_dash) ~ "missing_pair",
  sf::st_is_empty(joined) ~ "missing_geometry",
  TRUE ~ "pending"
)

needed_eras <- unique(na.omit(joined$era))
segments_by_era <- load_segment_layers(in_segments, buffer_ft = 1000, eras = needed_eras)
segment_id_by_row <- assign_points_to_segments(joined, joined$era, joined$pair_dash, segments_by_era, chunk_n = 50000L)
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
    ifelse(pair_available, "no_polygon_hit", "pair_not_in_segment_layer")
  )
}

lookup <- data.table(
  pin = as.character(joined$pin),
  segment_id = joined$segment_id,
  segment_reason = joined$segment_reason
)

if (nrow(lookup) != nrow(pre)) {
  stop(sprintf("Lookup row mismatch: expected %d rows, got %d.", nrow(pre), nrow(lookup)), call. = FALSE)
}
if (anyDuplicated(lookup$pin) > 0) {
  stop("Lookup has duplicate pin values; expected one row per pin.", call. = FALSE)
}

fwrite(lookup, out_lookup, na = "NA")

diag_dt <- merge(
  copy(pre)[, .(pin, boundary_year, construction_year, dist_to_boundary)],
  lookup,
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
diag_dt[, era := canonical_era_from_boundary_year(boundary_year)]

coverage_parts <- list(
  coverage_block(diag_dt, "all"),
  coverage_block(diag_dt[construction_year >= 2006], "regression_base"),
  coverage_block(diag_dt[construction_year >= 2006 & dist_to_boundary <= 1000], "regression_bw1000"),
  coverage_block(diag_dt[construction_year >= 2006 & dist_to_boundary <= 500], "regression_bw500"),
  coverage_block(diag_dt[construction_year >= 2006 & dist_to_boundary <= 250], "regression_bw250")
)

coverage <- rbindlist(coverage_parts, fill = TRUE)
coverage <- coverage[!is.na(scope)]
setorder(coverage, scope, era)
fwrite(coverage, out_coverage)

reason_summary <- diag_dt[, .(n_obs = .N), by = .(era, segment_reason)]
setorder(reason_summary, era, segment_reason)
fwrite(reason_summary, out_reason)

cat("\nCoverage diagnostics:\n")
print(coverage[scope %in% c("all", "regression_bw1000", "regression_bw500", "regression_bw250")])

cat("\nSaved:\n")
cat(" -", out_lookup, "\n")
cat(" -", out_coverage, "\n")
cat(" -", out_reason, "\n")
