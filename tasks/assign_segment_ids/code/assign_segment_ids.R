source("../../setup_environment/code/packages.R")

library(data.table)
library(sf)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids/code")
# in_pre_scores <- "../input/parcels_pre_scores.csv"
# in_geom <- "../input/parcels_with_geometry.gpkg"
# in_segments <- "../input/boundary_segments_1320ft.gpkg"
# out_lookup <- "../output/parcel_segment_ids.csv"
# out_coverage <- "../output/parcel_segment_ids_coverage.csv"
# source("assign_segment_ids.R")
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 5) {
  in_pre_scores <- args[1]
  in_geom <- args[2]
  in_segments <- args[3]
  out_lookup <- args[4]
  out_coverage <- args[5]
} else if (length(args) >= 4) {
  in_pre_scores <- args[1]
  in_geom <- args[2]
  in_segments <- args[3]
  out_lookup <- args[4]
  out_coverage <- "../output/parcel_segment_ids_coverage.csv"
} else {
  if (!exists("in_pre_scores") || !exists("in_geom") || !exists("in_segments") || !exists("out_lookup")) {
    stop("FATAL: Script requires args: <parcels_pre_scores_csv> <parcels_with_geometry_gpkg> <segment_gpkg> <out_lookup_csv> [<out_coverage_csv>]", call. = FALSE)
  }
  if (!exists("out_coverage")) {
    out_coverage <- "../output/parcel_segment_ids_coverage.csv"
  }
}

stopifnot(
  file.exists(in_pre_scores),
  file.exists(in_geom),
  file.exists(in_segments)
)

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

map_era <- function(boundary_year) {
  boundary_year <- as.integer(boundary_year)
  fifelse(
    boundary_year == 1998L, "1998_2002",
    fifelse(
      boundary_year == 2003L, "2003_2014",
      fifelse(boundary_year == 2015L, "2015_2023", fifelse(boundary_year == 2024L, "post_2023", NA_character_))
    )
  )
}

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
    row_id = row_number(),
    pair_dash = normalize_pair_dash(ward_pair),
    era = map_era(boundary_year)
  )

segment_layers <- st_layers(in_segments)$name
needed_eras <- unique(na.omit(joined$era))
needed_layers <- paste0(needed_eras, "_bw1000")
missing_layers <- setdiff(needed_layers, segment_layers)
if (length(missing_layers) > 0) {
  stop(sprintf("Segment GPKG missing expected bw1000 layers: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
}

segments_by_era <- list()
for (era_i in needed_eras) {
  layer_name <- paste0(era_i, "_bw1000")
  seg <- st_read(in_segments, layer = layer_name, quiet = TRUE)
  if (!all(c("segment_id", "ward_pair_id") %in% names(seg))) {
    stop(sprintf("Layer '%s' missing required columns segment_id/ward_pair_id.", layer_name), call. = FALSE)
  }
  seg <- seg[, c("segment_id", "ward_pair_id"), drop = FALSE]
  seg$segment_id <- as.character(seg$segment_id)
  seg$pair_dash <- normalize_pair_dash(seg$ward_pair_id)
  seg <- seg[!is.na(seg$pair_dash), ]
  segments_by_era[[era_i]] <- seg
}

geometry_ok <- !is.na(st_is_empty(joined)) & !st_is_empty(joined)
assignable <- joined[geometry_ok & !is.na(joined$era) & !is.na(joined$pair_dash), ]

segment_id_by_row <- rep(NA_character_, nrow(joined))

for (era_i in needed_eras) {
  seg_era <- segments_by_era[[era_i]]
  d_era <- assignable[assignable$era == era_i, ]
  if (nrow(d_era) == 0 || is.null(seg_era) || nrow(seg_era) == 0) {
    next
  }

  if (st_crs(d_era) != st_crs(seg_era)) {
    d_era <- st_transform(d_era, st_crs(seg_era))
  }

  valid_pairs <- intersect(unique(d_era$pair_dash), unique(seg_era$pair_dash))
  pair_count <- length(valid_pairs)
  if (pair_count == 0) next

  cat(sprintf("Assigning segments for era %s across %d ward-pairs...\n", era_i, pair_count))

  for (j in seq_along(valid_pairs)) {
    pair_j <- valid_pairs[j]
    pts_pair <- d_era[d_era$pair_dash == pair_j, ]
    seg_pair <- seg_era[seg_era$pair_dash == pair_j, ]
    if (nrow(pts_pair) == 0 || nrow(seg_pair) == 0) next

    hits <- st_within(pts_pair, seg_pair)
    hit_idx <- vapply(hits, function(v) {
      if (length(v) == 0) return(NA_integer_)
      v[1]
    }, integer(1))

    ok <- !is.na(hit_idx)
    if (any(ok)) {
      row_ids <- pts_pair$row_id[ok]
      segment_id_by_row[row_ids] <- as.character(seg_pair$segment_id[hit_idx[ok]])
    }

    if (j %% 25L == 0L || j == pair_count) {
      cat(sprintf("  era %s: processed %d / %d pairs\n", era_i, j, pair_count))
    }
  }
}

lookup <- data.table(
  pin = as.character(joined$pin),
  segment_id = segment_id_by_row
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
diag_dt[, era := map_era(boundary_year)]

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

cat("\nCoverage diagnostics:\n")
print(coverage[scope %in% c("all", "regression_bw1000", "regression_bw500", "regression_bw250")])

cat("\nSaved:\n")
cat(" -", out_lookup, "\n")
cat(" -", out_coverage, "\n")
