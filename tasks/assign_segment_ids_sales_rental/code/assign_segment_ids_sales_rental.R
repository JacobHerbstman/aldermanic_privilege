source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(arrow)
library(data.table)
library(sf)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids_sales_rental/code")
# sales_input <- "../input/sales_pre_scores.csv"
# rent_input <- "../input/rent_pre_scores_full.parquet"
# segment_gpkg <- "../input/boundary_segments_1320ft.gpkg"
# out_sales <- "../output/sales_pre_scores_with_segments.csv"
# out_rent <- "../output/rent_pre_scores_full_with_segments.parquet"
# out_coverage <- "../output/segment_assignment_coverage_summary.csv"
# out_spotcheck <- "../output/segment_assignment_spotcheck_queue.csv"
# out_reason <- "../output/segment_assignment_reason_summary.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sales_input, rent_input, segment_gpkg, out_sales, out_rent, out_coverage, out_spotcheck, out_reason)
}

if (length(cli_args) >= 8) {
  sales_input <- cli_args[1]
  rent_input <- cli_args[2]
  segment_gpkg <- cli_args[3]
  out_sales <- cli_args[4]
  out_rent <- cli_args[5]
  out_coverage <- cli_args[6]
  out_spotcheck <- cli_args[7]
  out_reason <- cli_args[8]
} else {
  if (!exists("sales_input") || !exists("rent_input") || !exists("segment_gpkg") ||
      !exists("out_sales") || !exists("out_rent") || !exists("out_coverage") || !exists("out_spotcheck") ||
      !exists("out_reason")) {
    stop(
      "FATAL: Script requires 8 args: <sales_input_csv> <rent_input_parquet> <segment_gpkg> <out_sales_csv> <out_rent_parquet> <out_coverage_csv> <out_spotcheck_csv> <out_reason_csv>",
      call. = FALSE
    )
  }
}

stopifnot(file.exists(sales_input), file.exists(rent_input), file.exists(segment_gpkg))

segments_by_era <- load_segment_layers(segment_gpkg, buffer_ft = 1000)

coverage_row <- function(dataset, scope, era, dt) {
  n_total <- nrow(dt)
  n_match <- if (n_total > 0) sum(!is.na(dt$segment_id) & dt$segment_id != "") else 0L
  data.table(
    dataset = dataset,
    scope = scope,
    era = era,
    n_obs = n_total,
    n_matched = n_match,
    coverage_rate = ifelse(n_total > 0, n_match / n_total, NA_real_)
  )
}

coverage_block <- function(dataset, dt, scope_name) {
  out <- list(coverage_row(dataset, scope_name, "all", dt))
  era_vals <- sort(unique(na.omit(dt$era)))
  if (length(era_vals) > 0) {
    for (era_i in era_vals) {
      out[[length(out) + 1L]] <- coverage_row(dataset, scope_name, era_i, dt[era == era_i])
    }
  }
  rbindlist(out, fill = TRUE)
}

assign_segments <- function(dt, dataset_name, date_col, pair_col, lon_col, lat_col, dist_col, allow_pre_2003, chunk_n = 50000L) {
  dt <- copy(dt)
  dt[, row_id := .I]
  dt[, pair_dash := normalize_pair_dash(get(pair_col))]
  dt[, obs_date := as.Date(get(date_col))]
  dt[, era := canonical_era_from_date(obs_date, allow_pre_2003 = allow_pre_2003)]
  dt[, segment_id := NA_character_]
  dt[, segment_reason := fifelse(
    !is.finite(get(lon_col)) | !is.finite(get(lat_col)),
    "missing_coords",
    fifelse(
      is.na(obs_date) | is.na(era),
      "missing_date_or_era",
      fifelse(is.na(pair_dash), "missing_pair", "pending")
    )
  )]

  assignable_idx <- which(
    !is.na(dt$era) &
      !is.na(dt$pair_dash) &
      is.finite(dt[[lon_col]]) &
      is.finite(dt[[lat_col]])
  )

  if (length(assignable_idx) > 0) {
    pts <- st_as_sf(
      data.table(
        row_id = assignable_idx,
        lon = dt[[lon_col]][assignable_idx],
        lat = dt[[lat_col]][assignable_idx]
      ),
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )

    seg_ids <- assign_points_to_segments(
      points_sf = pts,
      era_values = dt$era[assignable_idx],
      pair_values = dt$pair_dash[assignable_idx],
      segment_layers = segments_by_era,
      chunk_n = chunk_n
    )

    set(dt, i = assignable_idx, j = "segment_id", value = seg_ids)
  }

  pending_idx <- which(dt$segment_reason == "pending")
  if (length(pending_idx) > 0) {
    pair_available <- logical(length(pending_idx))
    for (era_i in unique(dt$era[pending_idx])) {
      seg_era <- segments_by_era[[era_i]]
      idx_era <- which(dt$era[pending_idx] == era_i)
      if (length(idx_era) == 0) next
      if (is.null(seg_era) || nrow(seg_era) == 0) next
      pair_available[idx_era] <- dt$pair_dash[pending_idx[idx_era]] %in% unique(seg_era$pair_dash)
    }

    dt[pending_idx, segment_reason := fifelse(
      !is.na(segment_id) & segment_id != "",
      "matched",
      fifelse(pair_available, "no_polygon_hit", "pair_not_in_segment_layer")
    )]
  }

  cov <- rbindlist(list(
    coverage_block(dataset_name, dt, "all"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 1000], "bw1000"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 500], "bw500"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 250], "bw250")
  ), fill = TRUE)

  reason_summary <- dt[, .(n_obs = .N), by = .(era, segment_reason)]
  reason_summary[, dataset := dataset_name]
  setcolorder(reason_summary, c("dataset", "era", "segment_reason", "n_obs"))
  setorder(reason_summary, era, segment_reason)

  out <- copy(dt)
  out[, c("row_id", "pair_dash", "obs_date", "era") := NULL]
  list(data = out, coverage = cov, reason_summary = reason_summary)
}

build_spotcheck <- function(dt_sales, dt_rent, n_each = 20L) {
  sales_q <- dt_sales[
    is.finite(dist_ft) & dist_ft <= 500,
    .(
      dataset = "sales",
      primary_id = as.character(pin),
      obs_date = as.character(sale_date),
      ward_pair_id = as.character(ward_pair_id),
      dist_ft = as.numeric(dist_ft),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      segment_id = as.character(segment_id),
      segment_reason = as.character(segment_reason),
      flag = fifelse(is.na(segment_id) | segment_id == "", "unmatched", "matched")
    )
  ]
  sales_q <- sales_q[order(flag, dist_ft)]

  rent_q <- dt_rent[
    is.finite(dist_ft) & dist_ft <= 500,
    .(
      dataset = "rental",
      primary_id = as.character(id),
      obs_date = as.character(file_date),
      ward_pair_id = as.character(ward_pair_id),
      dist_ft = as.numeric(dist_ft),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      segment_id = as.character(segment_id),
      segment_reason = as.character(segment_reason),
      flag = fifelse(is.na(segment_id) | segment_id == "", "unmatched", "matched")
    )
  ]
  rent_q <- rent_q[order(flag, dist_ft)]

  rbindlist(list(head(sales_q, n_each), head(rent_q, n_each)), fill = TRUE)
}

message("=== Assign Segment IDs for Sales + Rental Pre-Scores ===")
message(sprintf("Sales input: %s", sales_input))
message(sprintf("Rental input: %s", rent_input))
message(sprintf("Segment GPKG: %s", segment_gpkg))

sales_dt <- fread(sales_input)
if (!all(c("pin", "sale_date", "ward_pair_id", "dist_ft", "longitude", "latitude") %in% names(sales_dt))) {
  stop("sales_pre_scores.csv missing required columns.", call. = FALSE)
}
sales_dt[, pin := as.character(pin)]
sales_dt[, sale_date := as.Date(sale_date)]

rent_dt <- as.data.table(read_parquet(rent_input))
if (!all(c("id", "file_date", "ward_pair_id", "dist_ft", "longitude", "latitude") %in% names(rent_dt))) {
  stop("rent_pre_scores_full.parquet missing required columns.", call. = FALSE)
}
rent_dt[, id := as.character(id)]
rent_dt[, file_date := as.Date(file_date)]

sales_res <- assign_segments(
  dt = sales_dt,
  dataset_name = "sales",
  date_col = "sale_date",
  pair_col = "ward_pair_id",
  lon_col = "longitude",
  lat_col = "latitude",
  dist_col = "dist_ft",
  allow_pre_2003 = TRUE,
  chunk_n = 50000L
)

rent_res <- assign_segments(
  dt = rent_dt,
  dataset_name = "rental",
  date_col = "file_date",
  pair_col = "ward_pair_id",
  lon_col = "longitude",
  lat_col = "latitude",
  dist_col = "dist_ft",
  allow_pre_2003 = FALSE,
  chunk_n = 80000L
)

sales_out <- sales_res$data
rent_out <- rent_res$data
cov_out <- rbindlist(list(sales_res$coverage, rent_res$coverage), fill = TRUE)
reason_out <- rbindlist(list(sales_res$reason_summary, rent_res$reason_summary), fill = TRUE)
spotcheck <- build_spotcheck(sales_out, rent_out, n_each = 20L)

fwrite(sales_out, out_sales)
write_parquet(as.data.frame(rent_out), out_rent)
fwrite(cov_out, out_coverage)
fwrite(spotcheck, out_spotcheck)
fwrite(reason_out, out_reason)

message(sprintf("Saved sales output: %s (rows=%d)", out_sales, nrow(sales_out)))
message(sprintf("Saved rental output: %s (rows=%d)", out_rent, nrow(rent_out)))
message(sprintf("Saved coverage summary: %s", out_coverage))
message(sprintf("Saved spotcheck queue: %s", out_spotcheck))
message(sprintf("Saved reason summary: %s", out_reason))
