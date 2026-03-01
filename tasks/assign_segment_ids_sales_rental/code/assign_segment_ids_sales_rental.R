source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(sf)

sf_use_s2(FALSE)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids_sales_rental/code")
# sales_input <- "../input/sales_pre_scores.csv"
# rent_input <- "../input/rent_pre_scores_full.parquet"
# segment_gpkg <- "../input/boundary_segments_1320ft.gpkg"
# out_sales <- "../output/sales_pre_scores_with_segments.csv"
# out_rent <- "../output/rent_pre_scores_full_with_segments.parquet"
# out_coverage <- "../output/segment_assignment_coverage_summary.csv"
# out_spotcheck <- "../output/segment_assignment_spotcheck_queue.csv"
# source("assign_segment_ids_sales_rental.R")
# =======================================================================================

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  sales_input <- cli_args[1]
  rent_input <- cli_args[2]
  segment_gpkg <- cli_args[3]
  out_sales <- cli_args[4]
  out_rent <- cli_args[5]
  out_coverage <- cli_args[6]
  out_spotcheck <- cli_args[7]
} else {
  if (!exists("sales_input") || !exists("rent_input") || !exists("segment_gpkg") ||
      !exists("out_sales") || !exists("out_rent") || !exists("out_coverage") || !exists("out_spotcheck")) {
    stop(
      "FATAL: Script requires 7 args: <sales_input_csv> <rent_input_parquet> <segment_gpkg> <out_sales_csv> <out_rent_parquet> <out_coverage_csv> <out_spotcheck_csv>",
      call. = FALSE
    )
  }
}

stopifnot(file.exists(sales_input), file.exists(rent_input), file.exists(segment_gpkg))

d_2003 <- as.Date("2003-05-01")
d_2015 <- as.Date("2015-05-18")
d_2023 <- as.Date("2023-05-15")

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

sales_era_from_date <- function(d) {
  d <- as.Date(d)
  fifelse(
    is.na(d),
    NA_character_,
    fifelse(
      d < d_2003,
      "1998_2002",
      fifelse(d < d_2015, "2003_2014", fifelse(d < d_2023, "2015_2023", "post_2023"))
    )
  )
}

rent_era_from_date <- function(d) {
  d <- as.Date(d)
  fifelse(
    is.na(d),
    NA_character_,
    fifelse(d < d_2015, "2003_2014", fifelse(d < d_2023, "2015_2023", "post_2023"))
  )
}

load_segments_for_era <- function(gpkg_path, era_label) {
  layer_name <- sprintf("%s_bw1000", era_label)
  seg <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  if (!all(c("segment_id", "ward_pair_id") %in% names(seg))) {
    stop(sprintf("Layer '%s' missing required columns segment_id/ward_pair_id.", layer_name), call. = FALSE)
  }

  seg <- seg[, c("segment_id", "ward_pair_id"), drop = FALSE]
  seg <- st_make_valid(seg)
  seg$segment_id <- as.character(seg$segment_id)
  seg$pair_dash <- normalize_pair_dash(seg$ward_pair_id)
  seg <- seg[!is.na(seg$pair_dash), ]
  seg
}

layer_names <- st_layers(segment_gpkg)$name
required_layers <- paste0(c("1998_2002", "2003_2014", "2015_2023", "post_2023"), "_bw1000")
missing_layers <- setdiff(required_layers, layer_names)
if (length(missing_layers) > 0) {
  stop(sprintf("Missing required segment layers: %s", paste(missing_layers, collapse = ", ")), call. = FALSE)
}

segments_by_era <- list(
  "1998_2002" = load_segments_for_era(segment_gpkg, "1998_2002"),
  "2003_2014" = load_segments_for_era(segment_gpkg, "2003_2014"),
  "2015_2023" = load_segments_for_era(segment_gpkg, "2015_2023"),
  "post_2023" = load_segments_for_era(segment_gpkg, "post_2023")
)

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

assign_segments <- function(dt, dataset_name, date_col, pair_col, lon_col, lat_col, dist_col, era_fn, chunk_n = 50000L) {
  dt <- copy(dt)
  dt[, row_id := .I]
  dt[, pair_dash := normalize_pair_dash(get(pair_col))]
  dt[, obs_date := as.Date(get(date_col))]
  dt[, era := era_fn(obs_date)]
  dt[, segment_id := NA_character_]

  assignable_idx <- which(
    !is.na(dt$era) &
      !is.na(dt$pair_dash) &
      is.finite(dt[[lon_col]]) &
      is.finite(dt[[lat_col]])
  )

  if (length(assignable_idx) > 0) {
    eras <- sort(unique(dt$era[assignable_idx]))

    for (era_i in eras) {
      seg_era <- segments_by_era[[era_i]]
      idx_era <- assignable_idx[dt$era[assignable_idx] == era_i]
      if (length(idx_era) == 0 || is.null(seg_era) || nrow(seg_era) == 0) next

      pairs_era <- unique(dt$pair_dash[idx_era])
      valid_pairs <- intersect(pairs_era, unique(seg_era$pair_dash))
      if (length(valid_pairs) == 0) next

      message(sprintf("[%s] assigning era %s across %d pairs...", dataset_name, era_i, length(valid_pairs)))

      for (j in seq_along(valid_pairs)) {
        pair_j <- valid_pairs[j]
        idx_pair <- idx_era[dt$pair_dash[idx_era] == pair_j]
        if (length(idx_pair) == 0) next

        seg_pair <- seg_era[seg_era$pair_dash == pair_j, ]
        if (nrow(seg_pair) == 0) next

        starts <- seq(1L, length(idx_pair), by = chunk_n)
        for (s in starts) {
          e <- min(s + chunk_n - 1L, length(idx_pair))
          chunk_idx <- idx_pair[s:e]

          chunk_dt <- data.table(
            row_id = chunk_idx,
            lon = dt[[lon_col]][chunk_idx],
            lat = dt[[lat_col]][chunk_idx]
          )

          pts <- st_as_sf(chunk_dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
          if (st_crs(pts) != st_crs(seg_pair)) {
            pts <- st_transform(pts, st_crs(seg_pair))
          }

          hits <- st_within(pts, seg_pair)
          hit_idx <- vapply(hits, function(v) {
            if (length(v) == 0) return(NA_integer_)
            v[1]
          }, integer(1))

          ok <- !is.na(hit_idx)
          if (any(ok)) {
            set(
              dt,
              i = chunk_idx[ok],
              j = "segment_id",
              value = as.character(seg_pair$segment_id[hit_idx[ok]])
            )
          }
        }

        if (j %% 25L == 0L || j == length(valid_pairs)) {
          message(sprintf("[%s] era %s: %d/%d pairs complete", dataset_name, era_i, j, length(valid_pairs)))
        }
      }
    }
  }

  cov <- rbindlist(list(
    coverage_block(dataset_name, dt, "all"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 1000], "bw1000"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 500], "bw500"),
    coverage_block(dataset_name, dt[is.finite(get(dist_col)) & get(dist_col) <= 250], "bw250")
  ), fill = TRUE)

  out <- copy(dt)
  out[, c("row_id", "pair_dash", "obs_date", "era") := NULL]
  list(data = out, coverage = cov)
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
  era_fn = sales_era_from_date,
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
  era_fn = rent_era_from_date,
  chunk_n = 80000L
)

sales_out <- sales_res$data
rent_out <- rent_res$data
cov_out <- rbindlist(list(sales_res$coverage, rent_res$coverage), fill = TRUE)
spotcheck <- build_spotcheck(sales_out, rent_out, n_each = 20L)

fwrite(sales_out, out_sales)
write_parquet(as.data.frame(rent_out), out_rent)
fwrite(cov_out, out_coverage)
fwrite(spotcheck, out_spotcheck)

message(sprintf("Saved sales output: %s (rows=%d)", out_sales, nrow(sales_out)))
message(sprintf("Saved rental output: %s (rows=%d)", out_rent, nrow(rent_out)))
message(sprintf("Saved coverage summary: %s", out_coverage))
message(sprintf("Saved spotcheck queue: %s", out_spotcheck))
