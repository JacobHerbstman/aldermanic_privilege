source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(arrow)
library(data.table)
library(sf)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids_sales_rental/code")
# mode <- "all"
# sales_input <- "../input/sales_pre_scores.csv"
# rent_input <- "../input/rent_pre_scores_full.parquet"
# segment_gpkg <- "../input/boundary_segments_1320ft.gpkg"
# out_sales <- "../output/sales_pre_scores_with_segments.csv"
# out_rent <- "../output/rent_pre_scores_full_with_segments.parquet"
# out_coverage <- "../output/segment_assignment_coverage_summary.csv"
# out_spotcheck <- "../output/segment_assignment_spotcheck_queue.csv"
# out_reason <- "../output/segment_assignment_reason_summary.csv"
# segment_buffer_m <- 457.2
# coverage_bandwidths_m <- "100,250,305,457.2"
# spotcheck_bandwidth_m <- 457.2

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(mode, sales_input, rent_input, segment_gpkg, out_sales, out_rent, out_coverage, out_spotcheck, out_reason, segment_buffer_m, coverage_bandwidths_m, spotcheck_bandwidth_m)
}

mode <- cli_args[1]
if (!mode %in% c("sales", "all")) {
  stop("FATAL: mode must be 'sales' or 'all'.", call. = FALSE)
}

if (mode == "sales") {
  if (length(cli_args) != 6) {
    stop(
      "FATAL: sales mode requires 6 args: sales <sales_input_csv> <segment_gpkg> <out_sales_csv> <segment_buffer_m> <coverage_bandwidths_m>",
      call. = FALSE
    )
  }
  sales_input <- cli_args[2]
  segment_gpkg <- cli_args[3]
  out_sales <- cli_args[4]
  segment_buffer_m <- as.numeric(cli_args[5])
  coverage_bandwidths_m <- scan(text = gsub(",", " ", cli_args[6], fixed = TRUE), quiet = TRUE)
  spotcheck_bandwidth_m <- NA_real_
} else {
  if (length(cli_args) != 12) {
    stop(
      "FATAL: all mode requires 12 args: all <sales_input_csv> <rent_input_parquet> <segment_gpkg> <out_sales_csv> <out_rent_parquet> <out_coverage_csv> <out_spotcheck_csv> <out_reason_csv> <segment_buffer_m> <coverage_bandwidths_m> <spotcheck_bandwidth_m>",
      call. = FALSE
    )
  }
  sales_input <- cli_args[2]
  rent_input <- cli_args[3]
  segment_gpkg <- cli_args[4]
  out_sales <- cli_args[5]
  out_rent <- cli_args[6]
  out_coverage <- cli_args[7]
  out_spotcheck <- cli_args[8]
  out_reason <- cli_args[9]
  segment_buffer_m <- as.numeric(cli_args[10])
  coverage_bandwidths_m <- scan(text = gsub(",", " ", cli_args[11], fixed = TRUE), quiet = TRUE)
  spotcheck_bandwidth_m <- as.numeric(cli_args[12])
}

if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}
if (length(coverage_bandwidths_m) == 0 || any(!is.finite(coverage_bandwidths_m)) || any(coverage_bandwidths_m <= 0)) {
  stop("coverage_bandwidths_m must contain positive numeric bandwidths.", call. = FALSE)
}
coverage_bandwidths_m <- sort(unique(as.numeric(coverage_bandwidths_m)))
if (mode == "all" && (!is.finite(spotcheck_bandwidth_m) || spotcheck_bandwidth_m <= 0)) {
  stop("spotcheck_bandwidth_m must be positive in all mode.", call. = FALSE)
}

stopifnot(file.exists(sales_input), file.exists(segment_gpkg))
if (mode == "all") {
  stopifnot(file.exists(rent_input))
}

segments_by_era <- load_segment_line_layers(segment_gpkg)
segment_metadata <- segment_metadata_from_layers(segments_by_era)
segment_metadata_key <- paste(segment_metadata$era, segment_metadata$segment_id, sep = "\r")
segment_pair_lookup <- rbindlist(lapply(names(segments_by_era), function(era_i) {
  data.table(
    era = era_i,
    segment_id = as.character(segments_by_era[[era_i]]$segment_id),
    segment_pair_dash = as.character(segments_by_era[[era_i]]$pair_dash)
  )
}))
if (any(duplicated(segment_pair_lookup, by = c("era", "segment_id")))) {
  stop("Segment lookup has duplicate era/segment_id rows.", call. = FALSE)
}

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

segment_distance_audit_block <- function(dataset, dt, date_col, pair_col, lon_col, lat_col,
                                         dist_col, allow_pre_2003, chunk_n = 50000L) {
  dt <- copy(dt)
  dt[, audit_row_id := .I]
  dt[, pair_dash_audit := normalize_pair_dash(get(pair_col))]
  dt[, obs_date_audit := as.Date(get(date_col))]
  dt[, era_audit := canonical_era_from_date(obs_date_audit, allow_pre_2003 = allow_pre_2003)]

  max_audit_m <- max(c(coverage_bandwidths_m, 500 * 0.3048), na.rm = TRUE)
  audit_idx <- which(
    is.finite(dt[[dist_col]]) &
      dt[[dist_col]] <= max_audit_m &
      is.finite(dt[[lon_col]]) &
      is.finite(dt[[lat_col]]) &
      !is.na(dt$era_audit) &
      !is.na(dt$pair_dash_audit)
  )

  if (length(audit_idx) == 0) {
    return(data.table(
      dataset = dataset,
      scope = "within_500ft",
      era = character(),
      n_rows = integer(),
      n_missing_segment = integer(),
      n_missing_segment_distance = integer(),
      n_segment_pair_mismatch = integer(),
      n_segment_distance_mismatch = integer(),
      max_abs_segment_dist_diff_m = numeric()
    ))
  }

  pts <- st_as_sf(
    data.table(
      audit_row_id = audit_idx,
      lon = dt[[lon_col]][audit_idx],
      lat = dt[[lat_col]][audit_idx]
    ),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  segment_audit <- audit_nearest_segment_pair_constraints(
    points_sf = pts,
    era_values = dt$era_audit[audit_idx],
    pair_values = dt$pair_dash_audit[audit_idx],
    segment_layers = segments_by_era,
    constrained_segment_id = dt$segment_id[audit_idx],
    max_distance = units::set_units(segment_buffer_m, "m"),
    chunk_n = chunk_n
  )

  detail <- cbind(
    data.table(
      audit_row_id = audit_idx,
      era = dt$era_audit[audit_idx],
      ward_pair_id = dt[[pair_col]][audit_idx],
      pair_dash = dt$pair_dash_audit[audit_idx],
      dist_m = as.numeric(dt[[dist_col]][audit_idx]),
      segment_id = as.character(dt$segment_id[audit_idx])
    ),
    as.data.table(segment_audit)
  )
  detail[, segment_dist_diff_m := constrained_segment_dist_m - dist_m]
  detail[, abs_segment_dist_diff_m := abs(segment_dist_diff_m)]
  detail[, flag_missing_segment := is.na(segment_id) | segment_id == ""]
  detail[, flag_missing_segment_distance := !is.finite(constrained_segment_dist_m)]
  detail[, flag_segment_pair_mismatch := !flag_missing_segment &
    !is.na(constrained_pair_dash) & pair_dash != constrained_pair_dash]
  detail[, flag_segment_distance_mismatch := !flag_missing_segment &
    (!is.finite(abs_segment_dist_diff_m) | abs_segment_dist_diff_m > 1)]

  scopes <- c(
    list(list(name = "within_500ft", max_m = 500 * 0.3048)),
    lapply(coverage_bandwidths_m, function(bw_m_i) {
      list(name = sprintf("bw%.0fm", bw_m_i), max_m = bw_m_i)
    })
  )

  rbindlist(lapply(scopes, function(scope_i) {
    d <- detail[dist_m <= scope_i$max_m]
    if (nrow(d) == 0) {
      return(data.table(
        dataset = dataset,
        scope = scope_i$name,
        era = "all",
        n_rows = 0L,
        n_missing_segment = 0L,
        n_missing_segment_distance = 0L,
        n_segment_pair_mismatch = 0L,
        n_segment_distance_mismatch = 0L,
        max_abs_segment_dist_diff_m = NA_real_
      ))
    }

    out <- d[, .(
      n_rows = .N,
      n_missing_segment = sum(flag_missing_segment, na.rm = TRUE),
      n_missing_segment_distance = sum(flag_missing_segment_distance, na.rm = TRUE),
      n_segment_pair_mismatch = sum(flag_segment_pair_mismatch, na.rm = TRUE),
      n_segment_distance_mismatch = sum(flag_segment_distance_mismatch, na.rm = TRUE),
      max_abs_segment_dist_diff_m = suppressWarnings(max(abs_segment_dist_diff_m, na.rm = TRUE))
    ), by = .(era)]
    out_all <- d[, .(
      era = "all",
      n_rows = .N,
      n_missing_segment = sum(flag_missing_segment, na.rm = TRUE),
      n_missing_segment_distance = sum(flag_missing_segment_distance, na.rm = TRUE),
      n_segment_pair_mismatch = sum(flag_segment_pair_mismatch, na.rm = TRUE),
      n_segment_distance_mismatch = sum(flag_segment_distance_mismatch, na.rm = TRUE),
      max_abs_segment_dist_diff_m = suppressWarnings(max(abs_segment_dist_diff_m, na.rm = TRUE))
    )]
    out <- rbindlist(list(out_all, out), fill = TRUE)
    out[, `:=`(dataset = dataset, scope = scope_i$name)]
    setcolorder(out, c(
      "dataset", "scope", "era", "n_rows", "n_missing_segment",
      "n_missing_segment_distance", "n_segment_pair_mismatch",
      "n_segment_distance_mismatch", "max_abs_segment_dist_diff_m"
    ))
    out[order(scope, era)]
  }), fill = TRUE)
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
      fifelse(
        is.na(pair_dash),
        "missing_or_invalid_ward_pair",
        "pending"
      )
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

    seg_ids <- assign_points_to_nearest_segments(
      points_sf = pts,
      era_values = dt$era[assignable_idx],
      pair_values = dt$pair_dash[assignable_idx],
      segment_layers = segments_by_era,
      max_distance = units::set_units(segment_buffer_m, "m"),
      chunk_n = chunk_n
    )

    set(dt, i = assignable_idx, j = "segment_id", value = seg_ids)
  }

  assigned_segments <- dt[!is.na(segment_id) & segment_id != "", .(row_id, era, pair_dash, segment_id)]
  if (nrow(assigned_segments) > 0) {
    assigned_segments <- merge(
      assigned_segments,
      segment_pair_lookup,
      by = c("era", "segment_id"),
      all.x = TRUE,
      sort = FALSE
    )
    missing_segment <- is.na(assigned_segments$segment_pair_dash)
    if (any(missing_segment)) {
      stop(sprintf(
        "%s segment assignment has %d segment IDs missing from the segment lookup.",
        dataset_name,
        sum(missing_segment)
      ), call. = FALSE)
    }
    pair_mismatch <- assigned_segments$pair_dash != assigned_segments$segment_pair_dash
    if (any(pair_mismatch, na.rm = TRUE)) {
      stop(sprintf(
        "%s segment assignment is not in the input ward pair for %d rows.",
        dataset_name,
        sum(pair_mismatch, na.rm = TRUE)
      ), call. = FALSE)
    }
  }

  segment_idx <- match(paste(dt$era, dt$segment_id, sep = "\r"), segment_metadata_key)
  dt[, analysis_segment_id := segment_metadata$analysis_segment_id[segment_idx]]
  dt[, valid_segment := segment_metadata$valid_segment[segment_idx]]
  dt[, invalid_reason := segment_metadata$invalid_reason[segment_idx]]
  dt[, segment_length_ft := segment_metadata$segment_length_ft[segment_idx]]
  dt[, segment_lt500ft := segment_metadata$segment_lt500ft[segment_idx]]
  dt[, segment_lt1000ft := segment_metadata$segment_lt1000ft[segment_idx]]

  pending_idx <- which(dt$segment_reason == "pending")
  if (length(pending_idx) > 0) {
    dt[pending_idx, segment_reason := fifelse(
      !is.na(segment_id) & segment_id != "",
      "matched",
      "no_nearest_segment_within_radius"
    )]
  }

  cov <- rbindlist(c(
    list(coverage_block(dataset_name, dt, "all")),
    lapply(coverage_bandwidths_m, function(bw_m_i) {
      coverage_block(
        dataset_name,
        dt[is.finite(get(dist_col)) & get(dist_col) <= bw_m_i],
        sprintf("bw%.0fm", bw_m_i)
      )
    })
  ), fill = TRUE)

  reason_summary <- dt[, .(n_obs = .N), by = .(era, segment_reason)]
  reason_summary[, dataset := dataset_name]
  setcolorder(reason_summary, c("dataset", "era", "segment_reason", "n_obs"))
  setorder(reason_summary, era, segment_reason)

  segment_distance_audit <- segment_distance_audit_block(
    dataset = dataset_name,
    dt = dt,
    date_col = date_col,
    pair_col = pair_col,
    lon_col = lon_col,
    lat_col = lat_col,
    dist_col = dist_col,
    allow_pre_2003 = allow_pre_2003,
    chunk_n = chunk_n
  )

  out <- copy(dt)
  out[, c("row_id", "pair_dash", "obs_date", "era") := NULL]
  list(
    data = out,
    coverage = cov,
    reason_summary = reason_summary,
    segment_distance_audit = segment_distance_audit
  )
}

build_spotcheck <- function(dt_sales, dt_rent, n_each = 20L, bandwidth_m) {
  sales_q <- dt_sales[
    is.finite(dist_m) & dist_m <= bandwidth_m,
    .(
      dataset = "sales",
      primary_id = as.character(pin),
      obs_date = as.character(sale_date),
      ward_pair_id = as.character(ward_pair_id),
      dist_m = as.numeric(dist_m),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      segment_id = as.character(segment_id),
      segment_reason = as.character(segment_reason),
      flag = fifelse(is.na(segment_id) | segment_id == "", "unmatched", "matched")
    )
  ]
  sales_q <- sales_q[order(flag, dist_m)]

  rent_q <- dt_rent[
    is.finite(dist_m) & dist_m <= bandwidth_m,
    .(
      dataset = "rental",
      primary_id = as.character(id),
      obs_date = as.character(file_date),
      ward_pair_id = as.character(ward_pair_id),
      dist_m = as.numeric(dist_m),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      segment_id = as.character(segment_id),
      segment_reason = as.character(segment_reason),
      flag = fifelse(is.na(segment_id) | segment_id == "", "unmatched", "matched")
    )
  ]
  rent_q <- rent_q[order(flag, dist_m)]

  rbindlist(list(head(sales_q, n_each), head(rent_q, n_each)), fill = TRUE)
}

message("=== Assign Segment IDs for Sales + Rental Pre-Scores ===")
message(sprintf("Mode: %s", mode))
message(sprintf("Sales input: %s", sales_input))
if (mode == "all") {
  message(sprintf("Rental input: %s", rent_input))
}
message(sprintf("Segment GPKG: %s", segment_gpkg))
message(sprintf("Segment buffer: %.0fm", segment_buffer_m))
message(sprintf("Coverage bandwidths: %s", paste0(coverage_bandwidths_m, "m", collapse = ", ")))
if (mode == "all") {
  message(sprintf("Spotcheck bandwidth: %.0fm", spotcheck_bandwidth_m))
}

sales_dt <- fread(sales_input)
if (!all(c("pin", "sale_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(sales_dt))) {
  stop("sales_pre_scores.csv missing required columns.", call. = FALSE)
}
sales_dt[, pin := as.character(pin)]
sales_dt[, sale_date := as.Date(sale_date)]

sales_res <- assign_segments(
  dt = sales_dt,
  dataset_name = "sales",
  date_col = "sale_date",
  pair_col = "ward_pair_id",
  lon_col = "longitude",
  lat_col = "latitude",
  dist_col = "dist_m",
  allow_pre_2003 = TRUE,
  chunk_n = 50000L
)

sales_out <- sales_res$data
fwrite(sales_out, out_sales)
message(sprintf("Saved sales output: %s (rows=%d)", out_sales, nrow(sales_out)))

if (mode == "sales") {
  quit(save = "no", status = 0)
}

message("\nAssigning rental segment IDs...")
rent_dt <- as.data.table(read_parquet(rent_input))
if (!all(c("id", "file_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(rent_dt))) {
  stop("rent_pre_scores_full.parquet missing required columns.", call. = FALSE)
}
rent_dt[, id := as.character(id)]
rent_dt[, file_date := as.Date(file_date)]

rent_res <- assign_segments(
  dt = rent_dt,
  dataset_name = "rental",
  date_col = "file_date",
  pair_col = "ward_pair_id",
  lon_col = "longitude",
  lat_col = "latitude",
  dist_col = "dist_m",
  allow_pre_2003 = FALSE,
  chunk_n = 80000L
)

rent_out <- rent_res$data
rent_out[, modal_segment_id := NA_character_]
rent_out[, flag_modal_segment_sensitivity_checked := FALSE]
rent_out[, flag_modal_segment_missing := FALSE]
rent_out[, flag_modal_changes_segment := FALSE]

if (all(c("modal_longitude", "modal_latitude", "modal_ward_pair_id") %in% names(rent_out))) {
  modal_idx <- which(
    is.finite(rent_out$dist_m) &
      rent_out$dist_m <= 500 * 0.3048 &
      is.finite(rent_out$modal_longitude) &
      is.finite(rent_out$modal_latitude) &
      !is.na(rent_out$modal_ward_pair_id) &
      rent_out$modal_ward_pair_id != ""
  )

  if (length(modal_idx) > 0) {
    modal_pts <- st_as_sf(
      data.table(
        row_id = modal_idx,
        lon = rent_out$modal_longitude[modal_idx],
        lat = rent_out$modal_latitude[modal_idx]
      ),
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )
    modal_era <- canonical_era_from_date(rent_out$file_date[modal_idx], allow_pre_2003 = FALSE)
    modal_segment_ids <- assign_points_to_nearest_segments(
      points_sf = modal_pts,
      era_values = modal_era,
      pair_values = rent_out$modal_ward_pair_id[modal_idx],
      segment_layers = segments_by_era,
      max_distance = units::set_units(segment_buffer_m, "m"),
      chunk_n = 80000L
    )

    rent_out[modal_idx, modal_segment_id := modal_segment_ids]
    rent_out[modal_idx, flag_modal_segment_sensitivity_checked := TRUE]
    rent_out[modal_idx, flag_modal_segment_missing := is.na(modal_segment_id) | modal_segment_id == ""]
    rent_out[
      modal_idx,
      flag_modal_changes_segment := !flag_modal_segment_missing &
        !is.na(segment_id) & segment_id != "" & segment_id != modal_segment_id
    ]
  }
}

cov_out <- rbindlist(list(sales_res$coverage, rent_res$coverage), fill = TRUE)
reason_out <- rbindlist(list(sales_res$reason_summary, rent_res$reason_summary), fill = TRUE)
distance_audit_out <- rbindlist(
  list(sales_res$segment_distance_audit, rent_res$segment_distance_audit),
  fill = TRUE
)
spotcheck <- build_spotcheck(sales_out, rent_out, n_each = 20L, bandwidth_m = spotcheck_bandwidth_m)
fwrite(distance_audit_out, "../output/segment_assignment_distance_audit.csv")

critical_distance_audit <- distance_audit_out[scope == "within_500ft"]
if (nrow(critical_distance_audit) > 0 &&
    any(
      critical_distance_audit$n_missing_segment > 0 |
        critical_distance_audit$n_missing_segment_distance > 0 |
        critical_distance_audit$n_segment_pair_mismatch > 0 |
        critical_distance_audit$n_segment_distance_mismatch > 0,
      na.rm = TRUE
    )) {
  stop("Segment distance audit failed inside 500ft.", call. = FALSE)
}

write_parquet(as.data.frame(rent_out), out_rent)
fwrite(cov_out, out_coverage)
fwrite(spotcheck, out_spotcheck)
fwrite(reason_out, out_reason)

message(sprintf("Saved rental output: %s (rows=%d)", out_rent, nrow(rent_out)))
message(sprintf("Saved coverage summary: %s", out_coverage))
message(sprintf("Saved spotcheck queue: %s", out_spotcheck))
message(sprintf("Saved reason summary: %s", out_reason))
message("Saved distance audit: ../output/segment_assignment_distance_audit.csv")
