# --- Interactive Test Block ---
# setwd("tasks/assign_segment_ids_sales_rental/code")
# segment_length_ft <- 1320
# segment_buffer_m <- 457.2

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(arrow)
library(data.table)
library(sf)

sf_use_s2(FALSE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_length_ft, segment_buffer_m)
}
if (length(cli_args) != 2) {
  stop("FATAL: expected args: <segment_length_ft> <segment_buffer_m>", call. = FALSE)
}

segment_length_ft <- as.integer(cli_args[1])
segment_buffer_m <- as.numeric(cli_args[2])
segment_gpkg <- sprintf("../input/boundary_segments_%sft.gpkg", segment_length_ft)

if (!is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("segment_length_ft must be positive.", call. = FALSE)
}
if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
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

segment_distance_audit_block <- function(dataset, dt, date_col, pair_col, lon_col, lat_col,
                                         dist_col, allow_pre_2003, chunk_n = 50000L) {
  dt <- copy(dt)
  dt[, audit_row_id := .I]
  dt[, pair_dash_audit := normalize_pair_dash(get(pair_col))]
  dt[, obs_date_audit := as.Date(get(date_col))]
  dt[, era_audit := canonical_era_from_date(obs_date_audit, allow_pre_2003 = allow_pre_2003)]

  audit_idx <- which(
    is.finite(dt[[dist_col]]) &
      dt[[dist_col]] <= 500 * 0.3048 &
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

  scopes <- list(list(name = "within_500ft", max_m = 500 * 0.3048))

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
  if (nrow(segment_distance_audit) > 0 &&
      any(
        segment_distance_audit$n_missing_segment > 0 |
          segment_distance_audit$n_missing_segment_distance > 0 |
          segment_distance_audit$n_segment_pair_mismatch > 0 |
          segment_distance_audit$n_segment_distance_mismatch > 0,
        na.rm = TRUE
      )) {
    stop(sprintf("%s segment distance audit failed inside 500ft.", dataset_name), call. = FALSE)
  }

  out <- copy(dt)
  out[, c("row_id", "pair_dash", "obs_date", "era") := NULL]
  out
}

message("=== Assign Segment IDs for Sales + Rental Pre-Scores ===")
message(sprintf("Segment GPKG: %s", segment_gpkg))
message(sprintf("Segment buffer: %.0fm", segment_buffer_m))

sales_dt <- fread("../input/sales_pre_scores.csv")
if (!all(c("pin", "sale_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(sales_dt))) {
  stop("sales_pre_scores.csv missing required columns.", call. = FALSE)
}
sales_dt[, pin := as.character(pin)]
sales_dt[, sale_date := as.Date(sale_date)]

sales_out <- assign_segments(
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

message("\nAssigning rental segment IDs...")
rent_dt <- as.data.table(read_parquet("../input/rent_pre_scores_full.parquet"))
if (!all(c("id", "file_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(rent_dt))) {
  stop("rent_pre_scores_full.parquet missing required columns.", call. = FALSE)
}
rent_dt[, id := as.character(id)]
rent_dt[, file_date := as.Date(file_date)]

rent_out <- assign_segments(
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

write_parquet(as.data.frame(rent_out), "../output/rent_pre_scores_full_with_segments.parquet")
fwrite(sales_out, "../output/sales_pre_scores_with_segments.csv")

message(sprintf("Saved rental output: ../output/rent_pre_scores_full_with_segments.parquet (rows=%d)", nrow(rent_out)))
message(sprintf("Saved sales output: ../output/sales_pre_scores_with_segments.csv (rows=%d)", nrow(sales_out)))
