# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/assign_segment_ids_sales_rental/code")
# segment_length_ft <- 1320
# segment_buffer_m <- 457.2

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(arrow)
library(data.table)
library(sf)

suppressMessages(sf_use_s2(FALSE))

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_length_ft, segment_buffer_m)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <segment_length_ft> <segment_buffer_m>.", call. = FALSE)
}

segment_length_ft <- as.integer(cli_args[1])
segment_buffer_m <- as.numeric(cli_args[2])

if (!is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("segment_length_ft must be positive.", call. = FALSE)
}
if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}

segment_gpkg <- sprintf("../input/boundary_segments_%sft.gpkg", segment_length_ft)

segments_by_era <- load_segment_line_layers(segment_gpkg)
segment_metadata <- segment_metadata_from_layers(segments_by_era)
segment_metadata_key <- paste(segment_metadata$era, segment_metadata$segment_id, sep = "\r")
segment_pair_lookup_list <- list()
for (era_i in names(segments_by_era)) {
  segment_pair_lookup_list[[era_i]] <- data.table(
    era = era_i,
    segment_id = as.character(segments_by_era[[era_i]]$segment_id),
    segment_pair_dash = as.character(segments_by_era[[era_i]]$pair_dash)
  )
}
segment_pair_lookup <- rbindlist(segment_pair_lookup_list)
if (any(duplicated(segment_pair_lookup, by = c("era", "segment_id")))) {
  stop("Segment lookup has duplicate era/segment_id rows.", call. = FALSE)
}

sales_dt <- fread(
  "../input/sales_pre_scores.csv",
  colClasses = list(character = "pin")
)
if (!all(c("pin", "sale_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(sales_dt))) {
  stop("sales_pre_scores.csv missing required columns.", call. = FALSE)
}
sales_dt[, pin := gsub("[^0-9]", "", trimws(pin))]
sales_dt[nchar(pin) == 13L, pin := paste0("0", pin)]
if (any(nchar(sales_dt$pin) != 14L)) {
  stop("Sales input contains an invalid full PIN.", call. = FALSE)
}
sales_dt[, sale_date := as.Date(sale_date)]

rent_dt <- as.data.table(read_parquet("../input/rent_pre_scores_full.parquet"))
if (!all(c("id", "file_date", "ward_pair_id", "dist_m", "longitude", "latitude") %in% names(rent_dt))) {
  stop("rent_pre_scores_full.parquet missing required columns.", call. = FALSE)
}
rent_dt[, id := as.character(id)]
rent_dt[, file_date := as.Date(file_date)]

segment_outputs <- list()
dataset_specs <- list(
  sales = list(
    dt = sales_dt,
    date_col = "sale_date",
    pair_col = "ward_pair_id",
    lon_col = "longitude",
    lat_col = "latitude",
    allow_pre_2003 = TRUE,
    chunk_n = 50000L
  ),
  rental = list(
    dt = rent_dt,
    date_col = "file_date",
    pair_col = "ward_pair_id",
    lon_col = "longitude",
    lat_col = "latitude",
    allow_pre_2003 = FALSE,
    chunk_n = 80000L
  )
)

for (dataset_name in names(dataset_specs)) {
  spec <- dataset_specs[[dataset_name]]
  dt <- copy(spec$dt)
  dt[, assignment_row_id := .I]
  dt[, pair_dash := normalize_pair_dash(get(spec$pair_col))]
  dt[, obs_date := as.Date(get(spec$date_col))]
  dt[, era := canonical_era_from_date(obs_date, allow_pre_2003 = spec$allow_pre_2003)]
  dt[, segment_id := NA_character_]
  dt[, segment_reason := fifelse(
    !is.finite(get(spec$lon_col)) | !is.finite(get(spec$lat_col)),
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
      is.finite(dt[[spec$lon_col]]) &
      is.finite(dt[[spec$lat_col]])
  )

  if (length(assignable_idx) > 0) {
    pts <- st_as_sf(
      data.table(
        assignment_row_id = assignable_idx,
        lon = dt[[spec$lon_col]][assignable_idx],
        lat = dt[[spec$lat_col]][assignable_idx]
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
      chunk_n = spec$chunk_n
    )

    set(dt, i = assignable_idx, j = "segment_id", value = seg_ids)
  }

  assigned_segments <- dt[!is.na(segment_id) & segment_id != "", .(assignment_row_id, era, pair_dash, segment_id)]
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

  dt[, c("assignment_row_id", "pair_dash", "obs_date", "era") := NULL]
  segment_outputs[[dataset_name]] <- dt
}

write_parquet(as.data.frame(segment_outputs[["rental"]]), "../output/rent_pre_scores_full_with_segments.parquet")
fwrite(segment_outputs[["sales"]], "../output/sales_pre_scores_with_segments.csv")
