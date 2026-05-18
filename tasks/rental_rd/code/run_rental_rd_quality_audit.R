# Audit RentHub quality-flag balance in the rental RD sample.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd/code")
# bandwidth_ft <- 500
# sample <- "all"

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(arrow)
library(data.table)
library(sf)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, sample)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth_ft> <sample>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
sample <- cli_args[2]
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
valid_samples <- c(
  "all",
  "multifamily_only",
  "clean_location",
  "no_modal_pair_change",
  "no_modal_ward_change",
  "no_questionable_address"
)
if (!sample %in% valid_samples) {
  stop(sprintf("sample must be one of: %s.", paste(valid_samples, collapse = ", ")), call. = FALSE)
}

message("=== Rental RD Quality-Flag Balance Audit ===")
message(sprintf("Bandwidth: %.0f ft", bandwidth_ft))
message(sprintf("Sample: %s", sample))

rent <- as.data.table(read_parquet("../input/rent_with_ward_distances.parquet"))
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent[, signed_dist := signed_dist_m / 0.3048]
}
required_cols <- c(
  "rent_panel_id", "file_date", "rent_price", "signed_dist", "strictness_own",
  "strictness_neighbor", "segment_id", "ward_pair_id", "dist_m", "longitude", "latitude"
)
missing_cols <- setdiff(required_cols, names(rent))
if (length(missing_cols) > 0) {
  stop(sprintf("Rental RD audit input missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

rent[, file_date := as.Date(file_date)]
rent[, year := lubridate::year(file_date)]
rent[, year_month := format(file_date, "%Y-%m")]
rent[, signed_dist_ft := as.numeric(signed_dist)]
rent[, side := fifelse(signed_dist_ft >= 0, "stricter_side", "lenient_side")]
rent[, ward_pair_id := as.character(ward_pair_id)]
rent[, segment_id := as.character(segment_id)]
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental RD audit input contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental RD audit input must be unique by rent_panel_id.", call. = FALSE)
}

if ("address_missing" %in% names(rent)) {
  rent[, flag_address_missing := address_missing == 1 | address_missing == TRUE]
}
if ("multi_rent_days" %in% names(rent)) {
  rent[, flag_multi_rent_month := coalesce(multi_rent_days > 0, FALSE)]
}
if ("quality_flag_severity" %in% names(rent)) {
  rent[, flag_quality_high_or_severe := quality_flag_severity %in% c("high", "severe")]
  rent[, flag_quality_severe := quality_flag_severity == "severe"]
}
if (all(c("modal_ward_pair_id", "modal_ward", "ward", "neighbor_ward") %in% names(rent))) {
  rent[, modal_side_status := fifelse(
    is.na(modal_ward_pair_id) | modal_ward_pair_id == "",
    "not_checked",
    fifelse(
      gsub("_", "-", modal_ward_pair_id, fixed = TRUE) != gsub("_", "-", ward_pair_id, fixed = TRUE),
      "different_pair",
      fifelse(
        modal_ward == ward,
        "same_side",
        fifelse(modal_ward == neighbor_ward, "flipped_side", "changed_ward_same_pair")
      )
    )
  )]
  rent[, flag_modal_changes_side := modal_side_status %in% c("different_pair", "flipped_side", "changed_ward_same_pair")]
}
for (flag_col in c(
  "flag_location_questionable",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft",
  "flag_rd_location_questionable",
  "flag_quality_high_or_severe",
  "flag_quality_severe"
)) {
  if (!flag_col %in% names(rent)) {
    rent[, (flag_col) := FALSE]
  }
  set(rent, j = flag_col, value = as.logical(coalesce(rent[[flag_col]], FALSE)))
}
rent[, flag_clean_location_sample := !flag_location_questionable &
  !flag_modal_assignment_missing &
  !flag_modal_changes_ward &
  !flag_modal_changes_neighbor_ward &
  !flag_modal_changes_pair &
  !flag_modal_dist_diff_gt100ft]
rent[, flag_no_modal_pair_change_sample := !flag_modal_assignment_missing & !flag_modal_changes_pair]
rent[, flag_no_modal_ward_change_sample := !flag_modal_assignment_missing &
  !flag_modal_changes_ward &
  !flag_modal_changes_neighbor_ward]
rent[, flag_no_questionable_address_sample := !flag_location_questionable]

rd_base <- rent[
  !is.na(file_date) &
    year >= 2014 & year <= 2022 &
    is.finite(rent_price) & rent_price > 0 &
    is.finite(signed_dist_ft) & abs(signed_dist_ft) <= bandwidth_ft &
    !is.na(strictness_own) & !is.na(strictness_neighbor) &
    !is.na(ward_pair_id) & ward_pair_id != ""
]
if (nrow(rd_base) == 0) {
  stop("No rental observations remain after base RD audit filtering.", call. = FALSE)
}

message("Running rental segment contract audit...")
rd_base[, era := canonical_era_from_date(file_date, allow_pre_2003 = FALSE)]
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")
segment_contract_input <- copy(rd_base)
segment_contract_input[, segment_missing := is.na(segment_id) | segment_id == ""]

segment_points <- st_as_sf(
  segment_contract_input,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
segment_contract_raw <- audit_nearest_segment_pair_constraints(
  points_sf = segment_points,
  era_values = segment_contract_input$era,
  pair_values = segment_contract_input$ward_pair_id,
  segment_layers = segment_layers,
  constrained_segment_id = segment_contract_input$segment_id,
  max_distance = units::set_units(250, "m"),
  chunk_n = 50000L
)
segment_contract <- cbind(
  segment_contract_input[, .(
    rent_panel_id, file_date, year, year_month, ward_pair_id, segment_id,
    dist_m, signed_dist_ft, side,
    valid_segment = if ("valid_segment" %in% names(segment_contract_input)) valid_segment else NA,
    invalid_reason = if ("invalid_reason" %in% names(segment_contract_input)) invalid_reason else NA_character_,
    segment_missing
  )],
  as.data.table(segment_contract_raw)
)
segment_contract[, segment_pair_mismatch := !segment_missing &
  (is.na(constrained_pair_matches_input) | !constrained_pair_matches_input)]
segment_contract[, segment_distance_missing := !segment_missing & !is.finite(constrained_segment_dist_m)]
segment_contract[, segment_distance_mismatch := !segment_missing &
  is.finite(constrained_segment_dist_m) &
  abs(constrained_segment_dist_m - dist_m) > 0.05]
segment_contract[, invalid_segment_flag := !segment_missing &
  !is.na(valid_segment) &
  as.logical(valid_segment) == FALSE]
segment_contract[, sample := sample]
segment_contract[, bandwidth_ft := bandwidth_ft]
segment_contract_summary <- segment_contract[, .(
  n_rows = .N,
  n_missing_segment = sum(segment_missing, na.rm = TRUE),
  n_segment_pair_mismatch = sum(segment_pair_mismatch, na.rm = TRUE),
  n_segment_distance_missing = sum(segment_distance_missing, na.rm = TRUE),
  n_segment_distance_mismatch = sum(segment_distance_mismatch, na.rm = TRUE),
  n_invalid_segment = sum(invalid_segment_flag, na.rm = TRUE),
  max_abs_segment_distance_gap_m = suppressWarnings(max(abs(constrained_segment_dist_m - dist_m), na.rm = TRUE))
), by = .(sample = sample, bandwidth_ft)]
fwrite(segment_contract_summary, "../output/rd_segment_contract_audit.csv")
if (any(unlist(segment_contract_summary[, .(
  n_missing_segment,
  n_segment_pair_mismatch,
  n_segment_distance_missing,
  n_segment_distance_mismatch,
  n_invalid_segment
)]) > 0, na.rm = TRUE)) {
  stop("Rental RD segment contract audit failed.", call. = FALSE)
}

rd_model_base <- rd_base[!is.na(segment_id) & segment_id != ""]

apply_sample_filter <- function(dt, sample_name) {
  if (sample_name == "all") {
    return(copy(dt))
  }
  if (sample_name == "multifamily_only") {
    return(copy(dt[building_type_clean == "multi_family"]))
  }
  if (sample_name == "clean_location") {
    return(copy(dt[flag_clean_location_sample == TRUE]))
  }
  if (sample_name == "no_modal_pair_change") {
    return(copy(dt[flag_no_modal_pair_change_sample == TRUE]))
  }
  if (sample_name == "no_modal_ward_change") {
    return(copy(dt[flag_no_modal_ward_change_sample == TRUE]))
  }
  if (sample_name == "no_questionable_address") {
    return(copy(dt[flag_no_questionable_address_sample == TRUE]))
  }
  stop(sprintf("Unknown sample: %s", sample_name), call. = FALSE)
}

sample_summary <- rbindlist(lapply(valid_samples, function(sample_i) {
  dt_i <- apply_sample_filter(rd_model_base, sample_i)
  dt_i[, .(
    n_rows = .N,
    n_segments = uniqueN(segment_id),
    n_ward_pairs = uniqueN(ward_pair_id),
    n_months = uniqueN(year_month),
    share_stricter_side = mean(side == "stricter_side", na.rm = TRUE),
    share_location_questionable = mean(flag_location_questionable, na.rm = TRUE),
    share_rd_location_questionable = mean(flag_rd_location_questionable, na.rm = TRUE),
    share_modal_changes_pair = mean(flag_modal_changes_pair, na.rm = TRUE),
    share_modal_changes_ward = mean(flag_modal_changes_ward | flag_modal_changes_neighbor_ward, na.rm = TRUE),
    share_modal_dist_diff_gt100ft = mean(flag_modal_dist_diff_gt100ft, na.rm = TRUE)
  )][, sample := sample_i][]
}), fill = TRUE)
setcolorder(sample_summary, c("sample", setdiff(names(sample_summary), "sample")))
fwrite(sample_summary, "../output/rd_sample_definition_summary.csv")

rent <- apply_sample_filter(rd_model_base, sample)
if (nrow(rent) == 0) {
  stop("No rental observations remain after RD audit sample filtering.", call. = FALSE)
}

flag_cols <- names(rent)[startsWith(names(rent), "flag_")]
if (length(flag_cols) == 0) {
  stop("Rental RD audit input has no flag columns to summarize.", call. = FALSE)
}
for (flag_col in flag_cols) {
  set(rent, j = flag_col, value = as.logical(rent[[flag_col]]))
}

summarize_flags <- function(dt, scope_name, group_cols) {
  by_cols <- c(group_cols, "side")
  rbindlist(lapply(flag_cols, function(flag_col) {
    out <- dt[, .(
      n_rows = .N,
      n_flagged = sum(get(flag_col), na.rm = TRUE),
      share_flagged = mean(get(flag_col), na.rm = TRUE)
    ), by = by_cols]
    out[, flag := flag_col]
    out[, scope := scope_name]
    setcolorder(out, c("scope", group_cols, "side", "flag", "n_rows", "n_flagged", "share_flagged"))
    out
  }), fill = TRUE)
}

flag_balance <- rbindlist(list(
  summarize_flags(rent, "side", character()),
  summarize_flags(rent, "ward_pair_side", "ward_pair_id"),
  summarize_flags(rent, "segment_month_side", c("segment_id", "year_month"))
), fill = TRUE)

if ("quality_flag_severity" %in% names(rent)) {
  severity_balance <- rbindlist(list(
    rent[, .N, by = .(side, quality_flag_severity)][, scope := "side"][],
    rent[, .N, by = .(ward_pair_id, side, quality_flag_severity)][, scope := "ward_pair_side"][],
    rent[, .N, by = .(segment_id, year_month, side, quality_flag_severity)][, scope := "segment_month_side"][]
  ), fill = TRUE)
  severity_balance[, flag := paste0("quality_flag_severity:", quality_flag_severity)]
  severity_balance[, n_rows := sum(N), by = .(scope, ward_pair_id, segment_id, year_month, side)]
  severity_balance[, n_flagged := N]
  severity_balance[, share_flagged := n_flagged / n_rows]
  severity_balance[, c("N", "quality_flag_severity") := NULL]
  setcolorder(severity_balance, names(flag_balance))
  flag_balance <- rbindlist(list(flag_balance, severity_balance), fill = TRUE)
}

setorder(flag_balance, scope, ward_pair_id, segment_id, year_month, side, flag)
fwrite(flag_balance, "../output/rd_sample_flag_balance.csv")

segment_month_balance <- flag_balance[scope == "segment_month_side"]
if (nrow(segment_month_balance) > 0) {
  segment_month_wide <- dcast(
    segment_month_balance,
    flag + segment_id + year_month ~ side,
    value.var = c("n_rows", "n_flagged", "share_flagged"),
    fill = 0
  )
  for (needed_col in c(
    "n_rows_lenient_side", "n_rows_stricter_side",
    "share_flagged_lenient_side", "share_flagged_stricter_side"
  )) {
    if (!needed_col %in% names(segment_month_wide)) {
      segment_month_wide[, (needed_col) := 0]
    }
  }
  segment_month_wide[, n_rows_total := n_rows_lenient_side + n_rows_stricter_side]
  segment_month_wide[, abs_share_diff := abs(share_flagged_lenient_side - share_flagged_stricter_side)]
  segment_month_imbalance <- segment_month_wide[, .(
    n_segment_month_cells = .N,
    n_two_sided_cells = sum(n_rows_lenient_side > 0 & n_rows_stricter_side > 0, na.rm = TRUE),
    n_rows_total = sum(n_rows_total, na.rm = TRUE),
    mean_abs_share_diff = mean(abs_share_diff, na.rm = TRUE),
    weighted_mean_abs_share_diff = weighted.mean(abs_share_diff, pmax(n_rows_total, 1), na.rm = TRUE),
    max_abs_share_diff = max(abs_share_diff, na.rm = TRUE)
  ), by = flag]
  setorder(segment_month_imbalance, -weighted_mean_abs_share_diff, flag)
} else {
  segment_month_imbalance <- data.table(
    flag = character(),
    n_segment_month_cells = integer(),
    n_two_sided_cells = integer(),
    n_rows_total = integer(),
    mean_abs_share_diff = numeric(),
    weighted_mean_abs_share_diff = numeric(),
    max_abs_share_diff = numeric()
  )
}
fwrite(segment_month_imbalance, "../output/rd_segment_month_flag_imbalance.csv")

message(sprintf("Saved RD quality-flag balance rows: %s", format(nrow(flag_balance), big.mark = ",")))
