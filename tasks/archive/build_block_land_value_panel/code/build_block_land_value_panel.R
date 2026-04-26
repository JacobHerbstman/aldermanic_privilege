source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/build_block_land_value_panel/code")
# in_panel <- "../input/parcel_land_redistricting_panel.parquet"
# out_panel <- "../output/block_land_value_panel.parquet"
# out_support <- "../output/block_land_value_panel_support.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(in_panel, out_panel, out_support)
}

if (length(cli_args) != 3) {
  stop(
    "FATAL: Script requires 3 args: <parcel_land_redistricting_panel_parquet> <out_panel_parquet> <out_support_csv>",
    call. = FALSE
  )
}

in_panel <- cli_args[1]
out_panel <- cli_args[2]
out_support <- cli_args[3]

stopifnot(file.exists(in_panel))

sample_definitions <- tibble::tribble(
  ~sample_scope, ~sample_label,
  "developable_core", "Developable core",
  "history_vacant_core", "History vacant core"
)

message("Reading parcel land redistricting panel...")
parcel_panel <- arrow::read_parquet(in_panel) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    block_origin_side_id = as.character(block_origin_side_id),
    block_id = as.character(block_id),
    segment_id = as.character(segment_id),
    ward_pair_id = as.character(ward_pair_id),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014),
    any_exemption_current = as.logical(any_exemption_current),
    admin_source = tax_year_source %in% c("taxyear", "assr_year"),
    comparison_sample = switched_2015 %in% TRUE | valid_control_2015 %in% TRUE,
    treatment_sign = dplyr::case_when(
      strictness_change < 0 ~ "to_lenient",
      strictness_change > 0 ~ "to_stricter",
      TRUE ~ "no_change"
    ),
    block_border_side_id = paste(block_origin_side_id, ward_pair_id, segment_id, ward_origin, sep = "__")
  ) %>%
  dplyr::filter(
    in_1000ft %in% TRUE,
    !is.na(block_origin_side_id),
    block_origin_side_id != "",
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair_id),
    ward_pair_id != "",
    baseline_empty_last_pre_2012_2014 %in% TRUE
  )

if (nrow(parcel_panel) == 0) {
  stop("No baseline-empty near-boundary parcel-year rows remain for block aggregation.", call. = FALSE)
}

baseline_lookup <- parcel_panel %>%
  dplyr::filter(tax_year >= 2012L, tax_year <= 2014L) %>%
  dplyr::group_by(pin10) %>%
  dplyr::arrange(dplyr::desc(tax_year), .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    pin10,
    baseline_class_primary_last_pre_2012_2014 = class_primary,
    baseline_class_n_last_pre_2012_2014 = class_n
  )

if (anyDuplicated(baseline_lookup["pin10"]) > 0) {
  stop("Baseline class lookup has duplicate pin10 rows.", call. = FALSE)
}

parcel_panel <- parcel_panel %>%
  dplyr::left_join(baseline_lookup, by = "pin10", relationship = "many-to-one") %>%
  dplyr::mutate(
    sample_all_baseline_empty = TRUE,
    sample_no_ex = is.na(class_current) | class_current != "EX",
    sample_developable_core = sample_no_ex %in% TRUE &
      !is.na(lot_sqft_current) &
      lot_sqft_current >= 2500,
    baseline_class_vacant_code_last_pre_2012_2014 = !is.na(baseline_class_primary_last_pre_2012_2014) &
      stringr::str_starts(baseline_class_primary_last_pre_2012_2014, "V"),
    baseline_class_vacant_urban_last_pre_2012_2014 = baseline_class_primary_last_pre_2012_2014 %in%
      c("VRES", "VMSC", "VCOM", "VIND"),
    sample_history_vacant_core = sample_developable_core %in% TRUE &
      baseline_class_vacant_code_last_pre_2012_2014 %in% TRUE,
    sample_history_vacant_urban_core = sample_developable_core %in% TRUE &
      baseline_class_vacant_urban_last_pre_2012_2014 %in% TRUE,
    current_property_use_vacant_land = property_use_group_current == "VACANT LAND",
    current_owner_public_like = !is.na(owner_type_description1_current) &
      stringr::str_detect(
        owner_type_description1_current,
        "GOVERNMENT|MUNICIPAL|COUNTY|CITY|STATE|FEDERAL|PUBLIC|SCHOOL|CHURCH|RELIG|HOSPITAL|CEMETERY|PARK|DISTRICT|UTILITY|RAIL"
      ),
    sample_current_vacant_core = sample_developable_core %in% TRUE &
      current_property_use_vacant_land %in% TRUE,
    sample_current_vacant_private_core = sample_current_vacant_core %in% TRUE &
      !dplyr::coalesce(current_owner_public_like, FALSE) &
      !dplyr::coalesce(any_exemption_current, FALSE)
  ) %>%
  dplyr::select(
    pin10, tax_year, block_border_side_id, block_origin_side_id, block_id, ward_pair_id, segment_id,
    ward_origin, ward_dest, switched_2015, valid_control_2015, comparison_sample,
    strictness_change, treatment_sign, dist_to_boundary_ft, in_500ft,
    admin_source, land_sum, bldg_sum, total_av, lot_sqft_current,
    log_land_psf, land_psf,
    sample_all_baseline_empty, sample_developable_core, sample_history_vacant_core
  )

message("Preparing long sample table...")
parcel_dt <- data.table::as.data.table(parcel_panel)
block_inputs <- vector("list", nrow(sample_definitions) * 2L)
list_i <- 1L

for (i in seq_len(nrow(sample_definitions))) {
  sample_scope_i <- sample_definitions$sample_scope[[i]]
  sample_label_i <- sample_definitions$sample_label[[i]]
  sample_flag_i <- paste0("sample_", sample_scope_i)

  block_inputs[[list_i]] <- data.table::copy(parcel_dt[get(sample_flag_i) %in% TRUE])
  block_inputs[[list_i]][, `:=`(sample_scope = sample_scope_i, sample_label = sample_label_i, bandwidth = "1000ft")]
  list_i <- list_i + 1L

  block_inputs[[list_i]] <- data.table::copy(parcel_dt[get(sample_flag_i) %in% TRUE & in_500ft %in% TRUE])
  block_inputs[[list_i]][, `:=`(sample_scope = sample_scope_i, sample_label = sample_label_i, bandwidth = "500ft")]
  list_i <- list_i + 1L
}

sample_dt <- data.table::rbindlist(block_inputs, use.names = TRUE)
rm(block_inputs, parcel_dt)

message("Aggregating parcel-year rows to block-side-year rows...")
block_panel <- sample_dt[
  ,
  .(
    block_origin_side_id = block_origin_side_id[1L],
    block_id = block_id[1L],
    ward_pair_id = ward_pair_id[1L],
    segment_id = segment_id[1L],
    ward_origin = ward_origin[1L],
    ward_dest = ward_dest[1L],
    switched_2015 = switched_2015[1L],
    valid_control_2015 = valid_control_2015[1L],
    comparison_sample = comparison_sample[1L],
    strictness_change = strictness_change[1L],
    treatment_sign = treatment_sign[1L],
    mean_dist_to_boundary_ft = mean(dist_to_boundary_ft, na.rm = TRUE),
    n_pin10 = data.table::uniqueN(pin10),
    n_admin_pin10 = sum(admin_source, na.rm = TRUE),
    n_land_positive = sum(land_sum > 0, na.rm = TRUE),
    n_building_positive = sum(bldg_sum > 0, na.rm = TRUE),
    land_sum_block = sum(land_sum, na.rm = TRUE),
    bldg_sum_block = sum(bldg_sum, na.rm = TRUE),
    total_av_block = sum(total_av, na.rm = TRUE),
    lot_sqft_block = sum(lot_sqft_current, na.rm = TRUE),
    mean_log_land_psf = mean(log_land_psf, na.rm = TRUE),
    mean_land_psf = mean(land_psf, na.rm = TRUE)
  ),
  by = .(sample_scope, sample_label, bandwidth, block_border_side_id, tax_year)
]
rm(sample_dt)

block_panel[
  ,
  n_pin10_max := max(n_pin10, na.rm = TRUE),
  by = .(sample_scope, bandwidth, block_border_side_id)
]
block_panel[
  ,
  `:=`(
    coverage_share = n_pin10 / n_pin10_max,
    admin_share = n_admin_pin10 / n_pin10,
    land_psf_block = data.table::fifelse(
      is.finite(land_sum_block) & land_sum_block > 0 &
        is.finite(lot_sqft_block) & lot_sqft_block > 0,
      land_sum_block / lot_sqft_block,
      NA_real_
    ),
    log_land_sum_block = data.table::fifelse(land_sum_block > 0, log(land_sum_block), NA_real_),
    land_share_block = data.table::fifelse(total_av_block > 0, land_sum_block / total_av_block, NA_real_),
    building_positive_share = n_building_positive / n_pin10,
    land_positive_share = n_land_positive / n_pin10
  )
]
block_panel[, log_land_psf_block := data.table::fifelse(land_psf_block > 0, log(land_psf_block), NA_real_)]
data.table::setorder(block_panel, sample_scope, bandwidth, block_border_side_id, tax_year)

if (anyDuplicated(block_panel[, .(sample_scope, bandwidth, block_border_side_id, tax_year)]) > 0) {
  stop("Block panel is not unique by sample_scope x bandwidth x block_border_side_id x tax_year.", call. = FALSE)
}

static_field_counts <- block_panel[
  ,
  lapply(.SD, data.table::uniqueN),
  by = .(sample_scope, bandwidth, block_border_side_id),
  .SDcols = c(
    "block_origin_side_id",
    "block_id",
    "ward_pair_id",
    "segment_id",
    "ward_origin",
    "ward_dest",
    "switched_2015",
    "valid_control_2015",
    "comparison_sample",
    "strictness_change",
    "treatment_sign"
  )
]
static_check_cols <- setdiff(names(static_field_counts), c("sample_scope", "bandwidth", "block_border_side_id"))
if (nrow(static_field_counts[rowSums(static_field_counts[, ..static_check_cols] > 1L) > 0]) > 0) {
  stop("Static border fields vary within block_border_side_id.", call. = FALSE)
}

support <- block_panel[
  ,
  .(
    n_block_sides = data.table::uniqueN(block_origin_side_id),
    n_block_border_sides = data.table::uniqueN(block_border_side_id),
    n_census_blocks = data.table::uniqueN(block_id),
    n_segments = data.table::uniqueN(segment_id),
    n_pin10 = sum(n_pin10, na.rm = TRUE),
    mean_coverage_share = mean(coverage_share, na.rm = TRUE),
    share_admin_only_block_years = mean(admin_share == 1, na.rm = TRUE),
    share_admin_95_block_years = mean(admin_share >= 0.95, na.rm = TRUE),
    mean_building_positive_share = mean(building_positive_share, na.rm = TRUE),
    mean_land_psf_block = mean(land_psf_block, na.rm = TRUE)
  ),
  by = .(sample_scope, sample_label, bandwidth, treatment_sign, tax_year)
]
data.table::setorder(support, sample_scope, bandwidth, treatment_sign, tax_year)

message("Writing block land-value panel and support outputs...")
arrow::write_parquet(block_panel, out_panel, compression = "zstd")
readr::write_csv(support, out_support)

message(sprintf("Saved %s", out_panel))
message(sprintf("Saved %s", out_support))
