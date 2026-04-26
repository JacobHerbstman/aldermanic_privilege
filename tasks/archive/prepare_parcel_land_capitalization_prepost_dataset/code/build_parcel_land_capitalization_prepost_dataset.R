source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_parcel_land_capitalization_prepost_dataset/code")
# in_panel <- "../input/parcel_land_redistricting_panel.parquet"
# out_panel <- "../output/parcel_land_capitalization_prepost_2014_2016.parquet"
# out_support <- "../output/parcel_land_capitalization_prepost_2014_2016_support.csv"
# pre_year <- 2014L
# post_year <- 2016L
# baseline_start_year <- 2012L
# baseline_end_year <- 2014L

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    out_panel,
    out_support,
    pre_year,
    post_year,
    baseline_start_year,
    baseline_end_year
  )
}

if (!length(cli_args) %in% c(3, 7)) {
  stop(
    "FATAL: Script requires 3 or 7 args: <in_panel_parquet> <out_panel_parquet> <out_support_csv> [<pre_year> <post_year> <baseline_start_year> <baseline_end_year>]",
    call. = FALSE
  )
}

in_panel <- cli_args[1]
out_panel <- cli_args[2]
out_support <- cli_args[3]
pre_year <- if (length(cli_args) == 7) as.integer(cli_args[4]) else 2014L
post_year <- if (length(cli_args) == 7) as.integer(cli_args[5]) else 2016L
baseline_start_year <- if (length(cli_args) == 7) as.integer(cli_args[6]) else 2012L
baseline_end_year <- if (length(cli_args) == 7) as.integer(cli_args[7]) else 2014L

if (!is.finite(pre_year) || !is.finite(post_year) || pre_year >= post_year) {
  stop("pre_year and post_year must be finite years with pre_year < post_year.", call. = FALSE)
}
if (!is.finite(baseline_start_year) || !is.finite(baseline_end_year) || baseline_start_year > baseline_end_year) {
  stop("baseline_start_year and baseline_end_year must be finite years with baseline_start_year <= baseline_end_year.", call. = FALSE)
}

message(sprintf(
  "Preparing parcel land capitalization pre/post panel: %d to %d, baseline-empty window %d-%d.",
  pre_year,
  post_year,
  baseline_start_year,
  baseline_end_year
))

stopifnot(file.exists(in_panel))

panel <- arrow::read_parquet(in_panel) %>%
  mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015),
    baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014),
    land_only = as.logical(land_only)
  )

if (nrow(panel) == 0) {
  stop("Input parcel land redistricting panel is empty.", call. = FALSE)
}

if (anyDuplicated(panel[c("pin10", "tax_year")]) > 0) {
  stop("Input parcel land redistricting panel has duplicate pin10 x tax_year rows.", call. = FALSE)
}

baseline_lookup <- panel %>%
  filter(tax_year >= baseline_start_year, tax_year <= baseline_end_year) %>%
  group_by(pin10) %>%
  arrange(desc(tax_year), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    pin10,
    baseline_pre_year = tax_year,
    baseline_empty_last_pre_period = bldg_sum == 0,
    baseline_land_sum = land_sum,
    baseline_bldg_sum = bldg_sum,
    baseline_land_psf = land_psf,
    baseline_class_primary_last_pre_period = class_primary,
    baseline_class_n_last_pre_period = class_n
  )

if (anyDuplicated(baseline_lookup["pin10"]) > 0) {
  stop("Baseline lookup has duplicate pin10 rows.", call. = FALSE)
}

tax_year_source_pre_col <- paste0("tax_year_source_", pre_year)
tax_year_source_post_col <- paste0("tax_year_source_", post_year)
land_sum_pre_col <- paste0("land_sum_", pre_year)
land_sum_post_col <- paste0("land_sum_", post_year)
log_land_sum_pre_col <- paste0("log_land_sum_", pre_year)
log_land_sum_post_col <- paste0("log_land_sum_", post_year)
land_psf_pre_col <- paste0("land_psf_", pre_year)
land_psf_post_col <- paste0("land_psf_", post_year)
log_land_psf_pre_col <- paste0("log_land_psf_", pre_year)
log_land_psf_post_col <- paste0("log_land_psf_", post_year)
land_share_pre_col <- paste0("land_share_pin10_", pre_year)
land_share_post_col <- paste0("land_share_pin10_", post_year)
bldg_sum_pre_col <- paste0("bldg_sum_", pre_year)
bldg_sum_post_col <- paste0("bldg_sum_", post_year)
land_only_pre_col <- paste0("land_only_", pre_year)
land_only_post_col <- paste0("land_only_", post_year)
delta_log_land_sum_col <- sprintf("delta_log_land_sum_%d_minus_%d", post_year, pre_year)
delta_log_land_psf_col <- sprintf("delta_log_land_psf_%d_minus_%d", post_year, pre_year)
delta_land_share_col <- sprintf("delta_land_share_%d_minus_%d", post_year, pre_year)
building_positive_post_col <- sprintf("building_positive_%d", post_year)
gained_positive_building_col <- sprintf("gained_positive_building_%d_%d", pre_year, post_year)
exited_land_only_col <- sprintf("exited_land_only_%d_%d", pre_year, post_year)
admin_only_period_col <- sprintf("admin_only_%d_%d", pre_year, post_year)

panel_prepost <- panel %>%
  select(-any_of(c(
    "baseline_pre_year_2012_2014_last",
    "baseline_empty_last_pre_2012_2014",
    "baseline_land_sum",
    "baseline_bldg_sum",
    "baseline_land_psf"
  ))) %>%
  inner_join(
    baseline_lookup %>% filter(baseline_empty_last_pre_period %in% TRUE),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  filter(
    tax_year %in% c(pre_year, post_year),
    in_1000ft %in% TRUE
  ) %>%
  select(
    pin10, tax_year,
    tax_year_source, tax_year_source_n,
    strictness_change, switched_2015, valid_control_2015,
    segment_id, ward_origin, ward_dest, ward_pair_id,
    block_id, block_origin_side_id, dist_to_boundary_ft,
    in_500ft, in_1000ft,
    lot_sqft_current, lot_sqft_resolution,
    property_use_group_current, property_use_group_current_resolution,
    property_use_standardized_current, property_use_standardized_current_resolution,
    property_use_muni_current, property_use_muni_current_resolution,
    zoned_code_local_current, zoned_code_local_current_resolution,
    owner_type_description1_current, owner_type_description1_current_resolution,
    any_exemption_current, any_exemption_current_n_values,
    land_sum, log_land_sum, land_psf, log_land_psf,
    land_share_pin10, bldg_sum, total_av, land_only,
    class_current, triad_name, township_name,
    baseline_pre_year, baseline_empty_last_pre_period,
    baseline_land_sum, baseline_bldg_sum, baseline_land_psf,
    baseline_class_primary_last_pre_period, baseline_class_n_last_pre_period
  ) %>%
  mutate(
    pre_year = !!pre_year,
    post_year = !!post_year,
    baseline_start_year = !!baseline_start_year,
    baseline_end_year = !!baseline_end_year,
    treatment_sign = case_when(
      strictness_change < 0 ~ "to_lenient",
      strictness_change > 0 ~ "to_stricter",
      TRUE ~ "no_change"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = tax_year,
    values_from = c(
      tax_year_source, tax_year_source_n,
      land_sum, log_land_sum, land_psf, log_land_psf,
      land_share_pin10, bldg_sum, total_av, land_only
    ),
    names_sep = "_"
  ) %>%
  mutate(
    has_pre_year = !is.na(.data[[land_sum_pre_col]]) | !is.na(.data[[bldg_sum_pre_col]]),
    has_post_year = !is.na(.data[[land_sum_post_col]]) | !is.na(.data[[bldg_sum_post_col]])
  ) %>%
  filter(has_pre_year, has_post_year) %>%
  mutate(
    admin_only_prepost = .data[[tax_year_source_pre_col]] %in% c("taxyear", "assr_year") &
      .data[[tax_year_source_post_col]] %in% c("taxyear", "assr_year"),
    "{admin_only_period_col}" := admin_only_prepost,
    sample_no_ex = is.na(class_current) | class_current != "EX",
    sample_developable_loose = (is.na(class_current) | class_current != "EX") &
      !is.na(lot_sqft_current) &
      lot_sqft_current >= 1875,
    sample_developable_core = (is.na(class_current) | class_current != "EX") &
      !is.na(lot_sqft_current) &
      lot_sqft_current >= 2500,
    baseline_class_vacant_code_last_pre_period = !is.na(baseline_class_primary_last_pre_period) &
      stringr::str_starts(baseline_class_primary_last_pre_period, "V"),
    baseline_class_vacant_urban_last_pre_period = baseline_class_primary_last_pre_period %in%
      c("VRES", "VMSC", "VCOM", "VIND"),
    sample_history_vacant_loose = sample_developable_loose &
      baseline_class_vacant_code_last_pre_period %in% TRUE,
    sample_history_vacant_core = sample_developable_core &
      baseline_class_vacant_code_last_pre_period %in% TRUE,
    sample_history_vacant_urban_core = sample_developable_core &
      baseline_class_vacant_urban_last_pre_period %in% TRUE,
    current_property_use_vacant_land = property_use_group_current == "VACANT LAND",
    current_owner_public_like = !is.na(owner_type_description1_current) &
      stringr::str_detect(
        owner_type_description1_current,
        "GOVERNMENT|MUNICIPAL|COUNTY|CITY|STATE|FEDERAL|PUBLIC|SCHOOL|CHURCH|RELIG|HOSPITAL|CEMETERY|PARK|DISTRICT|UTILITY|RAIL"
      ),
    sample_current_vacant_land_loose = sample_developable_loose &
      current_property_use_vacant_land %in% TRUE,
    sample_current_vacant_land_core = sample_developable_core &
      current_property_use_vacant_land %in% TRUE,
    sample_current_vacant_private_core = sample_current_vacant_land_core &
      !dplyr::coalesce(current_owner_public_like, FALSE) &
      !dplyr::coalesce(any_exemption_current, FALSE),
    "{delta_log_land_sum_col}" := .data[[log_land_sum_post_col]] - .data[[log_land_sum_pre_col]],
    "{delta_log_land_psf_col}" := .data[[log_land_psf_post_col]] - .data[[log_land_psf_pre_col]],
    "{delta_land_share_col}" := .data[[land_share_post_col]] - .data[[land_share_pre_col]],
    "{building_positive_post_col}" := .data[[bldg_sum_post_col]] > 0,
    "{gained_positive_building_col}" := .data[[bldg_sum_pre_col]] == 0 & .data[[bldg_sum_post_col]] > 0,
    "{exited_land_only_col}" := .data[[land_only_pre_col]] %in% TRUE & .data[[land_only_post_col]] %in% FALSE
  ) %>%
  arrange(pin10)

sample_definitions <- tribble(
  ~sample_scope, ~sample_label,
  "all_baseline_empty", "All baseline-empty",
  "no_ex", "Exclude EX",
  "developable_loose", "Developable loose",
  "developable_core", "Developable core",
  "history_vacant_loose", "History vacant loose",
  "history_vacant_core", "History vacant core",
  "history_vacant_urban_core", "History vacant urban core",
  "current_vacant_loose", "Current vacant loose",
  "current_vacant_core", "Current vacant core",
  "current_vacant_private_core", "Current vacant private core"
)

support <- bind_rows(
  lapply(seq_len(nrow(sample_definitions)), function(i) {
    sample_scope <- sample_definitions$sample_scope[[i]]
    sample_label <- sample_definitions$sample_label[[i]]
    sample_panel <- panel_prepost %>%
      filter(
        case_when(
          sample_scope == "all_baseline_empty" ~ TRUE,
          sample_scope == "no_ex" ~ sample_no_ex %in% TRUE,
          sample_scope == "developable_loose" ~ sample_developable_loose %in% TRUE,
          sample_scope == "developable_core" ~ sample_developable_core %in% TRUE,
          sample_scope == "history_vacant_loose" ~ sample_history_vacant_loose %in% TRUE,
          sample_scope == "history_vacant_core" ~ sample_history_vacant_core %in% TRUE,
          sample_scope == "history_vacant_urban_core" ~ sample_history_vacant_urban_core %in% TRUE,
          sample_scope == "current_vacant_loose" ~ sample_current_vacant_land_loose %in% TRUE,
          sample_scope == "current_vacant_core" ~ sample_current_vacant_land_core %in% TRUE,
          sample_scope == "current_vacant_private_core" ~ sample_current_vacant_private_core %in% TRUE,
          TRUE ~ FALSE
        )
      )

    bind_rows(
      sample_panel %>% mutate(bandwidth = "1000ft"),
      sample_panel %>% filter(in_500ft %in% TRUE) %>% mutate(bandwidth = "500ft")
    ) %>%
      mutate(
        sample_scope = sample_scope,
        sample_label = sample_label
      )
  })
) %>%
  mutate(
    sample_scope = factor(
      sample_scope,
      levels = c(
        "all_baseline_empty", "no_ex", "developable_loose", "developable_core",
        "history_vacant_loose", "history_vacant_core", "history_vacant_urban_core",
        "current_vacant_loose", "current_vacant_core", "current_vacant_private_core"
      )
    ),
    sample_label = factor(
      sample_label,
      levels = c(
        "All baseline-empty", "Exclude EX", "Developable loose", "Developable core",
        "History vacant loose", "History vacant core", "History vacant urban core",
        "Current vacant loose", "Current vacant core", "Current vacant private core"
      )
    ),
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    treatment_sign = factor(treatment_sign, levels = c("to_lenient", "no_change", "to_stricter"))
  ) %>%
	  group_by(sample_scope, sample_label, bandwidth, treatment_sign) %>%
	  summarise(
    pre_year = dplyr::first(pre_year),
    post_year = dplyr::first(post_year),
    baseline_start_year = dplyr::first(baseline_start_year),
    baseline_end_year = dplyr::first(baseline_end_year),
	    n_pin10 = n(),
	    n_switchers = sum(switched_2015),
	    n_valid_controls = sum(valid_control_2015),
	    n_admin_only_prepost = sum(admin_only_prepost),
	    n_with_log_land_sum_delta = sum(!is.na(.data[[delta_log_land_sum_col]])),
	    n_with_log_land_psf_delta = sum(!is.na(.data[[delta_log_land_psf_col]])),
	    share_missing_log_land_sum_delta = mean(is.na(.data[[delta_log_land_sum_col]])),
	    share_missing_log_land_psf_delta = mean(is.na(.data[[delta_log_land_psf_col]])),
	    share_positive_building_post = mean(.data[[building_positive_post_col]], na.rm = TRUE),
	    share_gained_positive_building = mean(.data[[gained_positive_building_col]], na.rm = TRUE),
	    median_lot_sqft = median(lot_sqft_current, na.rm = TRUE),
	    median_baseline_land_psf = median(baseline_land_psf, na.rm = TRUE),
	    .groups = "drop"
  ) %>%
  arrange(sample_scope, bandwidth, treatment_sign)

arrow::write_parquet(panel_prepost, out_panel, compression = "zstd")
readr::write_csv(support, out_support)

message(sprintf("Prepared %s", out_panel))
message(sprintf("Prepared %s", out_support))
