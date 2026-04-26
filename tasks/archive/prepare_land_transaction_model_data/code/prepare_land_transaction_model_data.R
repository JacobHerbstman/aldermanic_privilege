source("../../setup_environment/code/packages.R")
source("../../_lib/land_transaction_model_helpers.R")

options(dplyr.summarise.inform = FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_land_transaction_model_data/code")
# events_input <- "../input/land_transaction_sale_events.parquet"
# incidence_input <- "../input/land_transaction_incidence_panel.parquet"
# output_incidence <- "../output/land_transaction_incidence_model_panel.parquet"
# output_price <- "../output/land_transaction_price_model_panel.parquet"
# output_support <- "../output/land_transaction_model_sample_support.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(events_input, incidence_input, output_incidence, output_price, output_support)
}

if (length(cli_args) != 5) {
  stop(
    paste(
      "FATAL: Script requires 5 args:",
      "<events_input_parquet> <incidence_input_parquet>",
      "<output_incidence_parquet> <output_price_parquet> <output_support_csv>"
    ),
    call. = FALSE
  )
}

events_input <- cli_args[1]
incidence_input <- cli_args[2]
output_incidence <- cli_args[3]
output_price <- cli_args[4]
output_support <- cli_args[5]

cohort_defs <- data.table::data.table(
  cohort = c("cohort_2012", "cohort_2015"),
  event_year = c(2012L, 2015L),
  start_year = c(2007L, 2007L),
  end_year = c(2020L, 2020L)
)

message("Loading land-transaction sale events...")
events_dt <- data.table::as.data.table(arrow::read_parquet(events_input))
events_dt[, `:=`(
  pin10 = as.character(pin10),
  sale_event_id = as.character(sale_event_id),
  sale_year = as.integer(sale_year),
  sale_price = as.numeric(sale_price),
  lot_sqft_current = as.numeric(lot_sqft_current),
  ward_origin = as.integer(ward_origin),
  dist_to_boundary_ft = as.numeric(dist_to_boundary_ft),
  switched_2015 = as.logical(switched_2015),
  valid_control_2015 = as.logical(valid_control_2015),
  strictness_change = as.numeric(strictness_change),
  market_deed_flag = as.logical(market_deed_flag),
  package_sale_flag = as.logical(package_sale_flag),
  single_parcel_sale_flag = as.logical(single_parcel_sale_flag),
  sale_filter_same_sale_within_365 = as.logical(sale_filter_same_sale_within_365),
  sale_filter_less_than_10k = as.logical(sale_filter_less_than_10k),
  sale_filter_deed_type = as.logical(sale_filter_deed_type),
  raw_land_sale_flag = as.logical(raw_land_sale_flag),
  sale_time_land_like_flag = as.logical(sale_time_land_like_flag),
  in_500ft = as.logical(in_500ft),
  in_1000ft = as.logical(in_1000ft)
)]

message("Loading land-transaction incidence panel...")
incidence_dt <- data.table::as.data.table(arrow::read_parquet(incidence_input))
incidence_dt[, `:=`(
  pin10 = as.character(pin10),
  calendar_year = as.integer(calendar_year),
  cohort = as.character(cohort),
  event_year = as.integer(event_year),
  ward_origin = as.integer(ward_origin),
  dist_to_boundary_ft = as.numeric(dist_to_boundary_ft),
  switched_2015 = as.logical(switched_2015),
  valid_control_2015 = as.logical(valid_control_2015),
  strictness_change = as.numeric(strictness_change),
  baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014),
  in_500ft = as.logical(in_500ft),
  in_1000ft = as.logical(in_1000ft)
)]

if (anyDuplicated(events_dt$sale_event_id) > 0) {
  stop("Land-transaction sale events are not unique by sale_event_id.", call. = FALSE)
}
if (anyDuplicated(incidence_dt[, .(pin10, cohort, calendar_year)]) > 0) {
  stop("Land-transaction incidence panel is not unique by pin10 x cohort x calendar_year.", call. = FALSE)
}
if (incidence_dt[baseline_empty_last_pre_2012_2014 %in% FALSE, .N] > 0) {
  stop("Incidence input includes non-baseline-empty parcels.", call. = FALSE)
}

incidence_dt <- incidence_dt[
  switched_2015 %in% TRUE | valid_control_2015 %in% TRUE
]
events_dt <- events_dt[
  switched_2015 %in% TRUE | valid_control_2015 %in% TRUE
]

incidence_dt <- merge(
  incidence_dt,
  cohort_defs,
  by = c("cohort", "event_year"),
  all.x = TRUE,
  sort = FALSE
)

if (incidence_dt[is.na(start_year) | is.na(end_year), .N] > 0) {
  stop("Incidence input contains unsupported cohort values.", call. = FALSE)
}
if (incidence_dt[calendar_year < start_year | calendar_year > end_year, .N] > 0) {
  stop("Incidence input includes years outside the documented cohort windows.", call. = FALSE)
}
if (incidence_dt[relative_year != calendar_year - event_year, .N] > 0) {
  stop("Incidence input has inconsistent relative-year coding.", call. = FALSE)
}

incidence_dt[, `:=`(
  treat = as.integer(switched_2015 %in% TRUE),
  treatment_stricter_continuous = pmax(strictness_change, 0),
  treatment_lenient_continuous = pmax(-strictness_change, 0),
  ward_pair_side = paste(ward_pair_id, ward_origin, sep = "_"),
  segment_side = paste(segment_id, ward_origin, sep = "_"),
  block_group_id = substr(block_id, 1, 12)
)]

pin_design_dt <- unique(incidence_dt[, .(
  pin10,
  block_group_id,
  ward_pair_id,
  treat
)])

bg_flags_dt <- pin_design_dt[, .(
  n_bg_pin10 = data.table::uniqueN(pin10),
  n_pairs = data.table::uniqueN(ward_pair_id),
  n_treat_status = data.table::uniqueN(treat),
  keep_single_pair_bg = data.table::uniqueN(ward_pair_id) == 1L,
  keep_unmixed_treat_bg = data.table::uniqueN(treat) == 1L
), by = block_group_id]

incidence_dt <- merge(
  incidence_dt,
  bg_flags_dt[, .(
    block_group_id,
    keep_single_pair_bg,
    keep_unmixed_treat_bg,
    n_bg_pin10,
    n_pairs,
    n_treat_status
  )],
  by = "block_group_id",
  all.x = TRUE,
  sort = FALSE
)

year_counts_dt <- incidence_dt[, .(
  n_pin10 = data.table::uniqueN(pin10)
), by = .(cohort, calendar_year)]
if (year_counts_dt[, data.table::uniqueN(n_pin10), by = cohort][V1 != 1L, .N] > 0) {
  stop("Incidence denominator count is not constant across years within a cohort.", call. = FALSE)
}

events_dt[, `:=`(
  treat = as.integer(switched_2015 %in% TRUE),
  treatment_stricter_continuous = pmax(strictness_change, 0),
  treatment_lenient_continuous = pmax(-strictness_change, 0),
  ward_pair_side = paste(ward_pair_id, ward_origin, sep = "_"),
  segment_side = paste(segment_id, ward_origin, sep = "_"),
  block_group_id = substr(block_id, 1, 12),
  normalized_buyer_name = normalize_land_transaction_party_name(sale_buyer_name),
  normalized_seller_name = normalize_land_transaction_party_name(sale_seller_name)
)]

events_dt[, `:=`(
  buyer_seller_name_present = normalized_buyer_name != "" & normalized_seller_name != "",
  buyer_seller_exact_match_flag = normalized_buyer_name != "" &
    normalized_seller_name != "" &
    normalized_buyer_name == normalized_seller_name
)]

events_dt[, arm_length_flag := (
  market_deed_flag %in% TRUE &
    !(package_sale_flag %in% TRUE) &
    single_parcel_sale_flag %in% TRUE &
    !(sale_filter_same_sale_within_365 %in% TRUE) &
    !(sale_filter_less_than_10k %in% TRUE) &
    !(sale_filter_deed_type %in% TRUE) &
    is.finite(sale_price) &
    sale_price > 10000 &
    buyer_seller_name_present %in% TRUE &
    !(buyer_seller_exact_match_flag %in% TRUE)
)]

events_dt[, `:=`(
  small_package_le2_flag = (
    !(package_sale_flag %in% TRUE) |
      (is.finite(num_parcels_sale) & num_parcels_sale <= 2)
  )
)]

events_dt[, `:=`(
  log_sale_price = dplyr::if_else(
    is.finite(sale_price) & sale_price > 0,
    log(sale_price),
    NA_real_
  ),
  log_sale_price_psf_current = dplyr::if_else(
    is.finite(sale_price) & sale_price > 0 &
      is.finite(lot_sqft_current) & lot_sqft_current > 0,
    log(sale_price / lot_sqft_current),
    NA_real_
  ),
  price_sample_broad_land_like = is.finite(sale_price) & sale_price > 10000 &
    !(sale_filter_less_than_10k %in% TRUE) &
    sale_time_land_like_flag %in% TRUE,
  price_sample_arm_length_land_like = is.finite(sale_price) & sale_price > 0 &
    sale_time_land_like_flag %in% TRUE & arm_length_flag %in% TRUE,
  price_sample_arm_length_raw_land = is.finite(sale_price) & sale_price > 0 &
    raw_land_sale_flag %in% TRUE & arm_length_flag %in% TRUE,
  price_sample_raw_land_small_package_le2 = is.finite(sale_price) & sale_price > 10000 &
    !(sale_filter_less_than_10k %in% TRUE) &
    raw_land_sale_flag %in% TRUE &
    small_package_le2_flag %in% TRUE,
  price_sample_warranty_small_package_le2 = is.finite(sale_price) & sale_price > 10000 &
    !(sale_filter_less_than_10k %in% TRUE) &
    sale_deed_type == "Warranty" &
    small_package_le2_flag %in% TRUE
)]

events_dt <- merge(
  events_dt,
  bg_flags_dt[, .(
    block_group_id,
    keep_single_pair_bg,
    keep_unmixed_treat_bg,
    n_bg_pin10,
    n_pairs,
    n_treat_status
  )],
  by = "block_group_id",
  all.x = TRUE,
  sort = FALSE
)

annual_sales_dt <- events_dt[, .(
  sold_any = as.integer(.N > 0),
  sold_any_arm_length = as.integer(any(arm_length_flag %in% TRUE)),
  sold_land_like = as.integer(any(sale_time_land_like_flag %in% TRUE)),
  sold_land_like_arm_length = as.integer(any(
    sale_time_land_like_flag %in% TRUE &
      arm_length_flag %in% TRUE
  )),
  sold_land_tag = as.integer(any(raw_land_sale_flag %in% TRUE)),
  sold_land_tag_arm_length = as.integer(any(
    raw_land_sale_flag %in% TRUE &
      arm_length_flag %in% TRUE
  )),
  n_sales_any = .N,
  n_sales_any_arm_length = sum(arm_length_flag %in% TRUE),
  n_sales_land_like = sum(sale_time_land_like_flag %in% TRUE),
  n_sales_land_like_arm_length = sum(
    sale_time_land_like_flag %in% TRUE &
      arm_length_flag %in% TRUE
  ),
  n_sales_land_tag = sum(raw_land_sale_flag %in% TRUE),
  n_sales_land_tag_arm_length = sum(
    raw_land_sale_flag %in% TRUE &
      arm_length_flag %in% TRUE
  )
), by = .(pin10, calendar_year = sale_year)]

data.table::setnames(
  incidence_dt,
  old = c(
    "sold_any",
    "sold_land_like",
    "sold_land_tag",
    "n_sales_any",
    "n_sales_land_like",
    "n_sales_land_tag"
  ),
  new = c(
    "sold_any_legacy",
    "sold_land_like_legacy",
    "sold_land_tag_legacy",
    "n_sales_any_legacy",
    "n_sales_land_like_legacy",
    "n_sales_land_tag_legacy"
  )
)

incidence_dt <- merge(
  incidence_dt,
  annual_sales_dt,
  by = c("pin10", "calendar_year"),
  all.x = TRUE,
  sort = FALSE
)

fill_zero_cols <- c(
  "sold_any",
  "sold_any_arm_length",
  "sold_land_like",
  "sold_land_like_arm_length",
  "sold_land_tag",
  "sold_land_tag_arm_length",
  "n_sales_any",
  "n_sales_any_arm_length",
  "n_sales_land_like",
  "n_sales_land_like_arm_length",
  "n_sales_land_tag",
  "n_sales_land_tag_arm_length"
)
for (col_name in fill_zero_cols) {
  data.table::set(
    incidence_dt,
    i = which(is.na(incidence_dt[[col_name]])),
    j = col_name,
    value = 0L
  )
}

legacy_compare_dt <- incidence_dt[, .(
  sold_any_mismatch = sum(sold_any != sold_any_legacy, na.rm = TRUE),
  sold_land_like_mismatch = sum(sold_land_like != sold_land_like_legacy, na.rm = TRUE),
  sold_land_tag_mismatch = sum(sold_land_tag != sold_land_tag_legacy, na.rm = TRUE),
  n_sales_any_mismatch = sum(n_sales_any != n_sales_any_legacy, na.rm = TRUE),
  n_sales_land_like_mismatch = sum(n_sales_land_like != n_sales_land_like_legacy, na.rm = TRUE),
  n_sales_land_tag_mismatch = sum(n_sales_land_tag != n_sales_land_tag_legacy, na.rm = TRUE)
)]
if (legacy_compare_dt[, sum(.SD)] > 0) {
  stop("Rebuilt incidence outcomes do not match the existing incidence panel.", call. = FALSE)
}

incidence_dt[, c(
  "sold_any_legacy",
  "sold_land_like_legacy",
  "sold_land_tag_legacy",
  "n_sales_any_legacy",
  "n_sales_land_like_legacy",
  "n_sales_land_tag_legacy",
  "start_year",
  "end_year"
) := NULL]

price_panel_list <- lapply(seq_len(nrow(cohort_defs)), function(i) {
  cohort_row <- cohort_defs[i]
  price_dt <- data.table::copy(events_dt[
    sale_year >= cohort_row$start_year &
      sale_year <= cohort_row$end_year
  ])
  price_dt[, `:=`(
    cohort = cohort_row$cohort,
    event_year = cohort_row$event_year,
    relative_year = sale_year - cohort_row$event_year
  )]
  price_dt
})

price_dt <- data.table::rbindlist(price_panel_list, use.names = TRUE)
data.table::setorder(price_dt, cohort, sale_event_id)

if (anyDuplicated(price_dt[, .(sale_event_id, cohort)]) > 0) {
  stop("Price model panel is not unique by sale_event_id x cohort.", call. = FALSE)
}

incidence_support_source_dt <- data.table::rbindlist(
  list(
    data.table::copy(incidence_dt)[, bandwidth := "1000ft"],
    data.table::copy(incidence_dt[in_500ft %in% TRUE])[, bandwidth := "500ft"]
  ),
  use.names = TRUE
)

incidence_support_dt <- incidence_support_source_dt[, .(
  panel = "incidence",
  sample_definition = "denominator",
  n_observations = .N,
  n_pin10 = data.table::uniqueN(pin10),
  n_treated_pin10 = data.table::uniqueN(pin10[treat == 1]),
  n_control_pin10 = data.table::uniqueN(pin10[treat == 0]),
  n_sale_events = NA_integer_,
  n_treated_sale_events = NA_integer_,
  n_control_sale_events = NA_integer_,
  n_exact_name_match_sales = NA_integer_,
  n_missing_party_name_sales = NA_integer_
), by = .(cohort, bandwidth)]

price_support_source_dt <- data.table::rbindlist(
  list(
    data.table::copy(price_dt)[, bandwidth := "1000ft"],
    data.table::copy(price_dt[in_500ft %in% TRUE])[, bandwidth := "500ft"]
  ),
  use.names = TRUE
)

build_price_support_block <- function(dt, sample_definition) {
  dt[, .(
    panel = "price",
    sample_definition = sample_definition,
    n_observations = .N,
    n_pin10 = data.table::uniqueN(pin10),
    n_treated_pin10 = data.table::uniqueN(pin10[treat == 1]),
    n_control_pin10 = data.table::uniqueN(pin10[treat == 0]),
    n_sale_events = data.table::uniqueN(sale_event_id),
    n_treated_sale_events = sum(treat == 1, na.rm = TRUE),
    n_control_sale_events = sum(treat == 0, na.rm = TRUE),
    n_exact_name_match_sales = sum(buyer_seller_exact_match_flag %in% TRUE, na.rm = TRUE),
    n_missing_party_name_sales = sum(!(buyer_seller_name_present %in% TRUE), na.rm = TRUE)
  ), by = .(cohort, bandwidth)]
}

price_support_dt <- data.table::rbindlist(
  list(
    build_price_support_block(price_support_source_dt, "all_sales"),
    build_price_support_block(
      price_support_source_dt[arm_length_flag %in% TRUE],
      "all_sales_arm_length"
    ),
    build_price_support_block(
      price_support_source_dt[price_sample_broad_land_like %in% TRUE],
      "price_sample_broad_land_like"
    ),
    build_price_support_block(
      price_support_source_dt[price_sample_arm_length_land_like %in% TRUE],
      "price_sample_arm_length_land_like"
    ),
    build_price_support_block(
      price_support_source_dt[price_sample_arm_length_raw_land %in% TRUE],
      "price_sample_arm_length_raw_land"
    ),
    build_price_support_block(
      price_support_source_dt[price_sample_raw_land_small_package_le2 %in% TRUE],
      "price_sample_raw_land_small_package_le2"
    ),
    build_price_support_block(
      price_support_source_dt[price_sample_warranty_small_package_le2 %in% TRUE],
      "price_sample_warranty_small_package_le2"
    )
  ),
  use.names = TRUE
)

support_dt <- data.table::rbindlist(
  list(incidence_support_dt, price_support_dt),
  use.names = TRUE,
  fill = TRUE
)
data.table::setorder(support_dt, panel, cohort, bandwidth, sample_definition)

arrow::write_parquet(incidence_dt, output_incidence)
arrow::write_parquet(price_dt, output_price)
readr::write_csv(tibble::as_tibble(support_dt), output_support)

message(sprintf("Saved %s", output_incidence))
message(sprintf("Saved %s", output_price))
message(sprintf("Saved %s", output_support))
