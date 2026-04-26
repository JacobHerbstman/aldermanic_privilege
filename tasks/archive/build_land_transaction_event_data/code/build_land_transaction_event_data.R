source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

unique_nonmissing_n <- function(x) {
  data.table::uniqueN(x[!is.na(x)])
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, na.rm = TRUE))
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  mean(x)
}

safe_max <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/build_land_transaction_event_data/code")
# sales_input <- "../input/land_transaction_sales_pin10_events.parquet"
# panel_input <- "../input/parcel_land_redistricting_panel.parquet"
# output_events <- "../output/land_transaction_sale_events.parquet"
# output_diag <- "../output/land_transaction_diagnostics.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sales_input, panel_input, output_events, output_diag)
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <sales_input_parquet> <panel_input_parquet> <output_events_parquet> <output_diag_csv>",
    call. = FALSE
  )
}

sales_input <- cli_args[1]
panel_input <- cli_args[2]
output_events <- cli_args[3]
output_diag <- cli_args[4]

message("Loading cleaned sale events...")
sales_dt <- data.table::as.data.table(arrow::read_parquet(sales_input))
sales_dt[, `:=`(
  pin10 = as.character(pin10),
  sale_event_id = as.character(sale_event_id),
  sale_date = as.Date(sale_date),
  sale_year = as.integer(sale_year),
  sale_price = as.numeric(sale_price),
  is_multisale = as.logical(is_multisale),
  num_parcels_sale = as.integer(num_parcels_sale),
  raw_land_sale_flag = as.logical(raw_land_sale_flag),
  market_deed_flag = as.logical(market_deed_flag),
  package_sale_flag = as.logical(package_sale_flag),
  single_parcel_sale_flag = as.logical(single_parcel_sale_flag),
  positive_price_flag = as.logical(positive_price_flag),
  sale_filter_same_sale_within_365 = as.logical(sale_filter_same_sale_within_365),
  sale_filter_less_than_10k = as.logical(sale_filter_less_than_10k),
  sale_filter_deed_type = as.logical(sale_filter_deed_type)
)]

if (anyDuplicated(sales_dt$sale_event_id) > 0) {
  stop("Input sale-event parquet is not unique by sale_event_id.", call. = FALSE)
}

message("Loading parcel land redistricting panel...")
panel_dt <- data.table::as.data.table(arrow::read_parquet(
  panel_input,
  col_select = c(
    "pin10",
    "tax_year",
    "land_sum",
    "bldg_sum",
    "total_av",
    "land_share_pin10",
    "log_land_sum",
    "land_psf",
    "log_land_psf",
    "land_only",
    "lot_sqft_current",
    "longitude",
    "latitude",
    "block_id",
    "block_origin_side_id",
    "ward_2014",
    "ward_2015",
    "ward_origin",
    "ward_dest",
    "ward_pair_id",
    "segment_id",
    "dist_to_boundary_ft",
    "in_500ft",
    "in_1000ft",
    "switched_2015",
    "valid_control_2015",
    "strictness_origin",
    "strictness_dest",
    "strictness_change",
    "baseline_pre_year_2012_2014_last",
    "baseline_empty_last_pre_2012_2014",
    "baseline_land_sum",
    "baseline_bldg_sum",
    "baseline_land_psf"
  )
))

panel_dt[, `:=`(
  pin10 = as.character(pin10),
  tax_year = as.integer(tax_year),
  land_sum = as.numeric(land_sum),
  bldg_sum = as.numeric(bldg_sum),
  total_av = as.numeric(total_av),
  land_share_pin10 = as.numeric(land_share_pin10),
  log_land_sum = as.numeric(log_land_sum),
  land_psf = as.numeric(land_psf),
  log_land_psf = as.numeric(log_land_psf),
  land_only = as.logical(land_only),
  lot_sqft_current = as.numeric(lot_sqft_current),
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude),
  ward_2014 = as.integer(ward_2014),
  ward_2015 = as.integer(ward_2015),
  ward_origin = as.integer(ward_origin),
  ward_dest = as.integer(ward_dest),
  dist_to_boundary_ft = as.numeric(dist_to_boundary_ft),
  in_500ft = as.logical(in_500ft),
  in_1000ft = as.logical(in_1000ft),
  switched_2015 = as.logical(switched_2015),
  valid_control_2015 = as.logical(valid_control_2015),
  strictness_origin = as.numeric(strictness_origin),
  strictness_dest = as.numeric(strictness_dest),
  strictness_change = as.numeric(strictness_change),
  baseline_pre_year_2012_2014_last = as.integer(baseline_pre_year_2012_2014_last),
  baseline_empty_last_pre_2012_2014 = as.logical(baseline_empty_last_pre_2012_2014),
  baseline_land_sum = as.numeric(baseline_land_sum),
  baseline_bldg_sum = as.numeric(baseline_bldg_sum),
  baseline_land_psf = as.numeric(baseline_land_psf)
)]

if (anyDuplicated(panel_dt[, .(pin10, tax_year)]) > 0) {
  stop("Parcel land redistricting panel is not unique by pin10 x tax_year.", call. = FALSE)
}

static_check <- panel_dt[, .(
  n_block = unique_nonmissing_n(block_id),
  n_block_origin_side = unique_nonmissing_n(block_origin_side_id),
  n_ward_pair = unique_nonmissing_n(ward_pair_id),
  n_segment = unique_nonmissing_n(segment_id),
  n_dist = unique_nonmissing_n(dist_to_boundary_ft),
  n_strictness = unique_nonmissing_n(strictness_change),
  n_baseline_flag = unique_nonmissing_n(baseline_empty_last_pre_2012_2014),
  n_in_1000 = unique_nonmissing_n(in_1000ft),
  n_longitude = unique_nonmissing_n(longitude),
  n_latitude = unique_nonmissing_n(latitude)
), by = pin10]

if (nrow(static_check[
  n_block > 1 |
    n_block_origin_side > 1 |
    n_ward_pair > 1 |
    n_segment > 1 |
    n_dist > 1 |
    n_strictness > 1 |
    n_baseline_flag > 1 |
    n_in_1000 > 1 |
    n_longitude > 1 |
    n_latitude > 1
]) > 0) {
  stop("Fixed parcel fields vary across tax years for at least one pin10.", call. = FALSE)
}

static_dt <- unique(panel_dt[, .(
  pin10,
  lot_sqft_current,
  longitude,
  latitude,
  block_id,
  block_origin_side_id,
  ward_2014,
  ward_2015,
  ward_origin,
  ward_dest,
  ward_pair_id,
  segment_id,
  dist_to_boundary_ft,
  in_500ft,
  in_1000ft,
  switched_2015,
  valid_control_2015,
  strictness_origin,
  strictness_dest,
  strictness_change,
  baseline_pre_year_2012_2014_last,
  baseline_empty_last_pre_2012_2014,
  baseline_land_sum,
  baseline_bldg_sum,
  baseline_land_psf
)])

design_year_min <- 2007L
design_year_max <- 2020L

sales_design <- sales_dt[
  !is.na(sale_year) &
    sale_year >= design_year_min &
    sale_year <= design_year_max
]
static_match_dt <- static_dt[, .(
  pin10,
  matched_to_assessed_panel = TRUE,
  baseline_empty_last_pre_2012_2014,
  in_1000ft
)]

design_match_dt <- merge(
  sales_design[, .(
    sale_event_id,
    pin10,
    sale_year,
    sale_price,
    raw_land_sale_flag,
    market_deed_flag,
    package_sale_flag,
    sale_type
  )],
  static_match_dt,
  by = "pin10",
  all.x = TRUE,
  sort = FALSE
)

denominator_dt <- static_dt[
  baseline_empty_last_pre_2012_2014 %in% TRUE &
    in_1000ft %in% TRUE
]

sales_baseline_dt <- merge(
  sales_design,
  denominator_dt,
  by = "pin10",
  all = FALSE,
  sort = FALSE
)

panel_roll_dt <- panel_dt[
  pin10 %in% denominator_dt$pin10,
  .(
    pin10,
    tax_year,
    land_sum,
    bldg_sum,
    total_av,
    land_share_pin10,
    log_land_sum,
    land_psf,
    log_land_psf,
    land_only
  )
]

data.table::setkey(panel_roll_dt, pin10, tax_year)
sales_baseline_dt[, join_sale_year := sale_year]
data.table::setkey(sales_baseline_dt, pin10, join_sale_year)

message("Rolling sale events onto the latest observed assessed status...")
events_dt <- panel_roll_dt[
  sales_baseline_dt,
  on = .(pin10, tax_year = join_sale_year),
  roll = TRUE,
  rollends = c(FALSE, FALSE)
]

data.table::setnames(
  events_dt,
  old = c(
    "tax_year",
    "land_sum",
    "bldg_sum",
    "total_av",
    "land_share_pin10",
    "log_land_sum",
    "land_psf",
    "log_land_psf",
    "land_only"
  ),
  new = c(
    "sale_time_tax_year",
    "sale_time_land_sum",
    "sale_time_bldg_sum",
    "sale_time_total_av",
    "sale_time_land_share_pin10",
    "sale_time_log_land_sum",
    "sale_time_land_psf",
    "sale_time_log_land_psf",
    "sale_time_land_only"
  )
)

events_dt[, treatment_sign := dplyr::case_when(
  strictness_change < 0 ~ "to_lenient",
  strictness_change > 0 ~ "to_stricter",
  TRUE ~ "no_change"
)]

events_dt[, `:=`(
  relative_year_2012 = sale_year - 2012L,
  relative_year_2015 = sale_year - 2015L,
  in_cohort_2012_window = data.table::between(sale_year, 2007L, 2017L),
  in_cohort_2015_window = data.table::between(sale_year, 2010L, 2020L),
  sale_time_gap_years = sale_year - sale_time_tax_year,
  sale_time_land_like_flag = !is.na(sale_time_bldg_sum) & sale_time_bldg_sum == 0
)]

events_dt <- events_dt[, .(
  pin10,
  sale_event_id,
  sale_date,
  sale_year,
  relative_year_2012,
  relative_year_2015,
  in_cohort_2012_window,
  in_cohort_2015_window,
  sale_price,
  sale_document_num_clean,
  sale_type,
  sale_deed_type,
  mydec_deed_type,
  sale_seller_name,
  sale_buyer_name,
  is_multisale,
  num_parcels_sale,
  package_sale_flag,
  single_parcel_sale_flag,
  market_deed_flag,
  positive_price_flag,
  sale_filter_same_sale_within_365,
  sale_filter_less_than_10k,
  sale_filter_deed_type,
  raw_land_sale_flag,
  n_raw_rows_in_event,
  n_pin14_in_pin10_event,
  pin14_list,
  lot_sqft_current,
  longitude,
  latitude,
  block_id,
  block_origin_side_id,
  ward_2014,
  ward_2015,
  ward_origin,
  ward_dest,
  ward_pair_id,
  segment_id,
  dist_to_boundary_ft,
  in_500ft,
  in_1000ft,
  switched_2015,
  valid_control_2015,
  strictness_origin,
  strictness_dest,
  strictness_change,
  treatment_sign,
  baseline_pre_year_2012_2014_last,
  baseline_empty_last_pre_2012_2014,
  baseline_land_sum,
  baseline_bldg_sum,
  baseline_land_psf,
  sale_time_tax_year,
  sale_time_land_sum,
  sale_time_bldg_sum,
  sale_time_total_av,
  sale_time_land_share_pin10,
  sale_time_log_land_sum,
  sale_time_land_psf,
  sale_time_log_land_psf,
  sale_time_land_only,
  sale_time_gap_years,
  sale_time_land_like_flag
)]

data.table::setorder(events_dt, sale_year, sale_date, pin10, sale_event_id)

if (anyDuplicated(events_dt$sale_event_id) > 0) {
  stop("Final land-transaction event data are not unique by sale_event_id.", call. = FALSE)
}

n_joined_events <- nrow(events_dt)
n_sale_time_matched <- events_dt[!is.na(sale_time_tax_year), .N]

diagnostics <- tibble(
  diagnostic_group = c(
    rep("event_counts", 9),
    rep("event_shares", 6),
    rep("sale_time_gap_years", 4)
  ),
  metric = c(
    "n_clean_sale_events_total",
    "n_clean_sale_events_design_window_2007_2020",
    "n_design_sale_events_matching_assessed_panel_pin10",
    "n_design_sale_events_not_matching_assessed_panel_pin10",
    "n_design_sale_events_in_baseline_empty_1000ft_denominator",
    "n_joined_sale_events_total",
    "n_joined_sale_events_500ft",
    "n_joined_sale_events_with_sale_time_assessment",
    "n_joined_sale_events_missing_sale_time_assessment",
    "share_joined_raw_land_sale_flag",
    "share_joined_sale_time_land_like_flag",
    "share_joined_market_deed_flag",
    "share_joined_package_sale_flag",
    "share_joined_missing_sale_price",
    "share_joined_missing_sale_type",
    "median_gap",
    "p90_gap",
    "p99_gap",
    "max_gap"
  ),
  value = c(
    nrow(sales_dt),
    nrow(sales_design),
    design_match_dt[matched_to_assessed_panel %in% TRUE, .N],
    design_match_dt[is.na(matched_to_assessed_panel), .N],
    design_match_dt[
      baseline_empty_last_pre_2012_2014 %in% TRUE &
        in_1000ft %in% TRUE,
      .N
    ],
    n_joined_events,
    events_dt[in_500ft %in% TRUE, .N],
    n_sale_time_matched,
    n_joined_events - n_sale_time_matched,
    safe_mean(events_dt$raw_land_sale_flag %in% TRUE),
    safe_mean(events_dt$sale_time_land_like_flag %in% TRUE),
    safe_mean(events_dt$market_deed_flag %in% TRUE),
    safe_mean(events_dt$package_sale_flag %in% TRUE),
    safe_mean(is.na(events_dt$sale_price)),
    safe_mean(is.na(events_dt$sale_type)),
    stats::median(events_dt$sale_time_gap_years, na.rm = TRUE),
    safe_quantile(events_dt$sale_time_gap_years, 0.90),
    safe_quantile(events_dt$sale_time_gap_years, 0.99),
    safe_max(events_dt$sale_time_gap_years)
  )
)

arrow::write_parquet(events_dt, output_events)
readr::write_csv(diagnostics, output_diag)

message(sprintf("Saved %s", output_events))
message(sprintf("Saved %s", output_diag))
