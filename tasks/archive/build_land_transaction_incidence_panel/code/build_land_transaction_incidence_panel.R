source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

build_support_block <- function(dt, sold_col, count_col, sale_definition) {
  dt[, .(
    n_denominator_pin10 = .N,
    n_sold_pin10 = sum(get(sold_col), na.rm = TRUE),
    n_sale_events = sum(get(count_col), na.rm = TRUE),
    n_switchers = sum(switched_2015 %in% TRUE),
    n_valid_controls = sum(valid_control_2015 %in% TRUE)
  ), by = .(cohort, bandwidth, treatment_sign, calendar_year)][
    , `:=`(
      sale_definition = sale_definition,
      share_sold = n_sold_pin10 / n_denominator_pin10
    )
  ][]
}

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/build_land_transaction_incidence_panel/code")
# events_input <- "../input/land_transaction_sale_events.parquet"
# panel_input <- "../input/parcel_land_redistricting_panel.parquet"
# output_panel <- "../output/land_transaction_incidence_panel.parquet"
# output_support <- "../output/land_transaction_support.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(events_input, panel_input, output_panel, output_support)
}

if (length(cli_args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <events_input_parquet> <panel_input_parquet> <output_panel_parquet> <output_support_csv>",
    call. = FALSE
  )
}

events_input <- cli_args[1]
panel_input <- cli_args[2]
output_panel <- cli_args[3]
output_support <- cli_args[4]

message("Loading land-transaction sale events...")
events_dt <- data.table::as.data.table(arrow::read_parquet(events_input))
events_dt[, `:=`(
  pin10 = as.character(pin10),
  sale_year = as.integer(sale_year),
  sale_time_land_like_flag = as.logical(sale_time_land_like_flag),
  raw_land_sale_flag = as.logical(raw_land_sale_flag)
)]

message("Loading parcel denominator from the land redistricting panel...")
panel_dt <- data.table::as.data.table(arrow::read_parquet(
  panel_input,
  col_select = c(
    "pin10",
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
  baseline_land_psf = as.numeric(baseline_land_psf),
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude)
)]

denominator_dt <- unique(panel_dt[
  baseline_empty_last_pre_2012_2014 %in% TRUE &
    in_1000ft %in% TRUE,
  .(
    pin10,
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
    baseline_land_sum,
    baseline_bldg_sum,
    baseline_land_psf
  )
])

if (anyDuplicated(denominator_dt$pin10) > 0) {
  stop("Incidence-panel denominator is not unique by pin10.", call. = FALSE)
}

denominator_dt[, baseline_empty_last_pre_2012_2014 := TRUE]

denominator_dt[, treatment_sign := dplyr::case_when(
  strictness_change < 0 ~ "to_lenient",
  strictness_change > 0 ~ "to_stricter",
  TRUE ~ "no_change"
)]

cohort_defs <- data.table(
  cohort = c("cohort_2012", "cohort_2015"),
  event_year = c(2012L, 2015L),
  start_year = c(2007L, 2007L),
  end_year = c(2020L, 2020L)
)

message("Building cohort-year denominator panel...")
incidence_list <- lapply(seq_len(nrow(cohort_defs)), function(i) {
  cohort_row <- cohort_defs[i]
  panel_years <- data.table::CJ(
    pin10 = denominator_dt$pin10,
    calendar_year = cohort_row$start_year:cohort_row$end_year,
    unique = TRUE
  )
  panel_years[, `:=`(
    cohort = cohort_row$cohort,
    event_year = cohort_row$event_year
  )]
  merge(panel_years, denominator_dt, by = "pin10", all.x = TRUE, sort = FALSE)
})

incidence_panel_dt <- data.table::rbindlist(incidence_list, use.names = TRUE)

events_agg_dt <- events_dt[, .(
  sold_any = as.integer(.N > 0),
  sold_land_like = as.integer(any(sale_time_land_like_flag %in% TRUE, na.rm = TRUE)),
  sold_land_tag = as.integer(any(raw_land_sale_flag %in% TRUE, na.rm = TRUE)),
  n_sales_any = .N,
  n_sales_land_like = sum(sale_time_land_like_flag %in% TRUE, na.rm = TRUE),
  n_sales_land_tag = sum(raw_land_sale_flag %in% TRUE, na.rm = TRUE)
), by = .(pin10, calendar_year = sale_year)]

incidence_panel_dt <- merge(
  incidence_panel_dt,
  events_agg_dt,
  by = c("pin10", "calendar_year"),
  all.x = TRUE,
  sort = FALSE
)

for (col_name in c(
  "sold_any",
  "sold_land_like",
  "sold_land_tag",
  "n_sales_any",
  "n_sales_land_like",
  "n_sales_land_tag"
)) {
  data.table::set(
    incidence_panel_dt,
    i = which(is.na(incidence_panel_dt[[col_name]])),
    j = col_name,
    value = 0L
  )
}

incidence_panel_dt[, relative_year := calendar_year - event_year]
data.table::setorder(incidence_panel_dt, cohort, pin10, calendar_year)

support_dt <- data.table::rbindlist(
  list(
    data.table::copy(incidence_panel_dt)[, bandwidth := "1000ft"],
    data.table::copy(incidence_panel_dt[in_500ft %in% TRUE])[, bandwidth := "500ft"]
  ),
  use.names = TRUE
)

support_dt <- data.table::rbindlist(
  list(
    build_support_block(support_dt, "sold_any", "n_sales_any", "sold_any"),
    build_support_block(support_dt, "sold_land_like", "n_sales_land_like", "sold_land_like"),
    build_support_block(support_dt, "sold_land_tag", "n_sales_land_tag", "sold_land_tag")
  ),
  use.names = TRUE
)

data.table::setorder(support_dt, cohort, bandwidth, sale_definition, calendar_year, treatment_sign)

arrow::write_parquet(incidence_panel_dt, output_panel)
readr::write_csv(tibble::as_tibble(support_dt), output_support)

message(sprintf("Saved %s", output_panel))
message(sprintf("Saved %s", output_support))
