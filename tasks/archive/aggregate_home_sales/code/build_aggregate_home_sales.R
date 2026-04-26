source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

library(data.table)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_home_sales/code")
# sales_input <- "../input/sales_with_hedonics.parquet"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# monthly_output <- "../output/home_sales_geography_monthly_summary.csv"
# yearly_output <- "../output/home_sales_geography_yearly_summary.csv"
# period_output <- "../output/home_sales_map_period_summary.csv"
# coverage_output <- "../output/home_sales_coverage_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    sales_input,
    ward_panel_input,
    community_area_input,
    monthly_output,
    yearly_output,
    period_output,
    coverage_output
  )
}

if (length(args) != 7) {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<sales_input> <ward_panel_input> <community_area_input>",
      "<monthly_output> <yearly_output> <period_output> <coverage_output>"
    ),
    call. = FALSE
  )
}

sales_input <- args[1]
ward_panel_input <- args[2]
community_area_input <- args[3]
monthly_output <- args[4]
yearly_output <- args[5]
period_output <- args[6]
coverage_output <- args[7]

summarize_sales <- function(dt, geography_level, time_level) {
  if (geography_level == "citywide") {
    if (time_level == "monthly") {
      return(dt[, .(
        median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
        median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
        n_transactions = .N
      ), by = .(month_start, year)][, .(
        geography_level = "citywide",
        geography_id = NA_integer_,
        geography_name = "Chicago",
        month_start,
        year,
        median_sale_price_real_2022,
        median_sale_price_nominal,
        n_transactions
      )])
    }

    return(dt[, .(
      median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_transactions = .N
    ), by = .(year)][, .(
      geography_level = "citywide",
      geography_id = NA_integer_,
      geography_name = "Chicago",
      month_start = as.Date(NA),
      year,
      median_sale_price_real_2022,
      median_sale_price_nominal,
      n_transactions
    )])
  } else if (geography_level == "ward") {
    if (time_level == "monthly") {
      return(dt[!is.na(ward), .(
        median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
        median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
        n_transactions = .N
      ), by = .(ward, month_start, year)][, .(
        geography_level = "ward",
        geography_id = as.integer(ward),
        geography_name = paste("Ward", as.integer(ward)),
        month_start,
        year,
        median_sale_price_real_2022,
        median_sale_price_nominal,
        n_transactions
      )])
    }

    return(dt[!is.na(ward), .(
      median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_transactions = .N
    ), by = .(ward, year)][, .(
      geography_level = "ward",
      geography_id = as.integer(ward),
      geography_name = paste("Ward", as.integer(ward)),
      month_start = as.Date(NA),
      year,
      median_sale_price_real_2022,
      median_sale_price_nominal,
      n_transactions
    )])
  }

  if (time_level == "monthly") {
    return(dt[!is.na(community_area), .(
      median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_transactions = .N
    ), by = .(community_area, community_name, month_start, year)][, .(
      geography_level = "community_area",
      geography_id = as.integer(community_area),
      geography_name = community_name,
      month_start,
      year,
      median_sale_price_real_2022,
      median_sale_price_nominal,
      n_transactions
    )])
  }

  dt[!is.na(community_area), .(
    median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
    median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
    n_transactions = .N
  ), by = .(community_area, community_name, year)][, .(
    geography_level = "community_area",
    geography_id = as.integer(community_area),
    geography_name = community_name,
    month_start = as.Date(NA),
    year,
    median_sale_price_real_2022,
    median_sale_price_nominal,
    n_transactions
  )]
}

summarize_period <- function(dt, geography_level, period_label, period_type, current_start, current_end, prior_start, prior_end, cutoff_month = NA_integer_) {
  current_dt <- dt[sale_date >= current_start & sale_date <= current_end]
  prior_dt <- dt[sale_date >= prior_start & sale_date <= prior_end]

  if (geography_level == "citywide") {
    current_summary <- current_dt[, .(
      geography_id = NA_integer_,
      geography_name = "Chicago",
      current_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      current_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_current = .N
    )]

    prior_summary <- prior_dt[, .(
      geography_id = NA_integer_,
      prior_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      prior_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_prior = .N
    )]

    out <- data.table(
      geography_id = NA_integer_,
      geography_name = "Chicago",
      current_median_sale_price_real_2022 = current_summary$current_median_sale_price_real_2022,
      current_median_sale_price_nominal = current_summary$current_median_sale_price_nominal,
      n_current = current_summary$n_current,
      prior_median_sale_price_real_2022 = prior_summary$prior_median_sale_price_real_2022,
      prior_median_sale_price_nominal = prior_summary$prior_median_sale_price_nominal,
      n_prior = prior_summary$n_prior
    )
  } else if (geography_level == "ward") {
    current_summary <- current_dt[!is.na(ward), .(
      geography_id = as.integer(ward[1]),
      geography_name = paste("Ward", as.integer(ward[1])),
      current_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      current_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_current = .N
    ), by = ward]

    prior_summary <- prior_dt[!is.na(ward), .(
      geography_id = as.integer(ward[1]),
      prior_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      prior_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_prior = .N
    ), by = ward]

    out <- merge(current_summary[, !"ward"], prior_summary[, !"ward"], by = "geography_id", all = TRUE)
    out[is.na(geography_name), geography_name := paste("Ward", geography_id)]
  } else {
    current_summary <- current_dt[!is.na(community_area), .(
      geography_id = as.integer(community_area[1]),
      geography_name = community_name[1],
      current_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      current_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_current = .N
    ), by = .(community_area, community_name)]

    prior_summary <- prior_dt[!is.na(community_area), .(
      geography_id = as.integer(community_area[1]),
      geography_name = community_name[1],
      prior_median_sale_price_real_2022 = median(sale_price_real, na.rm = TRUE),
      prior_median_sale_price_nominal = median(sale_price_nominal, na.rm = TRUE),
      n_prior = .N
    ), by = .(community_area, community_name)]

    out <- merge(
      current_summary[, .(geography_id, geography_name, current_median_sale_price_real_2022, current_median_sale_price_nominal, n_current)],
      prior_summary[, .(geography_id, prior_geography_name = geography_name, prior_median_sale_price_real_2022, prior_median_sale_price_nominal, n_prior)],
      by = "geography_id",
      all = TRUE
    )
    out[is.na(geography_name), geography_name := prior_geography_name]
    out[, prior_geography_name := NULL]
  }

  out[, `:=`(
    geography_level = geography_level,
    period_label = period_label,
    period_type = period_type,
    current_start = current_start,
    current_end = current_end,
    prior_start = prior_start,
    prior_end = prior_end,
    cutoff_month = cutoff_month,
    growth_pct = 100 * (current_median_sale_price_real_2022 / prior_median_sale_price_real_2022 - 1)
  )]

  out[]
}

message("Loading home sales panel...")
sales <- as.data.table(read_parquet(
  sales_input,
  col_select = c(
    "sale_date",
    "year",
    "year_month",
    "ward",
    "latitude",
    "longitude",
    "sale_price_real_2022_raw",
    "sale_price_nominal"
  )
))

sales[, sale_date := as.Date(sale_date)]
sales[, year := as.integer(year)]
sales[, ward := as.integer(ward)]
sales[, sale_price_real := as.numeric(sale_price_real_2022_raw)]
sales[, sale_price_nominal := as.numeric(sale_price_nominal)]
sales[, month_start := as.Date(paste0(year_month, "-01"))]
sales[, coord_key := coord_key_from_latlon(latitude, longitude)]

sales <- sales[
  !is.na(sale_date) &
    !is.na(month_start) &
    is.finite(latitude) &
    is.finite(longitude) &
    is.finite(sale_price_real) &
    sale_price_real > 0 &
    is.finite(sale_price_nominal) &
    sale_price_nominal > 0
]

message("Assigning community areas...")
coords_tbl <- unique(sales[, .(coord_key, latitude, longitude)])
geo_lookup <- build_coord_geography_lookup(coords_tbl, ward_panel_input, community_area_input)

sales <- merge(
  sales,
  geo_lookup[, .(coord_key, community_area, community_name)],
  by = "coord_key",
  all.x = TRUE
)

coverage_summary <- sales[, .(
  n_obs = .N,
  first_date = min(sale_date),
  last_date = max(sale_date),
  n_with_ward = sum(!is.na(ward)),
  n_with_community_area = sum(!is.na(community_area)),
  n_wards = uniqueN(ward[!is.na(ward)]),
  n_community_areas = uniqueN(community_area[!is.na(community_area)])
)]
coverage_summary[, share_with_ward := n_with_ward / n_obs]
coverage_summary[, share_with_community_area := n_with_community_area / n_obs]

message("Building monthly and yearly summaries...")
monthly_summary <- rbindlist(list(
  summarize_sales(sales, "citywide", "monthly"),
  summarize_sales(sales, "ward", "monthly"),
  summarize_sales(sales, "community_area", "monthly")
), fill = TRUE)

yearly_summary <- rbindlist(list(
  summarize_sales(sales, "citywide", "yearly"),
  summarize_sales(sales, "ward", "yearly"),
  summarize_sales(sales, "community_area", "yearly")
), fill = TRUE)

sales_ytd_cutoff <- sales[year == 2025, max(lubridate::month(sale_date), na.rm = TRUE)]

message("Building exact full-year and YTD period summaries...")
period_summary <- rbindlist(list(
  summarize_period(sales, "citywide", "2023_vs_2022_full_year", "full_year", as.Date("2023-01-01"), as.Date("2023-12-31"), as.Date("2022-01-01"), as.Date("2022-12-31")),
  summarize_period(sales, "ward", "2023_vs_2022_full_year", "full_year", as.Date("2023-01-01"), as.Date("2023-12-31"), as.Date("2022-01-01"), as.Date("2022-12-31")),
  summarize_period(sales, "community_area", "2023_vs_2022_full_year", "full_year", as.Date("2023-01-01"), as.Date("2023-12-31"), as.Date("2022-01-01"), as.Date("2022-12-31")),
  summarize_period(sales, "citywide", "2024_vs_2023_full_year", "full_year", as.Date("2024-01-01"), as.Date("2024-12-31"), as.Date("2023-01-01"), as.Date("2023-12-31")),
  summarize_period(sales, "ward", "2024_vs_2023_full_year", "full_year", as.Date("2024-01-01"), as.Date("2024-12-31"), as.Date("2023-01-01"), as.Date("2023-12-31")),
  summarize_period(sales, "community_area", "2024_vs_2023_full_year", "full_year", as.Date("2024-01-01"), as.Date("2024-12-31"), as.Date("2023-01-01"), as.Date("2023-12-31")),
  summarize_period(
    sales,
    "citywide",
    "2025_vs_2024_ytd",
    "matched_ytd",
    as.Date("2025-01-01"),
    as.Date(sprintf("2025-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2025-%02d-01", sales_ytd_cutoff))))),
    as.Date("2024-01-01"),
    as.Date(sprintf("2024-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2024-%02d-01", sales_ytd_cutoff))))),
    sales_ytd_cutoff
  ),
  summarize_period(
    sales,
    "ward",
    "2025_vs_2024_ytd",
    "matched_ytd",
    as.Date("2025-01-01"),
    as.Date(sprintf("2025-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2025-%02d-01", sales_ytd_cutoff))))),
    as.Date("2024-01-01"),
    as.Date(sprintf("2024-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2024-%02d-01", sales_ytd_cutoff))))),
    sales_ytd_cutoff
  ),
  summarize_period(
    sales,
    "community_area",
    "2025_vs_2024_ytd",
    "matched_ytd",
    as.Date("2025-01-01"),
    as.Date(sprintf("2025-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2025-%02d-01", sales_ytd_cutoff))))),
    as.Date("2024-01-01"),
    as.Date(sprintf("2024-%02d-%02d", sales_ytd_cutoff, lubridate::days_in_month(as.Date(sprintf("2024-%02d-01", sales_ytd_cutoff))))),
    sales_ytd_cutoff
  )
), fill = TRUE)

write_csv(monthly_summary %>% arrange(geography_level, geography_id, month_start), monthly_output)
write_csv(yearly_summary %>% arrange(geography_level, geography_id, year), yearly_output)
write_csv(period_summary %>% arrange(period_label, geography_level, geography_id), period_output)
write_csv(coverage_summary, coverage_output)

message("Saved home sales monthly summary: ", monthly_output)
message("Saved home sales yearly summary: ", yearly_output)
message("Saved home sales period summary: ", period_output)
message("Saved home sales coverage summary: ", coverage_output)
