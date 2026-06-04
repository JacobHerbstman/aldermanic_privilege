# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_benchmark_table/code")
# start_ym <- "2014-01"
# end_ym <- "2022-12"
# cpi_base_year <- 2022

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_ym, end_ym, cpi_base_year)
}
if (length(cli_args) != 3) {
  stop("FATAL: Script requires 3 args: <start_ym> <end_ym> <cpi_base_year>.", call. = FALSE)
}

start_ym <- cli_args[1]
end_ym <- cli_args[2]
cpi_base_year <- suppressWarnings(as.integer(cli_args[3]))

if (!grepl("^\\d{4}-\\d{2}$", start_ym) || !grepl("^\\d{4}-\\d{2}$", end_ym)) {
  stop("start_ym and end_ym must use YYYY-MM format.", call. = FALSE)
}
start_date <- as.Date(paste0(start_ym, "-01"))
end_date <- as.Date(paste0(end_ym, "-01"))
if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("start_ym must be before or equal to end_ym.", call. = FALSE)
}
if (!is.finite(cpi_base_year)) {
  stop("cpi_base_year must be an integer year.", call. = FALSE)
}
start_year <- as.integer(format(start_date, "%Y"))
requested_end_year <- as.integer(format(end_date, "%Y"))
cpi_base_start <- as.Date(sprintf("%d-01-01", cpi_base_year))
cpi_base_end <- as.Date(sprintf("%d-12-01", cpi_base_year))

rent_panel <- as.data.table(read_parquet("../input/chicago_rent_panel.parquet"))
rent_panel[, month_start := as.Date(month_start)]
rent_panel <- rent_panel[
  month_start >= start_date &
    month_start <= end_date &
    is.finite(rent_price) &
    rent_price > 0
]
if (nrow(rent_panel) == 0L) {
  stop("No valid listed-rent observations found in the benchmark window.", call. = FALSE)
}

fred_all_items_cpi <- as.data.table(read_csv("../input/fred_cpi_cuura207sa0.csv", show_col_types = FALSE))
if (!all(c("observation_date", "CUURA207SA0") %in% names(fred_all_items_cpi))) {
  stop("CPI input missing expected columns for CUURA207SA0.", call. = FALSE)
}
fred_all_items_cpi[, month_start := as.Date(observation_date)]
fred_all_items_cpi[, cpi_all_items := suppressWarnings(as.numeric(CUURA207SA0))]
fred_all_items_cpi <- fred_all_items_cpi[
  month_start >= start_date & month_start <= end_date,
  .(month_start, cpi_all_items)
]
base_cpi <- mean(
  fred_all_items_cpi[
    month_start >= cpi_base_start & month_start <= cpi_base_end,
    cpi_all_items
  ],
  na.rm = TRUE
)
if (!is.finite(base_cpi)) {
  stop("Chicago all-items CPI-U deflator is missing base-year values.", call. = FALSE)
}

rent_panel <- merge(rent_panel, fred_all_items_cpi, by = "month_start", all.x = TRUE, sort = FALSE)
if (any(!is.finite(rent_panel$cpi_all_items))) {
  stop("Listed rent panel has months missing Chicago all-items CPI-U deflator values.", call. = FALSE)
}
rent_panel[, rent_price_real_base := rent_price * base_cpi / cpi_all_items]

monthly_listed_rents <- rent_panel[
  ,
  .(value = median(rent_price_real_base, na.rm = TRUE)),
  by = month_start
]
monthly_listed_rents[, year := as.integer(format(month_start, "%Y"))]
listed_annual <- monthly_listed_rents[
  is.finite(value),
  .(value = mean(value, na.rm = TRUE)),
  by = year
]
listed_annual[, `:=`(
  source_id = "listed_rents",
  source_label = "Listed rents"
)]

zillow_city <- as.data.table(read_csv("../input/zillow_zori_city.csv", show_col_types = FALSE))
zillow_city <- zillow_city[RegionName == "Chicago" & State == "IL"]
if (nrow(zillow_city) != 1L) {
  stop("Expected one Zillow ZORI row for Chicago, IL.", call. = FALSE)
}
zillow_date_cols <- names(zillow_city)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(zillow_city))]
zillow_city <- melt(zillow_city, measure.vars = zillow_date_cols, variable.name = "period_end", value.name = "value")
zillow_city[, month_start := as.Date(format(as.Date(period_end), "%Y-%m-01"))]
zillow_city[, value := as.numeric(value)]
zillow_city <- zillow_city[
  month_start >= start_date & month_start <= end_date,
  .(month_start, value)
]
zillow_city <- merge(zillow_city, fred_all_items_cpi, by = "month_start", all.x = TRUE, sort = FALSE)
if (any(!is.finite(zillow_city$cpi_all_items))) {
  stop("Zillow ZORI has months missing Chicago all-items CPI-U deflator values.", call. = FALSE)
}
zillow_city[, value := value * base_cpi / cpi_all_items]
zillow_city[, year := as.integer(format(month_start, "%Y"))]
zillow_annual <- zillow_city[
  is.finite(value),
  .(value = mean(value, na.rm = TRUE)),
  by = year
]
zillow_annual[, `:=`(
  source_id = "zillow_zori",
  source_label = "Zillow ZORI"
)]

annual_series <- rbindlist(list(listed_annual, zillow_annual), use.names = TRUE, fill = TRUE)
annual_series <- annual_series[year >= start_year & year <= requested_end_year]

coverage <- annual_series[
  is.finite(value),
  .(
    first_year = min(year),
    last_year = max(year),
    has_end_year = any(year == requested_end_year)
  ),
  by = .(source_id, source_label)
]
benchmark_sources <- c("listed_rents", "zillow_zori")
if (!setequal(coverage$source_id, benchmark_sources)) {
  stop("Rental benchmark needs listed rents and Zillow ZORI.", call. = FALSE)
}
benchmark_start_year <- max(coverage$first_year)
benchmark_end_year <- min(coverage$last_year)
if (!all(
  coverage$first_year <= benchmark_start_year &
    coverage$last_year >= benchmark_end_year &
    coverage$has_end_year
)) {
  stop("Rental benchmark needs listed rents and Zillow ZORI with common start/end years.", call. = FALSE)
}

annual_series <- annual_series[year >= benchmark_start_year & year <= benchmark_end_year]

summary_table <- annual_series[
  year %in% c(benchmark_start_year, benchmark_end_year),
  .(
    start_value = value[year == benchmark_start_year][1],
    end_value = value[year == benchmark_end_year][1],
    growth_pct = 100 * (value[year == benchmark_end_year][1] / value[year == benchmark_start_year][1] - 1)
  ),
  by = .(source_id, source_label)
]
summary_table[, source_id := factor(source_id, levels = benchmark_sources)]
setorder(summary_table, source_id)
summary_table[, source_id := as.character(source_id)]

summary_table[, growth_label := ifelse(is.finite(growth_pct), sprintf("%.1f", growth_pct), "")]
summary_table[, start_label := ifelse(is.finite(start_value), format(round(start_value), big.mark = ","), "")]
summary_table[, end_label := ifelse(is.finite(end_value), format(round(end_value), big.mark = ","), "")]

writeLines(
  c(
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    sprintf("Series & Unit & %d & %d & Real Growth (\\%%) \\\\", benchmark_start_year, benchmark_end_year),
    "\\midrule",
    sprintf(
      "%s & %s & %s & %s & %s \\\\",
      summary_table$source_label,
      "\\$",
      summary_table$start_label,
      summary_table$end_label,
      summary_table$growth_label
    ),
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "../output/rental_benchmark_growth.tex"
)
