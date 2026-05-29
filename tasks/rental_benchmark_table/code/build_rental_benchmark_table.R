# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_benchmark_table/code")

source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(readr)

rent_panel <- as.data.table(read_parquet("../input/chicago_rent_panel.parquet"))
rent_panel[, month_start := as.Date(month_start)]
rent_panel <- rent_panel[
  month_start >= as.Date("2014-01-01") &
    month_start <= as.Date("2022-12-01") &
    is.finite(rent_price) &
    rent_price > 0
]
if (nrow(rent_panel) == 0L) {
  stop("No valid listed-rent observations found in the 2014-2022 validation window.", call. = FALSE)
}

bls_payload <- jsonlite::fromJSON(
  "https://api.bls.gov/publicAPI/v2/timeseries/data/CUURS23ASA0?startyear=2014&endyear=2022",
  simplifyVector = FALSE
)
if (!identical(bls_payload$status, "REQUEST_SUCCEEDED")) {
  stop("BLS request failed for CUURS23ASA0.", call. = FALSE)
}
fred_all_items_cpi <- rbindlist(lapply(bls_payload$Results$series[[1]]$data, as.data.table), fill = TRUE)
if (nrow(fred_all_items_cpi) == 0L) {
  stop("BLS response has no observations for CUURS23ASA0.", call. = FALSE)
}
fred_all_items_cpi <- fred_all_items_cpi[grepl("^M\\d{2}$", period)]
fred_all_items_cpi[, month_start := as.Date(sprintf("%s-%02d-01", year, as.integer(sub("^M", "", period))))]
fred_all_items_cpi[, cpi_all_items := suppressWarnings(as.numeric(value))]
fred_all_items_cpi <- fred_all_items_cpi[
  month_start >= as.Date("2014-01-01") & month_start <= as.Date("2022-12-01"),
  .(month_start, cpi_all_items)
]
cpi_2022 <- mean(
  fred_all_items_cpi[
    month_start >= as.Date("2022-01-01") & month_start <= as.Date("2022-12-01"),
    cpi_all_items
  ],
  na.rm = TRUE
)
if (!is.finite(cpi_2022)) {
  stop("Chicago all-items CPI-U deflator is missing 2022 values.", call. = FALSE)
}

rent_panel <- merge(rent_panel, fred_all_items_cpi, by = "month_start", all.x = TRUE, sort = FALSE)
if (any(!is.finite(rent_panel$cpi_all_items))) {
  stop("Listed rent panel has months missing Chicago all-items CPI-U deflator values.", call. = FALSE)
}
rent_panel[, rent_price_real_2022 := rent_price * cpi_2022 / cpi_all_items]

monthly_listed_rents <- rent_panel[
  ,
  .(value = median(rent_price_real_2022, na.rm = TRUE)),
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

zillow_city <- as.data.table(read_csv(
  "https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv",
  show_col_types = FALSE
))
zillow_city <- zillow_city[RegionName == "Chicago" & State == "IL"]
if (nrow(zillow_city) != 1L) {
  stop("Expected one Zillow ZORI row for Chicago, IL.", call. = FALSE)
}
zillow_date_cols <- names(zillow_city)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(zillow_city))]
zillow_city <- melt(zillow_city, measure.vars = zillow_date_cols, variable.name = "period_end", value.name = "value")
zillow_city[, month_start := as.Date(format(as.Date(period_end), "%Y-%m-01"))]
zillow_city[, value := as.numeric(value)]
zillow_city <- zillow_city[
  month_start >= as.Date("2014-01-01") & month_start <= as.Date("2022-12-01"),
  .(month_start, value)
]
zillow_city <- merge(zillow_city, fred_all_items_cpi, by = "month_start", all.x = TRUE, sort = FALSE)
if (any(!is.finite(zillow_city$cpi_all_items))) {
  stop("Zillow ZORI has months missing Chicago all-items CPI-U deflator values.", call. = FALSE)
}
zillow_city[, value := value * cpi_2022 / cpi_all_items]
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
annual_series <- annual_series[year >= 2014L & year <= 2022L]

coverage <- annual_series[
  is.finite(value),
  .(
    first_year = min(year),
    last_year = max(year),
    has_2022 = any(year == 2022L)
  ),
  by = .(source_id, source_label)
]
validation_sources <- c("listed_rents", "zillow_zori")
if (!setequal(coverage$source_id, validation_sources)) {
  stop("Rental validation needs listed rents and Zillow ZORI.", call. = FALSE)
}
base_year <- max(coverage$first_year)
end_year <- min(coverage$last_year)
if (!all(coverage$first_year <= base_year & coverage$last_year >= end_year & coverage$has_2022)) {
  stop("Rental validation needs listed rents and Zillow ZORI with common start/end years.", call. = FALSE)
}

annual_series <- annual_series[year >= base_year & year <= end_year]

summary_table <- annual_series[
  year %in% c(base_year, end_year),
  .(
    start_value = value[year == base_year][1],
    end_value = value[year == end_year][1],
    growth_pct = 100 * (value[year == end_year][1] / value[year == base_year][1] - 1)
  ),
  by = .(source_id, source_label)
]
summary_table[, source_id := factor(source_id, levels = validation_sources)]
setorder(summary_table, source_id)
summary_table[, source_id := as.character(source_id)]

summary_table[, growth_label := ifelse(is.finite(growth_pct), sprintf("%.1f", growth_pct), "")]
summary_table[, start_label := ifelse(is.finite(start_value), format(round(start_value), big.mark = ","), "")]
summary_table[, end_label := ifelse(is.finite(end_value), format(round(end_value), big.mark = ","), "")]

writeLines(
  c(
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    sprintf("Series & Unit & %d & %d & Real Growth (\\%%) \\\\", base_year, end_year),
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
  "../output/rental_data_validation_growth.tex"
)
