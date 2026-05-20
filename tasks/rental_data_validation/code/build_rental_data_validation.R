# Validate citywide listed-rent growth against external rent benchmarks.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_data_validation/code")

source("../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(ggplot2)
library(readr)
library(tidycensus)

month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

fetch_zillow_series <- function(url, region_name, state, value_name) {
  raw <- as.data.table(read_csv(url, show_col_types = FALSE))
  keep <- raw$RegionName == region_name
  if (!is.null(state) && "State" %in% names(raw)) {
    keep <- keep & raw$State == state
  }
  dt <- raw[keep]
  if (nrow(dt) != 1L) {
    stop(sprintf("Expected one Zillow row for %s.", region_name), call. = FALSE)
  }

  date_cols <- names(dt)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(dt))]
  long <- melt(dt, measure.vars = date_cols, variable.name = "period_end", value.name = value_name)
  long[, month_start := month_floor(period_end)]
  long[, (value_name) := as.numeric(get(value_name))]
  long[
    month_start >= as.Date("2014-01-01") & month_start <= as.Date("2022-12-01"),
    .(month_start, value = get(value_name))
  ]
}

fetch_fred_series <- function(series_id) {
  fred_url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", series_id)
  old_http_ua <- getOption("HTTPUserAgent")
  on.exit(options(HTTPUserAgent = old_http_ua), add = TRUE)
  options(HTTPUserAgent = paste0("curl/", curl::curl_version()$version))

  raw <- read_csv(fred_url, show_col_types = FALSE)
  if (!all(c("observation_date", series_id) %in% names(raw))) {
    stop(sprintf("FRED response missing expected columns for %s.", series_id), call. = FALSE)
  }
  dt <- as.data.table(raw)
  dt[, month_start := month_floor(observation_date)]
  dt[, value := suppressWarnings(as.numeric(get(series_id)))]
  dt[
    month_start >= as.Date("2014-01-01") & month_start <= as.Date("2022-12-01"),
    .(month_start, value)
  ]
}

fetch_acs_series <- function() {
  rbindlist(lapply(2014L:2022L, function(y) {
    tryCatch({
      raw <- tidycensus::get_acs(
        geography = "place",
        variables = "B25064_001",
        state = "IL",
        year = y,
        survey = "acs1",
        geometry = FALSE,
        cache_table = TRUE
      )
      row <- raw[grepl("^Chicago city", raw$NAME), ]
      if (nrow(row) == 0L) {
        stop(sprintf("Chicago city not found in ACS %d.", y), call. = FALSE)
      }
      data.table(
        year = y,
        value = as.numeric(row$estimate[1]),
        status = "ok",
        detail = "ACS 1-year B25064 median gross rent"
      )
    }, error = function(e) {
      data.table(
        year = y,
        value = NA_real_,
        status = "warning",
        detail = conditionMessage(e)
      )
    })
  }), fill = TRUE)
}

annual_from_monthly <- function(dt, source_id, source_label, measure_label) {
  dt <- copy(dt)
  dt[, year := as.integer(format(month_start, "%Y"))]
  dt[
    is.finite(value),
    .(
      value = mean(value, na.rm = TRUE),
      n_months = .N
    ),
    by = year
  ][
    ,
    `:=`(
      source_id = source_id,
      source_label = source_label,
      measure_label = measure_label
    )
  ]
}

write_tex_table <- function(dt, base_year, end_year) {
  table_dt <- copy(dt)
  table_dt[, growth_label := ifelse(is.finite(growth_pct), sprintf("%.1f", growth_pct), "")]
  table_dt[, end_label := ifelse(is.finite(end_index), sprintf("%.1f", end_index), "")]
  table_dt[, start_label := ifelse(is.finite(start_index), sprintf("%.1f", start_index), "")]

  lines <- c(
    "\\begin{tabular}{lccc}",
    "\\toprule",
    sprintf("Series & %d Index & %d Index & Growth (\\%%) \\\\", base_year, end_year),
    "\\midrule",
    sprintf(
      "%s & %s & %s & %s \\\\",
      table_dt$source_label,
      table_dt$start_label,
      table_dt$end_label,
      table_dt$growth_label
    ),
    "\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, "../output/rental_data_validation_growth.tex")
}

message("Reading cleaned rental panel...")
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

monthly_listed_rents <- rent_panel[
  ,
  .(
    value = median(rent_price, na.rm = TRUE),
    n_floorplan_months = .N,
    n_property_proxies = uniqueN(property_key)
  ),
  by = month_start
]
setorder(monthly_listed_rents, month_start)

listed_annual <- annual_from_monthly(
  monthly_listed_rents[, .(month_start, value)],
  source_id = "listed_rents",
  source_label = "Listed rents",
  measure_label = "Annual average of monthly median listed rent"
)
listed_counts <- monthly_listed_rents[
  ,
  .(
    n_floorplan_months = sum(n_floorplan_months),
    n_property_proxies = sum(n_property_proxies),
    n_months_observed = .N
  ),
  by = .(year = as.integer(format(month_start, "%Y")))
]
listed_annual <- merge(listed_annual, listed_counts, by = "year", all.x = TRUE, sort = TRUE)

message("Fetching Zillow, FRED, and ACS benchmarks...")
fetch_status <- data.table(source = character(), status = character(), detail = character())

zillow_city <- tryCatch({
  out <- fetch_zillow_series(
    url = "https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv",
    region_name = "Chicago",
    state = "IL",
    value_name = "zillow_city_zori"
  )
  fetch_status <- rbind(
    fetch_status,
    data.table(source = "zillow_city_zori", status = "ok", detail = "Zillow Observed Rent Index, Chicago city")
  )
  out
}, error = function(e) {
  fetch_status <<- rbind(
    fetch_status,
    data.table(source = "zillow_city_zori", status = "error", detail = conditionMessage(e))
  )
  data.table(month_start = as.Date(character()), value = numeric())
})

fred_rent_cpi <- tryCatch({
  out <- fetch_fred_series("CUURA207SEHA")
  fetch_status <- rbind(
    fetch_status,
    data.table(source = "fred_cuura207seha", status = "ok", detail = "FRED Chicago rent of primary residence CPI")
  )
  out
}, error = function(e) {
  fetch_status <<- rbind(
    fetch_status,
    data.table(source = "fred_cuura207seha", status = "error", detail = conditionMessage(e))
  )
  data.table(month_start = as.Date(character()), value = numeric())
})

acs_rent <- fetch_acs_series()
acs_status <- if (all(acs_rent$status == "ok")) {
  "ok"
} else if (any(acs_rent$status == "ok")) {
  "warning"
} else {
  "error"
}
acs_detail <- if (acs_status == "warning") {
  "ACS 1-year B25064 median gross rent; regular 2020 ACS 1-year is unavailable"
} else if (acs_status == "ok") {
  "ACS 1-year B25064 median gross rent"
} else {
  paste(unique(acs_rent$detail), collapse = " | ")
}
fetch_status <- rbind(
  fetch_status,
  data.table(
    source = "acs_b25064",
    status = acs_status,
    detail = acs_detail
  )
)
fwrite(fetch_status, "../output/rental_data_validation_source_status.csv", na = "")

zillow_annual <- annual_from_monthly(
  zillow_city,
  source_id = "zillow_zori",
  source_label = "Zillow ZORI",
  measure_label = "Annual average of monthly Zillow Observed Rent Index"
)
fred_annual <- annual_from_monthly(
  fred_rent_cpi,
  source_id = "fred_rent_cpi",
  source_label = "FRED rent CPI",
  measure_label = "Annual average of monthly Chicago rent-of-primary-residence CPI"
)
acs_annual <- acs_rent[
  ,
  .(
    year,
    value,
    n_months = NA_integer_,
    source_id = "acs_median_gross_rent",
    source_label = "ACS median gross rent",
    measure_label = "ACS 1-year B25064 median gross rent"
  )
]

annual_series <- rbindlist(
  list(
    listed_annual,
    zillow_annual,
    fred_annual,
    acs_annual
  ),
  use.names = TRUE,
  fill = TRUE
)
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
base_year <- max(coverage$first_year)
end_year <- min(coverage$last_year)

active_sources <- coverage[
  first_year <= base_year & last_year >= end_year & has_2022 == TRUE,
  source_id
]
external_sources <- setdiff(active_sources, "listed_rents")
if (!"listed_rents" %in% active_sources || length(external_sources) < 2L) {
  stop("Rental validation needs listed rents and at least two external benchmarks with common start/end years.", call. = FALSE)
}

annual_series <- annual_series[source_id %in% active_sources & year >= base_year & year <= end_year]
annual_series[, base_value := value[year == base_year][1], by = source_id]
annual_series[, index_value := 100 * value / base_value]

summary_table <- annual_series[
  year %in% c(base_year, end_year),
  .(
    start_index = index_value[year == base_year][1],
    end_index = index_value[year == end_year][1],
    growth_pct = 100 * (value[year == end_year][1] / value[year == base_year][1] - 1),
    measure_label = measure_label[1]
  ),
  by = .(source_id, source_label)
]
source_order <- c("listed_rents", "zillow_zori", "fred_rent_cpi", "acs_median_gross_rent")
summary_table[, source_id := factor(source_id, levels = source_order)]
setorder(summary_table, source_id)
summary_table[, source_id := as.character(source_id)]

annual_series[, source_id := factor(source_id, levels = source_order)]
setorder(annual_series, source_id, year)
annual_series[, source_id := as.character(source_id)]
fwrite(annual_series, "../output/rental_data_validation_growth_series.csv", na = "")
write_tex_table(summary_table, base_year, end_year)

plot_series <- copy(annual_series)
plot_series[, source_label := factor(
  source_label,
  levels = summary_table$source_label
)]

validation_plot <- ggplot(
  plot_series,
  aes(x = year, y = index_value, color = source_label, group = source_label)
) +
  geom_hline(yintercept = 100, color = "grey80", linewidth = 0.35) +
  geom_line(linewidth = 0.85, na.rm = TRUE) +
  geom_point(size = 2.1, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(base_year, end_year, by = 1)) +
  scale_color_manual(
    values = c(
      "Listed rents" = "#1f78b4",
      "Zillow ZORI" = "#33a02c",
      "FRED rent CPI" = "#6a3d9a",
      "ACS median gross rent" = "#555555"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Citywide Listed-Rent Growth Versus External Benchmarks",
    subtitle = sprintf("Annual indexes normalized to %d = 100", base_year),
    x = NULL,
    y = "Rent index",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

ggsave(
  "../output/rental_data_validation_growth.pdf",
  validation_plot,
  width = 7.6,
  height = 4.8,
  bg = "white"
)
