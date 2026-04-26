source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")
source("../../_lib/acs_benchmark_helpers.R")
source("../../_lib/renthub_market_helpers.R")
source("../../_lib/renthub_raw_listing_helpers.R")

library(DBI)
library(duckdb)
library(data.table)
library(dplyr)
library(fixest)
library(readr)
library(sf)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_market_index/code")
# raw_dir <- "../input/renthub_raw"
# benchmark_input <- "../input/external_benchmark_comparison.csv"
# cpi_input <- "../input/fred_chi_cpi_all_items.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# ward_controls_input <- "../input/ward_controls_2000_2023.csv"
# bg_controls_input <- "../input/block_group_controls_2000_2023.csv"
# bg_geometry_input <- "../input/block_group_geometry_2019.gpkg"
# trim_summary_input <- "../input/renthub_trim_sensitivity_summary.csv"
# citywide_output <- "../output/renthub_market_citywide_series.csv"
# benchmark_monthly_output <- "../output/renthub_market_benchmark_comparison.csv"
# benchmark_summary_output <- "../output/renthub_market_benchmark_summary.csv"
# neighborhood_monthly_output <- "../output/renthub_market_neighborhood_monthly.csv"
# neighborhood_period_output <- "../output/renthub_market_neighborhood_periods.csv"
# coverage_output <- "../output/renthub_market_coverage_summary.csv"
# acs_growth_output <- "../output/renthub_market_acs_growth_comparison.csv"
# acs_diagnostics_output <- "../output/renthub_market_acs_diagnostics.csv"
# temp_dir <- "../temp"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    raw_dir,
    benchmark_input,
    cpi_input,
    ward_panel_input,
    community_area_input,
    ward_controls_input,
    bg_controls_input,
    bg_geometry_input,
    trim_summary_input,
    citywide_output,
    benchmark_monthly_output,
    benchmark_summary_output,
    neighborhood_monthly_output,
    neighborhood_period_output,
    coverage_output,
    acs_growth_output,
    acs_diagnostics_output,
    temp_dir
  )
}

if (length(args) != 18) {
  stop(
    paste(
      "FATAL: Script requires 18 args:",
      "<raw_dir> <benchmark_input> <cpi_input> <ward_panel_input>",
      "<community_area_input> <ward_controls_input> <bg_controls_input>",
      "<bg_geometry_input> <trim_summary_input> <citywide_output>",
      "<benchmark_monthly_output> <benchmark_summary_output>",
      "<neighborhood_monthly_output> <neighborhood_period_output>",
      "<coverage_output> <acs_growth_output> <acs_diagnostics_output>",
      "<temp_dir>"
    ),
    call. = FALSE
  )
}

raw_dir <- args[1]
benchmark_input <- args[2]
cpi_input <- args[3]
ward_panel_input <- args[4]
community_area_input <- args[5]
ward_controls_input <- args[6]
bg_controls_input <- args[7]
bg_geometry_input <- args[8]
trim_summary_input <- args[9]
citywide_output <- args[10]
benchmark_monthly_output <- args[11]
benchmark_summary_output <- args[12]
neighborhood_monthly_output <- args[13]
neighborhood_period_output <- args[14]
coverage_output <- args[15]
acs_growth_output <- args[16]
acs_diagnostics_output <- args[17]
temp_dir <- args[18]

analysis_start_month <- as.Date("2019-01-01")
analysis_base_month <- as.Date("2019-01-01")
real_base_year <- 2024L
news_benchmark_input <- "../input/news_benchmarks.csv"

trim_spec <- load_trim_spec(trim_summary_input)
trim_suffix <- switch(
  trim_spec,
  strict_p1 = "01",
  strict_p25 = "025",
  strict_p5 = "05",
  "01"
)
rent_trim_col <- paste0("keep_rent_trim", trim_suffix)
sqft_trim_col <- paste0("keep_sqft_trim", trim_suffix)

benchmark_base <- fread(benchmark_input)
benchmark_base[, month_start := as.Date(month_start)]
benchmark_base <- benchmark_base[month_start >= analysis_start_month]

latest_benchmark_month <- max(benchmark_base$month_start, na.rm = TRUE)
cpi_dt <- load_monthly_cpi_deflator(
  cpi_input = cpi_input,
  start_month = analysis_start_month,
  end_month = latest_benchmark_month,
  base_year = real_base_year
)

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, sprintf("PRAGMA temp_directory='%s'", duck_escape(temp_dir)))
dbWriteTable(con, "cpi_deflator", cpi_dt, temporary = TRUE, overwrite = TRUE)

raw_glob <- file.path(raw_dir, "*.parquet")
create_renthub_raw_day_tables(con, raw_glob, analysis_start_month)

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

create_renthub_gap_cycles(con, "series_day_base", "series_day_final", gap_days = 90, require_same_day = TRUE)

coord_dt <- collect_query(
  sprintf(
    "
    SELECT DISTINCT
      coord_key,
      latitude,
      longitude
    FROM series_day_final
    WHERE month_start >= CAST('%s' AS DATE)
      AND coord_key IS NOT NULL
      AND latitude IS NOT NULL
      AND longitude IS NOT NULL
    ",
    format(analysis_start_month, "%Y-%m-%d")
  )
)

coord_lookup <- build_coord_geography_lookup(
  coords_tbl = coord_dt,
  ward_panel_input = ward_panel_input,
  community_area_input = community_area_input
)
setDT(coord_lookup)
dbWriteTable(con, "coord_lookup", coord_lookup, temporary = TRUE, overwrite = TRUE)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE market_day_final AS
  SELECT
    s.*,
    c.community_area,
    c.community_name,
    CASE
      WHEN s.year <= 2023 THEN c.ward_2015
      ELSE c.ward_2024
    END AS ward,
    d.deflator_to_2024,
    s.rent_price * d.deflator_to_2024 AS rent_real_2024,
    CASE
      WHEN s.sqft IS NOT NULL AND s.sqft > 0 THEN (s.rent_price * d.deflator_to_2024) / s.sqft
      ELSE NULL
    END AS rent_per_sqft_real_2024
  FROM series_day_final s
  INNER JOIN coord_lookup c
    ON s.coord_key = c.coord_key
  INNER JOIN cpi_deflator d
    ON s.month_start = d.month_start
  WHERE c.community_area IS NOT NULL
  "
)

latest_market_month <- collect_query("SELECT MAX(month_start) AS latest_month FROM market_day_final")
latest_market_month <- as.Date(latest_market_month$latest_month[1])
latest_full_year <- as.integer(format(latest_market_month, "%Y"))
if (format(latest_market_month, "%m") != "12") {
  latest_full_year <- latest_full_year - 1L
}

trim_filter <- sprintf("%s = 1 AND %s = 1", rent_trim_col, sqft_trim_col)
stable_company_sql <- sprintf(
  "
  CREATE OR REPLACE TEMP TABLE stable_companies AS
  SELECT company_norm
  FROM market_day_final
  WHERE company_norm IS NOT NULL
    AND %s
    AND year BETWEEN 2019 AND %d
  GROUP BY company_norm
  HAVING COUNT(DISTINCT year) = %d
     AND COUNT(DISTINCT month_start) >= 24
  ",
  trim_filter,
  latest_full_year,
  latest_full_year - 2019L + 1L
)
dbExecute(con, stable_company_sql)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE active_month_base AS
    WITH ranked AS (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, month_start
          ORDER BY file_date DESC, last_scraped_timestamp DESC, rent_price DESC
        ) AS month_row_number
      FROM market_day_final
      WHERE %s
    )
    SELECT
      r.*,
      CASE WHEN s.company_norm IS NOT NULL THEN 1 ELSE 0 END AS company_stable
    FROM ranked r
    LEFT JOIN stable_companies s
      ON r.company_norm = s.company_norm
    WHERE month_row_number = 1
    ",
    trim_filter
  )
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE cycle_first_base AS
    SELECT
      m.*,
      CASE WHEN s.company_norm IS NOT NULL THEN 1 ELSE 0 END AS company_stable
    FROM market_day_final m
    LEFT JOIN stable_companies s
      ON m.company_norm = s.company_norm
    WHERE cycle_row_number = 1
      AND %s
    ",
    trim_filter
  )
)

month_windows <- data.table(target_month = seq.Date(analysis_start_month, latest_market_month, by = "month"))
month_windows <- rbindlist(lapply(0:2, function(lag_value) {
  data.table(
    target_month = month_windows$target_month,
    source_month = seq.Date(analysis_start_month - 31 * lag_value, latest_market_month - 31 * lag_value, by = "month")[seq_len(nrow(month_windows))]
  )
}), use.names = TRUE)
month_windows <- month_windows[source_month >= analysis_start_month & source_month <= latest_market_month]
month_windows[, source_month := month_floor(source_month)]
month_windows <- unique(month_windows)
dbWriteTable(con, "month_windows", month_windows, temporary = TRUE, overwrite = TRUE)

citywide_monthly_sql <- "
  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'asking' AS series_family,
    w.target_month AS month_start,
    MEDIAN(c.rent_price) AS series_value_nominal,
    MEDIAN(c.rent_real_2024) AS series_value_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT c.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN cycle_first_base c
    ON c.month_start = w.source_month
  GROUP BY 1, 2, 3, 4

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'asking' AS series_family,
    w.target_month AS month_start,
    MEDIAN(a.rent_price) AS series_value_nominal,
    MEDIAN(a.rent_real_2024) AS series_value_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT a.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN active_month_base a
    ON a.month_start = w.source_month
  GROUP BY 1, 2, 3, 4

  UNION ALL

  SELECT
    'cycle_first_stable' AS series_id,
    'Cycle-first asking rent (stable companies)' AS series_label,
    'asking' AS series_family,
    w.target_month AS month_start,
    MEDIAN(c.rent_price) AS series_value_nominal,
    MEDIAN(c.rent_real_2024) AS series_value_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT c.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN cycle_first_base c
    ON c.month_start = w.source_month
  WHERE c.company_stable = 1
  GROUP BY 1, 2, 3, 4

  UNION ALL

  SELECT
    'active_month_stable' AS series_id,
    'Active-listing asking rent (stable companies)' AS series_label,
    'asking' AS series_family,
    w.target_month AS month_start,
    MEDIAN(a.rent_price) AS series_value_nominal,
    MEDIAN(a.rent_real_2024) AS series_value_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT a.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN active_month_base a
    ON a.month_start = w.source_month
  WHERE a.company_stable = 1
  GROUP BY 1, 2, 3, 4
"
citywide_monthly <- collect_query(citywide_monthly_sql)
citywide_monthly[, month_start := as.Date(month_start)]
setorder(citywide_monthly, series_id, month_start)
citywide_monthly <- normalize_to_base_or_first(
  dt = citywide_monthly,
  value_col = "series_value_nominal",
  month_col = "month_start",
  group_col = "series_id",
  base_month = analysis_base_month
)
setnames(citywide_monthly, "index_value", "index_2019m1")
citywide_monthly[, yoy_pct := compute_yoy_pct(series_value_nominal), by = series_id]
citywide_monthly[, yoy_log_pct := compute_log_growth_pct(series_value_nominal), by = series_id]

active_repeat_dt <- collect_query(
  "
  SELECT
    analysis_key,
    key_source,
    month_start,
    rent_price,
    rent_real_2024,
    beds,
    baths,
    sqft,
    building_type_clean,
    company_norm
  FROM active_month_base
  WHERE rent_price > 0
  "
)
active_repeat_dt[, month_start := as.Date(month_start)]
active_repeat_dt[, log_rent_nominal := log(rent_price)]

repeat_key <- two_way_repeat_index(
  dt = active_repeat_dt,
  value_col = "log_rent_nominal",
  key_col = "analysis_key",
  month_col = "month_start",
  base_month = analysis_base_month
)
repeat_key[, `:=`(
  series_id = "repeat_key",
  series_label = "Repeat-rent index",
  series_family = "smooth",
  n_obs = active_repeat_dt[, .N],
  n_keys = active_repeat_dt[, uniqueN(analysis_key)],
  series_value_nominal = index_value
)]

repeat_unit <- two_way_repeat_index(
  dt = active_repeat_dt[key_source == "unit_id"],
  value_col = "log_rent_nominal",
  key_col = "analysis_key",
  month_col = "month_start",
  base_month = analysis_base_month
)
repeat_unit[, `:=`(
  series_id = "repeat_unit_id",
  series_label = "Repeat-rent index (UNIT_ID only)",
  series_family = "smooth",
  n_obs = active_repeat_dt[key_source == "unit_id", .N],
  n_keys = active_repeat_dt[key_source == "unit_id", uniqueN(analysis_key)],
  series_value_nominal = index_value
)]

hedonic_dt <- copy(active_repeat_dt)[
  is.finite(rent_price) & rent_price > 0
]
hedonic_dt[, bed_bucket := fifelse(!is.finite(beds), -1, pmin(as.integer(round(beds)), 5L))]
hedonic_dt[, bath_bucket := fifelse(!is.finite(baths), -1, pmin(as.integer(round(2 * baths)), 8L))]
hedonic_dt[, sqft_bin := as.character(cut(
  sqft,
  breaks = c(0, 400, 600, 800, 1000, 1200, 1500, 2000, 3000, Inf),
  right = FALSE,
  include.lowest = TRUE
))]
hedonic_dt[!is.finite(sqft) | sqft <= 0, sqft_bin := "missing"]
top_companies <- hedonic_dt[!is.na(company_norm), .N, by = company_norm][order(-N)][1:min(25, .N), company_norm]
hedonic_dt[, company_bucket := fifelse(
  is.na(company_norm),
  "UNKNOWN",
  fifelse(company_norm %in% top_companies, company_norm, "OTHER")
)]
hedonic_dt[, month_factor := factor(month_start)]
hedonic_dt[, month_label := as.character(month_start)]
hedonic_collapsed <- hedonic_dt[
  ,
  .(
    log_rent_mean = mean(log_rent_nominal),
    n_obs = .N
  ),
  by = .(month_label, bed_bucket, bath_bucket, sqft_bin, building_type_clean, company_bucket)
]
hedonic_fit <- fixest::feols(
  log_rent_mean ~ i(month_label, ref = as.character(analysis_base_month)) + i(bed_bucket) + i(bath_bucket) +
    i(sqft_bin) + i(building_type_clean) + i(company_bucket),
  data = hedonic_collapsed,
  weights = ~n_obs
)
hedonic_months <- data.table(month_start = sort(unique(active_repeat_dt$month_start)))
hedonic_months[, month_label := as.character(month_start)]
hedonic_month_terms <- coef(hedonic_fit)
hedonic_months[, month_coef := 0]
hedonic_months[month_label != as.character(analysis_base_month), month_coef := hedonic_month_terms[paste0("month_label::", month_label)]]
hedonic_months[, index_value := 100 * exp(month_coef)]
hedonic_months[, `:=`(
  series_id = "hedonic_index",
  series_label = "Hedonic rent index",
  series_family = "smooth",
  n_obs = active_repeat_dt[, .N],
  n_keys = active_repeat_dt[, uniqueN(analysis_key)],
  series_value_nominal = index_value
)]

smooth_series <- rbindlist(
  list(
    repeat_key[, .(month_start = month_value, series_id, series_label, series_family, series_value_nominal, n_obs, n_keys)],
    repeat_unit[, .(month_start = month_value, series_id, series_label, series_family, series_value_nominal, n_obs, n_keys)],
    hedonic_months[, .(month_start, series_id, series_label, series_family, series_value_nominal, n_obs, n_keys)]
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(smooth_series, series_id, month_start)
smooth_series <- normalize_to_base_or_first(
  dt = smooth_series,
  value_col = "series_value_nominal",
  month_col = "month_start",
  group_col = "series_id",
  base_month = analysis_base_month
)
setnames(smooth_series, "index_value", "index_2019m1")
smooth_series[, yoy_pct := compute_yoy_pct(series_value_nominal), by = series_id]
smooth_series[, yoy_log_pct := compute_log_growth_pct(series_value_nominal), by = series_id]
smooth_series[, series_value_real_2024 := NA_real_]

citywide_series <- rbindlist(
  list(
    citywide_monthly[, .(
      month_start,
      series_id,
      series_label,
      series_family,
      series_value_nominal,
      series_value_real_2024,
      index_2019m1,
      yoy_pct,
      yoy_log_pct,
      n_obs,
      n_keys
    )],
    smooth_series[, .(
      month_start,
      series_id,
      series_label,
      series_family,
      series_value_nominal,
      series_value_real_2024,
      index_2019m1,
      yoy_pct,
      yoy_log_pct,
      n_obs,
      n_keys
    )]
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(citywide_series, series_family, series_id, month_start)

neighborhood_monthly_sql <- "
  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'citywide' AS geography_level,
    CAST(NULL AS INTEGER) AS geography_id,
    'Chicago' AS geography_name,
    w.target_month AS month_start,
    MEDIAN(c.rent_price) AS median_rent_nominal,
    MEDIAN(c.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT c.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN cycle_first_base c
    ON c.month_start = w.source_month
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'ward' AS geography_level,
    CAST(c.ward AS INTEGER) AS geography_id,
    'Ward ' || CAST(c.ward AS VARCHAR) AS geography_name,
    w.target_month AS month_start,
    MEDIAN(c.rent_price) AS median_rent_nominal,
    MEDIAN(c.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT c.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN cycle_first_base c
    ON c.month_start = w.source_month
  WHERE c.ward IS NOT NULL
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'community_area' AS geography_level,
    CAST(c.community_area AS INTEGER) AS geography_id,
    c.community_name AS geography_name,
    w.target_month AS month_start,
    MEDIAN(c.rent_price) AS median_rent_nominal,
    MEDIAN(c.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT c.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN cycle_first_base c
    ON c.month_start = w.source_month
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'citywide' AS geography_level,
    CAST(NULL AS INTEGER) AS geography_id,
    'Chicago' AS geography_name,
    w.target_month AS month_start,
    MEDIAN(a.rent_price) AS median_rent_nominal,
    MEDIAN(a.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT a.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN active_month_base a
    ON a.month_start = w.source_month
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'ward' AS geography_level,
    CAST(a.ward AS INTEGER) AS geography_id,
    'Ward ' || CAST(a.ward AS VARCHAR) AS geography_name,
    w.target_month AS month_start,
    MEDIAN(a.rent_price) AS median_rent_nominal,
    MEDIAN(a.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT a.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN active_month_base a
    ON a.month_start = w.source_month
  WHERE a.ward IS NOT NULL
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'community_area' AS geography_level,
    CAST(a.community_area AS INTEGER) AS geography_id,
    a.community_name AS geography_name,
    w.target_month AS month_start,
    MEDIAN(a.rent_price) AS median_rent_nominal,
    MEDIAN(a.rent_real_2024) AS median_rent_real_2024,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT a.analysis_key) AS n_keys
  FROM month_windows w
  INNER JOIN active_month_base a
    ON a.month_start = w.source_month
  GROUP BY 1, 2, 3, 4, 5, 6
"
neighborhood_monthly <- collect_query(neighborhood_monthly_sql)
neighborhood_monthly[, month_start := as.Date(month_start)]
setorder(neighborhood_monthly, series_id, geography_level, geography_id, month_start)
neighborhood_monthly[, raw_growth_nominal_pct := compute_log_growth_pct(median_rent_nominal), by = .(series_id, geography_level, geography_id)]
neighborhood_monthly[, raw_growth_real_pct := compute_log_growth_pct(median_rent_real_2024), by = .(series_id, geography_level, geography_id)]
neighborhood_monthly[, prior_n_keys := data.table::shift(n_keys, 12), by = .(series_id, geography_level, geography_id)]

city_growth <- neighborhood_monthly[
  geography_level == "citywide",
  .(series_id, month_start, city_growth_nominal_pct = raw_growth_nominal_pct)
]
neighborhood_monthly <- merge(
  neighborhood_monthly,
  city_growth,
  by = c("series_id", "month_start"),
  all.x = TRUE,
  sort = FALSE
)
neighborhood_monthly[, support_n := pmin(n_keys, prior_n_keys)]
neighborhood_monthly[, shrink_lambda := fifelse(
  is.finite(support_n),
  support_n / (support_n + 100),
  NA_real_
)]
neighborhood_monthly[, shrunk_growth_nominal_pct := fifelse(
  is.finite(raw_growth_nominal_pct) & is.finite(city_growth_nominal_pct) & is.finite(shrink_lambda),
  shrink_lambda * raw_growth_nominal_pct + (1 - shrink_lambda) * city_growth_nominal_pct,
  NA_real_
)]
neighborhood_monthly[, display_rule := support_rule_classification(n_keys, prior_n_keys)]
neighborhood_monthly[, display_growth_nominal_pct := fifelse(
  display_rule == "raw",
  raw_growth_nominal_pct,
  fifelse(display_rule == "shrunk", shrunk_growth_nominal_pct, NA_real_)
)]

annual_private_sql <- "
  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'citywide' AS geography_level,
    CAST(NULL AS INTEGER) AS geography_id,
    'Chicago' AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM cycle_first_base
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'ward' AS geography_level,
    CAST(ward AS INTEGER) AS geography_id,
    'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM cycle_first_base
  WHERE ward IS NOT NULL
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'cycle_first' AS series_id,
    'Cycle-first asking rent' AS series_label,
    'community_area' AS geography_level,
    CAST(community_area AS INTEGER) AS geography_id,
    community_name AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM cycle_first_base
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'citywide' AS geography_level,
    CAST(NULL AS INTEGER) AS geography_id,
    'Chicago' AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM active_month_base
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'ward' AS geography_level,
    CAST(ward AS INTEGER) AS geography_id,
    'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM active_month_base
  WHERE ward IS NOT NULL
  GROUP BY 1, 2, 3, 4, 5, 6

  UNION ALL

  SELECT
    'active_month' AS series_id,
    'Active-listing asking rent' AS series_label,
    'community_area' AS geography_level,
    CAST(community_area AS INTEGER) AS geography_id,
    community_name AS geography_name,
    CAST(YEAR(month_start) AS INTEGER) AS year,
    MEDIAN(rent_real_2024) AS value_real,
    COUNT(*) AS n_obs
  FROM active_month_base
  GROUP BY 1, 2, 3, 4, 5, 6
"
annual_private <- collect_query(annual_private_sql)
annual_private[, year := as.integer(year)]
annual_private <- annual_private[year >= 2019 & year <= latest_full_year]

acs_end_year <- min(
  latest_full_year,
  fread(ward_controls_input, select = "year")[, max(year, na.rm = TRUE)]
)
annual_cpi <- load_annual_cpi_deflators(cpi_input)

ward_acs <- fread(ward_controls_input)[
  year >= 2019L & year <= acs_end_year,
  .(
    geography_level = "ward",
    geography_id = as.integer(ward),
    geography_name = paste("Ward", as.integer(ward)),
    year = as.integer(year),
    acs_rent_nominal = as.numeric(avg_rent),
    hu_total = as.numeric(hu_total),
    homeownership_rate = as.numeric(homeownership_rate)
  )
]
ward_acs[, estimated_renter_units := hu_total * (1 - homeownership_rate)]

citywide_acs <- ward_acs[
  ,
  .(
    geography_level = "citywide",
    geography_id = NA_integer_,
    geography_name = "Chicago",
    acs_rent_nominal = weighted.mean(
      acs_rent_nominal,
      w = fifelse(is.finite(estimated_renter_units) & estimated_renter_units > 0, estimated_renter_units, 0),
      na.rm = TRUE
    )
  ),
  by = year
]

acs_panel <- rbindlist(
  list(
    citywide_acs[, .(geography_level, geography_id, geography_name, year, acs_rent_nominal)],
    ward_acs[, .(geography_level, geography_id, geography_name, year, acs_rent_nominal)]
  ),
  use.names = TRUE,
  fill = TRUE
)
acs_panel <- merge(
  acs_panel,
  annual_cpi[, .(year, deflator_to_2024)],
  by = "year",
  all.x = TRUE
)
acs_panel[, acs_rent_real_2024 := acs_rent_nominal * deflator_to_2024]

acs_growth <- acs_panel[
  ,
  .(
    start_year = 2019L,
    end_year = acs_end_year,
    acs_start_real = acs_rent_real_2024[year == 2019L][1],
    acs_end_real = acs_rent_real_2024[year == acs_end_year][1]
  ),
  by = .(geography_level, geography_id, geography_name)
]
acs_growth[, acs_growth_pct := 100 * (acs_end_real / acs_start_real - 1)]

private_growth <- annual_private[
  geography_level %in% c("citywide", "ward") & year <= acs_end_year,
  .(
    start_year = 2019L,
    end_year = acs_end_year,
    private_start_real = value_real[year == 2019L][1],
    private_end_real = value_real[year == acs_end_year][1],
    private_n_start = n_obs[year == 2019L][1],
    private_n_end = n_obs[year == acs_end_year][1]
  ),
  by = .(series_id, series_label, geography_level, geography_id, geography_name)
]
private_growth[, private_growth_pct := 100 * (private_end_real / private_start_real - 1)]

acs_growth_comparison <- merge(
  private_growth,
  acs_growth,
  by = c("geography_level", "geography_id", "geography_name", "start_year", "end_year"),
  all = FALSE
)
acs_growth_comparison <- acs_growth_comparison[
  is.finite(private_start_real) &
    is.finite(private_end_real) &
    is.finite(acs_start_real) &
    is.finite(acs_end_real)
]
acs_growth_comparison[, growth_gap_pct := private_growth_pct - acs_growth_pct]

slope_from_fit <- function(x, y) {
  if (length(x) < 2 || !is.finite(sd(x)) || sd(x) == 0) {
    return(NA_real_)
  }
  coef(lm(y ~ x))[2]
}

acs_diagnostics <- acs_growth_comparison[
  ,
  .(
    n_geographies = .N,
    correlation = if (.N >= 2) cor(private_growth_pct, acs_growth_pct) else NA_real_,
    slope = slope_from_fit(acs_growth_pct, private_growth_pct),
    mean_abs_gap_pct = mean(abs(growth_gap_pct)),
    median_abs_gap_pct = median(abs(growth_gap_pct)),
    mean_signed_gap_pct = mean(growth_gap_pct)
  ),
  by = .(series_id, series_label, geography_level, start_year, end_year)
]

benchmark_monthly <- merge(
  citywide_series,
  benchmark_base[
    ,
    .(
      month_start,
      zillow_city_zori,
      zillow_city_zori_yoy_pct,
      zillow_metro_zori,
      zillow_metro_zori_yoy_pct,
      fred_chi_rent_cpi,
      fred_chi_rent_cpi_yoy_pct,
      acs_chicago_median_gross_rent
    )
  ],
  by = "month_start",
  all.x = TRUE
)

if (file.exists(news_benchmark_input)) {
  news_benchmarks <- fread(news_benchmark_input)
  news_benchmarks[, month_start := as.Date(month_start)]
  benchmark_monthly <- merge(
    benchmark_monthly,
    news_benchmarks,
    by = "month_start",
    all.x = TRUE,
    sort = FALSE
  )
}

metric_rows <- list(
  data.table(benchmark_id = "zillow_city_zori", benchmark_label = "Zillow city ZORI", benchmark_family = "smooth", value_col = "zillow_city_zori", yoy_col = "zillow_city_zori_yoy_pct"),
  data.table(benchmark_id = "zillow_metro_zori", benchmark_label = "Zillow metro ZORI", benchmark_family = "smooth", value_col = "zillow_metro_zori", yoy_col = "zillow_metro_zori_yoy_pct"),
  data.table(benchmark_id = "fred_chi_rent_cpi", benchmark_label = "FRED Chicago rent CPI", benchmark_family = "smooth", value_col = "fred_chi_rent_cpi", yoy_col = "fred_chi_rent_cpi_yoy_pct")
)
if (exists("news_benchmarks")) {
  news_specs <- unique(news_benchmarks[, .(benchmark_id, benchmark_label, benchmark_family)])
  for (i in seq_len(nrow(news_specs))) {
    metric_rows[[length(metric_rows) + 1L]] <- data.table(
      benchmark_id = news_specs$benchmark_id[i],
      benchmark_label = news_specs$benchmark_label[i],
      benchmark_family = news_specs$benchmark_family[i],
      value_col = if ("benchmark_value" %in% names(news_benchmarks)) "benchmark_value" else NA_character_,
      yoy_col = if ("benchmark_yoy_pct" %in% names(news_benchmarks)) "benchmark_yoy_pct" else NA_character_
    )
  }
}
benchmark_specs <- rbindlist(metric_rows, use.names = TRUE, fill = TRUE)

benchmark_summary <- rbindlist(lapply(seq_len(nrow(benchmark_specs)), function(i) {
  spec <- benchmark_specs[i]
  out <- rbindlist(lapply(unique(citywide_series$series_id), function(series_name) {
    series_dt <- benchmark_monthly[series_id == series_name]
    if (!(spec$yoy_col %in% names(series_dt))) {
      return(NULL)
    }
    keep <- is.finite(series_dt$yoy_pct) & is.finite(series_dt[[spec$yoy_col]])
    if (!any(keep)) {
      return(NULL)
    }
    growth_gap <- median(abs(series_dt$yoy_pct[keep] - series_dt[[spec$yoy_col]][keep]), na.rm = TRUE)
    corr_value <- if (sum(keep) >= 6) cor(series_dt$yoy_pct[keep], series_dt[[spec$yoy_col]][keep]) else NA_real_
    cumulative_gap <- NA_real_
    if (!is.na(spec$value_col) && spec$value_col %in% names(series_dt)) {
      value_keep <- is.finite(series_dt$series_value_nominal) & is.finite(series_dt[[spec$value_col]])
      if (sum(value_keep) >= 2) {
        first_idx <- which(value_keep)[1]
        last_idx <- tail(which(value_keep), 1)
        cumulative_gap <- 100 * (
          (series_dt$series_value_nominal[last_idx] / series_dt$series_value_nominal[first_idx]) -
            (series_dt[[spec$value_col]][last_idx] / series_dt[[spec$value_col]][first_idx])
        )
      }
    }
    data.table(
      series_id = series_name,
      series_label = unique(series_dt$series_label),
      series_family = unique(series_dt$series_family),
      benchmark_id = spec$benchmark_id,
      benchmark_label = spec$benchmark_label,
      benchmark_family = spec$benchmark_family,
      n_pairs = sum(keep),
      correlation = corr_value,
      median_abs_gap_pp = growth_gap,
      cumulative_growth_gap_pct = cumulative_gap,
      first_overlap_month = series_dt$month_start[which(keep)[1]],
      last_overlap_month = series_dt$month_start[tail(which(keep), 1)]
    )
  }), use.names = TRUE, fill = TRUE)
  out
}), use.names = TRUE, fill = TRUE)

latest_ytd_month <- as.integer(format(latest_market_month, "%m"))
periods <- rbindlist(list(
  data.table(
    period_label = "2023_vs_2022_full_year",
    current_start = as.Date("2023-01-01"),
    current_end = as.Date("2023-12-31"),
    prior_start = as.Date("2022-01-01"),
    prior_end = as.Date("2022-12-31"),
    map_year = 2023L,
    period_type = "full_year"
  ),
  data.table(
    period_label = "2024_vs_2023_full_year",
    current_start = as.Date("2024-01-01"),
    current_end = as.Date("2024-12-31"),
    prior_start = as.Date("2023-01-01"),
    prior_end = as.Date("2023-12-31"),
    map_year = 2024L,
    period_type = "full_year"
  ),
  data.table(
    period_label = "2025_vs_2024_ytd",
    current_start = as.Date("2025-01-01"),
    current_end = month_end(as.Date(sprintf("2025-%02d-01", latest_ytd_month))),
    prior_start = as.Date("2024-01-01"),
    prior_end = month_end(as.Date(sprintf("2024-%02d-01", latest_ytd_month))),
    map_year = 2025L,
    period_type = "ytd"
  )
), use.names = TRUE, fill = TRUE)

period_rows <- rbindlist(lapply(seq_len(nrow(periods)), function(i) {
  p <- periods[i]
  current_start_sql <- format(as.Date(p$current_start), "%Y-%m-%d")
  current_end_sql <- format(as.Date(p$current_end), "%Y-%m-%d")
  prior_start_sql <- format(as.Date(p$prior_start), "%Y-%m-%d")
  prior_end_sql <- format(as.Date(p$prior_end), "%Y-%m-%d")

  out <- collect_query(
    sprintf(
      "
      WITH current_city AS (
        SELECT
          MEDIAN(rent_price) AS current_median_nominal,
          MEDIAN(rent_real_2024) AS current_median_real,
          COUNT(DISTINCT analysis_key) AS current_n_keys
        FROM cycle_first_base
        WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
      ),
      prior_city AS (
        SELECT
          MEDIAN(rent_price) AS prior_median_nominal,
          MEDIAN(rent_real_2024) AS prior_median_real,
          COUNT(DISTINCT analysis_key) AS prior_n_keys
        FROM cycle_first_base
        WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
      ),
      current_ward AS (
        SELECT
          'ward' AS geography_level,
          CAST(ward AS INTEGER) AS geography_id,
          'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
          MEDIAN(rent_price) AS current_median_nominal,
          MEDIAN(rent_real_2024) AS current_median_real,
          COUNT(DISTINCT analysis_key) AS current_n_keys
        FROM cycle_first_base
        WHERE ward IS NOT NULL
          AND month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
        GROUP BY 1, 2, 3
      ),
      prior_ward AS (
        SELECT
          'ward' AS geography_level,
          CAST(ward AS INTEGER) AS geography_id,
          'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
          MEDIAN(rent_price) AS prior_median_nominal,
          MEDIAN(rent_real_2024) AS prior_median_real,
          COUNT(DISTINCT analysis_key) AS prior_n_keys
        FROM cycle_first_base
        WHERE ward IS NOT NULL
          AND month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
        GROUP BY 1, 2, 3
      ),
      current_ca AS (
        SELECT
          'community_area' AS geography_level,
          CAST(community_area AS INTEGER) AS geography_id,
          community_name AS geography_name,
          MEDIAN(rent_price) AS current_median_nominal,
          MEDIAN(rent_real_2024) AS current_median_real,
          COUNT(DISTINCT analysis_key) AS current_n_keys
        FROM cycle_first_base
        WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
        GROUP BY 1, 2, 3
      ),
      prior_ca AS (
        SELECT
          'community_area' AS geography_level,
          CAST(community_area AS INTEGER) AS geography_id,
          community_name AS geography_name,
          MEDIAN(rent_price) AS prior_median_nominal,
          MEDIAN(rent_real_2024) AS prior_median_real,
          COUNT(DISTINCT analysis_key) AS prior_n_keys
        FROM cycle_first_base
        WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)
        GROUP BY 1, 2, 3
      )
      SELECT
        'citywide' AS geography_level,
        CAST(NULL AS INTEGER) AS geography_id,
        'Chicago' AS geography_name,
        current_city.current_median_nominal,
        current_city.current_median_real,
        current_city.current_n_keys,
        prior_city.prior_median_nominal,
        prior_city.prior_median_real,
        prior_city.prior_n_keys
      FROM current_city
      CROSS JOIN prior_city

      UNION ALL

      SELECT
        COALESCE(current_ward.geography_level, prior_ward.geography_level) AS geography_level,
        COALESCE(current_ward.geography_id, prior_ward.geography_id) AS geography_id,
        COALESCE(current_ward.geography_name, prior_ward.geography_name) AS geography_name,
        current_ward.current_median_nominal,
        current_ward.current_median_real,
        current_ward.current_n_keys,
        prior_ward.prior_median_nominal,
        prior_ward.prior_median_real,
        prior_ward.prior_n_keys
      FROM current_ward
      FULL OUTER JOIN prior_ward
        ON current_ward.geography_id = prior_ward.geography_id

      UNION ALL

      SELECT
        COALESCE(current_ca.geography_level, prior_ca.geography_level) AS geography_level,
        COALESCE(current_ca.geography_id, prior_ca.geography_id) AS geography_id,
        COALESCE(current_ca.geography_name, prior_ca.geography_name) AS geography_name,
        current_ca.current_median_nominal,
        current_ca.current_median_real,
        current_ca.current_n_keys,
        prior_ca.prior_median_nominal,
        prior_ca.prior_median_real,
        prior_ca.prior_n_keys
      FROM current_ca
      FULL OUTER JOIN prior_ca
        ON current_ca.geography_id = prior_ca.geography_id
      ",
      current_start_sql,
      current_end_sql,
      prior_start_sql,
      prior_end_sql,
      current_start_sql,
      current_end_sql,
      prior_start_sql,
      prior_end_sql,
      current_start_sql,
      current_end_sql,
      prior_start_sql,
      prior_end_sql
    )
  )
  out[, `:=`(
    period_label = p$period_label,
    period_type = p$period_type,
    map_year = p$map_year,
    current_start = p$current_start,
    current_end = p$current_end,
    prior_start = p$prior_start,
    prior_end = p$prior_end
  )]
  out
}), use.names = TRUE, fill = TRUE)

period_city <- period_rows[geography_level == "citywide", .(period_label, city_growth_nominal_pct = 100 * (log(current_median_nominal) - log(prior_median_nominal)))]
period_rows <- merge(period_rows, period_city, by = "period_label", all.x = TRUE)
period_rows[, raw_growth_nominal_pct := 100 * (log(current_median_nominal) - log(prior_median_nominal))]
period_rows[, raw_growth_real_pct := 100 * (log(current_median_real) - log(prior_median_real))]
period_rows[, support_n := pmin(current_n_keys, prior_n_keys)]
period_rows[, shrink_lambda := fifelse(is.finite(support_n), support_n / (support_n + 100), NA_real_)]
period_rows[, shrunk_growth_nominal_pct := fifelse(
  is.finite(raw_growth_nominal_pct) & is.finite(city_growth_nominal_pct) & is.finite(shrink_lambda),
  shrink_lambda * raw_growth_nominal_pct + (1 - shrink_lambda) * city_growth_nominal_pct,
  NA_real_
)]
period_rows[, display_rule := support_rule_classification(current_n_keys, prior_n_keys)]
period_rows[, display_growth_nominal_pct := fifelse(
  display_rule == "raw",
  raw_growth_nominal_pct,
  fifelse(display_rule == "shrunk", shrunk_growth_nominal_pct, NA_real_)
)]

coverage_summary <- rbindlist(
  list(
    data.table(
      section = "sample",
      item = c("chosen_trim_spec", "latest_market_month", "latest_full_year"),
      value = c(trim_spec, as.character(latest_market_month), as.character(latest_full_year))
    ),
    data.table(
      section = "sample",
      item = c("market_day_rows", "active_month_rows", "cycle_first_rows", "stable_companies"),
      value = c(
        collect_query("SELECT COUNT(*) AS n FROM market_day_final")$n[1],
        collect_query("SELECT COUNT(*) AS n FROM active_month_base")$n[1],
        collect_query("SELECT COUNT(*) AS n FROM cycle_first_base")$n[1],
        collect_query("SELECT COUNT(*) AS n FROM stable_companies")$n[1]
      )
    ),
    neighborhood_monthly[
      geography_level %in% c("ward", "community_area") & series_id == "cycle_first",
      .(
        section = "monthly_support",
        item = paste(series_id, geography_level, c("median_n_keys", "p25_n_keys", "p10_n_keys"), sep = "_"),
        value = c(median(n_keys), quantile(n_keys, 0.25), quantile(n_keys, 0.10))
      ),
      by = .(series_id, geography_level)
    ][, .(section, item, value)]
  ),
  use.names = TRUE,
  fill = TRUE
)

fwrite(citywide_series, citywide_output)
fwrite(benchmark_monthly, benchmark_monthly_output)
fwrite(benchmark_summary, benchmark_summary_output)
fwrite(neighborhood_monthly, neighborhood_monthly_output)
fwrite(period_rows, neighborhood_period_output)
fwrite(coverage_summary, coverage_output)
fwrite(acs_growth_comparison, acs_growth_output)
fwrite(acs_diagnostics, acs_diagnostics_output)
