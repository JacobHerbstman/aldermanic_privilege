source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)
library(readr)
library(ggplot2)
library(sf)
library(zoo)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/compare_renthub_dwellsy_aggregate/code")
# rent_panel_file <- "../input/chicago_rent_panel.parquet"
# raw_dir <- "../input/renthub_raw"
# renthub_audit_file <- "../input/series_variant_benchmark_comparison.csv"
# dwellsy_monthly_file <- "../input/dwellsy_monthly_series_variants.csv"
# dwellsy_benchmark_file <- "../input/dwellsy_benchmark_comparison_real.csv"
# boundary_file <- "../input/chicago_boundary.geojson"
# cpi_file <- "../input/fred_chi_cpi_all_items.csv"
# output_dir <- "../output"
# temp_dir <- "../temp"

args <- commandArgs(trailingOnly = TRUE)
if (interactive() && length(args) == 0) {
  args <- c(
    rent_panel_file,
    raw_dir,
    renthub_audit_file,
    dwellsy_monthly_file,
    dwellsy_benchmark_file,
    boundary_file,
    cpi_file,
    output_dir,
    temp_dir
  )
}

if (length(args) != 9) {
  stop(
    paste(
      "FATAL: Script requires 9 args:",
      "<rent_panel_file>",
      "<raw_dir>",
      "<renthub_audit_file>",
      "<dwellsy_monthly_file>",
      "<dwellsy_benchmark_file>",
      "<boundary_file>",
      "<cpi_file>",
      "<output_dir>",
      "<temp_dir>"
    ),
    call. = FALSE
  )
}

rent_panel_file <- args[1]
raw_dir <- args[2]
renthub_audit_file <- args[3]
dwellsy_monthly_file <- args[4]
dwellsy_benchmark_file <- args[5]
boundary_file <- args[6]
cpi_file <- args[7]
output_dir <- args[8]
temp_dir <- args[9]

primary_start <- as.Date("2021-01-01")
primary_end <- as.Date("2025-11-01")
real_base_year <- 2024L
crs_working <- 3435

sf::sf_use_s2(FALSE)

month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

duck_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

weighted_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0) {
    return(NA_real_)
  }
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cutoff <- 0.5 * sum(w)
  x[which(cumsum(w) >= cutoff)[1]]
}

load_cpi_deflator <- function(cpi_file, start_month, end_month, base_year) {
  raw <- suppressWarnings(read_csv(cpi_file, show_col_types = FALSE, progress = FALSE))
  raw <- raw[, 1:2]
  setDT(raw)
  setnames(raw, names(raw), c("date", "value"))
  raw[, month_start := as.Date(date)]
  raw[, rent_price_cpi_chi_all_items := as.numeric(value)]
  raw <- raw[!is.na(month_start), .(month_start, rent_price_cpi_chi_all_items)]

  month_grid <- data.table(month_start = seq.Date(
    from = month_floor(start_month),
    to = month_floor(end_month),
    by = "month"
  ))
  cpi_dt <- merge(month_grid, raw, by = "month_start", all.x = TRUE, sort = TRUE)

  if (cpi_dt[!is.finite(rent_price_cpi_chi_all_items), .N] > 0) {
    idx_known <- which(is.finite(cpi_dt$rent_price_cpi_chi_all_items))
    if (length(idx_known) < 2) {
      stop("CPI input does not have enough non-missing observations to interpolate.", call. = FALSE)
    }
    interp <- approx(
      x = idx_known,
      y = cpi_dt$rent_price_cpi_chi_all_items[idx_known],
      xout = seq_len(nrow(cpi_dt)),
      method = "linear",
      rule = 2
    )$y
    cpi_dt[, rent_price_cpi_chi_all_items := fifelse(
      is.finite(rent_price_cpi_chi_all_items),
      rent_price_cpi_chi_all_items,
      interp
    )]
  }

  base_cpi <- cpi_dt[format(month_start, "%Y") == as.character(base_year), mean(rent_price_cpi_chi_all_items)]
  if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop(sprintf("Unable to compute the CPI base for %d.", base_year), call. = FALSE)
  }

  cpi_dt[, rent_price_deflator_to_2024 := base_cpi / rent_price_cpi_chi_all_items]
  cpi_dt
}

compute_period_metrics <- function(dt, x_col, y_col, lags = 0L, min_obs = 12L) {
  best_corr <- -Inf
  best_gap <- NA_real_
  best_lag <- NA_integer_
  best_n <- 0L

  for (lag_value in lags) {
    y_shift <- data.table::shift(dt[[y_col]], n = lag_value, type = "lead")
    keep <- is.finite(dt[[x_col]]) & is.finite(y_shift)
    if (sum(keep) < min_obs) {
      next
    }

    corr_value <- suppressWarnings(cor(dt[[x_col]][keep], y_shift[keep]))
    gap_value <- median(abs(dt[[x_col]][keep] - y_shift[keep]), na.rm = TRUE)

    if (!is.finite(corr_value)) {
      next
    }

    if (!is.finite(best_corr) || corr_value > best_corr) {
      best_corr <- corr_value
      best_gap <- gap_value
      best_lag <- lag_value
      best_n <- sum(keep)
    }
  }

  data.table(
    n_pairs = if (best_n > 0) best_n else NA_integer_,
    best_corr = if (is.finite(best_corr)) best_corr else NA_real_,
    best_lag = best_lag,
    median_abs_gap_pp = best_gap
  )
}

collect_query <- function(con, sql) {
  as.data.table(dbGetQuery(con, sql))
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

collect_monthly_series <- function(con, table_name, weight_col) {
  collect_query(
    con,
    sprintf(
      "
      SELECT
        month_start,
        SUM(%s * rent_price) / SUM(%s) AS series_value_nominal,
        SUM(%s * rent_price_real_2024) / SUM(%s) AS series_value_real_2024,
        QUANTILE_CONT(rent_price, 0.50) AS median_rent,
        QUANTILE_CONT(rent_price_real_2024, 0.50) AS median_rent_real_2024,
        COUNT(*) AS n_obs,
        COUNT(DISTINCT analysis_key) AS n_distinct_keys,
        AVG(CASE WHEN key_source = 'unit_id' THEN 1.0 ELSE 0.0 END) AS share_unit_id_obs,
        AVG(CASE WHEN key_source = 'fingerprint' THEN 1.0 ELSE 0.0 END) AS share_fingerprint_obs,
        SUM(%s) AS weight_sum
      FROM %s
      GROUP BY 1
      ORDER BY 1
      ",
      weight_col,
      weight_col,
      weight_col,
      weight_col,
      weight_col,
      table_name
    )
  )
}

message("Loading CPI deflator and Chicago boundary...")
cpi_dt <- load_cpi_deflator(
  cpi_file = cpi_file,
  start_month = as.Date("2014-01-01"),
  end_month = as.Date("2026-12-01"),
  base_year = real_base_year
)

boundary_sf <- st_read(boundary_file, quiet = TRUE)
boundary_sf <- st_make_valid(boundary_sf)
boundary_sf <- st_transform(boundary_sf, crs_working)
boundary_union <- st_union(boundary_sf)

message("Opening DuckDB and reading Renthub inputs...")
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, sprintf("PRAGMA temp_directory='%s'", duck_escape(temp_dir)))
dbWriteTable(con, "cpi_deflator", cpi_dt, temporary = TRUE, overwrite = TRUE)

rent_panel_summary <- collect_query(
  con,
  sprintf(
    paste(
      "SELECT",
      "  COUNT(*) AS rent_panel_rows,",
      "  COUNT(DISTINCT id) AS rent_panel_distinct_ids,",
      "  MIN(CAST(file_date AS DATE)) AS rent_panel_first_file_date,",
      "  MAX(CAST(file_date AS DATE)) AS rent_panel_last_file_date",
      "FROM read_parquet('%s')"
    ),
    duck_escape(rent_panel_file)
  )
)

raw_files <- list.files(raw_dir, pattern = "\\.parquet$", full.names = TRUE)
if (length(raw_files) == 0) {
  stop(sprintf("No raw Renthub parquet shards found in %s.", raw_dir), call. = FALSE)
}

raw_glob <- file.path(raw_dir, "*.parquet")
view_sql <- sprintf(
  "
  CREATE OR REPLACE TEMP VIEW chicago_raw AS
  SELECT
    CASE
      WHEN UPPER(TRIM(CAST(ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(ID AS VARCHAR)))
    END AS id,
    CASE
      WHEN UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR)))
    END AS property_id,
    CASE
      WHEN UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(UNIT_ID AS VARCHAR)))
    END AS unit_id,
    CASE
      WHEN UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(ADDRESS AS VARCHAR))), ' +', ' '), '')
    END AS address_norm,
    CASE
      WHEN UPPER(TRIM(CAST(COMPANY AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(COMPANY AS VARCHAR))), ' +', ' '), '')
    END AS company_norm,
    CAST(SCRAPED_TIMESTAMP AS TIMESTAMP) AS scraped_timestamp,
    CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
    CAST(DATE_POSTED AS DATE) AS posted_date,
    CAST(AVAILABLE_AT AS DATE) AS available_date,
    CAST(DATE_TRUNC('month', CAST(SCRAPED_TIMESTAMP AS DATE)) AS DATE) AS month_start,
    CAST(RENT_PRICE AS DOUBLE) AS rent_price,
    CAST(BEDS AS DOUBLE) AS beds,
    CAST(BATHS AS DOUBLE) AS baths,
    CAST(SQFT AS DOUBLE) AS sqft,
    CAST(LATITUDE AS DOUBLE) AS latitude,
    CAST(LONGITUDE AS DOUBLE) AS longitude,
    CAST(AVAILABILITY_STATUS AS VARCHAR) AS availability_status,
    CASE
      WHEN UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDO%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDOMINIUM%%' THEN 'condo'
      WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%MULTI%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APART%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DUPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TRIPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%FOURPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APT%%' THEN 'multi_family'
      WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SINGLE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%HOUSE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DETACHED%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SFR%%' THEN 'single_family'
      WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TOWN%%' OR UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) = 'TH' THEN 'townhouse'
      ELSE 'other'
    END AS building_type_clean,
    CASE
      WHEN LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL THEN CAST(ROUND(CAST(LATITUDE AS DOUBLE), 4) AS VARCHAR) || '|' || CAST(ROUND(CAST(LONGITUDE AS DOUBLE), 4) AS VARCHAR)
      ELSE NULL
    END AS coord4,
    CASE
      WHEN UNIT_ID IS NULL OR UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN
        NULLIF(
          COALESCE(NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(ADDRESS AS VARCHAR))), ' +', ' '), ''), '') || '|' ||
          COALESCE(CASE WHEN LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL THEN CAST(ROUND(CAST(LATITUDE AS DOUBLE), 4) AS VARCHAR) || '|' || CAST(ROUND(CAST(LONGITUDE AS DOUBLE), 4) AS VARCHAR) END, '') || '|' ||
          COALESCE(CAST(CAST(BEDS AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(CAST(CAST(BATHS AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(CAST(CAST(SQFT AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(
            CASE
              WHEN UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
              WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDO%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDOMINIUM%%' THEN 'condo'
              WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%MULTI%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APART%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DUPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TRIPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%FOURPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APT%%' THEN 'multi_family'
              WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SINGLE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%HOUSE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DETACHED%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SFR%%' THEN 'single_family'
              WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TOWN%%' OR UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) = 'TH' THEN 'townhouse'
              ELSE 'other'
            END,
            ''
          ),
          '|||||'
        )
      ELSE NULL
    END AS fingerprint_key
  FROM read_parquet('%s', union_by_name = true)
  WHERE UPPER(TRIM(CAST(CITY AS VARCHAR))) = 'CHICAGO'
    AND SCRAPED_TIMESTAMP IS NOT NULL
  ",
  duck_escape(raw_glob)
)
dbExecute(con, view_sql)

raw_summary <- collect_query(
  con,
  "
  SELECT
    COUNT(*) AS chicago_raw_rows,
    COUNT(DISTINCT id) AS chicago_raw_distinct_ids,
    COUNT(DISTINCT unit_id) FILTER (WHERE unit_id IS NOT NULL) AS chicago_raw_distinct_unit_ids,
    MIN(file_date) AS chicago_raw_first_file_date,
    MAX(file_date) AS chicago_raw_last_file_date
  FROM chicago_raw
  "
)

message("Building Renthub clean key-day baseline...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE series_day_base AS
  WITH keyed AS (
    SELECT
      month_start,
      file_date,
      CAST(YEAR(file_date) AS INTEGER) AS year,
      unit_id,
      fingerprint_key,
      CASE
        WHEN unit_id IS NOT NULL THEN unit_id
        WHEN fingerprint_key IS NOT NULL THEN fingerprint_key
        ELSE NULL
      END AS analysis_key,
      CASE
        WHEN unit_id IS NOT NULL THEN 'unit_id'
        WHEN fingerprint_key IS NOT NULL THEN 'fingerprint'
        ELSE 'unkeyed'
      END AS key_source,
      id,
      property_id,
      rent_price,
      beds,
      baths,
      sqft,
      latitude,
      longitude,
      building_type_clean,
      company_norm,
      address_norm,
      scraped_timestamp
    FROM chicago_raw
    WHERE rent_price > 0
  ),
  day_collapsed AS (
    SELECT
      month_start,
      file_date,
      year,
      analysis_key,
      key_source,
      MIN(unit_id) AS unit_id,
      MIN(fingerprint_key) AS fingerprint_key,
      MIN(property_id) AS property_id,
      MIN(building_type_clean) AS building_type_clean,
      MIN(company_norm) AS company_norm,
      MIN(address_norm) AS address_norm,
      COUNT(*) AS n_rows_day,
      COUNT(DISTINCT id) AS n_ids_day,
      COUNT(DISTINCT rent_price) AS n_rents_day,
      QUANTILE_CONT(rent_price, 0.50) AS rent_price,
      QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
      QUANTILE_CONT(baths, 0.50) FILTER (WHERE baths IS NOT NULL) AS baths,
      QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
      QUANTILE_CONT(latitude, 0.50) FILTER (WHERE latitude IS NOT NULL) AS latitude,
      QUANTILE_CONT(longitude, 0.50) FILTER (WHERE longitude IS NOT NULL) AS longitude,
      MAX(scraped_timestamp) AS last_scraped_timestamp
    FROM keyed
    WHERE analysis_key IS NOT NULL
    GROUP BY 1, 2, 3, 4, 5
  ),
  cell_thresholds AS (
    SELECT
      year,
      COALESCE(CAST(ROUND(beds) AS INTEGER), -1) AS bed_cell,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
      QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
      QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
      QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99
    FROM day_collapsed
    WHERE n_rents_day = 1
    GROUP BY 1, 2
  ),
  year_thresholds AS (
    SELECT
      year,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
      QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
      QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
      QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99
    FROM day_collapsed
    WHERE n_rents_day = 1
    GROUP BY 1
  )
  SELECT
    d.*,
    COALESCE(CAST(ROUND(d.beds) AS INTEGER), -1) AS bed_cell,
    CASE
      WHEN d.latitude IS NOT NULL AND d.longitude IS NOT NULL THEN CAST(ROUND(d.latitude, 8) AS VARCHAR) || '|' || CAST(ROUND(d.longitude, 8) AS VARCHAR)
      ELSE NULL
    END AS coord_key,
    CASE WHEN d.n_rents_day = 1 THEN 1 ELSE 0 END AS keep_same_day,
    CASE
      WHEN d.rent_price >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p01 END, y.rent_p01)
       AND d.rent_price <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p99 END, y.rent_p99) THEN 1 ELSE 0
    END AS keep_rent_trim01,
    CASE
      WHEN d.sqft IS NULL OR d.sqft <= 0 THEN 1
      WHEN d.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01)
       AND d.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) THEN 1 ELSE 0
    END AS keep_sqft_trim01
  FROM day_collapsed d
  LEFT JOIN cell_thresholds c
    ON d.year = c.year
   AND COALESCE(CAST(ROUND(d.beds) AS INTEGER), -1) = c.bed_cell
  LEFT JOIN year_thresholds y
    ON d.year = y.year
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE series_day_final AS
  WITH base AS (
    SELECT *
    FROM series_day_base
    WHERE keep_same_day = 1
  ),
  counts AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY analysis_key, year) AS key_year_days,
      LAG(file_date) OVER (PARTITION BY analysis_key ORDER BY file_date, last_scraped_timestamp, rent_price) AS prev_file_date
    FROM base
  ),
  cycles AS (
    SELECT
      *,
      DATE_DIFF('day', prev_file_date, file_date) AS gap_days,
      CASE
        WHEN prev_file_date IS NULL OR DATE_DIFF('day', prev_file_date, file_date) >= 90 THEN 1 ELSE 0
      END AS cycle_start_flag,
      SUM(
        CASE
          WHEN prev_file_date IS NULL OR DATE_DIFF('day', prev_file_date, file_date) >= 90 THEN 1 ELSE 0
        END
      ) OVER (
        PARTITION BY analysis_key
        ORDER BY file_date, last_scraped_timestamp, rent_price
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
      ) AS cycle_id
    FROM counts
  )
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY analysis_key, cycle_id
      ORDER BY file_date, last_scraped_timestamp, rent_price
    ) AS cycle_row_number
  FROM cycles
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE baseline_rows AS
  SELECT
    s.month_start,
    s.file_date,
    s.year,
    s.analysis_key,
    s.key_source,
    s.unit_id,
    s.fingerprint_key,
    s.property_id,
    s.building_type_clean,
    s.company_norm,
    s.address_norm,
    s.latitude,
    s.longitude,
    s.coord_key,
    s.rent_price,
    s.beds,
    s.baths,
    s.sqft,
    CASE
      WHEN s.sqft > 0 THEN s.rent_price / s.sqft
      ELSE NULL
    END AS rent_per_sqft,
    s.n_rows_day,
    s.n_ids_day,
    s.n_rents_day,
    s.key_year_days,
    1.0 / s.key_year_days AS inv_key_year_weight,
    c.rent_price_cpi_chi_all_items,
    c.rent_price_deflator_to_2024,
    s.rent_price * c.rent_price_deflator_to_2024 AS rent_price_real_2024
  FROM series_day_final s
  LEFT JOIN cpi_deflator c
    USING (month_start)
  WHERE s.keep_rent_trim01 = 1
    AND s.keep_sqft_trim01 = 1
  "
)

message("Assigning baseline coordinates to the Chicago boundary...")
coord_dt <- collect_query(
  con,
  "
  SELECT DISTINCT
    coord_key,
    latitude,
    longitude
  FROM baseline_rows
  "
)
coord_dt[, valid_coordinates := as.integer(
  is.finite(latitude) &
    is.finite(longitude) &
    latitude != 0 &
    longitude != 0
)]
coord_dt[, inside_broad_chicago_bbox := as.integer(
  valid_coordinates == 1L &
    latitude >= 41 &
    latitude <= 43 &
    longitude >= -89 &
    longitude <= -87
)]
coord_dt[, inside_chicago_boundary := 0L]

coords_to_map <- coord_dt[inside_broad_chicago_bbox == 1L & !is.na(coord_key)]
if (nrow(coords_to_map) > 0) {
  points_sf <- st_as_sf(coords_to_map, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  points_sf <- st_transform(points_sf, crs_working)
  coords_to_map[, inside_chicago_boundary := as.integer(lengths(st_within(points_sf, boundary_union)) > 0L)]
  coord_dt[coords_to_map, on = "coord_key", inside_chicago_boundary := i.inside_chicago_boundary]
}

dbWriteTable(
  con,
  "coord_flags",
  coord_dt[, .(
    coord_key,
    valid_coordinates,
    inside_broad_chicago_bbox,
    inside_chicago_boundary
  )],
  temporary = TRUE,
  overwrite = TRUE
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_pre AS
  SELECT
    b.*,
    COALESCE(f.valid_coordinates, 0) AS valid_coordinates,
    COALESCE(f.inside_broad_chicago_bbox, 0) AS inside_broad_chicago_bbox,
    COALESCE(f.inside_chicago_boundary, 0) AS inside_chicago_boundary
  FROM baseline_rows b
  LEFT JOIN coord_flags f
    USING (coord_key)
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_sqft_flagged AS
  WITH base AS (
    SELECT *
    FROM strict_pre
    WHERE valid_coordinates = 1
      AND inside_broad_chicago_bbox = 1
      AND inside_chicago_boundary = 1
      AND sqft > 0
      AND rent_per_sqft IS NOT NULL
  ),
  cell_thresholds AS (
    SELECT
      year,
      COALESCE(CAST(ROUND(beds) AS INTEGER), -1) AS bed_cell,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(sqft, 0.01) AS sqft_p01,
      QUANTILE_CONT(sqft, 0.99) AS sqft_p99,
      QUANTILE_CONT(sqft, 0.025) AS sqft_p025,
      QUANTILE_CONT(sqft, 0.975) AS sqft_p975,
      QUANTILE_CONT(sqft, 0.05) AS sqft_p05,
      QUANTILE_CONT(sqft, 0.95) AS sqft_p95
    FROM base
    GROUP BY 1, 2
  ),
  year_thresholds AS (
    SELECT
      year,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(sqft, 0.01) AS sqft_p01,
      QUANTILE_CONT(sqft, 0.99) AS sqft_p99,
      QUANTILE_CONT(sqft, 0.025) AS sqft_p025,
      QUANTILE_CONT(sqft, 0.975) AS sqft_p975,
      QUANTILE_CONT(sqft, 0.05) AS sqft_p05,
      QUANTILE_CONT(sqft, 0.95) AS sqft_p95
    FROM base
    GROUP BY 1
  )
  SELECT
    b.*,
    COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) AS bed_cell,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01) AS sqft_lo_p1,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) AS sqft_hi_p1,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p025 END, y.sqft_p025) AS sqft_lo_p25,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p975 END, y.sqft_p975) AS sqft_hi_p25,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p05 END, y.sqft_p05) AS sqft_lo_p5,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p95 END, y.sqft_p95) AS sqft_hi_p5,
    CASE
      WHEN b.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01)
       AND b.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) THEN 1 ELSE 0
    END AS keep_sqft_trim_p1,
    CASE
      WHEN b.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p025 END, y.sqft_p025)
       AND b.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p975 END, y.sqft_p975) THEN 1 ELSE 0
    END AS keep_sqft_trim_p25,
    CASE
      WHEN b.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p05 END, y.sqft_p05)
       AND b.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p95 END, y.sqft_p95) THEN 1 ELSE 0
    END AS keep_sqft_trim_p5
  FROM base b
  LEFT JOIN cell_thresholds c
    ON b.year = c.year
   AND COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) = c.bed_cell
  LEFT JOIN year_thresholds y
    ON b.year = y.year
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rpsf_flagged_p1 AS
  WITH base AS (
    SELECT *
    FROM strict_sqft_flagged
    WHERE keep_sqft_trim_p1 = 1
  ),
  cell_thresholds AS (
    SELECT
      year,
      COALESCE(CAST(ROUND(beds) AS INTEGER), -1) AS bed_cell,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.01) AS rpsf_p01,
      QUANTILE_CONT(rent_per_sqft, 0.99) AS rpsf_p99
    FROM base
    GROUP BY 1, 2
  ),
  year_thresholds AS (
    SELECT
      year,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.01) AS rpsf_p01,
      QUANTILE_CONT(rent_per_sqft, 0.99) AS rpsf_p99
    FROM base
    GROUP BY 1
  )
  SELECT
    b.*,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p01 END, y.rpsf_p01) AS rpsf_lo_p1,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p99 END, y.rpsf_p99) AS rpsf_hi_p1,
    CASE
      WHEN b.rent_per_sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p01 END, y.rpsf_p01)
       AND b.rent_per_sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p99 END, y.rpsf_p99) THEN 1 ELSE 0
    END AS keep_rent_per_sqft_trim_p1
  FROM base b
  LEFT JOIN cell_thresholds c
    ON b.year = c.year
   AND COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) = c.bed_cell
  LEFT JOIN year_thresholds y
    ON b.year = y.year
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rpsf_flagged_p25 AS
  WITH base AS (
    SELECT *
    FROM strict_sqft_flagged
    WHERE keep_sqft_trim_p25 = 1
  ),
  cell_thresholds AS (
    SELECT
      year,
      COALESCE(CAST(ROUND(beds) AS INTEGER), -1) AS bed_cell,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.025) AS rpsf_p025,
      QUANTILE_CONT(rent_per_sqft, 0.975) AS rpsf_p975
    FROM base
    GROUP BY 1, 2
  ),
  year_thresholds AS (
    SELECT
      year,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.025) AS rpsf_p025,
      QUANTILE_CONT(rent_per_sqft, 0.975) AS rpsf_p975
    FROM base
    GROUP BY 1
  )
  SELECT
    b.*,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p025 END, y.rpsf_p025) AS rpsf_lo_p25,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p975 END, y.rpsf_p975) AS rpsf_hi_p25,
    CASE
      WHEN b.rent_per_sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p025 END, y.rpsf_p025)
       AND b.rent_per_sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p975 END, y.rpsf_p975) THEN 1 ELSE 0
    END AS keep_rent_per_sqft_trim_p25
  FROM base b
  LEFT JOIN cell_thresholds c
    ON b.year = c.year
   AND COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) = c.bed_cell
  LEFT JOIN year_thresholds y
    ON b.year = y.year
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rpsf_flagged_p5 AS
  WITH base AS (
    SELECT *
    FROM strict_sqft_flagged
    WHERE keep_sqft_trim_p5 = 1
  ),
  cell_thresholds AS (
    SELECT
      year,
      COALESCE(CAST(ROUND(beds) AS INTEGER), -1) AS bed_cell,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.05) AS rpsf_p05,
      QUANTILE_CONT(rent_per_sqft, 0.95) AS rpsf_p95
    FROM base
    GROUP BY 1, 2
  ),
  year_thresholds AS (
    SELECT
      year,
      COUNT(*) AS n_obs,
      QUANTILE_CONT(rent_per_sqft, 0.05) AS rpsf_p05,
      QUANTILE_CONT(rent_per_sqft, 0.95) AS rpsf_p95
    FROM base
    GROUP BY 1
  )
  SELECT
    b.*,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p05 END, y.rpsf_p05) AS rpsf_lo_p5,
    COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p95 END, y.rpsf_p95) AS rpsf_hi_p5,
    CASE
      WHEN b.rent_per_sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p05 END, y.rpsf_p05)
       AND b.rent_per_sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p95 END, y.rpsf_p95) THEN 1 ELSE 0
    END AS keep_rent_per_sqft_trim_p5
  FROM base b
  LEFT JOIN cell_thresholds c
    ON b.year = c.year
   AND COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) = c.bed_cell
  LEFT JOIN year_thresholds y
    ON b.year = y.year
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rows_p1 AS
  WITH kept AS (
    SELECT *
    FROM strict_rpsf_flagged_p1
    WHERE keep_rent_per_sqft_trim_p1 = 1
  ),
  counted AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY analysis_key, year) AS strict_key_year_days
    FROM kept
  )
  SELECT
    *,
    1.0 / strict_key_year_days AS strict_inv_key_year_weight,
    'strict_p1' AS trim_spec
  FROM counted
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rows_p25 AS
  WITH kept AS (
    SELECT *
    FROM strict_rpsf_flagged_p25
    WHERE keep_rent_per_sqft_trim_p25 = 1
  ),
  counted AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY analysis_key, year) AS strict_key_year_days
    FROM kept
  )
  SELECT
    *,
    1.0 / strict_key_year_days AS strict_inv_key_year_weight,
    'strict_p25' AS trim_spec
  FROM counted
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE strict_rows_p5 AS
  WITH kept AS (
    SELECT *
    FROM strict_rpsf_flagged_p5
    WHERE keep_rent_per_sqft_trim_p5 = 1
  ),
  counted AS (
    SELECT
      *,
      COUNT(*) OVER (PARTITION BY analysis_key, year) AS strict_key_year_days
    FROM kept
  )
  SELECT
    *,
    1.0 / strict_key_year_days AS strict_inv_key_year_weight,
    'strict_p5' AS trim_spec
  FROM counted
  "
)

strict_candidate_summary <- rbindlist(list(
  cbind(
    data.table(trim_spec = "strict_p1"),
    collect_query(
      con,
      "
      SELECT
        COUNT(*) AS rows_kept,
        COUNT(DISTINCT analysis_key) AS distinct_keys_kept,
        MAX(sqft) AS max_surviving_sqft,
        MAX(rent_per_sqft) AS max_surviving_rent_per_sqft
      FROM strict_rows_p1
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(sqft_lo_p1) AS sqft_cutoff_lo_min,
        MAX(sqft_hi_p1) AS sqft_cutoff_hi_max
      FROM strict_sqft_flagged
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(rpsf_lo_p1) AS rent_per_sqft_cutoff_lo_min,
        MAX(rpsf_hi_p1) AS rent_per_sqft_cutoff_hi_max
      FROM strict_rpsf_flagged_p1
      "
    )
  ),
  cbind(
    data.table(trim_spec = "strict_p25"),
    collect_query(
      con,
      "
      SELECT
        COUNT(*) AS rows_kept,
        COUNT(DISTINCT analysis_key) AS distinct_keys_kept,
        MAX(sqft) AS max_surviving_sqft,
        MAX(rent_per_sqft) AS max_surviving_rent_per_sqft
      FROM strict_rows_p25
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(sqft_lo_p25) AS sqft_cutoff_lo_min,
        MAX(sqft_hi_p25) AS sqft_cutoff_hi_max
      FROM strict_sqft_flagged
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(rpsf_lo_p25) AS rent_per_sqft_cutoff_lo_min,
        MAX(rpsf_hi_p25) AS rent_per_sqft_cutoff_hi_max
      FROM strict_rpsf_flagged_p25
      "
    )
  ),
  cbind(
    data.table(trim_spec = "strict_p5"),
    collect_query(
      con,
      "
      SELECT
        COUNT(*) AS rows_kept,
        COUNT(DISTINCT analysis_key) AS distinct_keys_kept,
        MAX(sqft) AS max_surviving_sqft,
        MAX(rent_per_sqft) AS max_surviving_rent_per_sqft
      FROM strict_rows_p5
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(sqft_lo_p5) AS sqft_cutoff_lo_min,
        MAX(sqft_hi_p5) AS sqft_cutoff_hi_max
      FROM strict_sqft_flagged
      "
    ),
    collect_query(
      con,
      "
      SELECT
        MIN(rpsf_lo_p5) AS rent_per_sqft_cutoff_lo_min,
        MAX(rpsf_hi_p5) AS rent_per_sqft_cutoff_hi_max
      FROM strict_rpsf_flagged_p5
      "
    )
  )
), fill = TRUE)
strict_candidate_summary[, passes_plausibility_gates := is.finite(max_surviving_sqft) &
  max_surviving_sqft <= 5000 &
  is.finite(max_surviving_rent_per_sqft) &
  max_surviving_rent_per_sqft <= 5.5]
chosen_trim_spec <- if (isTRUE(strict_candidate_summary[trim_spec == "strict_p1", passes_plausibility_gates][1])) {
  "strict_p1"
} else if (isTRUE(strict_candidate_summary[trim_spec == "strict_p25", passes_plausibility_gates][1])) {
  "strict_p25"
} else {
  "strict_p5"
}
strict_candidate_summary[, `:=`(
  chosen_trim_spec = chosen_trim_spec,
  is_chosen = trim_spec == chosen_trim_spec
)]

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE strict_rows AS
    SELECT
      *,
      '%s' AS chosen_trim_spec
    FROM %s
    ",
    chosen_trim_spec,
    switch(
      chosen_trim_spec,
      strict_p1 = "strict_rows_p1",
      strict_p25 = "strict_rows_p25",
      strict_p5 = "strict_rows_p5"
    )
  )
)

message("Writing Renthub row-level series files...")
dbExecute(
  con,
  sprintf(
    "COPY baseline_rows TO '%s' (FORMAT PARQUET)",
    duck_escape(file.path(output_dir, "renthub_citywide_series_baseline.parquet"))
  )
)
dbExecute(
  con,
  sprintf(
    "COPY strict_rows TO '%s' (FORMAT PARQUET)",
    duck_escape(file.path(output_dir, "renthub_citywide_series_strict.parquet"))
  )
)

message("Aggregating Renthub monthly series...")
baseline_monthly <- collect_monthly_series(con, "baseline_rows", "inv_key_year_weight")
baseline_monthly[, month_start := as.Date(month_start)]
baseline_monthly[, `:=`(
  series_id = "renthub_baseline_guangbin_citywide",
  series_label = "Renthub Baseline Guangbin-Style",
  source_family = "renthub"
)]

strict_p1_monthly <- collect_monthly_series(con, "strict_rows_p1", "strict_inv_key_year_weight")
strict_p1_monthly[, month_start := as.Date(month_start)]
strict_p1_monthly[, trim_spec := "strict_p1"]

strict_p25_monthly <- collect_monthly_series(con, "strict_rows_p25", "strict_inv_key_year_weight")
strict_p25_monthly[, month_start := as.Date(month_start)]
strict_p25_monthly[, trim_spec := "strict_p25"]

strict_p5_monthly <- collect_monthly_series(con, "strict_rows_p5", "strict_inv_key_year_weight")
strict_p5_monthly[, month_start := as.Date(month_start)]
strict_p5_monthly[, trim_spec := "strict_p5"]

strict_monthly <- collect_monthly_series(con, "strict_rows", "strict_inv_key_year_weight")
strict_monthly[, month_start := as.Date(month_start)]
strict_monthly[, `:=`(
  series_id = "renthub_strict_citywide",
  series_label = "Renthub Strict Citywide",
  source_family = "renthub"
)]

message("Computing the Renthub 2024 building-type mix sensitivity...")
btstd_core <- collect_query(
  con,
  "
  SELECT
    month_start,
    building_type_clean,
    rent_price,
    rent_price_real_2024,
    inv_key_year_weight
  FROM baseline_rows
  "
)
btstd_core[, month_start := as.Date(month_start)]

type_ref <- btstd_core[
  month_start >= as.Date("2024-01-01") & month_start <= as.Date("2024-12-01"),
  .(ref_weight = sum(inv_key_year_weight, na.rm = TRUE)),
  by = building_type_clean
]
type_ref <- type_ref[is.finite(ref_weight) & ref_weight > 0]
if (nrow(type_ref) == 0) {
  stop("Renthub baseline has no valid 2024 building-type weights.", call. = FALSE)
}
type_ref[, ref_weight := ref_weight / sum(ref_weight)]

month_type_weight <- btstd_core[
  ,
  .(month_type_weight = sum(inv_key_year_weight, na.rm = TRUE)),
  by = .(month_start, building_type_clean)
]
btstd_core <- merge(btstd_core, month_type_weight, by = c("month_start", "building_type_clean"), all.x = TRUE, sort = FALSE)
btstd_core <- merge(btstd_core, type_ref, by = "building_type_clean", all.x = TRUE, sort = FALSE)
btstd_core <- btstd_core[is.finite(ref_weight) & is.finite(month_type_weight) & month_type_weight > 0]
btstd_core[, std_weight := inv_key_year_weight * ref_weight / month_type_weight]

btstd_coverage <- unique(btstd_core[, .(month_start, building_type_clean, ref_weight)])
btstd_coverage <- btstd_coverage[, .(type_coverage_share = sum(ref_weight, na.rm = TRUE)), by = month_start]

btstd_monthly <- btstd_core[
  ,
  .(
    series_value_nominal = weighted.mean(rent_price, std_weight, na.rm = TRUE),
    series_value_real_2024 = weighted.mean(rent_price_real_2024, std_weight, na.rm = TRUE),
    median_rent = weighted_median(rent_price, std_weight),
    median_rent_real_2024 = weighted_median(rent_price_real_2024, std_weight),
    weight_sum = sum(std_weight, na.rm = TRUE)
  ),
  by = month_start
]
btstd_monthly <- merge(btstd_monthly, btstd_coverage, by = "month_start", all.x = TRUE, sort = TRUE)
btstd_monthly <- merge(
  btstd_monthly,
  baseline_monthly[, .(
    month_start,
    n_obs,
    n_distinct_keys,
    share_unit_id_obs,
    share_fingerprint_obs
  )],
  by = "month_start",
  all.x = TRUE,
  sort = TRUE
)
btstd_monthly[type_coverage_share < 0.95, `:=`(
  series_value_nominal = NA_real_,
  series_value_real_2024 = NA_real_,
  median_rent = NA_real_,
  median_rent_real_2024 = NA_real_
)]
btstd_monthly[, `:=`(
  series_id = "renthub_baseline_btstd_2024mix",
  series_label = "Renthub Baseline Building-Type Standardized",
  source_family = "renthub"
)]
btstd_monthly[, type_coverage_share := NULL]

message("Building diagnostics and validation tables...")
drop_metrics <- c(
  "same_day_clean_rows",
  "rent_tail_trim_1_99",
  "baseline_sample_kept",
  "strict_eligible_rows",
  "invalid_coordinates",
  "outside_broad_chicago_bbox",
  "outside_chicago_boundary",
  "nonpositive_sqft",
  "sqft_tail_trim_1_99",
  "rent_per_sqft_tail_trim_1_99",
  "strict_p1_kept",
  "sqft_tail_trim_2_5_97_5",
  "rent_per_sqft_tail_trim_2_5_97_5",
  "strict_p25_kept",
  "sqft_tail_trim_5_95",
  "rent_per_sqft_tail_trim_5_95",
  "strict_p5_kept",
  "strict_sample_kept"
)

drop_slices <- list(
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM series_day_final GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM series_day_final WHERE keep_rent_trim01 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM baseline_rows GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_sqft_flagged GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 1 AND inside_chicago_boundary = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 1 AND inside_chicago_boundary = 1 AND (sqft IS NULL OR sqft <= 0) GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p1 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rpsf_flagged_p1 WHERE keep_rent_per_sqft_trim_p1 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rows_p1 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p25 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rpsf_flagged_p25 WHERE keep_rent_per_sqft_trim_p25 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rows_p25 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p5 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rpsf_flagged_p5 WHERE keep_rent_per_sqft_trim_p5 = 0 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rows_p5 GROUP BY 1 ORDER BY 1"),
  collect_query(con, "SELECT month_start, COUNT(*) AS n_rows FROM strict_rows GROUP BY 1 ORDER BY 1")
)

drop_diagnostics <- rbindlist(lapply(seq_along(drop_metrics), function(i) {
  dt <- copy(drop_slices[[i]])
  dt[, month_start := as.Date(month_start)]
  dt[, `:=`(scope = "monthly", metric = drop_metrics[i])]
  dt
}), fill = TRUE)

overall_queries <- list(
  "SELECT COUNT(*) AS n_rows FROM series_day_final",
  "SELECT COUNT(*) AS n_rows FROM series_day_final WHERE keep_rent_trim01 = 0",
  "SELECT COUNT(*) AS n_rows FROM baseline_rows",
  "SELECT COUNT(*) AS n_rows FROM strict_sqft_flagged",
  "SELECT COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 1 AND inside_chicago_boundary = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_pre WHERE valid_coordinates = 1 AND inside_broad_chicago_bbox = 1 AND inside_chicago_boundary = 1 AND (sqft IS NULL OR sqft <= 0)",
  "SELECT COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p1 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rpsf_flagged_p1 WHERE keep_rent_per_sqft_trim_p1 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rows_p1",
  "SELECT COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p25 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rpsf_flagged_p25 WHERE keep_rent_per_sqft_trim_p25 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rows_p25",
  "SELECT COUNT(*) AS n_rows FROM strict_sqft_flagged WHERE keep_sqft_trim_p5 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rpsf_flagged_p5 WHERE keep_rent_per_sqft_trim_p5 = 0",
  "SELECT COUNT(*) AS n_rows FROM strict_rows_p5",
  "SELECT COUNT(*) AS n_rows FROM strict_rows"
)
drop_overall <- rbindlist(lapply(seq_along(drop_metrics), function(i) {
  dt <- collect_query(con, overall_queries[[i]])
  dt[, `:=`(
    month_start = as.Date(NA),
    scope = "overall",
    metric = drop_metrics[i]
  )]
  dt
}), fill = TRUE)
drop_diagnostics <- rbindlist(list(drop_diagnostics, drop_overall), fill = TRUE)
setcolorder(drop_diagnostics, c("scope", "month_start", "metric", "n_rows"))
fwrite(drop_diagnostics, file.path(output_dir, "renthub_citywide_drop_diagnostics.csv"))

audit_baseline <- suppressWarnings(fread(renthub_audit_file))
audit_baseline <- audit_baseline[
  variant_id == "clean_day_fullkey_invyear_trim01_sqft01",
  .(month_start = as.Date(month_start), audit_series_median_rent = series_median_rent, audit_series_yoy_pct = series_yoy_pct)
]

baseline_validation <- data.table(month_start = baseline_monthly$month_start)
baseline_validation <- merge(baseline_validation, baseline_monthly[, .(month_start, median_rent, series_value_nominal)], by = "month_start", all.x = TRUE, sort = TRUE)
baseline_validation[, series_value_nominal_sm3 := zoo::rollapplyr(series_value_nominal, 3, mean, fill = NA_real_, partial = FALSE)]
baseline_validation[, series_value_nominal_yoy_pct := 100 * (series_value_nominal_sm3 / shift(series_value_nominal_sm3, 12) - 1)]
baseline_validation <- merge(baseline_validation, audit_baseline, by = "month_start", all.x = TRUE, sort = TRUE)
baseline_validation[, median_diff := abs(median_rent - audit_series_median_rent)]
baseline_validation[, yoy_diff := abs(series_value_nominal_yoy_pct - audit_series_yoy_pct)]

baseline_month_counts <- merge(
  baseline_monthly[, .(month_start, baseline_n_obs = n_obs)],
  strict_monthly[, .(month_start, strict_n_obs = n_obs)],
  by = "month_start",
  all = TRUE,
  sort = TRUE
)

cpi_primary_missing <- cpi_dt[
  month_start >= primary_start & month_start <= primary_end & !is.finite(rent_price_deflator_to_2024),
  .N
]

strict_extremes <- collect_query(
  con,
  "
  SELECT
    MIN(rent_price) AS strict_min_rent,
    MAX(rent_price) AS strict_max_rent,
    MIN(sqft) AS strict_min_sqft,
    MAX(sqft) AS strict_max_sqft,
    MIN(rent_per_sqft) AS strict_min_rent_per_sqft,
    MAX(rent_per_sqft) AS strict_max_rent_per_sqft
  FROM strict_rows
  "
)

cleaning_summary <- rbindlist(list(
  data.table(metric = "rent_panel_rows", value = as.character(rent_panel_summary$rent_panel_rows[1])),
  data.table(metric = "rent_panel_distinct_ids", value = as.character(rent_panel_summary$rent_panel_distinct_ids[1])),
  data.table(metric = "rent_panel_first_file_date", value = as.character(rent_panel_summary$rent_panel_first_file_date[1])),
  data.table(metric = "rent_panel_last_file_date", value = as.character(rent_panel_summary$rent_panel_last_file_date[1])),
  data.table(metric = "chicago_raw_rows", value = as.character(raw_summary$chicago_raw_rows[1])),
  data.table(metric = "chicago_raw_distinct_ids", value = as.character(raw_summary$chicago_raw_distinct_ids[1])),
  data.table(metric = "chicago_raw_distinct_unit_ids", value = as.character(raw_summary$chicago_raw_distinct_unit_ids[1])),
  data.table(metric = "clean_key_day_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM series_day_final")$n_rows[1])),
  data.table(metric = "baseline_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM baseline_rows")$n_rows[1])),
  data.table(metric = "baseline_distinct_keys", value = as.character(collect_query(con, "SELECT COUNT(DISTINCT analysis_key) AS n_keys FROM baseline_rows")$n_keys[1])),
  data.table(metric = "strict_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM strict_rows")$n_rows[1])),
  data.table(metric = "strict_distinct_keys", value = as.character(collect_query(con, "SELECT COUNT(DISTINCT analysis_key) AS n_keys FROM strict_rows")$n_keys[1])),
  data.table(metric = "chosen_trim_spec", value = chosen_trim_spec),
  data.table(metric = "strict_p1_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM strict_rows_p1")$n_rows[1])),
  data.table(metric = "strict_p25_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM strict_rows_p25")$n_rows[1])),
  data.table(metric = "strict_p5_rows", value = as.character(collect_query(con, "SELECT COUNT(*) AS n_rows FROM strict_rows_p5")$n_rows[1])),
  data.table(metric = "strict_rows_leq_baseline_every_month", value = as.character(all(baseline_month_counts$strict_n_obs <= baseline_month_counts$baseline_n_obs, na.rm = TRUE))),
  data.table(metric = "cpi_missing_primary_window_months", value = as.character(cpi_primary_missing)),
  data.table(metric = "baseline_validation_max_abs_median_diff", value = sprintf("%.12f", safe_max(baseline_validation$median_diff))),
  data.table(metric = "baseline_validation_max_abs_yoy_diff", value = sprintf("%.12f", safe_max(baseline_validation$yoy_diff))),
  data.table(metric = "baseline_validation_pass_tolerance_1e_6", value = as.character(
    safe_max(baseline_validation$median_diff) <= 1e-6 &&
      safe_max(baseline_validation$yoy_diff) <= 1e-6
  )),
  data.table(metric = "strict_min_rent", value = sprintf("%.6f", strict_extremes$strict_min_rent[1])),
  data.table(metric = "strict_max_rent", value = sprintf("%.6f", strict_extremes$strict_max_rent[1])),
  data.table(metric = "strict_min_sqft", value = sprintf("%.6f", strict_extremes$strict_min_sqft[1])),
  data.table(metric = "strict_max_sqft", value = sprintf("%.6f", strict_extremes$strict_max_sqft[1])),
  data.table(metric = "strict_min_rent_per_sqft", value = sprintf("%.6f", strict_extremes$strict_min_rent_per_sqft[1])),
  data.table(metric = "strict_max_rent_per_sqft", value = sprintf("%.6f", strict_extremes$strict_max_rent_per_sqft[1]))
), fill = TRUE)
fwrite(cleaning_summary, file.path(output_dir, "renthub_citywide_cleaning_summary.csv"))

message("Reading Dwellsy monthly comparison inputs...")
dwellsy_monthly <- suppressWarnings(fread(dwellsy_monthly_file))
dwellsy_monthly[, month_start := as.Date(month_start)]
dwellsy_monthly <- dwellsy_monthly[variant_id %in% c("created_listing_first", "active_listing_span_last")]
dwellsy_monthly[, `:=`(
  series_id = fifelse(
    variant_id == "created_listing_first",
    "dwellsy_created_listing_first",
    "dwellsy_active_listing_span_last"
  ),
  series_label = fifelse(
    variant_id == "created_listing_first",
    "Dwellsy Created Listing First",
    "Dwellsy Active Listing Span Last"
  ),
  source_family = "dwellsy",
  series_value_nominal = mean_rent,
  series_value_real_2024 = mean_rent_real_2024
)]
dwellsy_monthly <- dwellsy_monthly[, .(
  month_start,
  series_id,
  series_label,
  source_family,
  series_value_nominal,
  series_value_real_2024,
  median_rent,
  median_rent_real_2024,
  n_obs,
  n_distinct_keys = NA_real_,
  share_unit_id_obs = NA_real_,
  share_fingerprint_obs = NA_real_,
  weight_sum = as.numeric(n_obs)
)]

dwellsy_benchmark <- suppressWarnings(fread(dwellsy_benchmark_file))
dwellsy_benchmark[, month_start := as.Date(month_start)]
benchmark_base <- unique(
  dwellsy_benchmark[, .(
    month_start,
    zillow_city_zori_real_2024,
    zillow_city_zori_real_2024_yoy_pct,
    fred_chi_rent_cpi_real_2024,
    fred_chi_rent_cpi_real_2024_yoy_pct
  )],
  by = "month_start"
)

strict_candidate_monthly <- rbindlist(list(
  copy(strict_p1_monthly)[, trim_spec := "strict_p1"],
  copy(strict_p25_monthly)[, trim_spec := "strict_p25"],
  copy(strict_p5_monthly)[, trim_spec := "strict_p5"]
), fill = TRUE)
strict_candidate_monthly <- merge(strict_candidate_monthly, benchmark_base, by = "month_start", all.x = TRUE, sort = TRUE)
setorder(strict_candidate_monthly, trim_spec, month_start)
strict_candidate_monthly[, median_rent_real_2024_sm3 := zoo::rollapplyr(median_rent_real_2024, 3, mean, fill = NA_real_, partial = FALSE), by = trim_spec]
strict_candidate_monthly[, median_rent_real_2024_yoy_pct := 100 * (median_rent_real_2024_sm3 / shift(median_rent_real_2024_sm3, 12) - 1), by = trim_spec]

strict_candidate_benchmark <- rbindlist(lapply(c("strict_p1", "strict_p25", "strict_p5"), function(trim_value) {
  dt <- strict_candidate_monthly[trim_spec == trim_value & month_start >= primary_start & month_start <= primary_end]
  zillow_metrics <- compute_period_metrics(dt, "median_rent_real_2024_yoy_pct", "zillow_city_zori_real_2024_yoy_pct", lags = 0L, min_obs = 12L)
  fred_metrics <- compute_period_metrics(dt, "median_rent_real_2024_yoy_pct", "fred_chi_rent_cpi_real_2024_yoy_pct", lags = 0:6, min_obs = 12L)
  latest_level <- dt[is.finite(median_rent_real_2024)][.N]
  latest_yoy <- dt[is.finite(median_rent_real_2024_yoy_pct)][.N]

  data.table(
    trim_spec = trim_value,
    latest_nonmissing_month = if (nrow(latest_level) == 0) as.Date(NA) else latest_level$month_start,
    latest_median_rent_real_2024 = if (nrow(latest_level) == 0) NA_real_ else latest_level$median_rent_real_2024,
    latest_yoy_month = if (nrow(latest_yoy) == 0) as.Date(NA) else latest_yoy$month_start,
    latest_series_yoy_pct = if (nrow(latest_yoy) == 0) NA_real_ else latest_yoy$median_rent_real_2024_yoy_pct,
    corr_zillow = zillow_metrics$best_corr[1],
    zillow_median_abs_gap_pp = zillow_metrics$median_abs_gap_pp[1],
    corr_fred = fred_metrics$best_corr[1],
    best_fred_lag = fred_metrics$best_lag[1],
    fred_median_abs_gap_pp = fred_metrics$median_abs_gap_pp[1]
  )
}), fill = TRUE)

strict_candidate_summary <- merge(
  strict_candidate_summary,
  strict_candidate_benchmark,
  by = "trim_spec",
  all.x = TRUE,
  sort = TRUE
)
strict_candidate_summary[, `:=`(
  sqft_percentile_rule = fcase(
    trim_spec == "strict_p1", "1/99",
    trim_spec == "strict_p25", "2.5/97.5",
    default = "5/95"
  ),
  rent_per_sqft_percentile_rule = fcase(
    trim_spec == "strict_p1", "1/99",
    trim_spec == "strict_p25", "2.5/97.5",
    default = "5/95"
  )
)]
setcolorder(strict_candidate_summary, c(
  "trim_spec",
  "sqft_percentile_rule",
  "rent_per_sqft_percentile_rule",
  "sqft_cutoff_lo_min",
  "sqft_cutoff_hi_max",
  "rent_per_sqft_cutoff_lo_min",
  "rent_per_sqft_cutoff_hi_max",
  "rows_kept",
  "distinct_keys_kept",
  "latest_nonmissing_month",
  "latest_yoy_month",
  "latest_median_rent_real_2024",
  "latest_series_yoy_pct",
  "corr_zillow",
  "zillow_median_abs_gap_pp",
  "corr_fred",
  "best_fred_lag",
  "fred_median_abs_gap_pp",
  "max_surviving_sqft",
  "max_surviving_rent_per_sqft",
  "passes_plausibility_gates",
  "chosen_trim_spec",
  "is_chosen"
))
fwrite(strict_candidate_summary, file.path(output_dir, "renthub_trim_sensitivity_summary.csv"))

message("Combining Renthub and Dwellsy monthly series...")
renthub_monthly <- rbindlist(list(
  baseline_monthly[, .(
    month_start,
    series_id,
    series_label,
    source_family,
    series_value_nominal,
    series_value_real_2024,
    median_rent,
    median_rent_real_2024,
    n_obs,
    n_distinct_keys,
    share_unit_id_obs,
    share_fingerprint_obs,
    weight_sum
  )],
  strict_monthly[, .(
    month_start,
    series_id,
    series_label,
    source_family,
    series_value_nominal,
    series_value_real_2024,
    median_rent,
    median_rent_real_2024,
    n_obs,
    n_distinct_keys,
    share_unit_id_obs,
    share_fingerprint_obs,
    weight_sum
  )],
  btstd_monthly[, .(
    month_start,
    series_id,
    series_label,
    source_family,
    series_value_nominal,
    series_value_real_2024,
    median_rent,
    median_rent_real_2024,
    n_obs,
    n_distinct_keys,
    share_unit_id_obs,
    share_fingerprint_obs,
    weight_sum
  )]
), fill = TRUE)

all_series <- rbindlist(list(renthub_monthly, dwellsy_monthly), fill = TRUE)
month_grid <- data.table(month_start = seq(
  from = as.Date("2014-01-01"),
  to = max(all_series$month_start, benchmark_base$month_start, na.rm = TRUE),
  by = "month"
))
series_lookup <- unique(all_series[, .(series_id, series_label, source_family)])

all_series <- merge(
  CJ(series_id = series_lookup$series_id, month_start = month_grid$month_start, unique = TRUE),
  all_series,
  by = c("series_id", "month_start"),
  all.x = TRUE,
  sort = TRUE
)
all_series <- merge(all_series, series_lookup, by = "series_id", all.x = TRUE, sort = TRUE, suffixes = c("", "_lookup"))
all_series[is.na(series_label), series_label := series_label_lookup]
all_series[is.na(source_family), source_family := source_family_lookup]
all_series[, c("series_label_lookup", "source_family_lookup") := NULL]
all_series <- merge(all_series, benchmark_base, by = "month_start", all.x = TRUE, sort = TRUE)
setnames(all_series, make.unique(names(all_series)))
setorder(all_series, series_id, month_start)

all_series[, series_value_nominal_sm3 := zoo::rollapplyr(series_value_nominal, 3, mean, fill = NA_real_, partial = FALSE), by = series_id]
all_series[, series_value_nominal_yoy_pct := 100 * (series_value_nominal_sm3 / shift(series_value_nominal_sm3, 12) - 1), by = series_id]
all_series[, series_value_real_2024_sm3 := zoo::rollapplyr(series_value_real_2024, 3, mean, fill = NA_real_, partial = FALSE), by = series_id]
all_series[, series_value_real_2024_yoy_pct := 100 * (series_value_real_2024_sm3 / shift(series_value_real_2024_sm3, 12) - 1), by = series_id]
all_series[, median_rent_real_2024_sm3 := zoo::rollapplyr(median_rent_real_2024, 3, mean, fill = NA_real_, partial = FALSE), by = series_id]
all_series[, median_rent_real_2024_yoy_pct := 100 * (median_rent_real_2024_sm3 / shift(median_rent_real_2024_sm3, 12) - 1), by = series_id]
all_series[, in_primary_window := month_start >= primary_start & month_start <= primary_end]

fwrite(all_series, file.path(output_dir, "renthub_dwellsy_monthly_comparison.csv"))

message("Writing annual and benchmark summaries...")
annual_summary <- all_series[
  in_primary_window == TRUE,
  .(
    n_months = sum(is.finite(median_rent_real_2024)),
    avg_median_rent_real_2024 = mean(median_rent_real_2024, na.rm = TRUE),
    avg_monthly_yoy_real_2024 = mean(median_rent_real_2024_yoy_pct, na.rm = TRUE)
  ),
  by = .(
    series_id,
    series_label,
    source_family,
    year = as.integer(format(month_start, "%Y"))
  )
]
setorder(annual_summary, series_id, year)
annual_summary[, annual_yoy_change_real_2024 := 100 * (avg_median_rent_real_2024 / shift(avg_median_rent_real_2024) - 1), by = series_id]
fwrite(annual_summary, file.path(output_dir, "renthub_dwellsy_annual_yoy_summary.csv"))

benchmark_summary <- rbindlist(lapply(unique(all_series$series_id), function(series_value) {
  dt <- all_series[series_id == series_value & in_primary_window == TRUE]
  zillow_metrics <- compute_period_metrics(dt, "median_rent_real_2024_yoy_pct", "zillow_city_zori_real_2024_yoy_pct", lags = 0L, min_obs = 12L)
  fred_metrics <- compute_period_metrics(dt, "median_rent_real_2024_yoy_pct", "fred_chi_rent_cpi_real_2024_yoy_pct", lags = 0:6, min_obs = 12L)
  latest_level <- dt[is.finite(median_rent_real_2024)][.N]
  latest_yoy <- dt[is.finite(median_rent_real_2024_yoy_pct)][.N]

  data.table(
    series_id = series_value,
    series_label = dt$series_label[which(!is.na(dt$series_label))[1]],
    source_family = dt$source_family[which(!is.na(dt$source_family))[1]],
    n_months = sum(is.finite(dt$median_rent_real_2024)),
    n_months_with_yoy = sum(is.finite(dt$median_rent_real_2024_yoy_pct)),
    latest_nonmissing_month = if (nrow(latest_level) == 0) as.Date(NA) else latest_level$month_start,
    latest_yoy_month = if (nrow(latest_yoy) == 0) as.Date(NA) else latest_yoy$month_start,
    latest_median_rent_real_2024 = if (nrow(latest_level) == 0) NA_real_ else latest_level$median_rent_real_2024,
    latest_series_yoy_pct = if (nrow(latest_yoy) == 0) NA_real_ else latest_yoy$median_rent_real_2024_yoy_pct,
    corr_zillow = zillow_metrics$best_corr[1],
    zillow_median_abs_gap_pp = zillow_metrics$median_abs_gap_pp[1],
    corr_fred = fred_metrics$best_corr[1],
    best_fred_lag = fred_metrics$best_lag[1],
    fred_median_abs_gap_pp = fred_metrics$median_abs_gap_pp[1]
  )
}), fill = TRUE)
fwrite(benchmark_summary, file.path(output_dir, "renthub_dwellsy_benchmark_summary.csv"))

message("Rendering comparison figures...")
plot_series <- all_series[
  series_id %in% c(
    "renthub_baseline_guangbin_citywide",
    "renthub_strict_citywide",
    "renthub_baseline_btstd_2024mix",
    "dwellsy_created_listing_first",
    "dwellsy_active_listing_span_last"
  ) &
    in_primary_window == TRUE
]
setnames(plot_series, make.unique(names(plot_series)))

series_colors <- c(
  renthub_baseline_guangbin_citywide = "#1b6ca8",
  renthub_strict_citywide = "#0a8754",
  renthub_baseline_btstd_2024mix = "#c17c10",
  dwellsy_created_listing_first = "#9a3412",
  dwellsy_active_listing_span_last = "#6b21a8"
)
plot_breaks <- names(series_colors)
plot_labels <- unique(plot_series[, .(series_id, series_label)])
plot_labels[, sort_order := match(series_id, plot_breaks)]
setorder(plot_labels, sort_order)
plot_labels[, sort_order := NULL]

levels_plot <- ggplot(
  plot_series[is.finite(median_rent_real_2024)],
  aes(x = month_start, y = median_rent_real_2024, color = series_id)
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = series_colors, breaks = plot_breaks, labels = plot_labels$series_label) +
  labs(
    x = NULL,
    y = "Real Median Rent (2024 dollars)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = file.path(output_dir, "fig_renthub_dwellsy_real_rent_levels.pdf"),
  plot = levels_plot,
  width = 10,
  height = 6
)

yoy_series <- plot_series[, .(
  month_start,
  series_id,
  series_label,
  value = median_rent_real_2024_yoy_pct
)]
yoy_benchmarks <- unique(plot_series[, .(
  month_start,
  zillow_city_zori_real_2024_yoy_pct,
  fred_chi_rent_cpi_real_2024_yoy_pct
)])[,
  .(
    month_start,
    series_id = c("zillow_city_real", "fred_rent_cpi_real"),
    series_label = c("Zillow Chicago Real YoY", "FRED Chicago Rent CPI Real YoY"),
    value = c(zillow_city_zori_real_2024_yoy_pct, fred_chi_rent_cpi_real_2024_yoy_pct)
  ),
  by = month_start
]
yoy_plot_dt <- rbindlist(list(yoy_series, yoy_benchmarks), fill = TRUE)
setnames(yoy_plot_dt, make.unique(names(yoy_plot_dt)))

yoy_colors <- c(
  series_colors,
  zillow_city_real = "#111111",
  fred_rent_cpi_real = "#666666"
)
yoy_linetypes <- c(
  renthub_baseline_guangbin_citywide = "solid",
  renthub_strict_citywide = "solid",
  renthub_baseline_btstd_2024mix = "solid",
  dwellsy_created_listing_first = "solid",
  dwellsy_active_listing_span_last = "solid",
  zillow_city_real = "longdash",
  fred_rent_cpi_real = "dashed"
)
yoy_breaks <- names(yoy_colors)
yoy_labels <- unique(yoy_plot_dt[, .(series_id, series_label)])
yoy_labels[, sort_order := match(series_id, yoy_breaks)]
setorder(yoy_labels, sort_order)
yoy_labels[, sort_order := NULL]

yoy_plot <- ggplot(
  yoy_plot_dt[is.finite(value)],
  aes(x = month_start, y = value, color = series_id, linetype = series_id)
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = yoy_colors, breaks = yoy_breaks, labels = yoy_labels$series_label) +
  scale_linetype_manual(values = yoy_linetypes, breaks = yoy_breaks, labels = yoy_labels$series_label) +
  labs(
    x = NULL,
    y = "YoY Change in Real Median Rent (%)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = file.path(output_dir, "fig_renthub_dwellsy_real_yoy.pdf"),
  plot = yoy_plot,
  width = 10,
  height = 6
)
