# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_rent_data/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_date, end_date)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <start_date> <end_date>.", call. = FALSE)
}

start_date <- as.Date(cli_args[1])
end_date <- as.Date(cli_args[2])
if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("start_date and end_date must define a valid date window.", call. = FALSE)
}

raw_glob <- "../input/renthub_raw/*.parquet"
raw_files <- Sys.glob(raw_glob)
if (length(raw_files) == 0) {
  stop("No RentHub parquet files found in ../input/renthub_raw.", call. = FALSE)
}

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))

invisible(dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE chicago_raw AS
    WITH raw_source AS (
      SELECT
        *,
        UPPER(TRIM(CAST(ID AS VARCHAR))) AS id_raw,
        UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) AS property_id_raw,
        UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) AS unit_id_raw,
        UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) AS address_raw,
        UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) AS building_type_raw,
        UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city_raw
      FROM read_parquet('%s', union_by_name = true)
      WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) >= CAST('%s' AS DATE)
        AND TRY_CAST(SCRAPED_TIMESTAMP AS DATE) <= CAST('%s' AS DATE)
    )
    SELECT
      CASE
        WHEN id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE id_raw
      END AS id,
      CASE
        WHEN property_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE property_id_raw
      END AS property_id,
      CASE
        WHEN unit_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE unit_id_raw
      END AS unit_id,
      CASE
        WHEN address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
      END AS address_norm,
      CASE WHEN address_raw = '0' THEN 1 ELSE 0 END AS address_zero_flag,
      CASE
        WHEN address_raw IS NULL
          OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 1
        ELSE 0
      END AS address_missing_flag,
      TRY_CAST(SCRAPED_TIMESTAMP AS TIMESTAMP) AS scraped_timestamp,
      TRY_CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
      TRY_CAST(DATE_POSTED AS DATE) AS posted_date,
      TRY_CAST(AVAILABLE_AT AS DATE) AS available_date,
      TRY_CAST(RENT_PRICE AS DOUBLE) AS rent_price,
      CAST(BUILDING_TYPE AS VARCHAR) AS building_type,
      CASE
        WHEN building_type_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 'other'
        WHEN building_type_raw = 'TH' OR building_type_raw LIKE '%%TOWN%%' THEN 'townhouse'
        WHEN building_type_raw IN ('CON', 'CONDO') OR building_type_raw LIKE '%%CONDO%%'
          OR building_type_raw LIKE '%%CONDOMINIUM%%' THEN 'condo'
        WHEN building_type_raw IN ('COMM', 'COMMERCIAL') OR building_type_raw LIKE '%%COMMERCIAL%%' THEN 'commercial'
        WHEN building_type_raw LIKE '%%MULTI%%'
          OR building_type_raw LIKE '%%APART%%'
          OR building_type_raw LIKE '%%APT%%'
          OR building_type_raw LIKE '%%DUPLEX%%'
          OR building_type_raw LIKE '%%TRIPLEX%%'
          OR building_type_raw LIKE '%%FOURPLEX%%' THEN 'multi_family'
        WHEN building_type_raw LIKE '%%SINGLE%%'
          OR building_type_raw LIKE '%%HOUSE%%'
          OR building_type_raw LIKE '%%DETACHED%%'
          OR building_type_raw LIKE '%%SFR%%' THEN 'single_family'
        ELSE 'other'
      END AS building_type_clean,
      TRY_CAST(BEDS AS DOUBLE) AS beds,
      TRY_CAST(BATHS AS DOUBLE) AS baths,
      TRY_CAST(SQFT AS DOUBLE) AS sqft,
      TRY_CAST(YEAR_BUILT AS DOUBLE) AS year_built,
      TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
      TRY_CAST(LONGITUDE AS DOUBLE) AS longitude,
      CAST(AVAILABILITY_STATUS AS VARCHAR) AS availability_status,
      CASE WHEN UPPER(TRIM(CAST(DOORMAN AS VARCHAR))) IN ('Y', 'YES', 'TRUE', '1') THEN 1 ELSE 0 END AS doorman,
      CASE WHEN UPPER(TRIM(CAST(FURNISHED AS VARCHAR))) IN ('Y', 'YES', 'TRUE', '1') THEN 1 ELSE 0 END AS furnished,
      CASE WHEN UPPER(TRIM(CAST(GYM AS VARCHAR))) IN ('Y', 'YES', 'TRUE', '1') THEN 1 ELSE 0 END AS gym,
      CASE WHEN UPPER(TRIM(CAST(LAUNDRY AS VARCHAR))) IN ('Y', 'YES', 'TRUE', '1') THEN 1 ELSE 0 END AS laundry,
      CASE WHEN UPPER(TRIM(CAST(POOL AS VARCHAR))) IN ('Y', 'YES', 'TRUE', '1') THEN 1 ELSE 0 END AS pool,
      CASE
        WHEN TRY_CAST(LATITUDE AS DOUBLE) IS NOT NULL
          AND TRY_CAST(LONGITUDE AS DOUBLE) IS NOT NULL
          AND TRY_CAST(LATITUDE AS DOUBLE) BETWEEN -90 AND 90
          AND TRY_CAST(LONGITUDE AS DOUBLE) BETWEEN -180 AND 180
          THEN 1
        ELSE 0
      END AS valid_coordinates,
      CASE
        WHEN TRY_CAST(LATITUDE AS DOUBLE) BETWEEN 41.55 AND 42.10
          AND TRY_CAST(LONGITUDE AS DOUBLE) BETWEEN -88.10 AND -87.40
          THEN 1
        ELSE 0
      END AS chicago_bbox,
      CASE
        WHEN TRY_CAST(DATE_POSTED AS DATE) IS NULL OR TRY_CAST(SCRAPED_TIMESTAMP AS DATE) IS NULL THEN NULL
        ELSE DATE_DIFF('day', TRY_CAST(DATE_POSTED AS DATE), TRY_CAST(SCRAPED_TIMESTAMP AS DATE))
      END AS posted_lag_days
    FROM raw_source
    WHERE city_raw IN ('CHICAGO', 'CHGO')
    ",
    gsub("'", "''", raw_glob, fixed = TRUE),
    start_date,
    end_date
  )
))

raw_count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM chicago_raw")$n[1]
if (!is.finite(raw_count) || raw_count == 0L) {
  stop("No Chicago RentHub rows found in the requested window.", call. = FALSE)
}
invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE keyed_rows AS
  WITH keyed_base AS (
    SELECT
      *,
      CASE
        WHEN valid_coordinates = 1 THEN PRINTF('%.4f|%.4f', latitude, longitude)
        ELSE NULL
      END AS coord_key,
      CASE WHEN address_norm IS NULL THEN 1 ELSE 0 END AS address_missing
    FROM chicago_raw
  ),
  property_keys AS (
    SELECT
      *,
      CASE
        WHEN coord_key IS NULL THEN NULL
        ELSE COALESCE(address_norm, 'NO_ADDRESS') || '|' || coord_key
      END AS property_key
    FROM keyed_base
  )
  SELECT
    *,
    CASE
      WHEN property_key IS NULL THEN NULL
      ELSE property_key || '|' ||
        COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(building_type_clean, 'other')
    END AS floorplan_key,
    CASE
      WHEN property_key IS NULL OR rent_price IS NULL THEN NULL
      ELSE property_key || '|' ||
        COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(building_type_clean, 'other') || '|' ||
        CAST(rent_price AS VARCHAR)
    END AS rent_cell_key,
    CASE
      WHEN unit_id IS NOT NULL THEN unit_id
      WHEN property_key IS NOT NULL THEN property_key || '|' ||
        COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(building_type_clean, 'other')
      ELSE NULL
    END AS analysis_key,
    CASE
      WHEN unit_id IS NOT NULL THEN 'unit_id'
      WHEN property_key IS NOT NULL THEN 'floorplan_fingerprint'
      ELSE 'unkeyed'
    END AS key_source
  FROM property_keys
  "
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE floorplan_day_base AS
  SELECT
    DATE_TRUNC('month', file_date)::DATE AS month_start,
    file_date,
    CAST(YEAR(file_date) AS INTEGER) AS year,
    analysis_key,
    key_source,
    MIN(id) AS id,
    MIN(property_id) AS property_id,
    MIN(unit_id) AS unit_id,
    MIN(property_key) AS property_key,
    MIN(floorplan_key) AS floorplan_key,
    MIN(address_norm) AS address_norm,
    MAX(address_missing) AS address_missing,
    MIN(posted_date) AS posted_date,
    MIN(available_date) AS available_date,
    MAX(scraped_timestamp) AS last_scraped_timestamp,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_price,
    AVG(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_price_mean,
    QUANTILE_CONT(rent_price, 0.25) FILTER (WHERE rent_price IS NOT NULL) AS rent_p25,
    QUANTILE_CONT(rent_price, 0.75) FILTER (WHERE rent_price IS NOT NULL) AS rent_p75,
    MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
    MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max,
    COUNT(*) AS raw_rows_day,
    COUNT(DISTINCT rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_values_day,
    COUNT(DISTINCT rent_cell_key) AS rent_cells_day,
    QUANTILE_CONT(latitude, 0.50) FILTER (WHERE latitude IS NOT NULL) AS latitude,
    QUANTILE_CONT(longitude, 0.50) FILTER (WHERE longitude IS NOT NULL) AS longitude,
    QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
    QUANTILE_CONT(baths, 0.50) FILTER (WHERE baths IS NOT NULL) AS baths,
    QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
    QUANTILE_CONT(year_built, 0.50) FILTER (WHERE year_built IS NOT NULL) AS year_built,
    MIN(building_type) AS building_type,
    MIN(building_type_clean) AS building_type_clean,
    MIN(availability_status) AS availability_status,
    MAX(doorman) AS doorman,
    MAX(furnished) AS furnished,
    MAX(gym) AS gym,
    MAX(laundry) AS laundry,
    MAX(pool) AS pool,
    QUANTILE_CONT(posted_lag_days, 0.50) FILTER (WHERE posted_lag_days IS NOT NULL) AS posted_lag_p50,
    QUANTILE_CONT(posted_lag_days, 0.90) FILTER (WHERE posted_lag_days IS NOT NULL) AS posted_lag_p90,
    AVG(CASE WHEN posted_lag_days IS NOT NULL AND posted_lag_days > 90 THEN 1.0 ELSE 0.0 END) AS share_posted_lag_gt90
  FROM keyed_rows
  WHERE analysis_key IS NOT NULL
    AND valid_coordinates = 1
  GROUP BY 1, 2, 3, 4, 5
  "
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE day_thresholds AS
  SELECT
    year,
    CASE
      WHEN beds IS NULL THEN 'missing'
      WHEN beds >= 4 THEN '4plus'
      ELSE CAST(CAST(beds AS INTEGER) AS VARCHAR)
    END AS beds_bin,
    QUANTILE_CONT(rent_price, 0.01) FILTER (WHERE rent_price > 0) AS rent_p01,
    QUANTILE_CONT(rent_price, 0.99) FILTER (WHERE rent_price > 0) AS rent_p99,
    QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
    QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99
  FROM floorplan_day_base
  GROUP BY 1, 2
  "
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE floorplan_day_clean AS
  SELECT
    d.*,
    CASE
      WHEN d.beds IS NULL THEN 'missing'
      WHEN d.beds >= 4 THEN '4plus'
      ELSE CAST(CAST(d.beds AS INTEGER) AS VARCHAR)
    END AS beds_bin,
    CASE
      WHEN d.latitude BETWEEN 41.55 AND 42.10
        AND d.longitude BETWEEN -88.10 AND -87.40 THEN 1
      ELSE 0
    END AS keep_chicago_bbox,
    CASE
      WHEN d.rent_price IS NOT NULL
        AND d.rent_price > 0
        AND d.rent_price >= COALESCE(t.rent_p01, d.rent_price)
        AND d.rent_price <= COALESCE(t.rent_p99, d.rent_price) THEN 1
      ELSE 0
    END AS keep_rent_trim01,
    CASE
      WHEN d.sqft IS NULL THEN 1
      WHEN d.sqft > 0
        AND d.sqft >= COALESCE(t.sqft_p01, d.sqft)
        AND d.sqft <= COALESCE(t.sqft_p99, d.sqft) THEN 1
      ELSE 0
    END AS keep_sqft_trim01,
    CASE WHEN d.rent_values_day > 1 THEN 1 ELSE 0 END AS multi_rent_day,
    CASE WHEN d.raw_rows_day > 1 AND d.rent_values_day = 1 THEN 1 ELSE 0 END AS same_rent_repeat_day
  FROM floorplan_day_base d
  LEFT JOIN day_thresholds t
    ON d.year = t.year
    AND (
      CASE
        WHEN d.beds IS NULL THEN 'missing'
        WHEN d.beds >= 4 THEN '4plus'
        ELSE CAST(CAST(d.beds AS INTEGER) AS VARCHAR)
      END
    ) = t.beds_bin
  "
))

invisible(dbExecute(
  con,
  "
  ALTER TABLE floorplan_day_clean ADD COLUMN main_day_flag INTEGER;
  UPDATE floorplan_day_clean
  SET main_day_flag = CASE
    WHEN keep_chicago_bbox = 1
      AND keep_rent_trim01 = 1
      AND keep_sqft_trim01 = 1 THEN 1
    ELSE 0
  END
  "
))

invisible(dbExecute(
  con,
  "
    CREATE OR REPLACE TEMP TABLE floorplan_month_main_first AS
    SELECT *
    FROM (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, month_start
          ORDER BY file_date, last_scraped_timestamp, id
        ) AS month_order
      FROM floorplan_day_clean
      WHERE main_day_flag = 1
    )
    WHERE month_order = 1
  "
))

invisible(dbExecute(
  con,
  "
    CREATE OR REPLACE TEMP TABLE floorplan_month_main_agg AS
    SELECT
      analysis_key,
      month_start,
      COUNT(*) AS active_days,
      COUNT(DISTINCT rent_price) AS distinct_daily_rents,
      SUM(raw_rows_day) AS raw_rows_month,
      SUM(multi_rent_day) AS multi_rent_days,
      SUM(same_rent_repeat_day) AS same_rent_repeat_days,
      SUM(rent_values_day) AS rent_values_across_days,
      QUANTILE_CONT(rent_price, 0.50) AS rent_price,
      AVG(rent_price) AS rent_price_mean,
      QUANTILE_CONT(rent_price, 0.25) AS rent_price_p25,
      QUANTILE_CONT(rent_price, 0.75) AS rent_price_p75,
      MIN(rent_price) AS rent_price_min,
      MAX(rent_price) AS rent_price_max,
      QUANTILE_CONT(latitude, 0.50) AS latitude,
      QUANTILE_CONT(longitude, 0.50) AS longitude,
      QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
      QUANTILE_CONT(baths, 0.50) FILTER (WHERE baths IS NOT NULL) AS baths,
      QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
      QUANTILE_CONT(year_built, 0.50) FILTER (WHERE year_built IS NOT NULL) AS year_built,
      MAX(doorman) AS doorman,
      MAX(furnished) AS furnished,
      MAX(gym) AS gym,
      MAX(laundry) AS laundry,
      MAX(pool) AS pool,
      AVG(address_missing) AS share_address_missing_days,
      AVG(multi_rent_day) AS share_multi_rent_days,
      AVG(same_rent_repeat_day) AS share_same_rent_repeat_days
    FROM floorplan_day_clean
    WHERE main_day_flag = 1
    GROUP BY 1, 2
  "
))

invisible(dbExecute(
  con,
  "
    CREATE OR REPLACE TEMP TABLE floorplan_month_main AS
    SELECT
      f.analysis_key || '__' || STRFTIME(f.month_start, '%Y-%m') AS rent_panel_id,
      f.id,
      f.property_id,
      f.unit_id,
      f.property_key,
      f.floorplan_key,
      f.analysis_key,
      f.key_source,
      f.address_norm,
      f.address_missing,
      f.month_start AS file_date,
      f.month_start,
      CAST(YEAR(f.month_start) AS INTEGER) AS year,
      f.file_date AS first_observed_date,
      f.posted_date,
      f.available_date,
      a.rent_price,
      f.rent_price AS first_observed_rent,
      a.rent_price_mean,
      a.rent_price_p25,
      a.rent_price_p75,
      a.rent_price_min,
      a.rent_price_max,
      a.active_days,
      a.distinct_daily_rents,
      a.raw_rows_month,
      a.multi_rent_days,
      a.same_rent_repeat_days,
      a.rent_values_across_days,
      a.share_address_missing_days,
      a.share_multi_rent_days,
      a.share_same_rent_repeat_days,
      f.building_type,
      f.building_type_clean,
      f.availability_status,
      a.beds,
      a.baths,
      a.sqft,
      a.year_built,
      a.latitude,
      a.longitude,
      a.doorman,
      a.furnished,
      a.gym,
      a.laundry,
      a.pool
    FROM floorplan_month_main_first f
    INNER JOIN floorplan_month_main_agg a
      ON f.analysis_key = a.analysis_key
      AND f.month_start = a.month_start
  "
))

unlink("../output/chicago_rent_panel.parquet", recursive = TRUE, force = TRUE)
invisible(dbExecute(
  con,
  "
  COPY floorplan_month_main
  TO '../output/chicago_rent_panel.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD)
  "
))

duplicate_months <- dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS n
  FROM (
    SELECT analysis_key, month_start, COUNT(*) AS n_rows
    FROM floorplan_month_main
    GROUP BY 1, 2
    HAVING COUNT(*) > 1
  )
  "
)$n[1]
if (duplicate_months > 0) {
  stop("Duplicate analysis_key-month rows found in main rent panel.", call. = FALSE)
}

address_zero_valid <- dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM floorplan_month_main WHERE address_norm = '0'"
)$n[1]
if (address_zero_valid > 0) {
  stop("ADDRESS=0 survived as a valid address in the main rent panel.", call. = FALSE)
}

con_mapping <- dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS n_rows,
    SUM(CASE WHEN building_type_clean = 'condo' THEN 1 ELSE 0 END) AS n_condo
  FROM keyed_rows
  WHERE UPPER(TRIM(CAST(building_type AS VARCHAR))) = 'CON'
  "
)
if (con_mapping$n_rows[1] > 0 && con_mapping$n_rows[1] != con_mapping$n_condo[1]) {
  stop("Building type CON did not map completely to condo.", call. = FALSE)
}

townhouse_mapping <- dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS n_rows,
    SUM(CASE WHEN building_type_clean = 'townhouse' THEN 1 ELSE 0 END) AS n_townhouse
  FROM keyed_rows
  WHERE UPPER(TRIM(CAST(building_type AS VARCHAR))) = 'TH'
    OR UPPER(TRIM(CAST(building_type AS VARCHAR))) LIKE '%TOWN%'
  "
)
if (townhouse_mapping$n_rows[1] > 0 && townhouse_mapping$n_rows[1] != townhouse_mapping$n_townhouse[1]) {
  stop("Townhouse/TH building types did not map completely to townhouse.", call. = FALSE)
}

month_span <- dbGetQuery(
  con,
  "
  SELECT
    MIN(month_start) AS min_month,
    MAX(month_start) AS max_month,
    COUNT(DISTINCT month_start) AS n_months
  FROM floorplan_month_main
  "
)
expected_months <- length(seq(as.Date(format(start_date, "%Y-%m-01")), as.Date(format(end_date, "%Y-%m-01")), by = "month"))
if (
  as.Date(month_span$min_month[1]) != as.Date(format(start_date, "%Y-%m-01")) ||
    as.Date(month_span$max_month[1]) != as.Date(format(end_date, "%Y-%m-01")) ||
    month_span$n_months[1] != expected_months
) {
  stop("Main rent panel does not span the expected complete monthly window.", call. = FALSE)
}
