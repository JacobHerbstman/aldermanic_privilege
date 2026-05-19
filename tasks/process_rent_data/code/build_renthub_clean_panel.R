# Build a cleaned RentHub floorplan-month panel for rental RD.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_rent_data/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"

source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_date, end_date)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <start_date> <end_date>", call. = FALSE)
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

sql_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

message("=== Build RentHub Floorplan-Month Panel ===")
message(sprintf("Input files: %s", format(length(raw_files), big.mark = ",")))
message(sprintf("Window: %s through %s", start_date, end_date))

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

message("Auditing Illinois export city coverage...")
city_filter_diagnostics <- collect_query(sprintf(
  "
  WITH scoped AS (
    SELECT
      CASE
        WHEN UPPER(TRIM(CAST(CITY AS VARCHAR))) IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE UPPER(TRIM(CAST(CITY AS VARCHAR)))
      END AS city_clean
    FROM read_parquet('%s', union_by_name = true)
    WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) >= CAST('%s' AS DATE)
      AND TRY_CAST(SCRAPED_TIMESTAMP AS DATE) <= CAST('%s' AS DATE)
  )
  SELECT
    COALESCE(city_clean, 'MISSING') AS city,
    COUNT(*) AS n_rows,
    AVG(CASE WHEN city_clean = 'CHICAGO' THEN 1.0 ELSE 0.0 END) AS share_chicago,
    AVG(CASE WHEN city_clean IN ('CHICAGO', 'CHGO') THEN 1.0 ELSE 0.0 END) AS share_chicago_alias
  FROM scoped
  GROUP BY 1
  ORDER BY n_rows DESC, city
  ",
  sql_escape(raw_glob),
  start_date,
  end_date
))
fwrite(city_filter_diagnostics, "../output/renthub_city_filter_diagnostics.csv")

message("Loading Chicago rows and building transparent property/floorplan keys...")
dbExecute(
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
    sql_escape(raw_glob),
    start_date,
    end_date
  )
)

raw_count <- collect_query("SELECT COUNT(*) AS n FROM chicago_raw")$n[1]
if (!is.finite(raw_count) || raw_count == 0L) {
  stop("No Chicago RentHub rows found in the requested window.", call. = FALSE)
}
message(sprintf(
  "Chicago rows after city filter: %s of %s raw Illinois rows in window.",
  format(raw_count, big.mark = ","),
  format(sum(city_filter_diagnostics$n_rows), big.mark = ",")
))

dbExecute(
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
)

message("Writing raw-to-clean identifier and building-type diagnostics...")
key_diagnostics <- rbindlist(list(
  collect_query(
    "
    SELECT
      'overall' AS scope,
      NULL AS year,
      COUNT(*) AS n_rows,
      AVG(CASE WHEN id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_id,
      AVG(CASE WHEN property_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_property_id,
      AVG(CASE WHEN unit_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_unit_id,
      AVG(CASE WHEN address_norm IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_valid_address,
      AVG(address_zero_flag) AS share_address_zero,
      AVG(address_missing_flag) AS share_address_zero_or_missing,
      AVG(valid_coordinates) AS share_valid_coordinates,
      AVG(chicago_bbox) AS share_chicago_bbox,
      AVG(CASE WHEN beds IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_beds,
      AVG(CASE WHEN baths IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_baths,
      AVG(CASE WHEN sqft IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_sqft,
      AVG(CASE WHEN building_type IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_building_type,
      COUNT(DISTINCT property_key) AS distinct_property_keys,
      COUNT(DISTINCT floorplan_key) AS distinct_floorplan_keys
    FROM keyed_rows
    "
  ),
  collect_query(
    "
    SELECT
      'year' AS scope,
      CAST(YEAR(file_date) AS INTEGER) AS year,
      COUNT(*) AS n_rows,
      AVG(CASE WHEN id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_id,
      AVG(CASE WHEN property_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_property_id,
      AVG(CASE WHEN unit_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_unit_id,
      AVG(CASE WHEN address_norm IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_valid_address,
      AVG(address_zero_flag) AS share_address_zero,
      AVG(address_missing_flag) AS share_address_zero_or_missing,
      AVG(valid_coordinates) AS share_valid_coordinates,
      AVG(chicago_bbox) AS share_chicago_bbox,
      AVG(CASE WHEN beds IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_beds,
      AVG(CASE WHEN baths IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_baths,
      AVG(CASE WHEN sqft IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_sqft,
      AVG(CASE WHEN building_type IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_building_type,
      COUNT(DISTINCT property_key) AS distinct_property_keys,
      COUNT(DISTINCT floorplan_key) AS distinct_floorplan_keys
    FROM keyed_rows
    GROUP BY 1, 2
    ORDER BY 2
    "
  )
), fill = TRUE)
fwrite(key_diagnostics, "../output/renthub_key_diagnostics.csv")

building_type_diagnostics <- collect_query(
  "
  SELECT
    COALESCE(UPPER(TRIM(CAST(building_type AS VARCHAR))), 'MISSING') AS building_type_raw,
    building_type_clean,
    COUNT(*) AS raw_rows,
    COUNT(DISTINCT floorplan_key) AS floorplan_keys,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
    QUANTILE_CONT(rent_price, 0.99) FILTER (WHERE rent_price IS NOT NULL) AS rent_p99
  FROM keyed_rows
  GROUP BY 1, 2
  ORDER BY raw_rows DESC, building_type_raw
  "
)
fwrite(building_type_diagnostics, "../output/renthub_building_type_diagnostics.csv")

message("Collapsing raw rows to property-day and floorplan-day diagnostics...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE property_day_base AS
  SELECT
    CAST(YEAR(file_date) AS INTEGER) AS year,
    file_date,
    property_key,
    MAX(address_missing) AS address_missing,
    COUNT(*) AS raw_rows,
    COUNT(DISTINCT floorplan_key) AS floorplans,
    COUNT(DISTINCT rent_cell_key) AS rent_cells,
    COUNT(DISTINCT rent_price) FILTER (WHERE rent_price IS NOT NULL) AS distinct_raw_rents,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_p50,
    MIN(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_min,
    MAX(rent_price) FILTER (WHERE rent_price IS NOT NULL) AS rent_max
  FROM keyed_rows
  WHERE property_key IS NOT NULL
  GROUP BY 1, 2, 3
  "
)

property_day_diagnostics <- collect_query(
  "
  SELECT
    year,
    COUNT(*) AS property_days,
    SUM(raw_rows) AS raw_rows,
    QUANTILE_CONT(floorplans, 0.50) AS floorplans_p50,
    QUANTILE_CONT(floorplans, 0.90) AS floorplans_p90,
    QUANTILE_CONT(floorplans, 0.99) AS floorplans_p99,
    MAX(floorplans) AS floorplans_max,
    AVG(CASE WHEN floorplans >= 10 THEN 1.0 ELSE 0.0 END) AS share_10plus_floorplans,
    QUANTILE_CONT(rent_cells, 0.50) AS rent_cells_p50,
    QUANTILE_CONT(rent_cells, 0.90) AS rent_cells_p90,
    QUANTILE_CONT(rent_cells, 0.99) AS rent_cells_p99,
    MAX(rent_cells) AS rent_cells_max,
    AVG(CASE WHEN rent_cells >= 10 THEN 1.0 ELSE 0.0 END) AS share_10plus_rent_cells,
    AVG(address_missing) AS share_address_missing
  FROM property_day_base
  GROUP BY 1
  ORDER BY 1
  "
)
fwrite(property_day_diagnostics, "../output/renthub_property_day_diagnostics.csv")

dbExecute(
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
)

dbExecute(
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
)

dbExecute(
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
)

dbExecute(
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
)

floorplan_day_diagnostics <- rbindlist(list(
  collect_query(
    "
    SELECT
      'year' AS scope,
      CAST(year AS VARCHAR) AS group_value,
      COUNT(*) AS floorplan_days,
      SUM(raw_rows_day) AS raw_rows,
      SUM(main_day_flag) AS main_floorplan_days,
      AVG(multi_rent_day) AS share_multi_rent_day,
      AVG(same_rent_repeat_day) AS share_same_rent_repeat_day,
      QUANTILE_CONT(rent_values_day, 0.50) AS rent_values_p50,
      QUANTILE_CONT(rent_values_day, 0.90) AS rent_values_p90,
      QUANTILE_CONT(rent_values_day, 0.99) AS rent_values_p99,
      MAX(rent_values_day) AS rent_values_max,
      QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
      QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
      QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
      QUANTILE_CONT(posted_lag_p50, 0.50) FILTER (WHERE posted_lag_p50 IS NOT NULL) AS posted_lag_p50,
      QUANTILE_CONT(posted_lag_p90, 0.50) FILTER (WHERE posted_lag_p90 IS NOT NULL) AS posted_lag_p90
    FROM floorplan_day_clean
    GROUP BY 1, 2
    ORDER BY 2
    "
  ),
  collect_query(
    "
    SELECT
      'building_type_clean' AS scope,
      building_type_clean AS group_value,
      COUNT(*) AS floorplan_days,
      SUM(raw_rows_day) AS raw_rows,
      SUM(main_day_flag) AS main_floorplan_days,
      AVG(multi_rent_day) AS share_multi_rent_day,
      AVG(same_rent_repeat_day) AS share_same_rent_repeat_day,
      QUANTILE_CONT(rent_values_day, 0.50) AS rent_values_p50,
      QUANTILE_CONT(rent_values_day, 0.90) AS rent_values_p90,
      QUANTILE_CONT(rent_values_day, 0.99) AS rent_values_p99,
      MAX(rent_values_day) AS rent_values_max,
      QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
      QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
      QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
      QUANTILE_CONT(posted_lag_p50, 0.50) FILTER (WHERE posted_lag_p50 IS NOT NULL) AS posted_lag_p50,
      QUANTILE_CONT(posted_lag_p90, 0.50) FILTER (WHERE posted_lag_p90 IS NOT NULL) AS posted_lag_p90
    FROM floorplan_day_clean
    GROUP BY 1, 2
    ORDER BY floorplan_days DESC
    "
  )
), fill = TRUE)
fwrite(floorplan_day_diagnostics, "../output/renthub_floorplan_day_diagnostics.csv")

message("Building main floorplan-month panel and drop-multi-rent robustness panel...")
create_month_panel <- function(output_table, day_filter) {
  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE %s_first AS
    SELECT *
    FROM (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, month_start
          ORDER BY file_date, last_scraped_timestamp, id
        ) AS month_order
      FROM floorplan_day_clean
      WHERE %s
    )
    WHERE month_order = 1
    ",
    output_table,
    day_filter
  ))

  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE %s_agg AS
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
    WHERE %s
    GROUP BY 1, 2
    ",
    output_table,
    day_filter
  ))

  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE %s AS
    SELECT
      f.analysis_key || '__' || STRFTIME(f.month_start, '%%Y-%%m') AS rent_panel_id,
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
    FROM %s_first f
    INNER JOIN %s_agg a
      ON f.analysis_key = a.analysis_key
      AND f.month_start = a.month_start
    ",
    output_table,
    output_table,
    output_table
  ))
}

create_month_panel("floorplan_month_main", "main_day_flag = 1")
create_month_panel("floorplan_month_drop_multi", "main_day_flag = 1 AND multi_rent_day = 0")

unlink("../output/chicago_rent_panel.parquet", recursive = TRUE, force = TRUE)
dbExecute(
  con,
  "
  COPY floorplan_month_main
  TO '../output/chicago_rent_panel.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD)
  "
)
unlink("../output/chicago_rent_panel_drop_multi_rent.parquet", recursive = TRUE, force = TRUE)
dbExecute(
  con,
  "
  COPY floorplan_month_drop_multi
  TO '../output/chicago_rent_panel_drop_multi_rent.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD)
  "
)

message("Building episode-start robustness panels for 30/45/60/90 day gaps...")
episode_gap_values <- c(30L, 45L, 60L, 90L)
for (episode_gap_days in episode_gap_values) {
  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_day_markers AS
    WITH ordered AS (
      SELECT
        *,
        LAG(file_date) OVER (PARTITION BY analysis_key ORDER BY file_date) AS previous_file_date
      FROM floorplan_day_clean
      WHERE main_day_flag = 1
    ),
    marked AS (
      SELECT
        *,
        CASE
          WHEN previous_file_date IS NULL THEN 1
          WHEN DATE_DIFF('day', previous_file_date, file_date) > %d THEN 1
          ELSE 0
        END AS starts_episode
      FROM ordered
    )
    SELECT
      *,
      SUM(starts_episode) OVER (
        PARTITION BY analysis_key
        ORDER BY file_date
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
      ) AS episode_number
    FROM marked
    ",
    episode_gap_days
  ))

  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_first AS
    SELECT *
    FROM (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, episode_number
          ORDER BY file_date, last_scraped_timestamp, id
        ) AS episode_order
      FROM episode_day_markers
    )
    WHERE episode_order = 1
    "
  ))

  dbExecute(con, sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_gap_panel AS
    WITH episode_agg AS (
      SELECT
        analysis_key,
        episode_number,
        MIN(file_date) AS episode_start_date,
        MAX(file_date) AS episode_end_date,
        COUNT(*) AS episode_obs_days,
        SUM(raw_rows_day) AS raw_rows_episode,
        COUNT(DISTINCT rent_price) AS episode_n_rent_values,
        QUANTILE_CONT(rent_price, 0.50) AS episode_median_rent,
        AVG(rent_price) AS episode_mean_rent,
        MIN(rent_price) AS episode_min_rent,
        MAX(rent_price) AS episode_max_rent,
        SUM(multi_rent_day) AS episode_multi_rent_days,
        SUM(same_rent_repeat_day) AS episode_same_rent_repeat_days,
        QUANTILE_CONT(latitude, 0.50) AS latitude,
        QUANTILE_CONT(longitude, 0.50) AS longitude,
        QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
        QUANTILE_CONT(baths, 0.50) FILTER (WHERE baths IS NOT NULL) AS baths,
        QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
        QUANTILE_CONT(year_built, 0.50) FILTER (WHERE year_built IS NOT NULL) AS year_built
      FROM episode_day_markers
      GROUP BY 1, 2
    )
    SELECT
      %d AS episode_gap_days,
      f.analysis_key || '__gap%d__' || CAST(f.episode_number AS VARCHAR) AS episode_id,
      f.id,
      f.property_id,
      f.unit_id,
      f.property_key,
      f.floorplan_key,
      f.analysis_key,
      f.key_source,
      f.address_norm,
      f.address_missing,
      a.episode_start_date AS file_date,
      DATE_TRUNC('month', a.episode_start_date)::DATE AS month_start,
      CAST(YEAR(a.episode_start_date) AS INTEGER) AS year,
      a.episode_start_date,
      a.episode_end_date,
      a.episode_obs_days,
      a.raw_rows_episode,
      a.episode_n_rent_values,
      a.episode_median_rent AS rent_price,
      a.episode_median_rent,
      f.rent_price AS first_observed_rent,
      a.episode_mean_rent,
      a.episode_min_rent,
      a.episode_max_rent,
      a.episode_multi_rent_days,
      a.episode_same_rent_repeat_days,
      f.building_type,
      f.building_type_clean,
      f.availability_status,
      a.beds,
      a.baths,
      a.sqft,
      a.year_built,
      a.latitude,
      a.longitude,
      f.doorman,
      f.furnished,
      f.gym,
      f.laundry,
      f.pool
    FROM episode_first f
    INNER JOIN episode_agg a
      ON f.analysis_key = a.analysis_key
      AND f.episode_number = a.episode_number
    ",
    episode_gap_days,
    episode_gap_days
  ))

  if (episode_gap_days == episode_gap_values[1]) {
    dbExecute(con, "CREATE OR REPLACE TEMP TABLE episode_robustness AS SELECT * FROM episode_gap_panel")
  } else {
    dbExecute(con, "INSERT INTO episode_robustness SELECT * FROM episode_gap_panel")
  }
}

unlink("../output/chicago_rent_episode_robustness.parquet", recursive = TRUE, force = TRUE)
dbExecute(
  con,
  "
  COPY episode_robustness
  TO '../output/chicago_rent_episode_robustness.parquet'
  (FORMAT PARQUET, COMPRESSION ZSTD)
  "
)

episode_diagnostics <- collect_query(
  "
  SELECT
    episode_gap_days,
    COUNT(*) AS episodes,
    COUNT(DISTINCT analysis_key) AS analysis_keys,
    QUANTILE_CONT(episode_obs_days, 0.50) AS obs_days_p50,
    QUANTILE_CONT(episode_obs_days, 0.90) AS obs_days_p90,
    QUANTILE_CONT(episode_obs_days, 0.99) AS obs_days_p99,
    AVG(CASE WHEN episode_n_rent_values > 1 THEN 1.0 ELSE 0.0 END) AS share_multi_rent_episodes,
    QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
    QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
    QUANTILE_CONT(rent_price, 0.99) AS rent_p99
  FROM episode_robustness
  GROUP BY 1
  ORDER BY 1
  "
)
fwrite(episode_diagnostics, "../output/renthub_episode_diagnostics.csv")

message("Writing monthly diagnostics and redacted collision examples...")
floorplan_month_diagnostics <- collect_query(
  "
  SELECT
    year,
    COUNT(*) AS floorplan_months,
    SUM(raw_rows_month) AS raw_rows_month,
    QUANTILE_CONT(active_days, 0.50) AS active_days_p50,
    QUANTILE_CONT(active_days, 0.90) AS active_days_p90,
    QUANTILE_CONT(active_days, 0.99) AS active_days_p99,
    MAX(active_days) AS active_days_max,
    AVG(CASE WHEN active_days >= 20 THEN 1.0 ELSE 0.0 END) AS share_20plus_active_days,
    AVG(CASE WHEN distinct_daily_rents = 1 THEN 1.0 ELSE 0.0 END) AS share_one_rent_months,
    AVG(CASE WHEN distinct_daily_rents > 1 THEN 1.0 ELSE 0.0 END) AS share_multi_rent_months,
    AVG(CASE WHEN multi_rent_days > 0 THEN 1.0 ELSE 0.0 END) AS share_any_multi_rent_day,
    AVG(address_missing) AS share_address_missing,
    QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
    QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
    QUANTILE_CONT(rent_price, 0.99) AS rent_p99
  FROM floorplan_month_main
  GROUP BY 1
  ORDER BY 1
  "
)
fwrite(floorplan_month_diagnostics, "../output/renthub_floorplan_month_diagnostics.csv")

monthly_series <- collect_query(
  "
  SELECT
    month_start,
    COUNT(*) AS floorplan_months,
    SUM(raw_rows_month) AS raw_rows_month,
    AVG(active_days) AS mean_active_days,
    QUANTILE_CONT(active_days, 0.50) AS active_days_p50,
    QUANTILE_CONT(rent_price, 0.25) AS rent_p25,
    QUANTILE_CONT(rent_price, 0.50) AS rent_p50,
    QUANTILE_CONT(rent_price, 0.75) AS rent_p75,
    AVG(rent_price) AS rent_mean,
    AVG(CASE WHEN building_type_clean = 'multi_family' THEN 1.0 ELSE 0.0 END) AS share_multi_family,
    AVG(CASE WHEN building_type_clean = 'single_family' THEN 1.0 ELSE 0.0 END) AS share_single_family,
    AVG(CASE WHEN building_type_clean = 'condo' THEN 1.0 ELSE 0.0 END) AS share_condo,
    AVG(CASE WHEN building_type_clean = 'townhouse' THEN 1.0 ELSE 0.0 END) AS share_townhouse,
    AVG(CASE WHEN building_type_clean = 'commercial' THEN 1.0 ELSE 0.0 END) AS share_commercial,
    AVG(address_missing) AS share_address_missing,
    AVG(CASE WHEN multi_rent_days > 0 THEN 1.0 ELSE 0.0 END) AS share_any_multi_rent_day
  FROM floorplan_month_main
  GROUP BY 1
  ORDER BY 1
  "
)
fwrite(monthly_series, "../output/renthub_monthly_series.csv")

collision_examples <- collect_query(
  "
  WITH collision_candidates AS (
    SELECT
      f.year,
      f.file_date,
      SUBSTR(MD5(COALESCE(f.property_key, 'missing')), 1, 16) AS property_hash,
      SUBSTR(MD5(COALESCE(f.floorplan_key, 'missing')), 1, 16) AS floorplan_hash,
      f.building_type_clean,
      f.beds,
      f.baths,
      f.sqft,
      f.address_missing,
      f.raw_rows_day,
      f.rent_values_day,
      f.rent_cells_day,
      f.rent_min,
      f.rent_price AS rent_p50,
      f.rent_max,
      p.floorplans AS property_day_floorplans,
      p.rent_cells AS property_day_rent_cells,
      p.raw_rows AS property_day_raw_rows
    FROM floorplan_day_clean f
    LEFT JOIN property_day_base p
      ON f.property_key = p.property_key
      AND f.file_date = p.file_date
    WHERE f.main_day_flag = 1
      AND (
        f.multi_rent_day = 1
        OR p.floorplans >= 10
        OR p.rent_cells >= 10
      )
  )
  SELECT *
  FROM collision_candidates
  ORDER BY property_day_raw_rows DESC NULLS LAST,
    raw_rows_day DESC,
    rent_values_day DESC
  LIMIT 250
  "
)
fwrite(collision_examples, "../output/renthub_collision_examples.csv")

drop_reasons <- collect_query(
  "
  WITH reason_rows AS (
    SELECT 'raw_chicago_rows' AS reason, COUNT(*) AS n_rows FROM keyed_rows
    UNION ALL
    SELECT 'missing_or_invalid_coordinates', COUNT(*) FROM keyed_rows WHERE valid_coordinates = 0
    UNION ALL
    SELECT 'outside_chicago_bbox_days', COUNT(*) FROM floorplan_day_clean WHERE keep_chicago_bbox = 0
    UNION ALL
    SELECT 'rent_nonpositive_or_outlier_days', COUNT(*) FROM floorplan_day_clean WHERE keep_rent_trim01 = 0
    UNION ALL
    SELECT 'sqft_outlier_days', COUNT(*) FROM floorplan_day_clean WHERE keep_sqft_trim01 = 0
    UNION ALL
    SELECT 'main_floorplan_days_kept', COUNT(*) FROM floorplan_day_clean WHERE main_day_flag = 1
    UNION ALL
    SELECT 'multi_rent_days_retained_main', COUNT(*) FROM floorplan_day_clean WHERE main_day_flag = 1 AND multi_rent_day = 1
    UNION ALL
    SELECT 'multi_rent_days_dropped_only_in_robustness', COUNT(*) FROM floorplan_day_clean WHERE main_day_flag = 1 AND multi_rent_day = 1
    UNION ALL
    SELECT 'main_floorplan_months', COUNT(*) FROM floorplan_month_main
    UNION ALL
    SELECT 'drop_multi_rent_floorplan_months', COUNT(*) FROM floorplan_month_drop_multi
  )
  SELECT * FROM reason_rows
  "
)
fwrite(drop_reasons, "../output/renthub_drop_reasons.csv")

drop_reasons_by_year <- collect_query(
  "
  WITH reason_rows AS (
    SELECT CAST(YEAR(file_date) AS INTEGER) AS year, 'raw_chicago_rows' AS reason, COUNT(*) AS n_rows
    FROM keyed_rows
    GROUP BY 1
    UNION ALL
    SELECT CAST(YEAR(file_date) AS INTEGER), 'missing_or_invalid_coordinates', COUNT(*)
    FROM keyed_rows
    WHERE valid_coordinates = 0
    GROUP BY 1
    UNION ALL
    SELECT year, 'outside_chicago_bbox_days', COUNT(*)
    FROM floorplan_day_clean
    WHERE keep_chicago_bbox = 0
    GROUP BY 1
    UNION ALL
    SELECT year, 'rent_nonpositive_or_outlier_days', COUNT(*)
    FROM floorplan_day_clean
    WHERE keep_rent_trim01 = 0
    GROUP BY 1
    UNION ALL
    SELECT year, 'sqft_outlier_days', COUNT(*)
    FROM floorplan_day_clean
    WHERE keep_sqft_trim01 = 0
    GROUP BY 1
    UNION ALL
    SELECT year, 'main_floorplan_days_kept', COUNT(*)
    FROM floorplan_day_clean
    WHERE main_day_flag = 1
    GROUP BY 1
    UNION ALL
    SELECT year, 'multi_rent_days_retained_main', COUNT(*)
    FROM floorplan_day_clean
    WHERE main_day_flag = 1 AND multi_rent_day = 1
    GROUP BY 1
    UNION ALL
    SELECT year, 'multi_rent_days_dropped_only_in_robustness', COUNT(*)
    FROM floorplan_day_clean
    WHERE main_day_flag = 1 AND multi_rent_day = 1
    GROUP BY 1
    UNION ALL
    SELECT year, 'main_floorplan_months', COUNT(*)
    FROM floorplan_month_main
    GROUP BY 1
    UNION ALL
    SELECT year, 'drop_multi_rent_floorplan_months', COUNT(*)
    FROM floorplan_month_drop_multi
    GROUP BY 1
  )
  SELECT * FROM reason_rows
  ORDER BY year, reason
  "
)
fwrite(drop_reasons_by_year, "../output/renthub_drop_reasons_by_year.csv")

processing_diagnostics <- data.table(
  metric = c(
    "raw_files",
    "raw_illinois_rows_in_window",
    "raw_chicago_rows",
    "valid_coordinate_rows",
    "valid_address_rows",
    "address_zero_rows",
    "property_day_rows",
    "floorplan_day_rows",
    "main_floorplan_day_rows",
    "main_floorplan_month_rows",
    "drop_multi_rent_floorplan_month_rows",
    "episode_robustness_rows",
    "distinct_property_keys",
    "distinct_floorplan_keys",
    "main_address_missing_share",
    "main_any_multi_rent_day_share"
  ),
  value = c(
    length(raw_files),
    sum(city_filter_diagnostics$n_rows),
    raw_count,
    collect_query("SELECT COUNT(*) AS n FROM keyed_rows WHERE valid_coordinates = 1")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM keyed_rows WHERE address_norm IS NOT NULL")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM keyed_rows WHERE address_zero_flag = 1")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM property_day_base")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM floorplan_day_clean")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM floorplan_day_clean WHERE main_day_flag = 1")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM floorplan_month_main")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM floorplan_month_drop_multi")$n[1],
    collect_query("SELECT COUNT(*) AS n FROM episode_robustness")$n[1],
    collect_query("SELECT COUNT(DISTINCT property_key) AS n FROM keyed_rows")$n[1],
    collect_query("SELECT COUNT(DISTINCT floorplan_key) AS n FROM keyed_rows")$n[1],
    collect_query("SELECT AVG(address_missing) AS n FROM floorplan_month_main")$n[1],
    collect_query("SELECT AVG(CASE WHEN multi_rent_days > 0 THEN 1.0 ELSE 0.0 END) AS n FROM floorplan_month_main")$n[1]
  )
)
fwrite(processing_diagnostics, "../output/renthub_processing_diagnostics.csv")

message("Running built-in validation checks...")
duplicate_months <- collect_query(
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

address_zero_valid <- collect_query(
  "SELECT COUNT(*) AS n FROM floorplan_month_main WHERE address_norm = '0'"
)$n[1]
if (address_zero_valid > 0) {
  stop("ADDRESS=0 survived as a valid address in the main rent panel.", call. = FALSE)
}

con_mapping <- collect_query(
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

townhouse_mapping <- collect_query(
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

month_span <- collect_query(
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

message(sprintf(
  "Wrote %s main floorplan-month rows and %s drop-multi-rent robustness rows.",
  format(collect_query("SELECT COUNT(*) AS n FROM floorplan_month_main")$n[1], big.mark = ","),
  format(collect_query("SELECT COUNT(*) AS n FROM floorplan_month_drop_multi")$n[1], big.mark = ",")
))
message("RentHub cleaning complete.")
