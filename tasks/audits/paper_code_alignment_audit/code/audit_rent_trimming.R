# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE rental_days AS
  WITH raw_source AS (
    SELECT
      UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) AS unit_id_raw,
      UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) AS address_raw,
      UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) AS building_type_raw,
      TRY_CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
      TRY_CAST(RENT_PRICE AS DOUBLE) AS rent_price,
      TRY_CAST(BEDS AS DOUBLE) AS beds,
      TRY_CAST(BATHS AS DOUBLE) AS baths,
      TRY_CAST(SQFT AS DOUBLE) AS sqft,
      TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
      TRY_CAST(LONGITUDE AS DOUBLE) AS longitude
    FROM read_parquet('../../../download_rent_data/output/*.parquet', union_by_name = true)
    WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) >= DATE '2014-01-01'
      AND TRY_CAST(SCRAPED_TIMESTAMP AS DATE) <= DATE '2022-12-31'
      AND UPPER(TRIM(CAST(CITY AS VARCHAR))) IN ('CHICAGO', 'CHGO')
  ),
  cleaned AS (
    SELECT
      CASE
        WHEN unit_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE unit_id_raw
      END AS unit_id,
      CASE
        WHEN address_raw IS NULL OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN')
          THEN NULL
        ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
      END AS address_norm,
      CASE
        WHEN building_type_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 'other'
        WHEN building_type_raw = 'TH' OR building_type_raw LIKE '%TOWN%' THEN 'townhouse'
        WHEN building_type_raw IN ('CON', 'CONDO') OR building_type_raw LIKE '%CONDO%'
          OR building_type_raw LIKE '%CONDOMINIUM%' THEN 'condo'
        WHEN building_type_raw IN ('COMM', 'COMMERCIAL') OR building_type_raw LIKE '%COMMERCIAL%' THEN 'commercial'
        WHEN building_type_raw LIKE '%MULTI%'
          OR building_type_raw LIKE '%APART%'
          OR building_type_raw LIKE '%APT%'
          OR building_type_raw LIKE '%DUPLEX%'
          OR building_type_raw LIKE '%TRIPLEX%'
          OR building_type_raw LIKE '%FOURPLEX%' THEN 'multi_family'
        WHEN building_type_raw LIKE '%SINGLE%'
          OR building_type_raw LIKE '%HOUSE%'
          OR building_type_raw LIKE '%DETACHED%'
          OR building_type_raw LIKE '%SFR%' THEN 'single_family'
        ELSE 'other'
      END AS building_type_clean,
      file_date,
      rent_price,
      beds,
      baths,
      sqft,
      latitude,
      longitude,
      CASE
        WHEN latitude IS NOT NULL AND longitude IS NOT NULL
          AND latitude BETWEEN -90 AND 90 AND longitude BETWEEN -180 AND 180
          THEN TRUE ELSE FALSE
      END AS valid_coordinates
    FROM raw_source
  ),
  keys AS (
    SELECT
      *,
      CASE
        WHEN valid_coordinates THEN
          COALESCE(address_norm, 'NO_ADDRESS') || '|' || PRINTF('%.4f|%.4f', latitude, longitude)
        ELSE NULL
      END AS property_key
    FROM cleaned
  ),
  analysis_keys AS (
    SELECT
      *,
      CASE
        WHEN unit_id IS NOT NULL THEN unit_id
        WHEN property_key IS NOT NULL THEN property_key || '|' ||
          COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' ||
          COALESCE(building_type_clean, 'other')
        ELSE NULL
      END AS analysis_key
    FROM keys
  )
  SELECT
    file_date,
    CAST(YEAR(file_date) AS INTEGER) AS year,
    analysis_key,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_price,
    QUANTILE_CONT(latitude, 0.50) FILTER (WHERE latitude IS NOT NULL) AS latitude,
    QUANTILE_CONT(longitude, 0.50) FILTER (WHERE longitude IS NOT NULL) AS longitude,
    QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
    QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
    COUNT(*) AS raw_rows_day
  FROM analysis_keys
  WHERE analysis_key IS NOT NULL
    AND valid_coordinates
  GROUP BY 1, 2, 3
  "
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE rental_day_flags AS
  WITH thresholds AS (
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
    FROM rental_days
    GROUP BY 1, 2
  )
  SELECT
    d.*,
    DATE_TRUNC('month', d.file_date)::DATE AS month_start,
    d.latitude BETWEEN 41.55 AND 42.10
      AND d.longitude BETWEEN -88.10 AND -87.40 AS keep_chicago_bbox,
    d.rent_price IS NOT NULL
      AND d.rent_price > 0
      AND d.rent_price >= COALESCE(t.rent_p01, d.rent_price)
      AND d.rent_price <= COALESCE(t.rent_p99, d.rent_price) AS keep_rent_trim01,
    d.sqft IS NULL OR (
      d.sqft > 0
      AND d.sqft >= COALESCE(t.sqft_p01, d.sqft)
      AND d.sqft <= COALESCE(t.sqft_p99, d.sqft)
    ) AS keep_sqft_trim01,
    t.rent_p01,
    t.rent_p99,
    t.sqft_p01,
    t.sqft_p99
  FROM rental_days d
  LEFT JOIN thresholds t
    ON d.year = t.year
    AND CASE
      WHEN d.beds IS NULL THEN 'missing'
      WHEN d.beds >= 4 THEN '4plus'
      ELSE CAST(CAST(d.beds AS INTEGER) AS VARCHAR)
    END = t.beds_bin
  "
))

summary <- dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS floorplan_day_rows,
    SUM(raw_rows_day) AS represented_raw_rows,
    COUNT(*) FILTER (WHERE keep_chicago_bbox) AS chicago_bbox_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND rent_price > 0) AS positive_rent_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01) AS rent_trimmed_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01 AND keep_sqft_trim01) AS production_main_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND rent_price > 0 AND NOT keep_rent_trim01) AS dropped_rent_percentile_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01 AND NOT keep_sqft_trim01) AS dropped_sqft_days,
    COUNT(DISTINCT analysis_key || '|' || CAST(month_start AS VARCHAR)) FILTER (
      WHERE keep_chicago_bbox AND keep_rent_trim01 AND keep_sqft_trim01
    ) AS production_month_rows
  FROM rental_day_flags
  "
)
production_panel_n <- nrow(read_parquet("../../../process_rent_data/output/chicago_rent_panel.parquet"))
summary$actual_production_panel_rows <- production_panel_n
summary$month_row_difference <- summary$production_month_rows - production_panel_n
write_csv(as_tibble(summary), "../output/rental_trimming_summary.csv")

by_year <- dbGetQuery(
  con,
  "
  SELECT
    year,
    COUNT(*) AS floorplan_day_rows,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND rent_price > 0) AS positive_rent_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01) AS rent_trimmed_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01 AND keep_sqft_trim01) AS production_main_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND rent_price > 0 AND NOT keep_rent_trim01) AS dropped_rent_percentile_days,
    COUNT(*) FILTER (WHERE keep_chicago_bbox AND keep_rent_trim01 AND NOT keep_sqft_trim01) AS dropped_sqft_days
  FROM rental_day_flags
  GROUP BY 1
  ORDER BY 1
  "
)
write_csv(as_tibble(by_year), "../output/rental_trimming_by_year.csv")
