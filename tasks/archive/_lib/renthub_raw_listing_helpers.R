create_renthub_raw_day_tables <- function(con, raw_glob, analysis_start_month) {
  dbExecute(
    con,
    sprintf(
      "
      CREATE OR REPLACE TEMP VIEW chicago_raw AS
      SELECT
        CAST(DATE_TRUNC('month', CAST(SCRAPED_TIMESTAMP AS DATE)) AS DATE) AS month_start,
        CAST(SCRAPED_TIMESTAMP AS TIMESTAMP) AS scraped_timestamp,
        CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
        CAST(DATE_POSTED AS DATE) AS posted_date,
        CAST(AVAILABLE_AT AS DATE) AS available_date,
        CAST(RENT_PRICE AS DOUBLE) AS rent_price,
        CAST(BEDS AS DOUBLE) AS beds,
        CAST(BATHS AS DOUBLE) AS baths,
        CAST(SQFT AS DOUBLE) AS sqft,
        CAST(LATITUDE AS DOUBLE) AS latitude,
        CAST(LONGITUDE AS DOUBLE) AS longitude,
        CAST(ID AS VARCHAR) AS id,
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
        CASE
          WHEN UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
          WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDO%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%CONDOMINIUM%%' THEN 'condo'
          WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%MULTI%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APART%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DUPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TRIPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%FOURPLEX%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%APT%%' THEN 'multi_family'
          WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SINGLE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%HOUSE%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%DETACHED%%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%SFR%%' THEN 'single_family'
          WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%%TOWN%%' OR UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) = 'TH' THEN 'townhouse'
          ELSE 'other'
        END AS building_type_clean,
        CASE
          WHEN LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL THEN PRINTF('%%.7f|%%.7f', CAST(LATITUDE AS DOUBLE), CAST(LONGITUDE AS DOUBLE))
          ELSE NULL
        END AS coord_key,
        CASE
          WHEN UNIT_ID IS NULL OR UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN
            NULLIF(
              COALESCE(NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(ADDRESS AS VARCHAR))), ' +', ' '), ''), '') || '|' ||
              COALESCE(CASE WHEN LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL THEN PRINTF('%%.4f|%%.4f', CAST(LATITUDE AS DOUBLE), CAST(LONGITUDE AS DOUBLE)) END, '') || '|' ||
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
        AND CAST(SCRAPED_TIMESTAMP AS DATE) >= CAST('%s' AS DATE)
      ",
      duck_escape(raw_glob),
      format(analysis_start_month, "%Y-%m-%d")
    )
  )

  dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE series_day_base AS
    WITH keyed AS (
      SELECT
        month_start,
        file_date,
        CAST(YEAR(file_date) AS INTEGER) AS year,
        posted_date,
        available_date,
        property_id,
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
        rent_price,
        beds,
        baths,
        sqft,
        latitude,
        longitude,
        coord_key,
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
        MIN(id) AS id,
        MIN(property_id) AS property_id,
        MIN(unit_id) AS unit_id,
        MIN(fingerprint_key) AS fingerprint_key,
        MIN(posted_date) AS posted_date,
        MIN(available_date) AS available_date,
        QUANTILE_CONT(latitude, 0.50) FILTER (WHERE latitude IS NOT NULL) AS latitude,
        QUANTILE_CONT(longitude, 0.50) FILTER (WHERE longitude IS NOT NULL) AS longitude,
        MIN(coord_key) AS coord_key,
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
        QUANTILE_CONT(rent_price, 0.025) AS rent_p025,
        QUANTILE_CONT(rent_price, 0.975) AS rent_p975,
        QUANTILE_CONT(rent_price, 0.05) AS rent_p05,
        QUANTILE_CONT(rent_price, 0.95) AS rent_p95,
        QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99,
        QUANTILE_CONT(sqft, 0.025) FILTER (WHERE sqft > 0) AS sqft_p025,
        QUANTILE_CONT(sqft, 0.975) FILTER (WHERE sqft > 0) AS sqft_p975,
        QUANTILE_CONT(sqft, 0.05) FILTER (WHERE sqft > 0) AS sqft_p05,
        QUANTILE_CONT(sqft, 0.95) FILTER (WHERE sqft > 0) AS sqft_p95
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
        QUANTILE_CONT(rent_price, 0.025) AS rent_p025,
        QUANTILE_CONT(rent_price, 0.975) AS rent_p975,
        QUANTILE_CONT(rent_price, 0.05) AS rent_p05,
        QUANTILE_CONT(rent_price, 0.95) AS rent_p95,
        QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99,
        QUANTILE_CONT(sqft, 0.025) FILTER (WHERE sqft > 0) AS sqft_p025,
        QUANTILE_CONT(sqft, 0.975) FILTER (WHERE sqft > 0) AS sqft_p975,
        QUANTILE_CONT(sqft, 0.05) FILTER (WHERE sqft > 0) AS sqft_p05,
        QUANTILE_CONT(sqft, 0.95) FILTER (WHERE sqft > 0) AS sqft_p95
      FROM day_collapsed
      WHERE n_rents_day = 1
      GROUP BY 1
    )
    SELECT
      d.*,
      CASE WHEN d.n_rents_day = 1 THEN 1 ELSE 0 END AS keep_same_day,
      CASE
        WHEN d.rent_price >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p01 END, y.rent_p01)
         AND d.rent_price <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p99 END, y.rent_p99) THEN 1 ELSE 0
      END AS keep_rent_trim01,
      CASE
        WHEN d.rent_price >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p025 END, y.rent_p025)
         AND d.rent_price <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p975 END, y.rent_p975) THEN 1 ELSE 0
      END AS keep_rent_trim025,
      CASE
        WHEN d.rent_price >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p05 END, y.rent_p05)
         AND d.rent_price <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p95 END, y.rent_p95) THEN 1 ELSE 0
      END AS keep_rent_trim05,
      CASE
        WHEN d.sqft IS NULL OR d.sqft <= 0 THEN 1
        WHEN d.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01)
         AND d.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) THEN 1 ELSE 0
      END AS keep_sqft_trim01,
      CASE
        WHEN d.sqft IS NULL OR d.sqft <= 0 THEN 1
        WHEN d.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p025 END, y.sqft_p025)
         AND d.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p975 END, y.sqft_p975) THEN 1 ELSE 0
      END AS keep_sqft_trim025,
      CASE
        WHEN d.sqft IS NULL OR d.sqft <= 0 THEN 1
        WHEN d.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p05 END, y.sqft_p05)
         AND d.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p95 END, y.sqft_p95) THEN 1 ELSE 0
      END AS keep_sqft_trim05
    FROM day_collapsed d
    LEFT JOIN cell_thresholds c
      ON d.year = c.year
     AND COALESCE(CAST(ROUND(d.beds) AS INTEGER), -1) = c.bed_cell
    LEFT JOIN year_thresholds y
      ON d.year = y.year
    "
  )
}

create_renthub_gap_cycles <- function(con, input_table, output_table, gap_days, require_same_day = TRUE, value_col = "rent_price") {
  same_day_clause <- if (require_same_day) {
    "WHERE keep_same_day = 1"
  } else {
    ""
  }

  dbExecute(
    con,
    sprintf(
      "
      CREATE OR REPLACE TEMP TABLE %s AS
      WITH base AS (
        SELECT *
        FROM %s
        %s
      ),
      counts AS (
        SELECT
          *,
          LAG(file_date) OVER (PARTITION BY analysis_key ORDER BY file_date, last_scraped_timestamp, %s) AS prev_file_date
        FROM base
      ),
      cycles AS (
        SELECT
          *,
          CASE
            WHEN prev_file_date IS NULL OR DATE_DIFF('day', prev_file_date, file_date) >= %d THEN 1 ELSE 0
          END AS cycle_start_flag,
          SUM(
            CASE
              WHEN prev_file_date IS NULL OR DATE_DIFF('day', prev_file_date, file_date) >= %d THEN 1 ELSE 0
            END
          ) OVER (
            PARTITION BY analysis_key
            ORDER BY file_date, last_scraped_timestamp, %s
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
          ) AS cycle_id
        FROM counts
      )
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, cycle_id
          ORDER BY file_date, last_scraped_timestamp, %s
        ) AS cycle_row_number
      FROM cycles
      ",
      output_table,
      input_table,
      same_day_clause,
      value_col,
      as.integer(gap_days),
      as.integer(gap_days),
      value_col,
      value_col
    )
  )
}
