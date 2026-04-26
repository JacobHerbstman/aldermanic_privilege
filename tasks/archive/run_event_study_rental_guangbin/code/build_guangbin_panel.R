source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_guangbin/code")
# panel_path <- "../input/rental_listing_panel_2023.parquet"
# raw_dir <- "../input/renthub_raw"
# trim_summary_path <- "../input/renthub_trim_sensitivity_summary.csv"
# panel_out <- "../output/rental_listing_panel_2023_guangbin.parquet"
# summary_out <- "../output/rental_listing_panel_2023_guangbin_summary.csv"
# diagnostic_out <- "../output/rental_listing_panel_2023_guangbin_trim_diagnostic.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(panel_path, raw_dir, trim_summary_path, panel_out, summary_out, diagnostic_out)
}

if (length(args) != 6) {
  stop(
    "FATAL: Script requires args: <panel_path> <raw_dir> <trim_summary_path> <panel_out> <summary_out> <diagnostic_out>",
    call. = FALSE
  )
}

panel_path <- args[1]
raw_dir <- args[2]
trim_summary_path <- args[3]
panel_out <- args[4]
summary_out <- args[5]
diagnostic_out <- args[6]

trim_summary <- fread(trim_summary_path)
chosen_trim_spec <- trim_summary[is_chosen == TRUE | is_chosen == 1, chosen_trim_spec][1]
if (!chosen_trim_spec %in% c("strict_p1", "strict_p25")) {
  message("Trim summary does not point to strict_p1/strict_p25; defaulting to strict_p1 for Guangbin-style 1/99 cleaning.")
  chosen_trim_spec <- "strict_p1"
}

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")

message("Preparing raw keyed join table...")
dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE raw_keyed AS
    WITH raw_norm AS (
      SELECT
        CAST(ID AS VARCHAR) AS id,
        CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
        CASE
          WHEN UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
          ELSE UPPER(TRIM(CAST(UNIT_ID AS VARCHAR)))
        END AS unit_id_norm,
        CASE
          WHEN UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
          ELSE UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR)))
        END AS property_id_norm,
        CASE
          WHEN UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
          ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(ADDRESS AS VARCHAR))), ' +', ' '), '')
        END AS address_norm,
        CASE
          WHEN UPPER(TRIM(CAST(COMPANY AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
          ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(COMPANY AS VARCHAR))), ' +', ' '), '')
        END AS company_norm,
        CAST(LATITUDE AS DOUBLE) AS latitude,
        CAST(LONGITUDE AS DOUBLE) AS longitude
      FROM read_parquet('%s', union_by_name = true)
      WHERE UPPER(TRIM(CAST(CITY AS VARCHAR))) = 'CHICAGO'
        AND ID IS NOT NULL
        AND SCRAPED_TIMESTAMP IS NOT NULL
    )
    SELECT
      id,
      file_date,
      CASE WHEN COUNT(DISTINCT unit_id_norm) FILTER (WHERE unit_id_norm IS NOT NULL) = 1 THEN MIN(unit_id_norm) FILTER (WHERE unit_id_norm IS NOT NULL) ELSE NULL END AS unit_id,
      CASE WHEN COUNT(DISTINCT property_id_norm) FILTER (WHERE property_id_norm IS NOT NULL) = 1 THEN MIN(property_id_norm) FILTER (WHERE property_id_norm IS NOT NULL) ELSE NULL END AS property_id,
      CASE WHEN COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) >= 1 THEN MIN(address_norm) FILTER (WHERE address_norm IS NOT NULL) ELSE NULL END AS address_norm,
      CASE WHEN COUNT(DISTINCT company_norm) FILTER (WHERE company_norm IS NOT NULL) >= 1 THEN MIN(company_norm) FILTER (WHERE company_norm IS NOT NULL) ELSE NULL END AS company_norm,
      AVG(latitude) FILTER (WHERE latitude IS NOT NULL) AS latitude,
      AVG(longitude) FILTER (WHERE longitude IS NOT NULL) AS longitude,
      COUNT(*) AS raw_match_rows,
      COUNT(DISTINCT unit_id_norm) FILTER (WHERE unit_id_norm IS NOT NULL) AS raw_match_unit_ids
    FROM raw_norm
    GROUP BY 1, 2
    ",
    gsub("'", "''", file.path(raw_dir, "*.parquet"))
  )
)

message("Building Guangbin-style 2023 cohort panel...")
panel_path_sql <- gsub("'", "''", panel_path)
chosen_trim_spec_sql <- gsub("'", "''", chosen_trim_spec)
dbExecute(
  con,
  paste0(
    "
    CREATE OR REPLACE TEMP TABLE panel_base AS
    SELECT
      ROW_NUMBER() OVER (
        ORDER BY
          CAST(p.file_date AS DATE),
          CAST(p.id AS VARCHAR),
          COALESCE(CAST(p.block_id AS VARCHAR), ''),
          COALESCE(CAST(p.segment_side AS VARCHAR), ''),
          COALESCE(CAST(p.segment_id_cohort AS VARCHAR), ''),
          COALESCE(CAST(p.dist_ft AS DOUBLE), 0),
          COALESCE(CAST(p.rent_price AS DOUBLE), 0),
          COALESCE(CAST(p.sqft AS DOUBLE), 0),
          COALESCE(CAST(p.beds AS DOUBLE), 0),
          COALESCE(CAST(p.baths AS DOUBLE), 0),
          COALESCE(CAST(p.building_type_clean AS VARCHAR), '')
      ) AS panel_row_id,
      p.*,
      r.unit_id,
      r.property_id,
      r.address_norm,
      r.company_norm,
      r.latitude,
      r.longitude,
      r.raw_match_rows,
      r.raw_match_unit_ids,
      COALESCE(CAST(ROUND(p.beds) AS INTEGER), -1) AS bed_cell,
      CASE
        WHEN r.unit_id IS NOT NULL THEN 'unit_id'
        ELSE 'fingerprint'
      END AS key_source,
      CASE
        WHEN p.rent_price > 0 AND p.sqft > 0 THEN p.rent_price / p.sqft
        ELSE NULL
      END AS rent_per_sqft,
      CASE
        WHEN r.unit_id IS NOT NULL THEN r.unit_id
        ELSE NULLIF(
          COALESCE(r.address_norm, '') || '|' ||
          COALESCE(CASE
            WHEN r.latitude IS NOT NULL AND r.longitude IS NOT NULL THEN CAST(ROUND(r.latitude, 4) AS VARCHAR) || '|' || CAST(ROUND(r.longitude, 4) AS VARCHAR)
            ELSE NULL
          END, '') || '|' ||
          COALESCE(CAST(CAST(p.beds AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(CAST(CAST(p.baths AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(CAST(CAST(p.sqft AS DOUBLE) AS VARCHAR), '') || '|' ||
          COALESCE(p.building_type_clean, ''),
          '|||||'
        )
      END AS analysis_key
    FROM read_parquet('", panel_path_sql, "') p
    LEFT JOIN raw_keyed r
      ON CAST(p.id AS VARCHAR) = r.id
     AND CAST(p.file_date AS DATE) = r.file_date
    WHERE p.year BETWEEN 2021 AND 2025
  "
  )
)
panel_sql <- paste0(
  "
    CREATE OR REPLACE TEMP TABLE guangbin_panel AS
    WITH winsor_cell_thresholds AS (
      SELECT
        year,
        bed_cell,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
        QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
        QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99
      FROM panel_base
      WHERE rent_price > 0
      GROUP BY 1, 2
    ),
    winsor_year_thresholds AS (
      SELECT
        year,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_price, 0.01) AS rent_p01,
        QUANTILE_CONT(rent_price, 0.99) AS rent_p99,
        QUANTILE_CONT(sqft, 0.01) FILTER (WHERE sqft > 0) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) FILTER (WHERE sqft > 0) AS sqft_p99
      FROM panel_base
      WHERE rent_price > 0
      GROUP BY 1
    ),
    panel_winsor AS (
      SELECT
        b.*,
        COUNT(*) OVER (PARTITION BY b.analysis_key, b.year) AS postings_per_key_year,
        CASE
          WHEN b.analysis_key IS NULL THEN NULL
          ELSE 1.0 / COUNT(*) OVER (PARTITION BY b.analysis_key, b.year)
        END AS posting_weight,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p01 END, y.rent_p01) AS rent_lo,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p99 END, y.rent_p99) AS rent_hi,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01) AS sqft_lo,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) AS sqft_hi,
        CASE
          WHEN b.rent_price IS NULL THEN NULL
          WHEN COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p01 END, y.rent_p01) IS NULL THEN b.rent_price
          ELSE GREATEST(COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p01 END, y.rent_p01), LEAST(b.rent_price, COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_p99 END, y.rent_p99)))
        END AS rent_price_winsor,
        CASE
          WHEN b.sqft IS NULL OR b.sqft <= 0 THEN b.sqft
          WHEN COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01) IS NULL THEN b.sqft
          ELSE GREATEST(COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01), LEAST(b.sqft, COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99)))
        END AS sqft_winsor
      FROM panel_base b
      LEFT JOIN winsor_cell_thresholds c
        ON b.year = c.year
       AND b.bed_cell = c.bed_cell
      LEFT JOIN winsor_year_thresholds y
        ON b.year = y.year
    ),
    trim_sqft_cell_thresholds AS (
      SELECT
        year,
        bed_cell,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(sqft, 0.01) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) AS sqft_p99,
        QUANTILE_CONT(sqft, 0.025) AS sqft_p025,
        QUANTILE_CONT(sqft, 0.975) AS sqft_p975
      FROM panel_winsor
      WHERE analysis_key IS NOT NULL
        AND rent_price > 0
        AND sqft > 0
        AND rent_per_sqft IS NOT NULL
      GROUP BY 1, 2
    ),
    trim_sqft_year_thresholds AS (
      SELECT
        year,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(sqft, 0.01) AS sqft_p01,
        QUANTILE_CONT(sqft, 0.99) AS sqft_p99,
        QUANTILE_CONT(sqft, 0.025) AS sqft_p025,
        QUANTILE_CONT(sqft, 0.975) AS sqft_p975
      FROM panel_winsor
      WHERE analysis_key IS NOT NULL
        AND rent_price > 0
        AND sqft > 0
        AND rent_per_sqft IS NOT NULL
      GROUP BY 1
    ),
    panel_sqft_flagged AS (
      SELECT
        b.*,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01) AS trim_sqft_lo_p1,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) AS trim_sqft_hi_p1,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p025 END, y.sqft_p025) AS trim_sqft_lo_p25,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p975 END, y.sqft_p975) AS trim_sqft_hi_p25,
        CASE
          WHEN b.analysis_key IS NOT NULL
           AND b.rent_price > 0
           AND b.sqft > 0
           AND b.rent_per_sqft IS NOT NULL
           AND b.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p01 END, y.sqft_p01)
           AND b.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p99 END, y.sqft_p99) THEN 1 ELSE 0
        END AS keep_sqft_trim_p1,
        CASE
          WHEN b.analysis_key IS NOT NULL
           AND b.rent_price > 0
           AND b.sqft > 0
           AND b.rent_per_sqft IS NOT NULL
           AND b.sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p025 END, y.sqft_p025)
           AND b.sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_p975 END, y.sqft_p975) THEN 1 ELSE 0
        END AS keep_sqft_trim_p25
      FROM panel_winsor b
      LEFT JOIN trim_sqft_cell_thresholds c
        ON b.year = c.year
       AND b.bed_cell = c.bed_cell
      LEFT JOIN trim_sqft_year_thresholds y
        ON b.year = y.year
    ),
    trim_rpsf_cell_thresholds_p1 AS (
      SELECT
        year,
        bed_cell,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_per_sqft, 0.01) AS rpsf_p01,
        QUANTILE_CONT(rent_per_sqft, 0.99) AS rpsf_p99
      FROM panel_sqft_flagged
      WHERE keep_sqft_trim_p1 = 1
      GROUP BY 1, 2
    ),
    trim_rpsf_year_thresholds_p1 AS (
      SELECT
        year,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_per_sqft, 0.01) AS rpsf_p01,
        QUANTILE_CONT(rent_per_sqft, 0.99) AS rpsf_p99
      FROM panel_sqft_flagged
      WHERE keep_sqft_trim_p1 = 1
      GROUP BY 1
    ),
    panel_rpsf_flagged_p1 AS (
      SELECT
        b.panel_row_id,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p01 END, y.rpsf_p01) AS trim_rpsf_lo_p1,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p99 END, y.rpsf_p99) AS trim_rpsf_hi_p1,
        CASE
          WHEN b.rent_per_sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p01 END, y.rpsf_p01)
           AND b.rent_per_sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p99 END, y.rpsf_p99) THEN 1 ELSE 0
        END AS keep_trim_p1
      FROM panel_sqft_flagged b
      LEFT JOIN trim_rpsf_cell_thresholds_p1 c
        ON b.year = c.year
       AND b.bed_cell = c.bed_cell
      LEFT JOIN trim_rpsf_year_thresholds_p1 y
        ON b.year = y.year
      WHERE b.keep_sqft_trim_p1 = 1
    ),
    trim_rpsf_cell_thresholds_p25 AS (
      SELECT
        year,
        bed_cell,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_per_sqft, 0.025) AS rpsf_p025,
        QUANTILE_CONT(rent_per_sqft, 0.975) AS rpsf_p975
      FROM panel_sqft_flagged
      WHERE keep_sqft_trim_p25 = 1
      GROUP BY 1, 2
    ),
    trim_rpsf_year_thresholds_p25 AS (
      SELECT
        year,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_per_sqft, 0.025) AS rpsf_p025,
        QUANTILE_CONT(rent_per_sqft, 0.975) AS rpsf_p975
      FROM panel_sqft_flagged
      WHERE keep_sqft_trim_p25 = 1
      GROUP BY 1
    ),
    panel_rpsf_flagged_p25 AS (
      SELECT
        b.panel_row_id,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p025 END, y.rpsf_p025) AS trim_rpsf_lo_p25,
        COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p975 END, y.rpsf_p975) AS trim_rpsf_hi_p25,
        CASE
          WHEN b.rent_per_sqft >= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p025 END, y.rpsf_p025)
           AND b.rent_per_sqft <= COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rpsf_p975 END, y.rpsf_p975) THEN 1 ELSE 0
        END AS keep_trim_p25
      FROM panel_sqft_flagged b
      LEFT JOIN trim_rpsf_cell_thresholds_p25 c
        ON b.year = c.year
       AND b.bed_cell = c.bed_cell
      LEFT JOIN trim_rpsf_year_thresholds_p25 y
        ON b.year = y.year
      WHERE b.keep_sqft_trim_p25 = 1
    )
    SELECT
      s.*,
      r1.trim_rpsf_lo_p1,
      r1.trim_rpsf_hi_p1,
      COALESCE(r1.keep_trim_p1, 0) AS keep_trim_p1,
      r25.trim_rpsf_lo_p25,
      r25.trim_rpsf_hi_p25,
      COALESCE(r25.keep_trim_p25, 0) AS keep_trim_p25,
      '", chosen_trim_spec_sql, "' AS chosen_trim_spec,
      CASE
        WHEN '", chosen_trim_spec_sql, "' = 'strict_p1' THEN COALESCE(r1.keep_trim_p1, 0)
        ELSE COALESCE(r25.keep_trim_p25, 0)
      END AS keep_trimmed_sample,
      CASE
        WHEN '", chosen_trim_spec_sql, "' = 'strict_p1' AND COALESCE(r1.keep_trim_p1, 0) = 1 AND s.rent_price > 0 THEN s.rent_price
        WHEN '", chosen_trim_spec_sql, "' = 'strict_p25' AND COALESCE(r25.keep_trim_p25, 0) = 1 AND s.rent_price > 0 THEN s.rent_price
        ELSE NULL
      END AS rent_price_trimmed,
      CASE
        WHEN '", chosen_trim_spec_sql, "' = 'strict_p1' AND COALESCE(r1.keep_trim_p1, 0) = 1 AND s.sqft > 0 THEN s.sqft
        WHEN '", chosen_trim_spec_sql, "' = 'strict_p25' AND COALESCE(r25.keep_trim_p25, 0) = 1 AND s.sqft > 0 THEN s.sqft
        ELSE NULL
      END AS sqft_trimmed
    FROM panel_sqft_flagged s
    LEFT JOIN panel_rpsf_flagged_p1 r1
      ON s.panel_row_id = r1.panel_row_id
    LEFT JOIN panel_rpsf_flagged_p25 r25
      ON s.panel_row_id = r25.panel_row_id
  "
)
dbExecute(
  con,
  panel_sql
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE guangbin_panel_final AS
  SELECT
    *,
    CASE WHEN rent_price_trimmed IS NOT NULL AND rent_price_trimmed > 0 THEN LOG(rent_price_trimmed) ELSE NULL END AS log_rent_price_trimmed,
    CASE WHEN sqft_trimmed IS NOT NULL AND sqft_trimmed > 0 THEN LOG(sqft_trimmed) ELSE NULL END AS log_sqft_trimmed,
    CASE WHEN rent_price_winsor IS NOT NULL AND rent_price_winsor > 0 THEN LOG(rent_price_winsor) ELSE NULL END AS log_rent_price_winsor,
    CASE WHEN sqft_winsor IS NOT NULL AND sqft_winsor > 0 THEN LOG(sqft_winsor) ELSE NULL END AS log_sqft_winsor,
    CASE WHEN beds IS NOT NULL AND beds > 0 THEN LOG(beds) ELSE NULL END AS log_beds_clean,
    CASE WHEN baths IS NOT NULL AND baths > 0 THEN LOG(baths) ELSE NULL END AS log_baths_clean,
    CASE WHEN ABS(rent_price_winsor - rent_price) > 1e-8 THEN 1 ELSE 0 END AS rent_winsorized_flag,
    CASE
      WHEN sqft IS NULL OR sqft_winsor IS NULL THEN 0
      WHEN ABS(sqft_winsor - sqft) > 1e-8 THEN 1 ELSE 0
    END AS sqft_winsorized_flag
  FROM guangbin_panel
  "
)

message("Writing cleaned panel parquet...")
dbExecute(
  con,
  sprintf(
    "COPY (SELECT * FROM guangbin_panel_final ORDER BY year, quarter, file_date, id) TO '%s' (FORMAT PARQUET)",
    gsub("'", "''", panel_out)
  )
)

panel_summary <- as.data.table(dbGetQuery(
  con,
  "
  SELECT
    year,
    quarter,
    chosen_trim_spec,
    COUNT(*) AS n_rows,
    COUNT(DISTINCT block_id) AS n_blocks,
    AVG(CASE WHEN key_source = 'unit_id' THEN 1.0 ELSE 0.0 END) AS share_unit_id_rows,
    AVG(CASE WHEN key_source = 'fingerprint' THEN 1.0 ELSE 0.0 END) AS share_fingerprint_rows,
    COUNT(DISTINCT analysis_key) FILTER (WHERE analysis_key IS NOT NULL) AS n_distinct_keys,
    QUANTILE_CONT(postings_per_key_year, 0.50) FILTER (WHERE postings_per_key_year IS NOT NULL) AS p50_postings_per_key_year,
    QUANTILE_CONT(postings_per_key_year, 0.95) FILTER (WHERE postings_per_key_year IS NOT NULL) AS p95_postings_per_key_year,
    AVG(rent_winsorized_flag) AS share_rent_winsorized,
    AVG(sqft_winsorized_flag) AS share_sqft_winsorized,
    AVG(CASE WHEN keep_trim_p1 = 1 THEN 1.0 ELSE 0.0 END) AS share_keep_trim_p1,
    AVG(CASE WHEN keep_trim_p25 = 1 THEN 1.0 ELSE 0.0 END) AS share_keep_trim_p25,
    AVG(CASE WHEN keep_trimmed_sample = 1 THEN 1.0 ELSE 0.0 END) AS share_keep_trimmed_sample,
    AVG(CASE WHEN building_type_clean = 'multi_family' THEN 1.0 ELSE 0.0 END) AS share_multifamily
  FROM guangbin_panel_final
  GROUP BY 1, 2, 3
  ORDER BY 1, 2, 3
  "
))

panel_trim_diagnostic <- rbindlist(list(
  as.data.table(dbGetQuery(
    con,
    "
    SELECT
      'strict_p1' AS trim_spec,
      COUNT(*) AS rows_total,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND sqft > 0 AND rent_per_sqft IS NOT NULL) AS rows_eligible,
      COUNT(*) FILTER (WHERE keep_trim_p1 = 1) AS rows_kept,
      COUNT(DISTINCT analysis_key) FILTER (WHERE keep_trim_p1 = 1) AS distinct_keys_kept,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND (sqft IS NULL OR sqft <= 0)) AS n_drop_nonpositive_sqft,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND sqft > 0 AND rent_per_sqft IS NOT NULL AND keep_sqft_trim_p1 = 0) AS n_drop_sqft_trim,
      COUNT(*) FILTER (WHERE keep_sqft_trim_p1 = 1 AND keep_trim_p1 = 0) AS n_drop_rent_per_sqft_trim,
      MIN(trim_sqft_lo_p1) AS sqft_cutoff_lo_min,
      MAX(trim_sqft_hi_p1) AS sqft_cutoff_hi_max,
      MIN(trim_rpsf_lo_p1) AS rent_per_sqft_cutoff_lo_min,
      MAX(trim_rpsf_hi_p1) AS rent_per_sqft_cutoff_hi_max,
      MAX(sqft) FILTER (WHERE keep_trim_p1 = 1) AS max_surviving_sqft,
      MAX(rent_per_sqft) FILTER (WHERE keep_trim_p1 = 1) AS max_surviving_rent_per_sqft
    FROM guangbin_panel_final
    "
  )),
  as.data.table(dbGetQuery(
    con,
    "
    SELECT
      'strict_p25' AS trim_spec,
      COUNT(*) AS rows_total,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND sqft > 0 AND rent_per_sqft IS NOT NULL) AS rows_eligible,
      COUNT(*) FILTER (WHERE keep_trim_p25 = 1) AS rows_kept,
      COUNT(DISTINCT analysis_key) FILTER (WHERE keep_trim_p25 = 1) AS distinct_keys_kept,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND (sqft IS NULL OR sqft <= 0)) AS n_drop_nonpositive_sqft,
      COUNT(*) FILTER (WHERE analysis_key IS NOT NULL AND rent_price > 0 AND sqft > 0 AND rent_per_sqft IS NOT NULL AND keep_sqft_trim_p25 = 0) AS n_drop_sqft_trim,
      COUNT(*) FILTER (WHERE keep_sqft_trim_p25 = 1 AND keep_trim_p25 = 0) AS n_drop_rent_per_sqft_trim,
      MIN(trim_sqft_lo_p25) AS sqft_cutoff_lo_min,
      MAX(trim_sqft_hi_p25) AS sqft_cutoff_hi_max,
      MIN(trim_rpsf_lo_p25) AS rent_per_sqft_cutoff_lo_min,
      MAX(trim_rpsf_hi_p25) AS rent_per_sqft_cutoff_hi_max,
      MAX(sqft) FILTER (WHERE keep_trim_p25 = 1) AS max_surviving_sqft,
      MAX(rent_per_sqft) FILTER (WHERE keep_trim_p25 = 1) AS max_surviving_rent_per_sqft
    FROM guangbin_panel_final
    "
  ))
), fill = TRUE)

panel_trim_diagnostic[, kept_share_of_eligible := fifelse(rows_eligible > 0, rows_kept / rows_eligible, NA_real_)]
panel_trim_diagnostic[, passes_plausibility_gates := is.finite(max_surviving_sqft) &
  max_surviving_sqft <= 5000 &
  is.finite(max_surviving_rent_per_sqft) &
  max_surviving_rent_per_sqft <= 5.5]
panel_trim_diagnostic[, `:=`(
  chosen_trim_spec = chosen_trim_spec,
  is_chosen = trim_spec == chosen_trim_spec
)]

write_csv(panel_summary, summary_out)
write_csv(panel_trim_diagnostic, diagnostic_out)
message(sprintf("Saved %s", summary_out))
message(sprintf("Saved %s", diagnostic_out))
