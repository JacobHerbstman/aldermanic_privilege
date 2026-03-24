source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)
library(ggplot2)
library(readr)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rd_listing_sample/code")
# Rscript run_rd_listing_sample.R ../input/renthub_raw ../input/rent_with_ward_distances_full.parquet ../output ../temp
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 4) {
  raw_dir <- args[1]
  geo_input <- args[2]
  out_dir <- args[3]
  temp_dir <- args[4]
} else {
  if (!exists("raw_dir") || !exists("geo_input") || !exists("out_dir") || !exists("temp_dir")) {
    stop("FATAL: Script requires 4 args: <raw_dir> <geo_input_parquet> <out_dir> <temp_dir>", call. = FALSE)
  }
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

sql_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

write_csv_dt <- function(dt, path) {
  data.table::fwrite(as.data.table(dt), path)
}

as_date_cols <- function(dt, cols) {
  for (col in cols) {
    if (col %in% names(dt)) {
      dt[, (col) := as.Date(get(col))]
    }
  }
  dt
}

band_filter <- function(col, bandwidth) {
  if (identical(bandwidth, "all")) {
    return("TRUE")
  }
  sprintf("ABS(%s) <= %d", col, as.integer(bandwidth))
}

classify_month_status <- function(
  observed_scrape_days,
  raw_listing_month_cells,
  n_month_ultra,
  keep_month_ultra_rate,
  p95_raw_listing_days_per_month,
  share_raw_key_month_gt14,
  share_month_any_conflict_day,
  share_pair_month_both_sides_ge5,
  side_gap_keep_month_ultra_rate_pp,
  side_gap_share_ultra_fingerprint_pp
) {
  fatal <- !is.finite(observed_scrape_days) ||
    observed_scrape_days < 18 ||
    !is.finite(raw_listing_month_cells) ||
    raw_listing_month_cells < 500 ||
    !is.finite(n_month_ultra) ||
    n_month_ultra < 300 ||
    !is.finite(share_pair_month_both_sides_ge5) ||
    share_pair_month_both_sides_ge5 < 0.25

  if (fatal) {
    return("RED")
  }

  if (
    observed_scrape_days >= 25 &&
      is.finite(keep_month_ultra_rate) && keep_month_ultra_rate >= 0.65 &&
      is.finite(p95_raw_listing_days_per_month) && p95_raw_listing_days_per_month <= 14 &&
      is.finite(share_raw_key_month_gt14) && share_raw_key_month_gt14 <= 0.05 &&
      is.finite(share_month_any_conflict_day) && share_month_any_conflict_day <= 0.10 &&
      is.finite(side_gap_keep_month_ultra_rate_pp) && side_gap_keep_month_ultra_rate_pp <= 0.08 &&
      is.finite(side_gap_share_ultra_fingerprint_pp) && side_gap_share_ultra_fingerprint_pp <= 0.15 &&
      is.finite(share_pair_month_both_sides_ge5) && share_pair_month_both_sides_ge5 >= 0.50
  ) {
    return("GREEN")
  }

  if (
    observed_scrape_days >= 20 &&
      is.finite(keep_month_ultra_rate) && keep_month_ultra_rate >= 0.50 &&
      is.finite(p95_raw_listing_days_per_month) && p95_raw_listing_days_per_month <= 21 &&
      is.finite(share_raw_key_month_gt14) && share_raw_key_month_gt14 <= 0.10 &&
      is.finite(share_month_any_conflict_day) && share_month_any_conflict_day <= 0.20 &&
      is.finite(side_gap_keep_month_ultra_rate_pp) && side_gap_keep_month_ultra_rate_pp <= 0.15 &&
      is.finite(side_gap_share_ultra_fingerprint_pp) && side_gap_share_ultra_fingerprint_pp <= 0.25 &&
      is.finite(share_pair_month_both_sides_ge5) && share_pair_month_both_sides_ge5 >= 0.30
  ) {
    return("YELLOW")
  }

  "RED"
}

build_window_runs <- function(dt) {
  dt <- copy(dt)
  dt <- dt[order(month_start)]
  dt <- dt[month_status != "RED"]
  if (nrow(dt) == 0) {
    return(data.table())
  }
  dt[, prev_month := shift(month_start)]
  dt[, prev_month_next := fifelse(
    is.na(prev_month),
    as.Date(NA),
    as.Date(format(prev_month + 32, "%Y-%m-01"))
  )]
  dt[, new_run := fifelse(is.na(prev_month_next) | month_start != prev_month_next, 1L, 0L)]
  dt[, run_id := cumsum(new_run)]
  dt[, .(
    window_start = min(month_start),
    window_end = max(month_start),
    n_months = .N,
    n_green = sum(month_status == "GREEN"),
    n_yellow = sum(month_status == "YELLOW"),
    share_green = mean(month_status == "GREEN"),
    median_n_month_ultra = median(n_month_ultra, na.rm = TRUE),
    median_keep_month_ultra_rate = median(keep_month_ultra_rate, na.rm = TRUE),
    median_share_pair_month_both_sides_ge5 = median(share_pair_month_both_sides_ge5, na.rm = TRUE)
  ), by = run_id]
}

build_plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

message("Opening raw RentHub listings and RD geometry...")

raw_dir_abs <- normalizePath(raw_dir, mustWork = TRUE)
geo_input_abs <- normalizePath(geo_input, mustWork = TRUE)
parquet_glob <- file.path(raw_dir_abs, "*.parquet")

duckdb_path <- file.path(temp_dir, "rd_listing_sample.duckdb")
unlink(c(duckdb_path, paste0(duckdb_path, ".wal")), force = TRUE)

con <- dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

building_type_case_raw <- paste(
  "CASE",
  "  WHEN BUILDING_TYPE IS NULL OR UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL",
  "  WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%CONDO%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%CONDOMINIUM%' THEN 'condo'",
  "  WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%MULTI%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%APART%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%DUPLEX%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%TRIPLEX%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%FOURPLEX%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%APT%' THEN 'multi_family'",
  "  WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%SINGLE%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%HOUSE%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%DETACHED%' OR UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%SFR%' THEN 'single_family'",
  "  WHEN UPPER(CAST(BUILDING_TYPE AS VARCHAR)) LIKE '%TOWN%' OR UPPER(TRIM(CAST(BUILDING_TYPE AS VARCHAR))) = 'TH' THEN 'townhouse'",
  "  ELSE 'other'",
  "END",
  sep = "\n"
)

raw_norm_sql <- sprintf(
  "
  CREATE OR REPLACE TABLE raw_norm AS
  SELECT
    CASE
      WHEN ID IS NULL OR UPPER(TRIM(CAST(ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(ID AS VARCHAR)))
    END AS id_norm,
    CASE
      WHEN PROPERTY_ID IS NULL OR UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR)))
    END AS property_id_norm,
    CASE
      WHEN UNIT_ID IS NULL OR UPPER(TRIM(CAST(UNIT_ID AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(UNIT_ID AS VARCHAR)))
    END AS unit_id_norm,
    CASE
      WHEN ADDRESS IS NULL OR UPPER(TRIM(CAST(ADDRESS AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(ADDRESS AS VARCHAR))), ' +', ' '), '')
    END AS address_norm,
    CASE
      WHEN COMPANY IS NULL OR UPPER(TRIM(CAST(COMPANY AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE NULLIF(REGEXP_REPLACE(UPPER(TRIM(CAST(COMPANY AS VARCHAR))), ' +', ' '), '')
    END AS company_norm,
    CAST(SCRAPED_TIMESTAMP AS TIMESTAMP) AS scraped_ts,
    CAST(SCRAPED_TIMESTAMP AS DATE) AS file_date,
    CAST(DATE_TRUNC('month', CAST(SCRAPED_TIMESTAMP AS DATE)) AS DATE) AS month_start,
    CAST(DATE_POSTED AS DATE) AS posted_date,
    CAST(AVAILABLE_AT AS DATE) AS available_date,
    CAST(RENT_PRICE AS DOUBLE) AS rent_price_nominal,
    CAST(BEDS AS DOUBLE) AS beds,
    CAST(BATHS AS DOUBLE) AS baths,
    CAST(SQFT AS DOUBLE) AS sqft,
    CAST(LATITUDE AS DOUBLE) AS latitude,
    CAST(LONGITUDE AS DOUBLE) AS longitude,
    (%s) AS building_type_clean,
    CAST(AVAILABILITY_STATUS AS VARCHAR) AS availability_status,
    CAST(LATITUDE AS DOUBLE) * 364000.0 AS latitude_ft,
    CAST(LONGITUDE AS DOUBLE) * 271300.0 AS longitude_ft,
    CASE
      WHEN LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL THEN
        CAST(ROUND(CAST(LATITUDE AS DOUBLE), 4) AS VARCHAR) || '|' ||
        CAST(ROUND(CAST(LONGITUDE AS DOUBLE), 4) AS VARCHAR)
      ELSE NULL
    END AS coord4
  FROM read_parquet('%s', union_by_name = true)
  WHERE UPPER(TRIM(CAST(CITY AS VARCHAR))) = 'CHICAGO'
    AND SCRAPED_TIMESTAMP IS NOT NULL
  ",
  building_type_case_raw,
  sql_escape(parquet_glob)
)
dbExecute(con, raw_norm_sql)

geo_norm_sql <- sprintf(
  "
  CREATE OR REPLACE TABLE geo_norm AS
  SELECT
    CASE
      WHEN id IS NULL OR UPPER(TRIM(CAST(id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
      ELSE UPPER(TRIM(CAST(id AS VARCHAR)))
    END AS id_norm,
    CAST(file_date AS DATE) AS file_date,
    CAST(available_date AS DATE) AS available_date,
    CAST(rent_price_nominal AS DOUBLE) AS rent_price_nominal_geo,
    CAST(rent_price AS DOUBLE) AS rent_price_real_2022,
    CAST(beds AS DOUBLE) AS beds,
    CAST(baths AS DOUBLE) AS baths,
    CAST(sqft AS DOUBLE) AS sqft,
    CAST(latitude AS DOUBLE) AS latitude_geo,
    CAST(longitude AS DOUBLE) AS longitude_geo,
    CAST(dist_ft AS DOUBLE) AS dist_ft,
    CAST(signed_dist AS DOUBLE) AS signed_dist,
    CAST(sign AS INTEGER) AS sign,
    CAST(ward_pair_id AS VARCHAR) AS ward_pair_id,
    CAST(segment_id AS VARCHAR) AS segment_id,
    CAST(strictness_own AS DOUBLE) AS strictness_own,
    CAST(strictness_neighbor AS DOUBLE) AS strictness_neighbor,
    CAST(alderman_own AS VARCHAR) AS alderman_own,
    CAST(alderman_neighbor AS VARCHAR) AS alderman_neighbor,
    CAST(building_type_clean AS VARCHAR) AS building_type_clean,
    CAST(latitude AS DOUBLE) * 364000.0 AS latitude_ft,
    CAST(longitude AS DOUBLE) * 271300.0 AS longitude_ft
  FROM read_parquet('%s')
  ",
  sql_escape(geo_input_abs)
)
dbExecute(con, geo_norm_sql)

message("Ranking raw and geometry rows for conservative join...")

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE raw_ranked AS
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY id_norm, file_date
      ORDER BY
        rent_price_nominal NULLS LAST,
        latitude NULLS LAST,
        longitude NULLS LAST,
        available_date NULLS LAST,
        beds NULLS LAST,
        baths NULLS LAST,
        sqft NULLS LAST,
        COALESCE(address_norm, ''),
        COALESCE(company_norm, ''),
        COALESCE(property_id_norm, '')
    ) AS match_rank
  FROM raw_norm
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE geo_ranked AS
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY id_norm, file_date
      ORDER BY
        rent_price_nominal_geo NULLS LAST,
        latitude_geo NULLS LAST,
        longitude_geo NULLS LAST,
        available_date NULLS LAST,
        beds NULLS LAST,
        baths NULLS LAST,
        sqft NULLS LAST,
        COALESCE(building_type_clean, '')
    ) AS match_rank
  FROM geo_norm
  "
)

message("Joining raw QA fields to signed-distance geometry...")

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE joined_base AS
  SELECT
    r.id_norm,
    r.property_id_norm,
    r.unit_id_norm,
    r.address_norm,
    r.company_norm,
    r.scraped_ts,
    r.file_date,
    r.month_start,
    STRFTIME(r.month_start, '%Y-%m') AS year_month,
    r.posted_date,
    r.available_date,
    r.rent_price_nominal,
    g.rent_price_real_2022,
    r.beds,
    r.baths,
    r.sqft,
    r.latitude,
    r.longitude,
    r.latitude_ft,
    r.longitude_ft,
    r.building_type_clean,
    r.availability_status,
    r.coord4,
    r.match_rank,
    g.ward_pair_id,
    g.segment_id,
    g.dist_ft,
    g.signed_dist,
    g.sign,
    g.strictness_own,
    g.strictness_neighbor,
    g.alderman_own,
    g.alderman_neighbor,
    CASE
      WHEN g.id_norm IS NULL THEN 0
      ELSE 1
    END AS geo_matched,
    CASE
      WHEN r.unit_id_norm IS NOT NULL THEN 'unit_id'
      WHEN r.address_norm IS NOT NULL
        AND r.coord4 IS NOT NULL
        AND (r.beds IS NOT NULL OR r.baths IS NOT NULL OR r.sqft IS NOT NULL OR r.building_type_clean IS NOT NULL)
      THEN 'fingerprint_strict'
      ELSE 'weak_or_missing'
    END AS listing_key_source,
    CASE
      WHEN r.unit_id_norm IS NOT NULL THEN 'UNIT::' || r.unit_id_norm
      WHEN r.address_norm IS NOT NULL
        AND r.coord4 IS NOT NULL
        AND (r.beds IS NOT NULL OR r.baths IS NOT NULL OR r.sqft IS NOT NULL OR r.building_type_clean IS NOT NULL)
      THEN 'FP::' ||
        r.address_norm || '|' ||
        r.coord4 || '|' ||
        COALESCE(CAST(r.beds AS VARCHAR), '') || '|' ||
        COALESCE(CAST(r.baths AS VARCHAR), '') || '|' ||
        COALESCE(CAST(r.sqft AS VARCHAR), '') || '|' ||
        COALESCE(r.building_type_clean, '')
      ELSE NULL
    END AS listing_key,
    CASE
      WHEN r.rent_price_nominal IS NOT NULL AND r.rent_price_nominal > 0 THEN 1
      ELSE 0
    END AS positive_rent,
    CASE
      WHEN g.id_norm IS NULL THEN 'unmatched_geo'
      WHEN r.rent_price_nominal IS NULL OR r.rent_price_nominal <= 0 THEN 'nonpositive_rent'
      WHEN (CASE
        WHEN r.unit_id_norm IS NOT NULL THEN 'UNIT::' || r.unit_id_norm
        WHEN r.address_norm IS NOT NULL
          AND r.coord4 IS NOT NULL
          AND (r.beds IS NOT NULL OR r.baths IS NOT NULL OR r.sqft IS NOT NULL OR r.building_type_clean IS NOT NULL)
        THEN 'FP::' ||
          r.address_norm || '|' ||
          r.coord4 || '|' ||
          COALESCE(CAST(r.beds AS VARCHAR), '') || '|' ||
          COALESCE(CAST(r.baths AS VARCHAR), '') || '|' ||
          COALESCE(CAST(r.sqft AS VARCHAR), '') || '|' ||
          COALESCE(r.building_type_clean, '')
        ELSE NULL
      END) IS NULL THEN 'weak_or_missing_listing_key'
      WHEN g.segment_id IS NULL OR g.segment_id = '' THEN 'missing_segment'
      WHEN g.signed_dist IS NULL OR g.sign IS NULL OR g.strictness_own IS NULL OR g.strictness_neighbor IS NULL THEN 'missing_border_scores'
      ELSE 'eligible'
    END AS base_status
  FROM raw_ranked r
  LEFT JOIN geo_ranked g
    ON r.id_norm = g.id_norm
   AND r.file_date = g.file_date
   AND r.match_rank = g.match_rank
  "
)

raw_row_count <- collect_query("SELECT COUNT(*) AS n_rows FROM raw_norm")
if (raw_row_count$n_rows[1] == 0L) {
  stop("No Chicago raw rows found in renthub_raw input.", call. = FALSE)
}

message("Building conservative day-level groups...")

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE joined_eligible AS
  SELECT *
  FROM joined_base
  WHERE base_status = 'eligible'
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE day_groups AS
  SELECT
    listing_key,
    listing_key_source,
    file_date,
    month_start,
    year_month,
    COUNT(*) AS n_raw_rows_day,
    COUNT(DISTINCT id_norm) AS n_ids_day,
    COUNT(DISTINCT rent_price_nominal) AS n_rents_day,
    COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) AS n_addresses_day,
    COUNT(DISTINCT ward_pair_id) FILTER (WHERE ward_pair_id IS NOT NULL) AS n_pairs_day,
    COUNT(DISTINCT segment_id) FILTER (WHERE segment_id IS NOT NULL AND segment_id != '') AS n_segments_day,
    COUNT(DISTINCT sign) FILTER (WHERE sign IS NOT NULL) AS n_signs_day,
    MIN(rent_price_nominal) AS min_rent_day,
    MAX(rent_price_nominal) AS max_rent_day,
    MIN(longitude_ft) AS min_x_day,
    MAX(longitude_ft) AS max_x_day,
    MIN(latitude_ft) AS min_y_day,
    MAX(latitude_ft) AS max_y_day
  FROM joined_eligible
  GROUP BY 1, 2, 3, 4, 5
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE day_groups_labeled AS
  SELECT
    *,
    SQRT(POWER(max_x_day - min_x_day, 2) + POWER(max_y_day - min_y_day, 2)) AS geo_span_ft_day,
    CASE
      WHEN n_rents_day > 1 THEN 'multi_rent'
      WHEN n_signs_day > 1 THEN 'multi_side'
      WHEN n_pairs_day > 1 THEN 'multi_pair'
      WHEN n_segments_day > 1 THEN 'multi_segment'
      WHEN n_addresses_day > 1 THEN 'multi_address'
      WHEN SQRT(POWER(max_x_day - min_x_day, 2) + POWER(max_y_day - min_y_day, 2)) > 100 THEN 'geo_span_gt100'
      ELSE 'keep'
    END AS day_status,
    CASE
      WHEN
        CASE
          WHEN n_rents_day > 1 THEN 'multi_rent'
          WHEN n_signs_day > 1 THEN 'multi_side'
          WHEN n_pairs_day > 1 THEN 'multi_pair'
          WHEN n_segments_day > 1 THEN 'multi_segment'
          WHEN n_addresses_day > 1 THEN 'multi_address'
          WHEN SQRT(POWER(max_x_day - min_x_day, 2) + POWER(max_y_day - min_y_day, 2)) > 100 THEN 'geo_span_gt100'
          ELSE 'keep'
        END = 'keep'
      THEN TRUE
      ELSE FALSE
    END AS keep_day
  FROM day_groups
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE day_representatives AS
  SELECT *
  FROM (
    SELECT
      je.*,
      ROW_NUMBER() OVER (
        PARTITION BY je.listing_key, je.file_date
        ORDER BY je.scraped_ts DESC NULLS LAST, je.match_rank DESC
      ) AS keep_rank
    FROM joined_eligible je
  ) q
  WHERE keep_rank = 1
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE rd_listing_day_flagged AS
  SELECT
    r.listing_key,
    r.listing_key_source,
    r.file_date,
    r.month_start,
    r.year_month,
    r.id_norm,
    r.property_id_norm,
    r.unit_id_norm,
    r.address_norm,
    r.company_norm,
    r.rent_price_nominal,
    r.rent_price_real_2022,
    r.posted_date,
    r.available_date,
    r.beds,
    r.baths,
    r.sqft,
    r.building_type_clean,
    r.latitude,
    r.longitude,
    r.ward_pair_id,
    r.segment_id,
    r.sign,
    r.signed_dist,
    r.dist_ft,
    r.strictness_own,
    r.strictness_neighbor,
    r.alderman_own,
    r.alderman_neighbor,
    d.n_raw_rows_day,
    d.n_ids_day,
    d.n_rents_day,
    d.n_addresses_day,
    d.n_pairs_day,
    d.n_segments_day,
    d.n_signs_day,
    d.geo_span_ft_day,
    d.day_status,
    d.keep_day
  FROM day_representatives r
  INNER JOIN day_groups_labeled d
    ON r.listing_key = d.listing_key
   AND r.file_date = d.file_date
  "
)

message("Building conservative month-level groups...")

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE month_groups AS
  SELECT
    listing_key,
    listing_key_source,
    month_start,
    year_month,
    COUNT(*) AS n_day_rows_all,
    SUM(CASE WHEN keep_day THEN 1 ELSE 0 END) AS n_clean_days,
    SUM(CASE WHEN keep_day THEN 0 ELSE 1 END) AS n_conflict_days,
    COUNT(DISTINCT rent_price_nominal) FILTER (WHERE keep_day) AS n_distinct_clean_day_rents,
    MIN(rent_price_nominal) FILTER (WHERE keep_day) AS rent_min_clean,
    MAX(rent_price_nominal) FILTER (WHERE keep_day) AS rent_max_clean,
    MEDIAN(rent_price_nominal) FILTER (WHERE keep_day) AS rent_median_clean,
    COUNT(DISTINCT address_norm) FILTER (WHERE keep_day AND address_norm IS NOT NULL) AS n_addresses_clean,
    COUNT(DISTINCT ward_pair_id) FILTER (WHERE keep_day AND ward_pair_id IS NOT NULL) AS n_pairs_clean,
    COUNT(DISTINCT segment_id) FILTER (WHERE keep_day AND segment_id IS NOT NULL AND segment_id != '') AS n_segments_clean,
    COUNT(DISTINCT sign) FILTER (WHERE keep_day AND sign IS NOT NULL) AS n_signs_clean,
    MIN(longitude * 271300.0) FILTER (WHERE keep_day) AS min_x_clean,
    MAX(longitude * 271300.0) FILTER (WHERE keep_day) AS max_x_clean,
    MIN(latitude * 364000.0) FILTER (WHERE keep_day) AS min_y_clean,
    MAX(latitude * 364000.0) FILTER (WHERE keep_day) AS max_y_clean
  FROM rd_listing_day_flagged
  GROUP BY 1, 2, 3, 4
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE month_first_clean AS
  SELECT *
  FROM (
    SELECT
      *,
      ROW_NUMBER() OVER (
        PARTITION BY listing_key, month_start
        ORDER BY file_date ASC, posted_date ASC NULLS LAST
      ) AS rn_first
    FROM rd_listing_day_flagged
    WHERE keep_day
  ) q
  WHERE rn_first = 1
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE month_last_clean AS
  SELECT *
  FROM (
    SELECT
      *,
      ROW_NUMBER() OVER (
        PARTITION BY listing_key, month_start
        ORDER BY file_date DESC, posted_date DESC NULLS LAST
      ) AS rn_last
    FROM rd_listing_day_flagged
    WHERE keep_day
  ) q
  WHERE rn_last = 1
  "
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE rd_listing_month_flagged AS
  SELECT
    g.listing_key,
    g.listing_key_source,
    g.month_start,
    g.year_month,
    g.n_day_rows_all,
    g.n_clean_days,
    g.n_conflict_days,
    g.n_distinct_clean_day_rents,
    g.rent_min_clean,
    g.rent_max_clean,
    g.rent_median_clean,
    f.file_date AS first_clean_file_date,
    f.rent_price_nominal AS rent_first_clean_day,
    l.file_date AS last_clean_file_date,
    l.rent_price_nominal AS rent_last_clean_day,
    CASE
      WHEN g.rent_min_clean IS NOT NULL AND g.rent_min_clean > 0 AND g.rent_max_clean IS NOT NULL
      THEN g.rent_max_clean / g.rent_min_clean - 1
      ELSE NULL
    END AS rent_range_pct,
    SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) AS geo_span_ft_clean,
    g.n_addresses_clean,
    g.n_pairs_clean,
    g.n_segments_clean,
    g.n_signs_clean,
    l.id_norm AS id_norm_last,
    l.property_id_norm AS property_id_norm_last,
    l.unit_id_norm AS unit_id_norm_last,
    l.address_norm AS address_norm_last,
    l.company_norm AS company_norm_last,
    l.beds AS beds_last,
    l.baths AS baths_last,
    l.sqft AS sqft_last,
    l.building_type_clean AS building_type_clean_last,
    l.latitude AS latitude_last,
    l.longitude AS longitude_last,
    l.ward_pair_id AS ward_pair_id_last,
    l.segment_id AS segment_id_last,
    l.sign AS sign_last,
    l.signed_dist AS signed_dist_last,
    l.dist_ft AS dist_ft_last,
    l.strictness_own AS strictness_own_last,
    l.strictness_neighbor AS strictness_neighbor_last,
    l.alderman_own AS alderman_own_last,
    l.alderman_neighbor AS alderman_neighbor_last,
    CASE
      WHEN g.n_clean_days < 1 THEN 'no_clean_days'
      WHEN g.n_signs_clean > 1 THEN 'multi_side'
      WHEN g.n_pairs_clean > 1 THEN 'multi_pair'
      WHEN g.n_segments_clean > 1 THEN 'multi_segment'
      WHEN g.n_addresses_clean > 1 THEN 'multi_address'
      WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 250 THEN 'geo_span_gt250'
      ELSE 'keep_main'
    END AS month_status_main,
    CASE
      WHEN
        CASE
          WHEN g.n_clean_days < 1 THEN 'no_clean_days'
          WHEN g.n_signs_clean > 1 THEN 'multi_side'
          WHEN g.n_pairs_clean > 1 THEN 'multi_pair'
          WHEN g.n_segments_clean > 1 THEN 'multi_segment'
          WHEN g.n_addresses_clean > 1 THEN 'multi_address'
          WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 250 THEN 'geo_span_gt250'
          ELSE 'keep_main'
        END = 'keep_main'
      THEN TRUE
      ELSE FALSE
    END AS keep_month_main,
    CASE
      WHEN
        CASE
          WHEN g.n_clean_days < 1 THEN 'no_clean_days'
          WHEN g.n_signs_clean > 1 THEN 'multi_side'
          WHEN g.n_pairs_clean > 1 THEN 'multi_pair'
          WHEN g.n_segments_clean > 1 THEN 'multi_segment'
          WHEN g.n_addresses_clean > 1 THEN 'multi_address'
          WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 250 THEN 'geo_span_gt250'
          ELSE 'keep_main'
        END != 'keep_main'
      THEN
        CASE
          WHEN g.n_clean_days < 1 THEN 'no_clean_days'
          WHEN g.n_signs_clean > 1 THEN 'multi_side'
          WHEN g.n_pairs_clean > 1 THEN 'multi_pair'
          WHEN g.n_segments_clean > 1 THEN 'multi_segment'
          WHEN g.n_addresses_clean > 1 THEN 'multi_address'
          WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 250 THEN 'geo_span_gt250'
          ELSE 'keep_main'
        END
      WHEN g.n_conflict_days > 0 THEN 'conflict_days_present'
      WHEN (CASE
        WHEN g.rent_min_clean IS NOT NULL AND g.rent_min_clean > 0 AND g.rent_max_clean IS NOT NULL
        THEN g.rent_max_clean / g.rent_min_clean - 1
        ELSE NULL
      END) > 0.25 THEN 'rent_range_gt25pct'
      WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 100 THEN 'geo_span_gt100'
      ELSE 'keep_ultra'
    END AS month_status_ultra,
    CASE
      WHEN
        CASE
          WHEN
            CASE
              WHEN g.n_clean_days < 1 THEN 'no_clean_days'
              WHEN g.n_signs_clean > 1 THEN 'multi_side'
              WHEN g.n_pairs_clean > 1 THEN 'multi_pair'
              WHEN g.n_segments_clean > 1 THEN 'multi_segment'
              WHEN g.n_addresses_clean > 1 THEN 'multi_address'
              WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 250 THEN 'geo_span_gt250'
              ELSE 'keep_main'
            END != 'keep_main'
          THEN TRUE
          WHEN g.n_conflict_days > 0 THEN TRUE
          WHEN (CASE
            WHEN g.rent_min_clean IS NOT NULL AND g.rent_min_clean > 0 AND g.rent_max_clean IS NOT NULL
            THEN g.rent_max_clean / g.rent_min_clean - 1
            ELSE NULL
          END) > 0.25 THEN TRUE
          WHEN SQRT(POWER(g.max_x_clean - g.min_x_clean, 2) + POWER(g.max_y_clean - g.min_y_clean, 2)) > 100 THEN TRUE
          ELSE FALSE
        END
      THEN FALSE
      ELSE TRUE
    END AS keep_month_ultra
  FROM month_groups g
  LEFT JOIN month_first_clean f
    ON g.listing_key = f.listing_key
   AND g.month_start = f.month_start
  LEFT JOIN month_last_clean l
    ON g.listing_key = l.listing_key
   AND g.month_start = l.month_start
  "
)

message("Exporting strict day and month samples...")

dbExecute(
  con,
  sprintf(
    "COPY (SELECT * FROM rd_listing_day_flagged WHERE keep_day) TO '%s' (FORMAT PARQUET)",
    sql_escape(file.path(normalizePath(out_dir, mustWork = TRUE), "rd_listing_day_strict.parquet"))
  )
)

dbExecute(
  con,
  sprintf(
    "COPY (SELECT * FROM rd_listing_month_flagged) TO '%s' (FORMAT PARQUET)",
    sql_escape(file.path(normalizePath(out_dir, mustWork = TRUE), "rd_listing_month_flagged.parquet"))
  )
)

dbExecute(
  con,
  sprintf(
    "COPY (SELECT * FROM rd_listing_month_flagged WHERE keep_month_ultra) TO '%s' (FORMAT PARQUET)",
    sql_escape(file.path(normalizePath(out_dir, mustWork = TRUE), "rd_listing_month_ultra.parquet"))
  )
)

message("Collecting QA summaries...")

join_coverage_summary <- collect_query(
  "
  SELECT
    month_start,
    COUNT(*) AS raw_rows,
    SUM(CASE WHEN geo_matched = 1 THEN 1 ELSE 0 END) AS matched_geo_rows,
    AVG(CASE WHEN geo_matched = 1 THEN 1.0 ELSE 0.0 END) AS share_geo_matched,
    SUM(CASE WHEN base_status = 'eligible' THEN 1 ELSE 0 END) AS eligible_rows,
    AVG(CASE WHEN base_status = 'eligible' THEN 1.0 ELSE 0.0 END) AS share_eligible
  FROM joined_base
  GROUP BY 1
  ORDER BY 1
  "
)
join_coverage_summary <- as_date_cols(join_coverage_summary, "month_start")

listing_key_source_by_month <- collect_query(
  "
  SELECT
    month_start,
    listing_key_source,
    COUNT(*) AS n_rows
  FROM joined_base
  GROUP BY 1, 2
  ORDER BY 1, 2
  "
)
listing_key_source_by_month <- as_date_cols(listing_key_source_by_month, "month_start")

day_drop_reason_by_month <- collect_query(
  "
  SELECT
    month_start,
    day_status,
    COUNT(*) AS n_listing_days
  FROM rd_listing_day_flagged
  GROUP BY 1, 2
  ORDER BY 1, 2
  "
)
day_drop_reason_by_month <- as_date_cols(day_drop_reason_by_month, "month_start")

month_drop_reason_by_month <- collect_query(
  "
  SELECT
    month_start,
    month_status_ultra,
    COUNT(*) AS n_listing_months
  FROM rd_listing_month_flagged
  GROUP BY 1, 2
  ORDER BY 1, 2
  "
)
month_drop_reason_by_month <- as_date_cols(month_drop_reason_by_month, "month_start")

bandwidths <- list(all = "all", bw1000 = 1000L, bw500 = 500L, bw250 = 250L)

monthly_quality_list <- list()
side_quality_list <- list()
pair_support_list <- list()

for (band_name in names(bandwidths)) {
  band_value <- bandwidths[[band_name]]
  day_cond <- band_filter("signed_dist", band_value)
  month_cond <- band_filter("signed_dist_last", band_value)

  overall_sql <- sprintf(
    "
    WITH raw_month AS (
      SELECT
        month_start,
        listing_key,
        COUNT(*) AS n_listing_days_raw
      FROM rd_listing_day_flagged
      WHERE %s
      GROUP BY 1, 2
    ),
    raw_month_s AS (
      SELECT
        month_start,
        COUNT(*) AS raw_listing_month_cells,
        SUM(n_listing_days_raw) AS raw_listing_day_cells,
        QUANTILE_CONT(n_listing_days_raw, 0.95) AS p95_raw_listing_days_per_month,
        AVG(CASE WHEN n_listing_days_raw > 7 THEN 1.0 ELSE 0.0 END) AS share_raw_key_month_gt7,
        AVG(CASE WHEN n_listing_days_raw > 14 THEN 1.0 ELSE 0.0 END) AS share_raw_key_month_gt14,
        AVG(CASE WHEN n_listing_days_raw > 21 THEN 1.0 ELSE 0.0 END) AS share_raw_key_month_gt21
      FROM raw_month
      GROUP BY 1
    ),
    day_s AS (
      SELECT
        month_start,
        COUNT(*) AS raw_listing_day_groups,
        SUM(CASE WHEN keep_day THEN 1 ELSE 0 END) AS kept_listing_day_groups,
        AVG(CASE WHEN keep_day THEN 1.0 ELSE 0.0 END) AS keep_day_rate,
        AVG(CASE WHEN day_status = 'multi_rent' THEN 1.0 ELSE 0.0 END) AS share_day_multi_rent,
        AVG(CASE WHEN day_status = 'multi_address' THEN 1.0 ELSE 0.0 END) AS share_day_multi_address,
        AVG(CASE WHEN day_status = 'multi_side' OR day_status = 'multi_pair' OR day_status = 'multi_segment' THEN 1.0 ELSE 0.0 END) AS share_day_multi_border_assignment,
        AVG(CASE WHEN day_status = 'geo_span_gt100' THEN 1.0 ELSE 0.0 END) AS share_day_geo_span_gt100
      FROM rd_listing_day_flagged
      WHERE %s
      GROUP BY 1
    ),
    month_s AS (
      SELECT
        month_start,
        COUNT(*) AS raw_listing_month_groups,
        SUM(CASE WHEN keep_month_main THEN 1 ELSE 0 END) AS n_month_main,
        SUM(CASE WHEN keep_month_ultra THEN 1 ELSE 0 END) AS n_month_ultra,
        AVG(CASE WHEN keep_month_main THEN 1.0 ELSE 0.0 END) AS keep_month_main_rate,
        AVG(CASE WHEN keep_month_ultra THEN 1.0 ELSE 0.0 END) AS keep_month_ultra_rate,
        AVG(CASE WHEN n_conflict_days > 0 THEN 1.0 ELSE 0.0 END) AS share_month_any_conflict_day,
        AVG(CASE WHEN rent_range_pct > 0.25 THEN 1.0 ELSE 0.0 END) AS share_month_range_gt25,
        AVG(CASE WHEN geo_span_ft_clean > 100 THEN 1.0 ELSE 0.0 END) AS share_month_geo_span_gt100,
        AVG(CASE WHEN keep_month_ultra AND listing_key_source = 'unit_id' THEN 1.0 ELSE 0.0 END) FILTER (WHERE keep_month_ultra) AS share_ultra_unit_id,
        AVG(CASE WHEN keep_month_ultra AND listing_key_source = 'fingerprint_strict' THEN 1.0 ELSE 0.0 END) FILTER (WHERE keep_month_ultra) AS share_ultra_fingerprint
      FROM rd_listing_month_flagged
      WHERE %s
      GROUP BY 1
    ),
    pair_s AS (
      SELECT
        month_start,
        AVG(CASE WHEN n_less_ge1 > 0 AND n_more_ge1 > 0 THEN 1.0 ELSE 0.0 END) AS share_pair_month_both_sides_ge1,
        AVG(CASE WHEN n_less_ge5 >= 5 AND n_more_ge5 >= 5 THEN 1.0 ELSE 0.0 END) AS share_pair_month_both_sides_ge5
      FROM (
        SELECT
          month_start,
          ward_pair_id_last,
          SUM(CASE WHEN keep_month_ultra AND sign_last = -1 THEN 1 ELSE 0 END) AS n_less_ge1,
          SUM(CASE WHEN keep_month_ultra AND sign_last = 1 THEN 1 ELSE 0 END) AS n_more_ge1,
          SUM(CASE WHEN keep_month_ultra AND sign_last = -1 THEN 1 ELSE 0 END) AS n_less_ge5,
          SUM(CASE WHEN keep_month_ultra AND sign_last = 1 THEN 1 ELSE 0 END) AS n_more_ge5
        FROM rd_listing_month_flagged
        WHERE %s
        GROUP BY 1, 2
      ) q
      GROUP BY 1
    ),
    scrape_s AS (
      SELECT
        month_start,
        COUNT(DISTINCT file_date) AS observed_scrape_days,
        COUNT(*) AS joined_rows,
        SUM(CASE WHEN base_status = 'eligible' THEN 1 ELSE 0 END) AS eligible_rows
      FROM joined_base
      GROUP BY 1
    )
    SELECT
      scrape_s.month_start,
      '%s' AS bandwidth,
      scrape_s.observed_scrape_days,
      scrape_s.joined_rows,
      scrape_s.eligible_rows,
      raw_month_s.raw_listing_month_cells,
      raw_month_s.raw_listing_day_cells,
      raw_month_s.p95_raw_listing_days_per_month,
      raw_month_s.share_raw_key_month_gt7,
      raw_month_s.share_raw_key_month_gt14,
      raw_month_s.share_raw_key_month_gt21,
      day_s.raw_listing_day_groups,
      day_s.kept_listing_day_groups,
      day_s.keep_day_rate,
      day_s.share_day_multi_rent,
      day_s.share_day_multi_address,
      day_s.share_day_multi_border_assignment,
      day_s.share_day_geo_span_gt100,
      month_s.raw_listing_month_groups,
      month_s.n_month_main,
      month_s.n_month_ultra,
      month_s.keep_month_main_rate,
      month_s.keep_month_ultra_rate,
      month_s.share_month_any_conflict_day,
      month_s.share_month_range_gt25,
      month_s.share_month_geo_span_gt100,
      month_s.share_ultra_unit_id,
      month_s.share_ultra_fingerprint,
      pair_s.share_pair_month_both_sides_ge1,
      pair_s.share_pair_month_both_sides_ge5
    FROM scrape_s
    LEFT JOIN raw_month_s USING (month_start)
    LEFT JOIN day_s USING (month_start)
    LEFT JOIN month_s USING (month_start)
    LEFT JOIN pair_s USING (month_start)
    ORDER BY scrape_s.month_start
    ",
    day_cond, day_cond, month_cond, month_cond, band_name
  )
  monthly_quality_i <- collect_query(overall_sql)
  monthly_quality_i <- as_date_cols(monthly_quality_i, "month_start")
  monthly_quality_list[[band_name]] <- monthly_quality_i

  side_sql <- sprintf(
    "
    SELECT
      month_start,
      '%s' AS bandwidth,
      CASE WHEN sign_last = -1 THEN 'less_strict' ELSE 'more_strict' END AS side,
      COUNT(*) AS raw_listing_month_groups_side,
      SUM(CASE WHEN keep_month_ultra THEN 1 ELSE 0 END) AS n_month_ultra_side,
      AVG(CASE WHEN keep_month_ultra THEN 1.0 ELSE 0.0 END) AS keep_month_ultra_rate_side,
      AVG(CASE WHEN keep_month_ultra AND listing_key_source = 'unit_id' THEN 1.0 ELSE 0.0 END) FILTER (WHERE keep_month_ultra) AS share_ultra_unit_id_side,
      AVG(CASE WHEN keep_month_ultra AND listing_key_source = 'fingerprint_strict' THEN 1.0 ELSE 0.0 END) FILTER (WHERE keep_month_ultra) AS share_ultra_fingerprint_side,
      MEDIAN(rent_median_clean) FILTER (WHERE keep_month_ultra) AS median_rent_ultra_side,
      MEDIAN(beds_last) FILTER (WHERE keep_month_ultra) AS median_beds_ultra_side,
      MEDIAN(baths_last) FILTER (WHERE keep_month_ultra) AS median_baths_ultra_side,
      MEDIAN(sqft_last) FILTER (WHERE keep_month_ultra) AS median_sqft_ultra_side,
      AVG(CASE WHEN keep_month_ultra AND building_type_clean_last = 'multi_family' THEN 1.0 ELSE 0.0 END) FILTER (WHERE keep_month_ultra) AS share_multifamily_ultra_side
    FROM rd_listing_month_flagged
    WHERE %s
    GROUP BY 1, 2, 3
    ORDER BY 1, 3
    ",
    band_name,
    month_cond
  )
  side_quality_i <- collect_query(side_sql)
  side_quality_i <- as_date_cols(side_quality_i, "month_start")
  side_quality_list[[band_name]] <- side_quality_i

  pair_sql <- sprintf(
    "
    SELECT
      month_start,
      '%s' AS bandwidth,
      ward_pair_id_last AS ward_pair_id,
      SUM(CASE WHEN keep_month_ultra AND sign_last = -1 THEN 1 ELSE 0 END) AS n_less_ultra,
      SUM(CASE WHEN keep_month_ultra AND sign_last = 1 THEN 1 ELSE 0 END) AS n_more_ultra,
      CASE
        WHEN SUM(CASE WHEN keep_month_ultra AND sign_last = -1 THEN 1 ELSE 0 END) > 0
         AND SUM(CASE WHEN keep_month_ultra AND sign_last = 1 THEN 1 ELSE 0 END) > 0
        THEN 1 ELSE 0
      END AS both_sides_ge1,
      CASE
        WHEN SUM(CASE WHEN keep_month_ultra AND sign_last = -1 THEN 1 ELSE 0 END) >= 5
         AND SUM(CASE WHEN keep_month_ultra AND sign_last = 1 THEN 1 ELSE 0 END) >= 5
        THEN 1 ELSE 0
      END AS both_sides_ge5
    FROM rd_listing_month_flagged
    WHERE %s
    GROUP BY 1, 2, 3
    ORDER BY 1, 3
    ",
    band_name,
    month_cond
  )
  pair_support_i <- collect_query(pair_sql)
  pair_support_i <- as_date_cols(pair_support_i, "month_start")
  pair_support_list[[band_name]] <- pair_support_i
}

monthly_rd_quality_overall <- rbindlist(monthly_quality_list, fill = TRUE)
monthly_rd_quality_by_side <- rbindlist(side_quality_list, fill = TRUE)
pair_month_support <- rbindlist(pair_support_list, fill = TRUE)

monthly_rd_quality_side_gaps <- dcast(
  monthly_rd_quality_by_side,
  month_start + bandwidth ~ side,
  value.var = c(
    "n_month_ultra_side",
    "keep_month_ultra_rate_side",
    "share_ultra_fingerprint_side",
    "median_rent_ultra_side",
    "median_beds_ultra_side",
    "median_baths_ultra_side",
    "median_sqft_ultra_side",
    "share_multifamily_ultra_side"
  )
)
monthly_rd_quality_side_gaps[, `:=`(
  side_gap_n_month_ultra = abs(n_month_ultra_side_less_strict - n_month_ultra_side_more_strict),
  side_gap_keep_month_ultra_rate_pp = 100 * abs(keep_month_ultra_rate_side_less_strict - keep_month_ultra_rate_side_more_strict),
  side_gap_share_ultra_fingerprint_pp = 100 * abs(share_ultra_fingerprint_side_less_strict - share_ultra_fingerprint_side_more_strict),
  side_gap_median_rent_ultra = abs(median_rent_ultra_side_less_strict - median_rent_ultra_side_more_strict),
  side_gap_median_beds_ultra = abs(median_beds_ultra_side_less_strict - median_beds_ultra_side_more_strict),
  side_gap_median_baths_ultra = abs(median_baths_ultra_side_less_strict - median_baths_ultra_side_more_strict),
  side_gap_median_sqft_ultra = abs(median_sqft_ultra_side_less_strict - median_sqft_ultra_side_more_strict),
  side_gap_share_multifamily_ultra_pp = 100 * abs(share_multifamily_ultra_side_less_strict - share_multifamily_ultra_side_more_strict)
)]

monthly_skeleton <- data.table(month_start = seq(as.Date("2014-01-01"), as.Date("2025-12-01"), by = "month"))

monthly_rd_quality_overall <- monthly_rd_quality_overall[
  CJ(month_start = monthly_skeleton$month_start, bandwidth = names(bandwidths), unique = TRUE),
  on = .(month_start, bandwidth)
]
monthly_rd_quality_by_side <- monthly_rd_quality_by_side[
  CJ(month_start = monthly_skeleton$month_start, bandwidth = names(bandwidths), side = c("less_strict", "more_strict"), unique = TRUE),
  on = .(month_start, bandwidth, side)
]
monthly_rd_quality_side_gaps <- monthly_rd_quality_side_gaps[
  CJ(month_start = monthly_skeleton$month_start, bandwidth = names(bandwidths), unique = TRUE),
  on = .(month_start, bandwidth)
]

overall_with_gaps <- merge(
  monthly_rd_quality_overall,
  monthly_rd_quality_side_gaps[, .(
    month_start,
    bandwidth,
    side_gap_keep_month_ultra_rate_pp,
    side_gap_share_ultra_fingerprint_pp
  )],
  by = c("month_start", "bandwidth"),
  all.x = TRUE,
  sort = TRUE
)

overall_with_gaps <- overall_with_gaps[month_start >= as.Date("2021-01-01") & month_start <= as.Date("2025-12-01")]
overall_with_gaps[, month_status := vapply(
  seq_len(.N),
  function(i) {
    classify_month_status(
      observed_scrape_days = observed_scrape_days[i],
      raw_listing_month_cells = raw_listing_month_cells[i],
      n_month_ultra = n_month_ultra[i],
      keep_month_ultra_rate = keep_month_ultra_rate[i],
      p95_raw_listing_days_per_month = p95_raw_listing_days_per_month[i],
      share_raw_key_month_gt14 = share_raw_key_month_gt14[i],
      share_month_any_conflict_day = share_month_any_conflict_day[i],
      share_pair_month_both_sides_ge5 = share_pair_month_both_sides_ge5[i],
      side_gap_keep_month_ultra_rate_pp = side_gap_keep_month_ultra_rate_pp[i],
      side_gap_share_ultra_fingerprint_pp = side_gap_share_ultra_fingerprint_pp[i]
    )
  },
  character(1)
)]

candidate_windows <- rbindlist(lapply(split(overall_with_gaps, by = "bandwidth", keep.by = TRUE), function(dt_band) {
  band_name <- unique(dt_band$bandwidth)
  runs <- build_window_runs(dt_band)
  if (nrow(runs) == 0) {
    return(data.table(
      bandwidth = band_name,
      window_start = as.Date(character()),
      window_end = as.Date(character()),
      n_months = integer(),
      n_green = integer(),
      n_yellow = integer(),
      share_green = numeric(),
      median_n_month_ultra = numeric(),
      median_keep_month_ultra_rate = numeric(),
      median_share_pair_month_both_sides_ge5 = numeric()
    ))
  }
  runs[, bandwidth := band_name]
  setcolorder(runs, c("bandwidth", setdiff(names(runs), "bandwidth")))
  runs
}), fill = TRUE)

window_recommendation <- candidate_windows[
  order(-n_months, -share_green, -median_keep_month_ultra_rate, -median_share_pair_month_both_sides_ge5),
  .SD[1],
  by = bandwidth
]

top_day_multi_rent <- collect_query(
  "
  SELECT *
  FROM (
    SELECT
      'day_multi_rent' AS issue_type,
      month_start,
      file_date AS event_date,
      listing_key_source,
      listing_key,
      ward_pair_id,
      segment_id,
      sign,
      signed_dist,
      dist_ft,
      address_norm,
      company_norm,
      n_raw_rows_day AS metric_primary,
      n_rents_day AS metric_secondary,
      geo_span_ft_day AS metric_tertiary,
      ROW_NUMBER() OVER (ORDER BY n_rents_day DESC, n_raw_rows_day DESC, geo_span_ft_day DESC) AS rn
    FROM rd_listing_day_flagged
    WHERE day_status = 'multi_rent'
  ) q
  WHERE rn <= 75
  "
)

top_day_geo <- collect_query(
  "
  SELECT *
  FROM (
    SELECT
      'day_geo_or_assignment_conflict' AS issue_type,
      month_start,
      file_date AS event_date,
      listing_key_source,
      listing_key,
      ward_pair_id,
      segment_id,
      sign,
      signed_dist,
      dist_ft,
      address_norm,
      company_norm,
      n_raw_rows_day AS metric_primary,
      n_addresses_day AS metric_secondary,
      geo_span_ft_day AS metric_tertiary,
      ROW_NUMBER() OVER (ORDER BY geo_span_ft_day DESC, n_addresses_day DESC, n_raw_rows_day DESC) AS rn
    FROM rd_listing_day_flagged
    WHERE day_status IN ('multi_address', 'multi_side', 'multi_pair', 'multi_segment', 'geo_span_gt100')
  ) q
  WHERE rn <= 75
  "
)

top_month_range <- collect_query(
  "
  SELECT *
  FROM (
    SELECT
      'month_rent_range_or_conflict' AS issue_type,
      month_start,
      last_clean_file_date AS event_date,
      listing_key_source,
      listing_key,
      ward_pair_id_last AS ward_pair_id,
      segment_id_last AS segment_id,
      sign_last AS sign,
      signed_dist_last AS signed_dist,
      dist_ft_last AS dist_ft,
      address_norm_last AS address_norm,
      company_norm_last AS company_norm,
      n_conflict_days AS metric_primary,
      100 * rent_range_pct AS metric_secondary,
      geo_span_ft_clean AS metric_tertiary,
      ROW_NUMBER() OVER (ORDER BY n_conflict_days DESC, rent_range_pct DESC, geo_span_ft_clean DESC) AS rn
    FROM rd_listing_month_flagged
    WHERE month_status_ultra IN ('conflict_days_present', 'rent_range_gt25pct', 'geo_span_gt100')
  ) q
  WHERE rn <= 100
  "
)

manual_review_worst_cases <- rbindlist(list(
  top_day_multi_rent[, rn := NULL],
  top_day_geo[, rn := NULL],
  top_month_range[, rn := NULL]
), fill = TRUE)
manual_review_worst_cases <- as_date_cols(manual_review_worst_cases, c("month_start", "event_date"))

message("Writing QA outputs...")

write_csv_dt(join_coverage_summary, file.path(out_dir, "join_coverage_summary.csv"))
write_csv_dt(listing_key_source_by_month, file.path(out_dir, "listing_key_source_by_month.csv"))
write_csv_dt(day_drop_reason_by_month, file.path(out_dir, "day_drop_reason_by_month.csv"))
write_csv_dt(month_drop_reason_by_month, file.path(out_dir, "month_drop_reason_by_month.csv"))
write_csv_dt(monthly_rd_quality_overall, file.path(out_dir, "monthly_rd_quality_overall.csv"))
write_csv_dt(monthly_rd_quality_by_side, file.path(out_dir, "monthly_rd_quality_by_side.csv"))
write_csv_dt(monthly_rd_quality_side_gaps, file.path(out_dir, "monthly_rd_quality_side_gaps.csv"))
write_csv_dt(pair_month_support, file.path(out_dir, "pair_month_support.csv"))
write_csv_dt(candidate_windows, file.path(out_dir, "candidate_windows.csv"))
write_csv_dt(window_recommendation, file.path(out_dir, "window_recommendation.csv"))
write_csv_dt(manual_review_worst_cases, file.path(out_dir, "manual_review_worst_cases.csv"))

message("Rendering figures...")

keep_plot_dt <- monthly_rd_quality_overall[bandwidth %in% c("bw500", "bw1000")]
keep_long <- melt(
  keep_plot_dt,
  id.vars = c("month_start", "bandwidth"),
  measure.vars = c("keep_day_rate", "keep_month_ultra_rate"),
  variable.name = "series",
  value.name = "value"
)
keep_long[, series := factor(
  series,
  levels = c("keep_day_rate", "keep_month_ultra_rate"),
  labels = c("Keep Rate: Listing-Day", "Keep Rate: Listing-Month Ultra")
)]

p_keep <- ggplot(keep_long, aes(month_start, value, color = series)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~ bandwidth, ncol = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "RD Sample Keep Rates by Month"
  ) +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_monthly_keep_rates.pdf"), p_keep, width = 12, height = 6.8, bg = "white")

gap_plot_dt <- monthly_rd_quality_side_gaps[bandwidth == "bw500", .(
  month_start,
  side_gap_keep_month_ultra_rate_pp,
  side_gap_share_ultra_fingerprint_pp
)]
gap_long <- melt(
  gap_plot_dt,
  id.vars = "month_start",
  variable.name = "series",
  value.name = "value"
)
gap_long[, series := factor(
  series,
  levels = c("side_gap_keep_month_ultra_rate_pp", "side_gap_share_ultra_fingerprint_pp"),
  labels = c("Side Gap: Ultra Keep Rate (pp)", "Side Gap: Fingerprint Share (pp)")
)]

p_gap <- ggplot(gap_long, aes(month_start, value, color = series)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Border-Side QA Gaps Within 500 ft"
  ) +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_monthly_side_gaps.pdf"), p_gap, width = 12, height = 4.8, bg = "white")

status_fill <- c(GREEN = "#2a7f3b", YELLOW = "#d98e04", RED = "#b83a2f")
status_plot_dt <- copy(overall_with_gaps)
status_plot_dt[, bandwidth := factor(bandwidth, levels = c("bw250", "bw500", "bw1000", "all"))]

p_status <- ggplot(status_plot_dt, aes(month_start, bandwidth, fill = month_status)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = status_fill, na.value = "#d9d9d9", drop = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Candidate Window Status by Bandwidth"
  ) +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_candidate_window_timeline.pdf"), p_status, width = 12, height = 3.8, bg = "white")

message("RD listing sample task complete. Outputs written to:")
message("  - ../output/rd_listing_day_strict.parquet")
message("  - ../output/rd_listing_month_flagged.parquet")
message("  - ../output/rd_listing_month_ultra.parquet")
message("  - ../output/window_recommendation.csv")
