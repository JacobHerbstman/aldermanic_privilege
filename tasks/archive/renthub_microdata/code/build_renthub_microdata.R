source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")
source("../../_lib/renthub_market_helpers.R")
source("../../_lib/renthub_raw_listing_helpers.R")

library(DBI)
library(duckdb)
library(data.table)
library(dplyr)
library(readr)
library(sf)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_microdata/code")
# raw_dir <- "../input/renthub_raw"
# cpi_input <- "../input/fred_chi_cpi_all_items.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# trim_summary_input <- "../input/renthub_trim_sensitivity_summary.csv"
# scored_rent_input <- "../input/rent_with_ward_distances_full.parquet"
# census_blocks_2010_input <- "../input/census_blocks_2010.csv"
# census_blocks_2020_input <- "../input/census_blocks_2020.csv"
# market_series_input <- "../input/renthub_market_citywide_series.csv"
# clean_rows_output <- "../output/renthub_clean_rows.parquet"
# episodes_output <- "../output/renthub_listing_episodes.parquet"
# repeat_pairs_output <- "../output/renthub_repeat_pairs.parquet"
# key_coverage_output <- "../output/renthub_microdata_key_coverage.csv"
# episode_diag_output <- "../output/renthub_microdata_episode_diagnostics.csv"
# repeat_pair_diag_output <- "../output/renthub_microdata_repeat_pair_diagnostics.csv"
# geography_cov_output <- "../output/renthub_microdata_geography_coverage.csv"
# trend_validation_output <- "../output/renthub_microdata_trend_validation.csv"
# temp_dir <- "../temp"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    raw_dir,
    cpi_input,
    ward_panel_input,
    community_area_input,
    trim_summary_input,
    scored_rent_input,
    census_blocks_2010_input,
    census_blocks_2020_input,
    market_series_input,
    clean_rows_output,
    episodes_output,
    repeat_pairs_output,
    key_coverage_output,
    episode_diag_output,
    repeat_pair_diag_output,
    geography_cov_output,
    trend_validation_output,
    temp_dir
  )
}

if (length(args) != 18) {
  stop(
    paste(
      "FATAL: Script requires 18 args:",
      "<raw_dir> <cpi_input> <ward_panel_input> <community_area_input>",
      "<trim_summary_input> <scored_rent_input> <census_blocks_2010_input>",
      "<census_blocks_2020_input> <market_series_input> <clean_rows_output>",
      "<episodes_output> <repeat_pairs_output> <key_coverage_output>",
      "<episode_diag_output> <repeat_pair_diag_output>",
      "<geography_cov_output> <trend_validation_output> <temp_dir>"
    ),
    call. = FALSE
  )
}

raw_dir <- args[1]
cpi_input <- args[2]
ward_panel_input <- args[3]
community_area_input <- args[4]
trim_summary_input <- args[5]
scored_rent_input <- args[6]
census_blocks_2010_input <- args[7]
census_blocks_2020_input <- args[8]
market_series_input <- args[9]
clean_rows_output <- args[10]
episodes_output <- args[11]
repeat_pairs_output <- args[12]
key_coverage_output <- args[13]
episode_diag_output <- args[14]
repeat_pair_diag_output <- args[15]
geography_cov_output <- args[16]
trend_validation_output <- args[17]
temp_dir <- args[18]

analysis_start_month <- as.Date("2019-01-01")
real_base_year <- 2024L
episode_gap_days <- 45L
meaningful_price_change_log_threshold <- 0.01
repeat_min_gap_days <- 30L
repeat_max_gap_months <- 60L
censor_window_days <- 30L

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

market_series <- fread(market_series_input)
market_series[, month_start := as.Date(month_start)]
market_cycle_first <- market_series[series_id == "cycle_first"]
latest_market_month <- max(market_cycle_first$month_start, na.rm = TRUE)

cpi_dt <- load_monthly_cpi_deflator(
  cpi_input = cpi_input,
  start_month = analysis_start_month,
  end_month = latest_market_month,
  base_year = real_base_year
)

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, sprintf("PRAGMA temp_directory='%s'", duck_escape(temp_dir)))
dbWriteTable(con, "cpi_deflator", cpi_dt, temporary = TRUE, overwrite = TRUE)

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

raw_glob <- file.path(raw_dir, "*.parquet")
create_renthub_raw_day_tables(con, raw_glob, analysis_start_month)

coord_dt <- collect_query(
  sprintf(
    "
    SELECT DISTINCT
      coord_key,
      latitude,
      longitude
    FROM series_day_base
    WHERE keep_same_day = 1
      AND coord_key IS NOT NULL
      AND latitude IS NOT NULL
      AND longitude IS NOT NULL
    "
  )
)

coord_lookup <- build_coord_geography_lookup(
  coords_tbl = coord_dt,
  ward_panel_input = ward_panel_input,
  community_area_input = community_area_input
) %>%
  left_join(
    build_coord_block_lookup(
      coords_tbl = coord_dt,
      census_blocks_2010_input = census_blocks_2010_input,
      census_blocks_2020_input = census_blocks_2020_input
    ) %>%
      select(coord_key, block_id_2010, block_id_2020),
    by = "coord_key"
  )
setDT(coord_lookup)
dbWriteTable(con, "coord_lookup", coord_lookup, temporary = TRUE, overwrite = TRUE)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE scored_lookup AS
    SELECT DISTINCT
      CAST(id AS VARCHAR) AS id,
      CAST(file_date AS DATE) AS file_date,
      CAST(boundary_year AS INTEGER) AS boundary_year,
      CAST(ward AS INTEGER) AS scored_ward,
      CAST(neighbor_ward AS INTEGER) AS neighbor_ward,
      CAST(ward_pair_id AS VARCHAR) AS ward_pair_id,
      CASE
        WHEN ward_pair_id IS NOT NULL AND ward IS NOT NULL THEN CONCAT(CAST(ward_pair_id AS VARCHAR), '_', CAST(CAST(ward AS INTEGER) AS VARCHAR))
        ELSE NULL
      END AS ward_pair_side,
      CAST(dist_ft AS DOUBLE) AS dist_ft,
      CAST(signed_dist AS DOUBLE) AS signed_dist,
      CAST(sign AS INTEGER) AS boundary_side_sign,
      CAST(segment_id AS VARCHAR) AS segment_id,
      CAST(segment_reason AS VARCHAR) AS segment_reason
    FROM read_parquet('%s')
    WHERE CAST(file_date AS DATE) >= CAST('%s' AS DATE)
    ",
    duck_escape(scored_rent_input),
    format(analysis_start_month, "%Y-%m-%d")
  )
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE clean_rows_stage AS
    SELECT
      base.id,
      base.property_id,
      base.unit_id,
      base.analysis_key,
      base.key_source,
      base.fingerprint_key,
      base.month_start,
      base.file_date,
      base.posted_date,
      base.available_date,
      base.last_scraped_timestamp,
      CAST(base.rent_price AS DOUBLE) AS rent_price_nominal,
      CAST(base.rent_price * cpi.deflator_to_2024 AS DOUBLE) AS rent_price_real_2024,
      CASE
        WHEN base.sqft IS NOT NULL AND base.sqft > 0 THEN CAST((base.rent_price * cpi.deflator_to_2024) / base.sqft AS DOUBLE)
        ELSE NULL
      END AS rent_per_sqft_real_2024,
      base.beds,
      base.baths,
      base.sqft,
      CASE
        WHEN base.sqft IS NULL OR base.sqft <= 0 THEN 'missing'
        WHEN base.sqft < 500 THEN '0150_0499'
        WHEN base.sqft < 750 THEN '0500_0749'
        WHEN base.sqft < 1000 THEN '0750_0999'
        WHEN base.sqft < 1500 THEN '1000_1499'
        WHEN base.sqft < 2000 THEN '1500_1999'
        ELSE '2000_plus'
      END AS sqft_bin,
      base.building_type_clean,
      base.company_norm,
      base.address_norm,
      base.latitude,
      base.longitude,
      base.coord_key,
      CAST(lookup.community_area AS INTEGER) AS community_area,
      lookup.community_name,
      CASE
        WHEN YEAR(base.file_date) <= 2023 THEN CAST(lookup.ward_2015 AS INTEGER)
        ELSE CAST(lookup.ward_2024 AS INTEGER)
      END AS ward,
      lookup.block_id_2010,
      lookup.block_id_2020,
      scored.boundary_year,
      scored.neighbor_ward,
      scored.ward_pair_id,
      scored.ward_pair_side,
      scored.dist_ft,
      scored.signed_dist,
      scored.boundary_side_sign,
      scored.segment_id,
      scored.segment_reason,
      base.n_rows_day,
      base.n_ids_day,
      base.n_rents_day,
      base.keep_rent_trim01,
      base.keep_rent_trim025,
      base.keep_rent_trim05,
      base.keep_sqft_trim01,
      base.keep_sqft_trim025,
      base.keep_sqft_trim05,
      CASE
        WHEN base.%s = 1 AND base.%s = 1 THEN 1 ELSE 0
      END AS main_sample_flag,
      CASE WHEN base.beds IS NULL THEN 1 ELSE 0 END AS missing_beds_flag,
      CASE WHEN base.baths IS NULL THEN 1 ELSE 0 END AS missing_baths_flag,
      CASE WHEN base.sqft IS NULL OR base.sqft <= 0 THEN 1 ELSE 0 END AS missing_sqft_flag,
      CASE WHEN base.building_type_clean IS NULL OR base.building_type_clean = 'other' THEN 1 ELSE 0 END AS missing_building_type_flag,
      CASE WHEN base.unit_id IS NOT NULL THEN 1 ELSE 0 END AS has_unit_id_flag,
      CASE WHEN base.property_id IS NOT NULL THEN 1 ELSE 0 END AS has_property_id_flag,
      CASE WHEN base.key_source = 'fingerprint' THEN 1 ELSE 0 END AS fingerprint_fallback_flag,
      CAST(0 AS INTEGER) AS same_day_conflict_flag
    FROM series_day_base base
    INNER JOIN coord_lookup lookup
      ON base.coord_key = lookup.coord_key
    INNER JOIN cpi_deflator cpi
      ON base.month_start = cpi.month_start
    LEFT JOIN scored_lookup scored
      ON base.id = scored.id
     AND base.file_date = scored.file_date
    WHERE base.keep_same_day = 1
      AND base.rent_price BETWEEN 400 AND 20000
      AND (base.beds IS NULL OR CAST(ROUND(base.beds) AS INTEGER) BETWEEN 0 AND 6)
      AND (base.baths IS NULL OR base.baths <= 8)
      AND (base.sqft IS NULL OR (base.sqft >= 150 AND base.sqft <= 10000))
      AND lookup.community_area IS NOT NULL
    ",
    rent_trim_col,
    sqft_trim_col
  )
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE clean_rows_export AS
  SELECT
    id,
    property_id,
    unit_id,
    analysis_key,
    key_source,
    fingerprint_key,
    month_start,
    file_date,
    posted_date,
    available_date,
    last_scraped_timestamp AS scraped_timestamp,
    rent_price_nominal,
    rent_price_real_2024,
    rent_per_sqft_real_2024,
    beds,
    baths,
    sqft,
    sqft_bin,
    building_type_clean,
    company_norm,
    address_norm,
    latitude,
    longitude,
    coord_key,
    community_area,
    community_name,
    ward,
    block_id_2010,
    block_id_2020,
    boundary_year,
    neighbor_ward,
    ward_pair_id,
    ward_pair_side,
    dist_ft,
    signed_dist,
    boundary_side_sign,
    segment_id,
    segment_reason,
    n_rows_day,
    n_ids_day,
    n_rents_day,
    keep_rent_trim01,
    keep_rent_trim025,
    keep_rent_trim05,
    keep_sqft_trim01,
    keep_sqft_trim025,
    keep_sqft_trim05,
    main_sample_flag,
    missing_beds_flag,
    missing_baths_flag,
    missing_sqft_flag,
    missing_building_type_flag,
    has_unit_id_flag,
    has_property_id_flag,
    fingerprint_fallback_flag,
    same_day_conflict_flag
  FROM clean_rows_stage
  "
)

dbExecute(
  con,
  sprintf(
    "COPY clean_rows_export TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    duck_escape(clean_rows_output)
  )
)

create_renthub_gap_cycles(
  con,
  "clean_rows_stage",
  "episode_rows",
  gap_days = episode_gap_days,
  require_same_day = FALSE,
  value_col = "rent_price_nominal"
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episode_rows_enriched AS
    WITH ordered AS (
      SELECT
        e.*,
        LAG(rent_price_nominal) OVER (
          PARTITION BY analysis_key, cycle_id
          ORDER BY file_date, last_scraped_timestamp, rent_price_nominal, id
        ) AS prev_rent_in_episode,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, cycle_id
          ORDER BY file_date, last_scraped_timestamp, rent_price_nominal, id
        ) AS rn_first,
        ROW_NUMBER() OVER (
          PARTITION BY analysis_key, cycle_id
          ORDER BY file_date DESC, last_scraped_timestamp DESC, rent_price_nominal DESC, id DESC
        ) AS rn_last
      FROM episode_rows e
    )
    SELECT
      *,
      CASE
        WHEN prev_rent_in_episode IS NOT NULL
         AND ABS(LOG(rent_price_nominal) - LOG(prev_rent_in_episode)) > %f THEN 1 ELSE 0
      END AS meaningful_price_change_flag
    FROM ordered
    ",
    meaningful_price_change_log_threshold
  )
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE episodes_stage AS
  SELECT
    CONCAT(analysis_key, '__', CAST(cycle_id AS VARCHAR)) AS episode_id,
    analysis_key,
    key_source,
    cycle_id AS episode_number,
    MIN(file_date) AS episode_start_date,
    MAX(file_date) AS episode_end_date,
    DATE_DIFF('day', MIN(file_date), MAX(file_date)) AS days_active_observed,
    COUNT(*) AS n_obs,
    COUNT(DISTINCT file_date) AS n_unique_scrape_days,
    SUM(meaningful_price_change_flag) AS n_meaningful_price_changes,
    MAX(CASE WHEN rn_first = 1 THEN id END) AS id,
    MAX(CASE WHEN rn_first = 1 THEN property_id END) AS property_id,
    MAX(CASE WHEN rn_first = 1 THEN unit_id END) AS unit_id,
    MAX(CASE WHEN rn_first = 1 THEN fingerprint_key END) AS fingerprint_key,
    MAX(CASE WHEN rn_first = 1 THEN posted_date END) AS posted_date,
    MAX(CASE WHEN rn_first = 1 THEN available_date END) AS available_date,
    MAX(CASE WHEN rn_first = 1 THEN last_scraped_timestamp END) AS first_scraped_timestamp,
    MAX(CASE WHEN rn_last = 1 THEN last_scraped_timestamp END) AS last_scraped_timestamp,
    MAX(CASE WHEN rn_first = 1 THEN rent_price_nominal END) AS first_rent,
    MAX(CASE WHEN rn_last = 1 THEN rent_price_nominal END) AS last_rent,
    MIN(rent_price_nominal) AS min_rent,
    MAX(rent_price_nominal) AS max_rent,
    MAX(CASE WHEN rn_first = 1 THEN rent_price_real_2024 END) AS first_rent_real_2024,
    MAX(CASE WHEN rn_last = 1 THEN rent_price_real_2024 END) AS last_rent_real_2024,
    MAX(CASE WHEN rn_first = 1 THEN beds END) AS first_beds,
    MAX(CASE WHEN rn_last = 1 THEN beds END) AS last_beds,
    MAX(CASE WHEN rn_first = 1 THEN baths END) AS first_baths,
    MAX(CASE WHEN rn_last = 1 THEN baths END) AS last_baths,
    MAX(CASE WHEN rn_first = 1 THEN sqft END) AS first_sqft,
    MAX(CASE WHEN rn_last = 1 THEN sqft END) AS last_sqft,
    MAX(CASE WHEN rn_first = 1 THEN sqft_bin END) AS first_sqft_bin,
    MAX(CASE WHEN rn_last = 1 THEN sqft_bin END) AS last_sqft_bin,
    MAX(CASE WHEN rn_first = 1 THEN building_type_clean END) AS first_building_type_clean,
    MAX(CASE WHEN rn_last = 1 THEN building_type_clean END) AS last_building_type_clean,
    MAX(CASE WHEN rn_first = 1 THEN company_norm END) AS first_company_norm,
    MAX(CASE WHEN rn_last = 1 THEN company_norm END) AS last_company_norm,
    MAX(CASE WHEN rn_first = 1 THEN address_norm END) AS first_address_norm,
    MAX(CASE WHEN rn_last = 1 THEN address_norm END) AS last_address_norm,
    MAX(CASE WHEN rn_first = 1 THEN latitude END) AS latitude,
    MAX(CASE WHEN rn_first = 1 THEN longitude END) AS longitude,
    MAX(CASE WHEN rn_first = 1 THEN coord_key END) AS coord_key,
    MAX(CASE WHEN rn_first = 1 THEN community_area END) AS community_area,
    MAX(CASE WHEN rn_first = 1 THEN community_name END) AS community_name,
    MAX(CASE WHEN rn_first = 1 THEN ward END) AS ward,
    MAX(CASE WHEN rn_first = 1 THEN block_id_2010 END) AS block_id_2010,
    MAX(CASE WHEN rn_first = 1 THEN block_id_2020 END) AS block_id_2020,
    MAX(CASE WHEN rn_first = 1 THEN boundary_year END) AS boundary_year,
    MAX(CASE WHEN rn_first = 1 THEN neighbor_ward END) AS neighbor_ward,
    MAX(CASE WHEN rn_first = 1 THEN ward_pair_id END) AS ward_pair_id,
    MAX(CASE WHEN rn_first = 1 THEN ward_pair_side END) AS ward_pair_side,
    MAX(CASE WHEN rn_first = 1 THEN dist_ft END) AS dist_ft,
    MAX(CASE WHEN rn_first = 1 THEN signed_dist END) AS signed_dist,
    MAX(CASE WHEN rn_first = 1 THEN boundary_side_sign END) AS boundary_side_sign,
    MAX(CASE WHEN rn_first = 1 THEN segment_id END) AS segment_id,
    MAX(CASE WHEN rn_first = 1 THEN segment_reason END) AS segment_reason
  FROM episode_rows_enriched
  GROUP BY 1, 2, 3, 4
  "
)

latest_clean_date <- as.Date(collect_query("SELECT MAX(file_date) AS latest_clean_date FROM clean_rows_stage")$latest_clean_date[1])

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE episodes_final AS
    WITH base AS (
      SELECT
        *,
        LAG(episode_end_date) OVER (
          PARTITION BY analysis_key
          ORDER BY episode_start_date, episode_id
        ) AS prev_episode_end_date,
        LEAD(episode_start_date) OVER (
          PARTITION BY analysis_key
          ORDER BY episode_start_date, episode_id
        ) AS next_episode_start_date
      FROM episodes_stage
    )
    SELECT
      *,
      CASE WHEN first_rent > 0 THEN LOG(first_rent) ELSE NULL END AS log_first_rent,
      CASE WHEN last_rent > 0 THEN LOG(last_rent) ELSE NULL END AS log_last_rent,
      CASE
        WHEN first_rent > 0 AND last_rent > 0 THEN LOG(last_rent) - LOG(first_rent)
        ELSE NULL
      END AS episode_price_change,
      CASE
        WHEN first_rent > 0 AND min_rent > 0 AND LOG(min_rent) - LOG(first_rent) < -%f THEN 1 ELSE 0
      END AS rent_cut_flag,
      CASE
        WHEN first_rent > 0 AND max_rent > 0 AND LOG(max_rent) - LOG(first_rent) > %f THEN 1 ELSE 0
      END AS rent_increase_flag,
      CASE
        WHEN episode_start_date <= CAST('%s' AS DATE) + INTERVAL %d DAY THEN 1 ELSE 0
      END AS left_censor_flag,
      CASE
        WHEN episode_end_date >= CAST('%s' AS DATE) - INTERVAL %d DAY THEN 1 ELSE 0
      END AS right_censor_flag,
      DATE_DIFF('day', episode_end_date, next_episode_start_date) AS gap_to_next_episode_days,
      DATE_DIFF('month', episode_end_date, next_episode_start_date) AS gap_to_next_episode_months,
      CASE
        WHEN next_episode_start_date IS NOT NULL AND DATE_DIFF('day', episode_end_date, next_episode_start_date) >= %d THEN 1
        WHEN next_episode_start_date IS NULL AND episode_end_date <= CAST('%s' AS DATE) - INTERVAL %d DAY THEN 1
        ELSE 0
      END AS transaction_proxy_flag
    FROM base
    ",
    meaningful_price_change_log_threshold,
    meaningful_price_change_log_threshold,
    format(analysis_start_month, "%Y-%m-%d"),
    censor_window_days,
    format(latest_clean_date, "%Y-%m-%d"),
    censor_window_days,
    repeat_min_gap_days,
    format(latest_clean_date, "%Y-%m-%d"),
    censor_window_days
  )
)

dbExecute(
  con,
  sprintf(
    "COPY episodes_final TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    duck_escape(episodes_output)
  )
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE repeat_pairs_final AS
    SELECT
      prev.analysis_key,
      prev.key_source,
      CONCAT(prev.episode_id, '__', next.episode_id) AS repeat_pair_id,
      prev.episode_id AS episode_id_prev,
      next.episode_id AS episode_id_next,
      prev.episode_start_date AS prev_episode_start_date,
      prev.episode_end_date AS prev_episode_end_date,
      next.episode_start_date AS next_episode_start_date,
      next.episode_end_date AS next_episode_end_date,
      DATE_DIFF('day', prev.episode_end_date, next.episode_start_date) AS gap_between_episodes_days,
      DATE_DIFF('month', prev.episode_end_date, next.episode_start_date) AS gap_between_episodes_months,
      prev.first_rent AS prev_first_rent,
      next.first_rent AS next_first_rent,
      prev.last_rent AS prev_last_rent,
      next.last_rent AS next_last_rent,
      CASE
        WHEN prev.first_rent > 0 AND next.first_rent > 0 THEN LOG(next.first_rent) - LOG(prev.first_rent)
        ELSE NULL
      END AS repeat_log_change_first,
      CASE
        WHEN prev.last_rent > 0 AND next.last_rent > 0 THEN LOG(next.last_rent) - LOG(prev.last_rent)
        ELSE NULL
      END AS repeat_log_change_last,
      CASE
        WHEN prev.first_beds IS NOT NULL
         AND next.first_beds IS NOT NULL
         AND CAST(ROUND(prev.first_beds) AS INTEGER) = CAST(ROUND(next.first_beds) AS INTEGER) THEN 1 ELSE 0
      END AS same_beds_flag,
      CASE
        WHEN prev.first_baths IS NOT NULL
         AND next.first_baths IS NOT NULL
         AND prev.first_baths = next.first_baths THEN 1 ELSE 0
      END AS same_baths_flag,
      CASE
        WHEN prev.first_sqft_bin IS NOT NULL
         AND next.first_sqft_bin IS NOT NULL
         AND prev.first_sqft_bin = next.first_sqft_bin THEN 1 ELSE 0
      END AS same_sqft_bin_flag,
      CASE
        WHEN prev.first_building_type_clean IS NOT NULL
         AND next.first_building_type_clean IS NOT NULL
         AND prev.first_building_type_clean = next.first_building_type_clean THEN 1 ELSE 0
      END AS same_building_type_flag,
      CASE
        WHEN prev.coord_key IS NOT NULL AND prev.coord_key = next.coord_key THEN 1 ELSE 0
      END AS same_geography_flag,
      CASE
        WHEN prev.community_area IS NOT NULL AND prev.community_area = next.community_area THEN 1 ELSE 0
      END AS same_community_area_flag,
      CASE
        WHEN prev.ward IS NOT NULL AND prev.ward = next.ward THEN 1 ELSE 0
      END AS same_ward_flag,
      CASE
        WHEN prev.block_id_2010 IS NOT NULL AND prev.block_id_2010 = next.block_id_2010 THEN 1 ELSE 0
      END AS same_block_2010_flag,
      CASE
        WHEN prev.block_id_2020 IS NOT NULL AND prev.block_id_2020 = next.block_id_2020 THEN 1 ELSE 0
      END AS same_block_2020_flag,
      prev.community_area,
      prev.community_name,
      prev.ward,
      prev.block_id_2010,
      prev.block_id_2020,
      prev.ward_pair_id,
      prev.ward_pair_side,
      prev.dist_ft,
      prev.signed_dist,
      prev.boundary_side_sign,
      prev.segment_id,
      prev.segment_reason,
      prev.first_beds,
      prev.first_baths,
      prev.first_sqft,
      prev.first_sqft_bin,
      prev.first_building_type_clean,
      prev.transaction_proxy_flag AS prev_transaction_proxy_flag,
      next.transaction_proxy_flag AS next_transaction_proxy_flag
    FROM episodes_final prev
    INNER JOIN episodes_final next
      ON prev.analysis_key = next.analysis_key
     AND next.episode_number = prev.episode_number + 1
    WHERE DATE_DIFF('day', prev.episode_end_date, next.episode_start_date) >= %d
      AND DATE_DIFF('month', prev.episode_end_date, next.episode_start_date) <= %d
      AND prev.first_beds IS NOT NULL
      AND next.first_beds IS NOT NULL
      AND CAST(ROUND(prev.first_beds) AS INTEGER) = CAST(ROUND(next.first_beds) AS INTEGER)
      AND prev.first_sqft_bin IS NOT NULL
      AND next.first_sqft_bin IS NOT NULL
      AND prev.first_sqft_bin = next.first_sqft_bin
    ",
    repeat_min_gap_days,
    repeat_max_gap_months
  )
)

dbExecute(
  con,
  sprintf(
    "COPY repeat_pairs_final TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
    duck_escape(repeat_pairs_output)
  )
)

key_coverage <- rbindlist(list(
  data.table(
    section = "rows",
    item = c(
      "raw_day_rows",
      "same_day_rows_kept",
      "same_day_rows_dropped",
      "clean_rows",
      "unique_analysis_keys",
      "main_sample_rows"
    ),
    value = c(
      collect_query("SELECT COUNT(*) AS n FROM series_day_base")$n[1],
      collect_query("SELECT COUNT(*) AS n FROM series_day_base WHERE keep_same_day = 1")$n[1],
      collect_query("SELECT COUNT(*) AS n FROM series_day_base WHERE keep_same_day = 0")$n[1],
      collect_query("SELECT COUNT(*) AS n FROM clean_rows_stage")$n[1],
      collect_query("SELECT COUNT(DISTINCT analysis_key) AS n FROM clean_rows_stage")$n[1],
      collect_query("SELECT SUM(main_sample_flag) AS n FROM clean_rows_stage")$n[1]
    )
  ),
  data.table(
    section = "shares",
    item = c(
      "share_has_unit_id",
      "share_has_property_id",
      "share_fingerprint_fallback",
      "share_missing_beds",
      "share_missing_baths",
      "share_missing_sqft",
      "share_main_sample"
    ),
    value = c(
      collect_query("SELECT AVG(has_unit_id_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(has_property_id_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(fingerprint_fallback_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(missing_beds_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(missing_baths_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(missing_sqft_flag) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(main_sample_flag) AS v FROM clean_rows_stage")$v[1]
    )
  )
), fill = TRUE)
fwrite(key_coverage, key_coverage_output)

episode_diagnostics <- collect_query(
  "
  SELECT
    'overall' AS panel,
    'all' AS key_source,
    COUNT(*) AS n_episodes,
    COUNT(DISTINCT analysis_key) AS n_keys,
    AVG(n_obs) AS mean_obs_per_episode,
    QUANTILE_CONT(n_obs, 0.50) AS median_obs_per_episode,
    QUANTILE_CONT(n_obs, 0.90) AS p90_obs_per_episode,
    AVG(days_active_observed) AS mean_days_active_observed,
    QUANTILE_CONT(days_active_observed, 0.50) AS median_days_active_observed,
    QUANTILE_CONT(days_active_observed, 0.90) AS p90_days_active_observed,
    AVG(n_meaningful_price_changes) AS mean_meaningful_price_changes,
    AVG(left_censor_flag) AS share_left_censor,
    AVG(right_censor_flag) AS share_right_censor,
    AVG(transaction_proxy_flag) AS share_transaction_proxy
  FROM episodes_final

  UNION ALL

  SELECT
    'by_key_source' AS panel,
    key_source,
    COUNT(*) AS n_episodes,
    COUNT(DISTINCT analysis_key) AS n_keys,
    AVG(n_obs) AS mean_obs_per_episode,
    QUANTILE_CONT(n_obs, 0.50) AS median_obs_per_episode,
    QUANTILE_CONT(n_obs, 0.90) AS p90_obs_per_episode,
    AVG(days_active_observed) AS mean_days_active_observed,
    QUANTILE_CONT(days_active_observed, 0.50) AS median_days_active_observed,
    QUANTILE_CONT(days_active_observed, 0.90) AS p90_days_active_observed,
    AVG(n_meaningful_price_changes) AS mean_meaningful_price_changes,
    AVG(left_censor_flag) AS share_left_censor,
    AVG(right_censor_flag) AS share_right_censor,
    AVG(transaction_proxy_flag) AS share_transaction_proxy
  FROM episodes_final
  GROUP BY key_source
  "
)
fwrite(episode_diagnostics, episode_diag_output)

repeat_pair_diagnostics <- collect_query(
  "
  SELECT
    'overall' AS panel,
    'all' AS key_source,
    COUNT(*) AS n_repeat_pairs,
    COUNT(DISTINCT analysis_key) AS n_keys,
    AVG(gap_between_episodes_days) AS mean_gap_days,
    QUANTILE_CONT(gap_between_episodes_days, 0.50) AS median_gap_days,
    QUANTILE_CONT(gap_between_episodes_days, 0.90) AS p90_gap_days,
    AVG(same_baths_flag) AS share_same_baths,
    AVG(same_building_type_flag) AS share_same_building_type,
    AVG(same_geography_flag) AS share_same_geography,
    AVG(prev_transaction_proxy_flag) AS share_prev_transaction_proxy,
    AVG(next_transaction_proxy_flag) AS share_next_transaction_proxy
  FROM repeat_pairs_final

  UNION ALL

  SELECT
    'by_key_source' AS panel,
    key_source,
    COUNT(*) AS n_repeat_pairs,
    COUNT(DISTINCT analysis_key) AS n_keys,
    AVG(gap_between_episodes_days) AS mean_gap_days,
    QUANTILE_CONT(gap_between_episodes_days, 0.50) AS median_gap_days,
    QUANTILE_CONT(gap_between_episodes_days, 0.90) AS p90_gap_days,
    AVG(same_baths_flag) AS share_same_baths,
    AVG(same_building_type_flag) AS share_same_building_type,
    AVG(same_geography_flag) AS share_same_geography,
    AVG(prev_transaction_proxy_flag) AS share_prev_transaction_proxy,
    AVG(next_transaction_proxy_flag) AS share_next_transaction_proxy
  FROM repeat_pairs_final
  GROUP BY key_source
  "
)
fwrite(repeat_pair_diagnostics, repeat_pair_diag_output)

geography_coverage <- rbindlist(list(
  data.table(
    item = c(
      "share_nonmissing_community_area",
      "share_nonmissing_ward",
      "share_nonmissing_block_id_2010",
      "share_nonmissing_block_id_2020",
      "share_nonmissing_ward_pair_id",
      "share_nonmissing_signed_dist",
      "share_nonmissing_segment_id"
    ),
    value = c(
      collect_query("SELECT AVG(CASE WHEN community_area IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN ward IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN block_id_2010 IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN block_id_2020 IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN ward_pair_id IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN signed_dist IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1],
      collect_query("SELECT AVG(CASE WHEN segment_id IS NOT NULL THEN 1 ELSE 0 END) AS v FROM clean_rows_stage")$v[1]
    )
  )
), fill = TRUE)
fwrite(geography_coverage, geography_cov_output)

episode_trend <- collect_query(
  "
  SELECT
    CAST(DATE_TRUNC('month', episode_start_date) AS DATE) AS month_start,
    MEDIAN(first_rent) AS episode_first_rent_nominal,
    COUNT(*) AS n_episodes
  FROM episodes_final
  GROUP BY 1
  ORDER BY 1
  "
)
episode_trend[, episode_yoy_pct := compute_yoy_pct(episode_first_rent_nominal)]

market_cycle_first <- market_cycle_first[
  ,
  .(
    month_start,
    market_cycle_first_nominal = series_value_nominal,
    market_cycle_first_yoy_pct = yoy_pct,
    market_cycle_first_n_keys = n_keys
  )
]

trend_monthly <- merge(
  episode_trend,
  market_cycle_first,
  by = "month_start",
  all = TRUE
)
setorder(trend_monthly, month_start)
trend_monthly[, nominal_gap_pct := 100 * (episode_first_rent_nominal / market_cycle_first_nominal - 1)]
trend_monthly[, yoy_gap_pct := episode_yoy_pct - market_cycle_first_yoy_pct]
trend_monthly[, section := "monthly"]
trend_monthly[, c("metric", "value") := .(NA_character_, NA_real_)]

trend_summary <- data.table(
  section = "summary",
  month_start = as.Date(NA),
  episode_first_rent_nominal = NA_real_,
  n_episodes = NA_real_,
  episode_yoy_pct = NA_real_,
  market_cycle_first_nominal = NA_real_,
  market_cycle_first_yoy_pct = NA_real_,
  market_cycle_first_n_keys = NA_real_,
  nominal_gap_pct = NA_real_,
  yoy_gap_pct = NA_real_,
  metric = c(
    "month_overlap",
    "correlation_nominal",
    "median_abs_nominal_gap_pct",
    "correlation_yoy",
    "median_abs_yoy_gap_pct"
  ),
  value = c(
    trend_monthly[is.finite(episode_first_rent_nominal) & is.finite(market_cycle_first_nominal), .N],
    trend_monthly[is.finite(episode_first_rent_nominal) & is.finite(market_cycle_first_nominal), cor(episode_first_rent_nominal, market_cycle_first_nominal)],
    trend_monthly[is.finite(nominal_gap_pct), median(abs(nominal_gap_pct))],
    trend_monthly[is.finite(episode_yoy_pct) & is.finite(market_cycle_first_yoy_pct), cor(episode_yoy_pct, market_cycle_first_yoy_pct)],
    trend_monthly[is.finite(yoy_gap_pct), median(abs(yoy_gap_pct))]
  )
)

trend_validation <- rbindlist(list(trend_monthly, trend_summary), fill = TRUE)
fwrite(trend_validation, trend_validation_output)
