# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/rental_data_cleaning_audit/code")

source("../../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(DBI)
library(duckdb)
library(fixest)

con <- dbConnect(duckdb::duckdb(), dbdir = "../temp/rental_data_cleaning_audit.duckdb")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA threads=4"))
invisible(dbExecute(con, "PRAGMA temp_directory='../temp'"))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE MACRO clean_address_stem(address_value) AS (
    NULLIF(TRIM(REGEXP_REPLACE(
      REGEXP_REPLACE(
        REGEXP_REPLACE(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              REGEXP_REPLACE(
                REPLACE(REPLACE(UPPER(TRIM(CAST(address_value AS VARCHAR))), '.', ''), ',', ''),
                '\\bNORTH\\b', 'N'
              ),
              '\\bSOUTH\\b', 'S'
            ),
            '\\bEAST\\b', 'E'
          ),
          '\\bWEST\\b', 'W'
        ),
        ' (STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|PLACE|PLAZA|PLZ|PL|COURT|CT|DRIVE|DR|TERRACE|TER|LANE|LN)$',
        ''
      ),
      ' +', ' '
    )), '')
  )
  "
))

message("Materializing the production raw-data window...")
invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE raw_keyed AS
  WITH raw_source AS (
    SELECT
      UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city_raw,
      UPPER(TRIM(CAST(ID AS VARCHAR))) AS id_raw,
      UPPER(TRIM(CAST(PROPERTY_ID AS VARCHAR))) AS property_id_raw,
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
    FROM read_parquet('../input/renthub_raw/*.parquet', union_by_name = true)
    WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) BETWEEN DATE '2014-01-01' AND DATE '2022-12-31'
  ),
  cleaned AS (
    SELECT
      city_raw,
      CASE WHEN id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL ELSE id_raw END AS id,
      CASE WHEN property_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL ELSE property_id_raw END AS property_id,
      CASE WHEN unit_id_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL ELSE unit_id_raw END AS unit_id,
      CASE
        WHEN address_raw IS NULL OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
      END AS address_norm,
      clean_address_stem(CASE
        WHEN address_raw IS NULL OR address_raw IN ('', '0', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN NULL
        ELSE NULLIF(REGEXP_REPLACE(address_raw, ' +', ' '), '')
      END) AS address_stem,
      file_date,
      rent_price,
      beds,
      baths,
      sqft,
      latitude,
      longitude,
      CASE
        WHEN building_type_raw IN ('', 'NA', 'N/A', 'NAN', 'NULL', 'NONE', 'UNKNOWN') THEN 'other'
        WHEN building_type_raw = 'TH' OR building_type_raw LIKE '%TOWN%' THEN 'townhouse'
        WHEN building_type_raw IN ('CON', 'CONDO') OR building_type_raw LIKE '%CONDO%' OR building_type_raw LIKE '%CONDOMINIUM%' THEN 'condo'
        WHEN building_type_raw IN ('COMM', 'COMMERCIAL') OR building_type_raw LIKE '%COMMERCIAL%' THEN 'commercial'
        WHEN building_type_raw LIKE '%MULTI%' OR building_type_raw LIKE '%APART%' OR building_type_raw LIKE '%APT%'
          OR building_type_raw LIKE '%DUPLEX%' OR building_type_raw LIKE '%TRIPLEX%' OR building_type_raw LIKE '%FOURPLEX%' THEN 'multi_family'
        WHEN building_type_raw LIKE '%SINGLE%' OR building_type_raw LIKE '%HOUSE%'
          OR building_type_raw LIKE '%DETACHED%' OR building_type_raw LIKE '%SFR%' THEN 'single_family'
        ELSE 'other'
      END AS building_type_clean,
      CASE
        WHEN latitude BETWEEN -90 AND 90 AND longitude BETWEEN -180 AND 180
          THEN PRINTF('%.4f|%.4f', latitude, longitude)
        ELSE NULL
      END AS coord_key,
      CASE WHEN latitude BETWEEN 41.55 AND 42.10 AND longitude BETWEEN -88.10 AND -87.40 THEN TRUE ELSE FALSE END AS chicago_bbox
    FROM raw_source
    WHERE city_raw IN ('CHICAGO', 'CHGO')
  ),
  properties AS (
    SELECT
      *,
      CASE WHEN coord_key IS NULL THEN NULL ELSE COALESCE(address_norm, 'NO_ADDRESS') || '|' || coord_key END AS property_key
    FROM cleaned
  )
  SELECT
    *,
    CASE
      WHEN property_key IS NULL THEN NULL
      ELSE property_key || '|' || COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' || COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' || building_type_clean
    END AS floorplan_key,
    CASE
      WHEN unit_id IS NOT NULL THEN unit_id
      WHEN property_key IS NOT NULL THEN property_key || '|' || COALESCE(CAST(beds AS VARCHAR), 'MISSING') || '|' ||
        COALESCE(CAST(baths AS VARCHAR), 'MISSING') || '|' || COALESCE(CAST(sqft AS VARCHAR), 'MISSING') || '|' || building_type_clean
      ELSE NULL
    END AS analysis_key,
    CASE WHEN unit_id IS NOT NULL THEN 'unit_id' WHEN property_key IS NOT NULL THEN 'floorplan_fingerprint' ELSE 'unkeyed' END AS key_source
  FROM properties
  "
))

message("Reconstructing day-level trimming and aggregation...")
invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE floorplan_day_base AS
  SELECT
    DATE_TRUNC('month', file_date)::DATE AS month_start,
    file_date,
    CAST(YEAR(file_date) AS INTEGER) AS year,
    analysis_key,
    key_source,
    MIN(unit_id) AS unit_id,
    MIN(property_key) AS property_key,
    MIN(floorplan_key) AS floorplan_key,
    MIN(address_norm) AS address_norm,
    QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price IS NOT NULL) AS rent_price,
    QUANTILE_CONT(latitude, 0.50) FILTER (WHERE latitude IS NOT NULL) AS latitude,
    QUANTILE_CONT(longitude, 0.50) FILTER (WHERE longitude IS NOT NULL) AS longitude,
    QUANTILE_CONT(beds, 0.50) FILTER (WHERE beds IS NOT NULL) AS beds,
    QUANTILE_CONT(baths, 0.50) FILTER (WHERE baths IS NOT NULL) AS baths,
    QUANTILE_CONT(sqft, 0.50) FILTER (WHERE sqft IS NOT NULL) AS sqft,
    COUNT(*) AS raw_rows_day,
    COUNT(DISTINCT property_key) AS property_keys_day,
    COUNT(DISTINCT address_stem) AS address_stems_day,
    COUNT(DISTINCT floorplan_key) AS floorplan_keys_day
  FROM raw_keyed
  WHERE analysis_key IS NOT NULL AND coord_key IS NOT NULL
  GROUP BY 1, 2, 3, 4, 5
  "
))

invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE day_thresholds AS
  SELECT
    year,
    CASE WHEN beds IS NULL THEN 'missing' WHEN beds >= 4 THEN '4plus' ELSE CAST(CAST(beds AS INTEGER) AS VARCHAR) END AS beds_bin,
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
  CREATE OR REPLACE TABLE floorplan_day_flags AS
  SELECT
    d.*,
    d.latitude BETWEEN 41.55 AND 42.10 AND d.longitude BETWEEN -88.10 AND -87.40 AS keep_bbox,
    d.rent_price IS NOT NULL AND d.rent_price > 0
      AND d.rent_price >= COALESCE(t.rent_p01, d.rent_price)
      AND d.rent_price <= COALESCE(t.rent_p99, d.rent_price) AS keep_rent,
    d.sqft IS NULL OR (
      d.sqft > 0 AND d.sqft >= COALESCE(t.sqft_p01, d.sqft) AND d.sqft <= COALESCE(t.sqft_p99, d.sqft)
    ) AS keep_sqft
  FROM floorplan_day_base d
  LEFT JOIN day_thresholds t
    ON d.year = t.year
    AND CASE WHEN d.beds IS NULL THEN 'missing' WHEN d.beds >= 4 THEN '4plus' ELSE CAST(CAST(d.beds AS INTEGER) AS VARCHAR) END = t.beds_bin
  "
))

raw_city_coverage <- as.data.table(dbGetQuery(
  con,
  "
  WITH all_cities AS (
    SELECT
      UPPER(TRIM(CAST(CITY AS VARCHAR))) AS city,
      COUNT(*) AS raw_rows,
      COUNT(DISTINCT TRY_CAST(SCRAPED_TIMESTAMP AS DATE)) AS scrape_days,
      MIN(TRY_CAST(SCRAPED_TIMESTAMP AS DATE)) AS first_date,
      MAX(TRY_CAST(SCRAPED_TIMESTAMP AS DATE)) AS last_date
    FROM read_parquet('../input/renthub_raw/*.parquet', union_by_name = true)
    WHERE TRY_CAST(SCRAPED_TIMESTAMP AS DATE) BETWEEN DATE '2014-01-01' AND DATE '2022-12-31'
    GROUP BY 1
  )
  SELECT * FROM all_cities ORDER BY raw_rows DESC
  "
))
fwrite(raw_city_coverage, "../output/raw_city_coverage.csv")

monthly_coverage <- as.data.table(dbGetQuery(
  con,
  "
  SELECT
    DATE_TRUNC('month', file_date)::DATE AS month_start,
    COUNT(*) AS raw_rows,
    COUNT(DISTINCT file_date) AS scrape_days,
    COUNT(DISTINCT analysis_key) AS analysis_keys,
    AVG(CASE WHEN city_raw = 'CHGO' THEN 1.0 ELSE 0.0 END) AS share_chgo,
    AVG(CASE WHEN unit_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_with_unit_id,
    AVG(CASE WHEN chicago_bbox THEN 1.0 ELSE 0.0 END) AS share_chicago_bbox
  FROM raw_keyed
  GROUP BY 1
  ORDER BY 1
  "
))
fwrite(monthly_coverage, "../output/monthly_coverage.csv")

cleaning_stage_counts <- rbindlist(list(
  data.table(stage = "raw Chicago/CHGO rows", unit = "raw rows", n = dbGetQuery(con, "SELECT COUNT(*) n FROM raw_keyed")$n),
  data.table(stage = "raw rows with world-valid coordinates", unit = "raw rows", n = dbGetQuery(con, "SELECT COUNT(*) n FROM raw_keyed WHERE coord_key IS NOT NULL")$n),
  data.table(stage = "raw rows with analysis key and coordinates", unit = "raw rows", n = dbGetQuery(con, "SELECT COUNT(*) n FROM raw_keyed WHERE analysis_key IS NOT NULL AND coord_key IS NOT NULL")$n),
  data.table(stage = "collapsed analysis-key days", unit = "analysis-key days", n = dbGetQuery(con, "SELECT COUNT(*) n FROM floorplan_day_flags")$n),
  data.table(stage = "days inside Chicago bounding box", unit = "analysis-key days", n = dbGetQuery(con, "SELECT COUNT(*) n FROM floorplan_day_flags WHERE keep_bbox")$n),
  data.table(stage = "days after rent trim", unit = "analysis-key days", n = dbGetQuery(con, "SELECT COUNT(*) n FROM floorplan_day_flags WHERE keep_bbox AND keep_rent")$n),
  data.table(stage = "days after rent and square-foot trims", unit = "analysis-key days", n = dbGetQuery(con, "SELECT COUNT(*) n FROM floorplan_day_flags WHERE keep_bbox AND keep_rent AND keep_sqft")$n),
  data.table(stage = "production monthly panel", unit = "analysis-key months", n = dbGetQuery(con, "SELECT COUNT(*) n FROM read_parquet('../input/chicago_rent_panel.parquet')")$n)
))
cleaning_stage_counts[, prior_stage_n := shift(n)]
cleaning_stage_counts[, share_of_prior := n / prior_stage_n]
fwrite(cleaning_stage_counts, "../output/cleaning_stage_counts.csv")

message("Auditing native unit identifiers...")
invisible(dbExecute(
  con,
  "
  CREATE OR REPLACE TABLE unit_contract AS
  SELECT
    unit_id,
    COUNT(*) AS raw_rows,
    COUNT(DISTINCT property_id) AS property_ids,
    COUNT(DISTINCT address_norm) AS addresses,
    COUNT(DISTINCT address_stem) AS address_stems,
    COUNT(DISTINCT coord_key) AS coordinate_keys,
    COUNT(DISTINCT property_key) AS property_keys,
    MIN(file_date) AS first_date,
    MAX(file_date) AS last_date,
    MIN(latitude) AS min_latitude,
    MAX(latitude) AS max_latitude,
    MIN(longitude) AS min_longitude,
    MAX(longitude) AS max_longitude,
    SQRT(
      POW((MAX(latitude) - MIN(latitude)) * 364000, 2) +
      POW((MAX(longitude) - MIN(longitude)) * 288200 * COS(AVG(latitude) * PI() / 180), 2)
    ) AS coordinate_span_ft,
    STRING_AGG(DISTINCT address_norm, '; ' ORDER BY address_norm) AS address_values
  FROM raw_keyed
  WHERE unit_id IS NOT NULL
  GROUP BY 1
  "
))

unit_id_contract_summary <- as.data.table(dbGetQuery(
  con,
  "
  SELECT 'all native unit IDs' AS diagnostic, COUNT(*) AS n_unit_ids, SUM(raw_rows) AS raw_rows FROM unit_contract
  UNION ALL
  SELECT 'multiple normalized addresses', COUNT(*), SUM(raw_rows) FROM unit_contract WHERE addresses > 1
  UNION ALL
  SELECT 'multiple normalized address stems', COUNT(*), SUM(raw_rows) FROM unit_contract WHERE address_stems > 1
  UNION ALL
  SELECT 'coordinate span over 500ft', COUNT(*), SUM(raw_rows) FROM unit_contract WHERE coordinate_span_ft > 500
  UNION ALL
  SELECT 'multiple property IDs', COUNT(*), SUM(raw_rows) FROM unit_contract WHERE property_ids > 1
  UNION ALL
  SELECT 'multiple production property keys', COUNT(*), SUM(raw_rows) FROM unit_contract WHERE property_keys > 1
  "
))
fwrite(unit_id_contract_summary, "../output/unit_id_contract_summary.csv")

unit_id_collision_examples <- as.data.table(dbGetQuery(
  con,
  "
  SELECT *
  FROM unit_contract
  WHERE address_stems > 1 OR coordinate_span_ft > 500
  ORDER BY raw_rows DESC, coordinate_span_ft DESC
  LIMIT 100
  "
))
fwrite(unit_id_collision_examples, "../output/unit_id_collision_examples.csv")

panel_identity_summary <- as.data.table(dbGetQuery(
  con,
  "
  WITH panel AS (
    SELECT * FROM read_parquet('../input/chicago_rent_panel.parquet')
  ),
  chgo_keys AS (
    SELECT
      analysis_key,
      MAX(city_raw = 'CHICAGO') AS seen_chicago,
      MAX(city_raw = 'CHGO') AS seen_chgo
    FROM raw_keyed
    WHERE analysis_key IS NOT NULL
    GROUP BY 1
  )
  SELECT 'monthly rows' AS diagnostic, COUNT(*) AS n, COUNT(DISTINCT analysis_key) AS distinct_analysis_keys FROM panel
  UNION ALL
  SELECT 'native unit ID rows', COUNT(*), COUNT(DISTINCT analysis_key) FROM panel WHERE key_source = 'unit_id'
  UNION ALL
  SELECT 'floorplan-fingerprint rows', COUNT(*), COUNT(DISTINCT analysis_key) FROM panel WHERE key_source = 'floorplan_fingerprint'
  UNION ALL
  SELECT 'rows from CHGO-only analysis keys', COUNT(*), COUNT(DISTINCT p.analysis_key)
    FROM panel p INNER JOIN chgo_keys c ON p.analysis_key = c.analysis_key
    WHERE c.seen_chgo AND NOT c.seen_chicago
  UNION ALL
  SELECT 'native-unit months merging property keys in raw data', COUNT(*), COUNT(DISTINCT d.analysis_key)
    FROM floorplan_day_base d
    WHERE d.key_source = 'unit_id' AND d.property_keys_day > 1
  UNION ALL
  SELECT 'native-unit months merging address stems in raw data', COUNT(*), COUNT(DISTINCT d.analysis_key)
    FROM floorplan_day_base d
    WHERE d.key_source = 'unit_id' AND d.address_stems_day > 1
  "
))
fwrite(panel_identity_summary, "../output/panel_identity_summary.csv")

panel_proxy_duplicates <- as.data.table(dbGetQuery(
  con,
  "
  WITH panel AS (
    SELECT
      p.rent_panel_id,
      p.month_start,
      p.rent_price,
      p.beds,
      p.baths,
      p.sqft,
      p.building_type_clean,
      q.address_stem,
      q.geometry_latitude,
      q.geometry_longitude
    FROM read_parquet('../input/chicago_rent_panel.parquet') p
    INNER JOIN read_parquet('../input/chicago_rent_panel_quality_flags.parquet') q
      USING (rent_panel_id)
  ),
  proxy_groups AS (
    SELECT
      month_start,
      address_stem,
      geometry_latitude,
      geometry_longitude,
      beds,
      baths,
      sqft,
      building_type_clean,
      COUNT(*) AS n_rows,
      COUNT(DISTINCT rent_price) AS n_rents,
      MIN(rent_price) AS min_rent,
      MAX(rent_price) AS max_rent
    FROM panel
    WHERE address_stem IS NOT NULL
    GROUP BY 1, 2, 3, 4, 5, 6, 7, 8
  )
  SELECT *
  FROM proxy_groups
  WHERE n_rows > 1
  ORDER BY n_rows DESC, month_start, address_stem
  "
))

floorplan_proxy_duplicate_summary <- data.table(
  scope = "full production panel",
  n_rows = dbGetQuery(con, "SELECT COUNT(*) n FROM read_parquet('../input/chicago_rent_panel.parquet')")$n,
  duplicate_groups = nrow(panel_proxy_duplicates),
  rows_in_duplicate_groups = sum(panel_proxy_duplicates$n_rows),
  excess_rows_over_one_per_proxy_month = sum(panel_proxy_duplicates$n_rows - 1),
  groups_with_multiple_rents = sum(panel_proxy_duplicates$n_rents > 1)
)
fwrite(head(panel_proxy_duplicates, 100), "../output/floorplan_proxy_duplicate_examples.csv")

message("Tracing the final 500ft estimation sample...")
rent <- as.data.table(read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet"))
quality_flags <- as.data.table(read_parquet("../input/chicago_rent_panel_quality_flags.parquet"))
if (anyDuplicated(quality_flags$rent_panel_id) > 0) {
  stop("Quality-key audit input is not unique by rent_panel_id.", call. = FALSE)
}
quality_audit_cols <- c(
  "rent_panel_id",
  "address_stem",
  "flag_manual_location_verified",
  "flag_hard_invalid_candidate",
  "flag_building_type_conflict",
  "flag_large_property_has_single_family",
  "flag_property_adjacent_jump_gt2",
  "flag_property_adjacent_jump_gt3",
  "flag_same_day_rent_spread_gt2",
  "flag_same_day_rent_spread_gt3",
  "flag_one_day_bulk",
  "flag_one_day_bulk_no_support",
  "flag_distance_to_corrected_location_gt100ft",
  "flag_distance_to_corrected_location_gt200ft",
  "flag_posted_lag_gt90",
  "flag_posted_lag_gt180"
)
rent <- merge(
  rent,
  quality_flags[, ..quality_audit_cols],
  by = "rent_panel_id",
  all.x = TRUE,
  sort = FALSE
)
chgo_only_keys <- dbGetQuery(
  con,
  "
  SELECT analysis_key
  FROM raw_keyed
  WHERE analysis_key IS NOT NULL
  GROUP BY 1
  HAVING MAX(city_raw = 'CHGO') AND NOT MAX(city_raw = 'CHICAGO')
  "
)$analysis_key
rent[, chgo_only_analysis_key := analysis_key %chin% chgo_only_keys]
rent[, file_date := as.Date(file_date)]
rent[, year := as.integer(format(file_date, "%Y"))]
rent[, year_month := format(file_date, "%Y-%m")]
rent[, signed_dist_ft := as.numeric(signed_dist)]
rent[, right := as.integer(signed_dist_ft >= 0)]
rent[, log_sqft_audit := fifelse(is.finite(sqft) & sqft > 0, log(sqft), NA_real_)]
rent[, log_baths_audit := fifelse(is.finite(baths) & baths > 0, log(baths), NA_real_)]

amenity_cols <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
rent[, amenities_complete := Reduce(`&`, lapply(.SD, is.finite)), .SDcols = amenity_cols]

sample_steps <- list(
  "characteristics task 500ft input" = rep(TRUE, nrow(rent)),
  "valid date, rent, scores, segment, and pair" = !is.na(rent$file_date) & rent$year >= 2014 & rent$year <= 2022 &
    is.finite(rent$rent_price) & rent$rent_price > 0 & is.finite(rent$signed_dist_ft) & abs(rent$signed_dist_ft) <= 500 &
    is.finite(rent$strictness_own) & is.finite(rent$strictness_neighbor) & !is.na(rent$segment_id) & rent$segment_id != "" &
    !is.na(rent$ward_pair_id) & rent$ward_pair_id != "",
  "clean location" = rent$flag_clean_location_sample,
  "nonmissing nonnegative bedrooms" = is.finite(rent$beds) & rent$beds >= 0,
  "positive nonmissing square feet" = is.finite(rent$log_sqft_audit),
  "positive nonmissing bathrooms" = is.finite(rent$log_baths_audit),
  "complete amenity distances" = rent$amenities_complete
)

keep <- rep(TRUE, nrow(rent))
final_sample_attrition <- rbindlist(lapply(names(sample_steps), function(step) {
  keep <<- keep & coalesce(sample_steps[[step]], FALSE)
  rent[keep, .(
    stage = step,
    n_rows = .N,
    n_analysis_keys = uniqueN(analysis_key),
    n_property_keys = uniqueN(property_key),
    n_segments = uniqueN(segment_id),
    n_ward_pairs = uniqueN(ward_pair_id),
    share_stricter_side = mean(right == 1),
    share_unit_id_source = mean(key_source == "unit_id"),
    share_studio = mean(beds == 0, na.rm = TRUE),
    share_quality_high_or_severe = mean(quality_flag_severity %in% c("high", "severe"), na.rm = TRUE)
  )]
}), fill = TRUE)
final_sample_attrition[, prior_stage_n := shift(n_rows)]
final_sample_attrition[, share_of_prior := n_rows / prior_stage_n]
fwrite(final_sample_attrition, "../output/final_sample_attrition.csv")

selection_base <- rent[coalesce(sample_steps[["valid date, rent, scores, segment, and pair"]], FALSE)]
selection_base[, `:=`(
  clean_location_available = as.integer(flag_clean_location_sample),
  sqft_available = as.integer(is.finite(sqft) & sqft > 0),
  baths_available = as.integer(is.finite(baths) & baths > 0)
)]
selection_samples <- list(
  "clean location retained" = list(data = selection_base, outcome = "clean_location_available"),
  "square feet observed among clean locations" = list(
    data = selection_base[flag_clean_location_sample == TRUE],
    outcome = "sqft_available"
  ),
  "bathrooms observed among clean locations" = list(
    data = selection_base[flag_clean_location_sample == TRUE],
    outcome = "baths_available"
  )
)
sample_selection_balance <- rbindlist(lapply(names(selection_samples), function(diagnostic) {
  selection_model <- feols(
    as.formula(paste0(selection_samples[[diagnostic]]$outcome, " ~ right | segment_id^year_month")),
    data = selection_samples[[diagnostic]]$data,
    cluster = ~segment_id,
    warn = FALSE,
    notes = FALSE
  )
  selection_row <- coeftable(selection_model)["right", ]
  data.table(
    diagnostic,
    estimate = unname(selection_row["Estimate"]),
    std_error = unname(selection_row["Std. Error"]),
    p_value = unname(selection_row["Pr(>|t|)"]),
    n = nobs(selection_model)
  )
}))
fwrite(sample_selection_balance, "../output/sample_selection_balance.csv")

rent[, final_sample := keep]
final_proxy_duplicates <- rent[
  final_sample == TRUE & !is.na(address_stem) & address_stem != "",
  .(
    n_rows = .N,
    n_rents = uniqueN(rent_price),
    min_rent = min(rent_price),
    max_rent = max(rent_price)
  ),
  by = .(
    year_month,
    address_stem,
    longitude,
    latitude,
    beds,
    baths,
    sqft,
    building_type_clean
  )
][n_rows > 1]
floorplan_proxy_duplicate_summary <- rbind(
  floorplan_proxy_duplicate_summary,
  data.table(
    scope = "final fully controlled RD sample",
    n_rows = sum(rent$final_sample),
    duplicate_groups = nrow(final_proxy_duplicates),
    rows_in_duplicate_groups = sum(final_proxy_duplicates$n_rows),
    excess_rows_over_one_per_proxy_month = sum(final_proxy_duplicates$n_rows - 1),
    groups_with_multiple_rents = sum(final_proxy_duplicates$n_rents > 1)
  ),
  fill = TRUE
)
fwrite(floorplan_proxy_duplicate_summary, "../output/floorplan_proxy_duplicate_summary.csv")
final_sample_quality_balance <- rent[, .(
  n_rows = .N,
  share_stricter_side = mean(right == 1),
  share_geometry_corrected = mean(flag_geometry_uses_address_location_correction, na.rm = TRUE),
  share_location_questionable = mean(flag_location_questionable, na.rm = TRUE),
  share_modal_assignment_missing = mean(flag_modal_assignment_missing, na.rm = TRUE),
  share_modal_changes_ward = mean(flag_modal_changes_ward | flag_modal_changes_neighbor_ward, na.rm = TRUE),
  share_modal_changes_pair = mean(flag_modal_changes_pair, na.rm = TRUE),
  share_modal_distance_diff_over_100ft = mean(flag_modal_dist_diff_gt100ft, na.rm = TRUE),
  share_quality_none = mean(quality_flag_severity == "none", na.rm = TRUE),
  share_quality_low = mean(quality_flag_severity == "low", na.rm = TRUE),
  share_quality_high = mean(quality_flag_severity == "high", na.rm = TRUE),
  share_quality_severe = mean(quality_flag_severity == "severe", na.rm = TRUE)
), by = .(sample = fifelse(final_sample, "final model sample", "excluded before model"), side = fifelse(right == 1, "stricter", "lenient"))]
fwrite(final_sample_quality_balance, "../output/final_sample_quality_balance.csv")

audit_flag_cols <- setdiff(quality_audit_cols, c("rent_panel_id", "address_stem"))
final_sample_flag_prevalence <- rbindlist(lapply(audit_flag_cols, function(flag) {
  rent[final_sample == TRUE, .(
    flag,
    n_rows = .N,
    n_flagged = sum(get(flag), na.rm = TRUE),
    share_flagged = mean(get(flag), na.rm = TRUE)
  )]
}))
setorder(final_sample_flag_prevalence, -share_flagged)
fwrite(final_sample_flag_prevalence, "../output/final_sample_flag_prevalence.csv")

building_type_conflict_summary <- as.data.table(dbGetQuery(
  con,
  "
  WITH property_types AS (
    SELECT
      property_key,
      COUNT(*) AS raw_rows,
      COUNT(DISTINCT building_type_clean) AS n_types,
      STRING_AGG(DISTINCT building_type_clean, ';' ORDER BY building_type_clean) AS type_values
    FROM raw_keyed
    WHERE property_key IS NOT NULL
    GROUP BY 1
  )
  SELECT
    n_types,
    type_values,
    COUNT(*) AS property_keys,
    SUM(raw_rows) AS raw_rows
  FROM property_types
  GROUP BY 1, 2
  ORDER BY n_types DESC, property_keys DESC
  "
))
fwrite(building_type_conflict_summary, "../output/building_type_conflict_summary.csv")

message("Running diagnostic model variants...")
model_data <- rent[final_sample == TRUE]
model_data[, beds_factor := factor(beds)]
model_data[, building_type_factor := factor(fifelse(is.na(building_type_clean), "other", building_type_clean))]
model_data[, `:=`(
  nearest_school_dist_kft = nearest_school_dist_ft / 1000,
  nearest_park_dist_kft = nearest_park_dist_ft / 1000,
  nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
  nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
  lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
)]

fml <- log(rent_price) ~ right + log_sqft_audit + beds_factor + log_baths_audit + building_type_factor +
  nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft +
  lake_michigan_dist_kft | segment_id^year_month

collapsed_model_data <- model_data[
  ,
  .(
    rent_price = median(rent_price),
    log_sqft_audit = first(log_sqft_audit),
    log_baths_audit = first(log_baths_audit),
    nearest_school_dist_kft = first(nearest_school_dist_kft),
    nearest_park_dist_kft = first(nearest_park_dist_kft),
    nearest_major_road_dist_kft = first(nearest_major_road_dist_kft),
    nearest_cta_stop_dist_kft = first(nearest_cta_stop_dist_kft),
    lake_michigan_dist_kft = first(lake_michigan_dist_kft)
  ),
  by = .(
    proxy_address = fifelse(is.na(address_stem) | address_stem == "", paste0("NO_ADDRESS|", property_key), address_stem),
    longitude,
    latitude,
    year_month,
    segment_id,
    ward_pair_id,
    right,
    beds,
    baths,
    sqft,
    building_type_clean
  )
]
collapsed_model_data[, beds_factor := factor(beds)]
collapsed_model_data[, building_type_factor := factor(fifelse(is.na(building_type_clean), "other", building_type_clean))]

collapsed_without_type <- model_data[
  ,
  .(
    rent_price = median(rent_price),
    log_sqft_audit = first(log_sqft_audit),
    log_baths_audit = first(log_baths_audit),
    nearest_school_dist_kft = first(nearest_school_dist_kft),
    nearest_park_dist_kft = first(nearest_park_dist_kft),
    nearest_major_road_dist_kft = first(nearest_major_road_dist_kft),
    nearest_cta_stop_dist_kft = first(nearest_cta_stop_dist_kft),
    lake_michigan_dist_kft = first(lake_michigan_dist_kft)
  ),
  by = .(
    proxy_address = fifelse(is.na(address_stem) | address_stem == "", paste0("NO_ADDRESS|", property_key), address_stem),
    longitude,
    latitude,
    year_month,
    segment_id,
    ward_pair_id,
    right,
    beds,
    baths,
    sqft
  )
]
collapsed_without_type[, beds_factor := factor(beds)]

fml_without_building_type <- log(rent_price) ~ right + log_sqft_audit + beds_factor + log_baths_audit +
  nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft +
  lake_michigan_dist_kft | segment_id^year_month

fml_without_sqft <- log(rent_price) ~ right + beds_factor + log_baths_audit + building_type_factor +
  nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft +
  lake_michigan_dist_kft | segment_id^year_month

model_datasets <- list(
  "production sample" = model_data,
  "exclude severe quality flags" = model_data[quality_flag_severity != "severe"],
  "retain only none/low quality flags" = model_data[quality_flag_severity %in% c("none", "low")],
  "exclude building-type conflicts" = model_data[flag_building_type_conflict == FALSE],
  "exclude large properties labeled single-family" = model_data[flag_large_property_has_single_family == FALSE],
  "exclude manually verified address locations" = model_data[flag_manual_location_verified == FALSE],
  "exclude coordinate corrections over 100ft" = model_data[flag_distance_to_corrected_location_gt100ft == FALSE],
  "exclude unsupported one-day bulk records" = model_data[flag_one_day_bulk_no_support == FALSE],
  "exclude addressless floorplan proxies" = model_data[coalesce(as.logical(address_missing), FALSE) == FALSE],
  "exclude CHGO-only source keys" = model_data[chgo_only_analysis_key == FALSE],
  "exclude May 2015 transition month" = model_data[year_month != "2015-05"],
  "2016-2022 higher-frequency scrape period" = model_data[year >= 2016],
  "2019-2022 recent scrape period" = model_data[year >= 2019],
  "one row per corrected floorplan proxy-month" = collapsed_model_data
)

model_sensitivity <- rbindlist(lapply(names(model_datasets), function(specification) {
  model_sample <- model_datasets[[specification]]
  model <- feols(fml, data = model_sample, cluster = ~segment_id, warn = FALSE, notes = FALSE)
  row <- coeftable(model)["right", ]
  data.table(
    specification,
    estimate = unname(row["Estimate"]),
    std_error = unname(row["Std. Error"]),
    p_value = unname(row["Pr(>|t|)"]),
    n = nobs(model),
    segments = uniqueN(model_sample$segment_id),
    ward_pairs = uniqueN(model_sample$ward_pair_id)
  )
}))

baseline_estimate <- model_sensitivity[specification == "production sample", estimate]
manual_addresses <- sort(unique(model_data[flag_manual_location_verified == TRUE, address_stem]))
manual_address_influence <- rbindlist(lapply(manual_addresses, function(address) {
  address_sample <- model_data[is.na(address_stem) | address_stem != address]
  address_model <- feols(fml, data = address_sample, cluster = ~segment_id, warn = FALSE, notes = FALSE)
  address_row <- coeftable(address_model)["right", ]
  support <- model_data[address_stem == address, .(
    dropped_rows = .N,
    dropped_stricter_share = mean(right == 1),
    dropped_segments = uniqueN(segment_id),
    dropped_ward_pairs = paste(sort(unique(ward_pair_id)), collapse = ";")
  )]
  cbind(
    data.table(address_stem = address),
    support,
    data.table(
      estimate = unname(address_row["Estimate"]),
      std_error = unname(address_row["Std. Error"]),
      p_value = unname(address_row["Pr(>|t|)"]),
      estimate_change = unname(address_row["Estimate"]) - baseline_estimate,
      n = nobs(address_model)
    )
  )
}))
setorder(manual_address_influence, estimate_change)
fwrite(manual_address_influence, "../output/manual_address_influence.csv")

model_without_type <- feols(
  fml_without_building_type,
  data = model_data,
  cluster = ~segment_id,
  warn = FALSE,
  notes = FALSE
)
row_without_type <- coeftable(model_without_type)["right", ]
model_sensitivity <- rbind(
  model_sensitivity,
  data.table(
    specification = "drop building-type control",
    estimate = unname(row_without_type["Estimate"]),
    std_error = unname(row_without_type["Std. Error"]),
    p_value = unname(row_without_type["Pr(>|t|)"]),
    n = nobs(model_without_type),
    segments = uniqueN(model_data$segment_id),
    ward_pairs = uniqueN(model_data$ward_pair_id)
  )
)

model_collapsed_without_type <- feols(
  fml_without_building_type,
  data = collapsed_without_type,
  cluster = ~segment_id,
  warn = FALSE,
  notes = FALSE
)
row_collapsed_without_type <- coeftable(model_collapsed_without_type)["right", ]
model_sensitivity <- rbind(
  model_sensitivity,
  data.table(
    specification = "collapse corrected proxies ignoring building type",
    estimate = unname(row_collapsed_without_type["Estimate"]),
    std_error = unname(row_collapsed_without_type["Std. Error"]),
    p_value = unname(row_collapsed_without_type["Pr(>|t|)"]),
    n = nobs(model_collapsed_without_type),
    segments = uniqueN(collapsed_without_type$segment_id),
    ward_pairs = uniqueN(collapsed_without_type$ward_pair_id)
  )
)

model_complete_without_sqft <- feols(
  fml_without_sqft,
  data = model_data,
  cluster = ~segment_id,
  warn = FALSE,
  notes = FALSE
)
row_complete_without_sqft <- coeftable(model_complete_without_sqft)["right", ]

expanded_without_sqft <- rent[
  coalesce(sample_steps[["valid date, rent, scores, segment, and pair"]], FALSE) &
    flag_clean_location_sample == TRUE &
    is.finite(beds) & beds >= 0 &
    is.finite(log_baths_audit) &
    amenities_complete == TRUE
]
expanded_without_sqft[, beds_factor := factor(beds)]
expanded_without_sqft[, building_type_factor := factor(fifelse(is.na(building_type_clean), "other", building_type_clean))]
expanded_without_sqft[, `:=`(
  nearest_school_dist_kft = nearest_school_dist_ft / 1000,
  nearest_park_dist_kft = nearest_park_dist_ft / 1000,
  nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
  nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
  lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
)]
model_expanded_without_sqft <- feols(
  fml_without_sqft,
  data = expanded_without_sqft,
  cluster = ~segment_id,
  warn = FALSE,
  notes = FALSE
)
row_expanded_without_sqft <- coeftable(model_expanded_without_sqft)["right", ]

model_sensitivity <- rbind(
  model_sensitivity,
  data.table(
    specification = "drop square-foot control, complete-case sample",
    estimate = unname(row_complete_without_sqft["Estimate"]),
    std_error = unname(row_complete_without_sqft["Std. Error"]),
    p_value = unname(row_complete_without_sqft["Pr(>|t|)"]),
    n = nobs(model_complete_without_sqft),
    segments = uniqueN(model_data$segment_id),
    ward_pairs = uniqueN(model_data$ward_pair_id)
  ),
  data.table(
    specification = "drop square-foot control, retain missing square feet",
    estimate = unname(row_expanded_without_sqft["Estimate"]),
    std_error = unname(row_expanded_without_sqft["Std. Error"]),
    p_value = unname(row_expanded_without_sqft["Pr(>|t|)"]),
    n = nobs(model_expanded_without_sqft),
    segments = uniqueN(expanded_without_sqft$segment_id),
    ward_pairs = uniqueN(expanded_without_sqft$ward_pair_id)
  )
)
fwrite(model_sensitivity, "../output/model_sensitivity.csv")

summary_lines <- c(
  "# Rental data cleaning audit",
  "",
  "This audit reconstructs production cleaning from the raw RentHub parquet files and traces the current 500ft rental RD sample. It does not modify production or paper files.",
  "",
  sprintf("- Raw Chicago/CHGO rows: %s", format(cleaning_stage_counts[1, n], big.mark = ",")),
  sprintf("- Production analysis-key months: %s", format(cleaning_stage_counts[.N, n], big.mark = ",")),
  sprintf("- Final fully controlled RD observations: %s", format(final_sample_attrition[.N, n_rows], big.mark = ",")),
  sprintf("- Native unit-ID share in final sample: %.1f%%", 100 * final_sample_attrition[.N, share_unit_id_source]),
  sprintf("- Production RD estimate: %.4f (SE %.4f)", model_sensitivity[specification == "production sample", estimate], model_sensitivity[specification == "production sample", std_error]),
  "",
  "See the accompanying CSV files for stage counts, identifier contracts, location flags, and model sensitivities."
)
writeLines(summary_lines, "../output/audit_summary.md")

message("Rental data cleaning audit complete.")
