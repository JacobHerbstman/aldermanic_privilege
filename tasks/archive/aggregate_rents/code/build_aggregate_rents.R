source("../../setup_environment/code/packages.R")
source("../../_lib/aggregate_geography_helpers.R")

library(DBI)
library(duckdb)
library(data.table)

sf_use_s2(FALSE)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/aggregate_rents/code")
# renthub_input <- "../input/chicago_rent_panel.parquet"
# dwellsy_input <- "../input/chicago_dwellsy_listing_history.parquet"
# cpi_input <- "../input/fred_chi_cpi_all_items.csv"
# ward_panel_input <- "../input/ward_panel.gpkg"
# community_area_input <- "../input/community_areas.geojson"
# trim_summary_input <- "../input/renthub_trim_sensitivity_summary.csv"
# monthly_output <- "../output/rent_geography_monthly_summary.csv"
# yearly_output <- "../output/rent_geography_yearly_summary.csv"
# period_output <- "../output/rent_map_period_summary.csv"
# coverage_output <- "../output/rent_coverage_summary.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    renthub_input,
    dwellsy_input,
    cpi_input,
    ward_panel_input,
    community_area_input,
    trim_summary_input,
    monthly_output,
    yearly_output,
    period_output,
    coverage_output
  )
}

if (length(args) != 10) {
  stop(
    paste(
      "FATAL: Script requires 10 args:",
      "<renthub_input> <dwellsy_input> <cpi_input> <ward_panel_input>",
      "<community_area_input> <trim_summary_input> <monthly_output>",
      "<yearly_output> <period_output> <coverage_output>"
    ),
    call. = FALSE
  )
}

renthub_input <- args[1]
dwellsy_input <- args[2]
cpi_input <- args[3]
ward_panel_input <- args[4]
community_area_input <- args[5]
trim_summary_input <- args[6]
monthly_output <- args[7]
yearly_output <- args[8]
period_output <- args[9]
coverage_output <- args[10]

month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

duck_escape <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
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
    interpolated <- approx(
      x = idx_known,
      y = cpi_dt$rent_price_cpi_chi_all_items[idx_known],
      xout = seq_len(nrow(cpi_dt)),
      method = "linear",
      rule = 2
    )$y
    cpi_dt[, rent_price_cpi_chi_all_items := fifelse(
      is.finite(rent_price_cpi_chi_all_items),
      rent_price_cpi_chi_all_items,
      interpolated
    )]
  }

  base_cpi <- cpi_dt[format(month_start, "%Y") == as.character(base_year), mean(rent_price_cpi_chi_all_items)]
  if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop(sprintf("Unable to compute the CPI base for %d.", base_year), call. = FALSE)
  }

  cpi_dt[, rent_price_deflator_to_2024 := base_cpi / rent_price_cpi_chi_all_items]
  cpi_dt
}

normalize_trim_spec <- function(trim_summary_input) {
  if (!file.exists(trim_summary_input)) {
    return("strict_p1")
  }

  trim_summary <- suppressWarnings(fread(trim_summary_input))
  if ("chosen_trim_spec" %in% names(trim_summary) && "is_chosen" %in% names(trim_summary)) {
    chosen <- trim_summary[is_chosen %in% c(TRUE, 1), chosen_trim_spec][1]
    if (length(chosen) == 1 && !is.na(chosen) && chosen %in% c("strict_p1", "strict_p25")) {
      return(chosen)
    }
  }

  if ("chosen_trim_spec" %in% names(trim_summary)) {
    chosen <- trim_summary$chosen_trim_spec[1]
    if (length(chosen) == 1 && !is.na(chosen) && chosen %in% c("strict_p1", "strict_p25")) {
      return(chosen)
    }
  }

  "strict_p1"
}

collect_rent_summary <- function(con, table_name, time_level, source_name) {
  time_group <- if (time_level == "monthly") "month_start, year" else "year"
  time_select <- if (time_level == "monthly") "month_start, year" else "CAST(NULL AS DATE) AS month_start, year"

  sql <- sprintf(
    "
    SELECT
      '%s' AS source,
      'citywide' AS geography_level,
      CAST(NULL AS INTEGER) AS geography_id,
      'Chicago' AS geography_name,
      %s,
      MEDIAN(rent_real_2024) AS median_rent_real_2024,
      MEDIAN(rent_per_sqft_real_2024) FILTER (WHERE rent_per_sqft_real_2024 IS NOT NULL) AS median_rent_per_sqft_real_2024,
      COUNT(*) AS n_obs
    FROM %s
    GROUP BY %s

    UNION ALL

    SELECT
      '%s' AS source,
      'ward' AS geography_level,
      CAST(ward AS INTEGER) AS geography_id,
      'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
      %s,
      MEDIAN(rent_real_2024) AS median_rent_real_2024,
      MEDIAN(rent_per_sqft_real_2024) FILTER (WHERE rent_per_sqft_real_2024 IS NOT NULL) AS median_rent_per_sqft_real_2024,
      COUNT(*) AS n_obs
    FROM %s
    WHERE ward IS NOT NULL
    GROUP BY ward, %s

    UNION ALL

    SELECT
      '%s' AS source,
      'community_area' AS geography_level,
      CAST(community_area AS INTEGER) AS geography_id,
      community_name AS geography_name,
      %s,
      MEDIAN(rent_real_2024) AS median_rent_real_2024,
      MEDIAN(rent_per_sqft_real_2024) FILTER (WHERE rent_per_sqft_real_2024 IS NOT NULL) AS median_rent_per_sqft_real_2024,
      COUNT(*) AS n_obs
    FROM %s
    WHERE community_area IS NOT NULL
    GROUP BY community_area, community_name, %s
    ",
    source_name,
    time_select,
    table_name,
    time_group,
    source_name,
    time_select,
    table_name,
    time_group,
    source_name,
    time_select,
    table_name,
    time_group
  )

  as.data.table(dbGetQuery(con, sql))
}

collect_period_summary <- function(con, table_name, source_name, period_label, period_type, current_start, current_end, prior_start, prior_end, cutoff_month = NA_integer_) {
  current_start_sql <- format(as.Date(current_start), "%Y-%m-%d")
  current_end_sql <- format(as.Date(current_end), "%Y-%m-%d")
  prior_start_sql <- format(as.Date(prior_start), "%Y-%m-%d")
  prior_end_sql <- format(as.Date(prior_end), "%Y-%m-%d")
  cutoff_sql <- if (is.na(cutoff_month)) "NULL" else as.character(cutoff_month)

  sql <- sprintf(
    "
    SELECT
      '%s' AS source,
      '%s' AS period_label,
      '%s' AS period_type,
      CAST('%s' AS DATE) AS current_start,
      CAST('%s' AS DATE) AS current_end,
      CAST('%s' AS DATE) AS prior_start,
      CAST('%s' AS DATE) AS prior_end,
      %s AS cutoff_month,
      'citywide' AS geography_level,
      CAST(NULL AS INTEGER) AS geography_id,
      'Chicago' AS geography_name,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS current_median_rent_real_2024,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS prior_median_rent_real_2024,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_current,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_prior
    FROM %s

    UNION ALL

    SELECT
      '%s' AS source,
      '%s' AS period_label,
      '%s' AS period_type,
      CAST('%s' AS DATE) AS current_start,
      CAST('%s' AS DATE) AS current_end,
      CAST('%s' AS DATE) AS prior_start,
      CAST('%s' AS DATE) AS prior_end,
      %s AS cutoff_month,
      'ward' AS geography_level,
      CAST(ward AS INTEGER) AS geography_id,
      'Ward ' || CAST(ward AS VARCHAR) AS geography_name,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS current_median_rent_real_2024,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS prior_median_rent_real_2024,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_current,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_prior
    FROM %s
    WHERE ward IS NOT NULL
    GROUP BY ward

    UNION ALL

    SELECT
      '%s' AS source,
      '%s' AS period_label,
      '%s' AS period_type,
      CAST('%s' AS DATE) AS current_start,
      CAST('%s' AS DATE) AS current_end,
      CAST('%s' AS DATE) AS prior_start,
      CAST('%s' AS DATE) AS prior_end,
      %s AS cutoff_month,
      'community_area' AS geography_level,
      CAST(community_area AS INTEGER) AS geography_id,
      community_name AS geography_name,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS current_median_rent_real_2024,
      MEDIAN(CASE WHEN month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE) THEN rent_real_2024 END) AS prior_median_rent_real_2024,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_current,
      COUNT(*) FILTER (WHERE month_start BETWEEN CAST('%s' AS DATE) AND CAST('%s' AS DATE)) AS n_prior
    FROM %s
    WHERE community_area IS NOT NULL
    GROUP BY community_area, community_name
    ",
    source_name, period_label, period_type, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, cutoff_sql,
    current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, table_name,
    source_name, period_label, period_type, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, cutoff_sql,
    current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, table_name,
    source_name, period_label, period_type, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, cutoff_sql,
    current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, current_start_sql, current_end_sql, prior_start_sql, prior_end_sql, table_name
  )

  as.data.table(dbGetQuery(con, sql))
}

trim_spec <- normalize_trim_spec(trim_summary_input)
trim_prob_lo <- if (trim_spec == "strict_p25") 0.025 else 0.01
trim_prob_hi <- 1 - trim_prob_lo

message("Loading CPI deflator...")
cpi_dt <- load_cpi_deflator(
  cpi_file = cpi_input,
  start_month = as.Date("2014-01-01"),
  end_month = as.Date("2026-12-01"),
  base_year = 2024L
)

message("Opening DuckDB...")
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")
dbExecute(con, "PRAGMA temp_directory='../temp'")
dbWriteTable(con, "cpi_deflator", cpi_dt, temporary = TRUE, overwrite = TRUE)

message("Preparing citywide rent views...")
dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW renthub_base AS
    SELECT
      CAST(id AS VARCHAR) AS id,
      CASE
        WHEN UPPER(TRIM(CAST(property_id AS VARCHAR))) IN ('', 'NA', 'NAN', 'NULL', 'NONE') THEN NULL
        ELSE UPPER(TRIM(CAST(property_id AS VARCHAR)))
      END AS property_id_norm,
      CAST(file_date AS DATE) AS file_date,
      CAST(date_trunc('month', CAST(file_date AS DATE)) AS DATE) AS month_start,
      YEAR(CAST(file_date AS DATE)) AS year,
      CAST(rent_price AS DOUBLE) AS rent_price,
      CAST(beds AS DOUBLE) AS beds,
      CAST(baths AS DOUBLE) AS baths,
      CAST(sqft AS DOUBLE) AS sqft,
      CAST(building_type AS VARCHAR) AS building_type,
      CAST(latitude AS DOUBLE) AS latitude,
      CAST(longitude AS DOUBLE) AS longitude,
      printf('%%.7f|%%.7f', CAST(latitude AS DOUBLE), CAST(longitude AS DOUBLE)) AS coord_key
    FROM read_parquet('%s')
    WHERE CAST(file_date AS DATE) IS NOT NULL
      AND CAST(rent_price AS DOUBLE) > 0
      AND CAST(latitude AS DOUBLE) IS NOT NULL
      AND CAST(longitude AS DOUBLE) IS NOT NULL
    ",
    duck_escape(renthub_input)
  )
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP VIEW dwellsy_base AS
    SELECT
      CAST(listing_id AS VARCHAR) AS listing_id,
      CAST(creation_month AS DATE) AS month_start,
      YEAR(CAST(creation_month AS DATE)) AS year,
      CAST(rent_price_real_2024 AS DOUBLE) AS rent_real_2024,
      CASE
        WHEN CAST(sqft AS DOUBLE) > 0 THEN CAST(rent_price_real_2024 AS DOUBLE) / CAST(sqft AS DOUBLE)
        ELSE NULL
      END AS rent_per_sqft_real_2024,
      CAST(latitude AS DOUBLE) AS latitude,
      CAST(longitude AS DOUBLE) AS longitude,
      printf('%%.7f|%%.7f', CAST(latitude AS DOUBLE), CAST(longitude AS DOUBLE)) AS coord_key
    FROM read_parquet('%s')
    WHERE CAST(creation_month AS DATE) IS NOT NULL
      AND CAST(rent_price_real_2024 AS DOUBLE) > 0
      AND CAST(latitude AS DOUBLE) IS NOT NULL
      AND CAST(longitude AS DOUBLE) IS NOT NULL
    ",
    duck_escape(dwellsy_input)
  )
)

message("Building coordinate geography lookup...")
coords_tbl <- as_tibble(dbGetQuery(
  con,
  "
  SELECT DISTINCT coord_key, latitude, longitude FROM renthub_base
  UNION
  SELECT DISTINCT coord_key, latitude, longitude FROM dwellsy_base
  "
))

geo_lookup <- build_coord_geography_lookup(coords_tbl, ward_panel_input, community_area_input)
dbWriteTable(con, "geo_lookup", geo_lookup, temporary = TRUE, overwrite = TRUE)

message("Constructing source-specific rent tables...")
dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE renthub_raw_geo AS
  SELECT
    b.month_start,
    b.year,
    CASE
      WHEN b.year <= 2014 THEN g.ward_2003
      WHEN b.year <= 2023 THEN g.ward_2015
      ELSE g.ward_2024
    END AS ward,
    g.community_area,
    g.community_name,
    b.rent_price * c.rent_price_deflator_to_2024 AS rent_real_2024,
    CASE
      WHEN b.sqft > 0 THEN (b.rent_price * c.rent_price_deflator_to_2024) / b.sqft
      ELSE NULL
    END AS rent_per_sqft_real_2024
  FROM renthub_base b
  LEFT JOIN geo_lookup g USING (coord_key)
  LEFT JOIN cpi_deflator c USING (month_start)
  "
)

dbExecute(
  con,
  sprintf(
    "
    CREATE OR REPLACE TEMP TABLE renthub_clean_geo AS
    WITH base AS (
      SELECT
        b.month_start,
        b.year,
        CASE
          WHEN b.year <= 2014 THEN g.ward_2003
          WHEN b.year <= 2023 THEN g.ward_2015
          ELSE g.ward_2024
        END AS ward,
        g.community_area,
        g.community_name,
        COALESCE(CAST(ROUND(b.beds) AS INTEGER), -1) AS bed_cell,
        COALESCE(
          b.property_id_norm,
          printf(
            'coord|%%s|%%s|%%s|%%s|%%s',
            b.coord_key,
            COALESCE(CAST(ROUND(b.beds, 1) AS VARCHAR), ''),
            COALESCE(CAST(ROUND(b.baths, 1) AS VARCHAR), ''),
            COALESCE(CAST(ROUND(b.sqft) AS VARCHAR), ''),
            COALESCE(b.building_type, '')
          )
        ) AS analysis_key,
        b.rent_price,
        b.sqft,
        c.rent_price_deflator_to_2024
      FROM renthub_base b
      LEFT JOIN geo_lookup g USING (coord_key)
      LEFT JOIN cpi_deflator c USING (month_start)
    ),
    cell_thresholds AS (
      SELECT
        year,
        bed_cell,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_price, %.3f) AS rent_lo,
        QUANTILE_CONT(rent_price, %.3f) AS rent_hi,
        QUANTILE_CONT(sqft, %.3f) FILTER (WHERE sqft > 0) AS sqft_lo,
        QUANTILE_CONT(sqft, %.3f) FILTER (WHERE sqft > 0) AS sqft_hi
      FROM base
      GROUP BY 1, 2
    ),
    year_thresholds AS (
      SELECT
        year,
        COUNT(*) AS n_obs,
        QUANTILE_CONT(rent_price, %.3f) AS rent_lo,
        QUANTILE_CONT(rent_price, %.3f) AS rent_hi,
        QUANTILE_CONT(sqft, %.3f) FILTER (WHERE sqft > 0) AS sqft_lo,
        QUANTILE_CONT(sqft, %.3f) FILTER (WHERE sqft > 0) AS sqft_hi
      FROM base
      GROUP BY 1
    ),
    winsorized AS (
      SELECT
        b.month_start,
        b.year,
        b.ward,
        b.community_area,
        b.community_name,
        b.analysis_key,
        GREATEST(
          COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_lo END, y.rent_lo),
          LEAST(b.rent_price, COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_hi END, y.rent_hi))
        ) * b.rent_price_deflator_to_2024 AS rent_real_2024,
        CASE
          WHEN b.sqft > 0 THEN
            (
              GREATEST(
                COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_lo END, y.rent_lo),
                LEAST(b.rent_price, COALESCE(CASE WHEN c.n_obs >= 200 THEN c.rent_hi END, y.rent_hi))
              ) * b.rent_price_deflator_to_2024
            ) /
            GREATEST(
              COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_lo END, y.sqft_lo),
              LEAST(b.sqft, COALESCE(CASE WHEN c.n_obs >= 200 THEN c.sqft_hi END, y.sqft_hi))
            )
          ELSE NULL
        END AS rent_per_sqft_real_2024
      FROM base b
      LEFT JOIN cell_thresholds c
        ON b.year = c.year
       AND b.bed_cell = c.bed_cell
      LEFT JOIN year_thresholds y
        ON b.year = y.year
    )
    SELECT
      month_start,
      year,
      ward,
      community_area,
      community_name,
      MEDIAN(rent_real_2024) AS rent_real_2024,
      MEDIAN(rent_per_sqft_real_2024) FILTER (WHERE rent_per_sqft_real_2024 IS NOT NULL) AS rent_per_sqft_real_2024,
      COUNT(*) AS duplicate_rows
    FROM winsorized
    GROUP BY month_start, year, ward, community_area, community_name, analysis_key
    ",
    trim_prob_lo,
    trim_prob_hi,
    trim_prob_lo,
    trim_prob_hi,
    trim_prob_lo,
    trim_prob_hi,
    trim_prob_lo,
    trim_prob_hi
  )
)

dbExecute(
  con,
  "
  CREATE OR REPLACE TEMP TABLE dwellsy_geo AS
  SELECT
    d.month_start,
    d.year,
    CASE
      WHEN d.year <= 2014 THEN g.ward_2003
      WHEN d.year <= 2023 THEN g.ward_2015
      ELSE g.ward_2024
    END AS ward,
    g.community_area,
    g.community_name,
    d.rent_real_2024,
    d.rent_per_sqft_real_2024
  FROM dwellsy_base d
  LEFT JOIN geo_lookup g USING (coord_key)
  "
)

message("Collecting monthly and yearly summaries...")
monthly_summary <- rbindlist(list(
  collect_rent_summary(con, "renthub_raw_geo", "monthly", "renthub_raw"),
  collect_rent_summary(con, "renthub_clean_geo", "monthly", "renthub_clean"),
  collect_rent_summary(con, "dwellsy_geo", "monthly", "dwellsy")
), fill = TRUE)

yearly_summary <- rbindlist(list(
  collect_rent_summary(con, "renthub_raw_geo", "yearly", "renthub_raw"),
  collect_rent_summary(con, "renthub_clean_geo", "yearly", "renthub_clean"),
  collect_rent_summary(con, "dwellsy_geo", "yearly", "dwellsy")
), fill = TRUE)

message("Collecting source coverage summaries...")
renthub_ytd_cutoff <- dbGetQuery(
  con,
  "SELECT MAX(EXTRACT(MONTH FROM month_start)) AS cutoff_month FROM renthub_raw_geo WHERE year = 2025"
)$cutoff_month[[1]]
dwellsy_ytd_cutoff <- dbGetQuery(
  con,
  "SELECT MAX(EXTRACT(MONTH FROM month_start)) AS cutoff_month FROM dwellsy_geo WHERE year = 2026"
)$cutoff_month[[1]]

coverage_summary <- rbindlist(list(
  as.data.table(dbGetQuery(
    con,
    "
    SELECT
      'renthub_raw' AS source,
      COUNT(*) AS n_obs,
      MIN(month_start) AS first_month,
      MAX(month_start) AS last_month,
      SUM(CASE WHEN ward IS NOT NULL THEN 1 ELSE 0 END) AS n_with_ward,
      SUM(CASE WHEN community_area IS NOT NULL THEN 1 ELSE 0 END) AS n_with_community_area,
      COUNT(DISTINCT ward) AS n_wards,
      COUNT(DISTINCT community_area) AS n_community_areas,
      NULL::DOUBLE AS mean_duplicate_rows,
      NULL::VARCHAR AS trim_spec
    FROM renthub_raw_geo
    "
  )),
  as.data.table(dbGetQuery(
    con,
    sprintf(
      "
      SELECT
        'renthub_clean' AS source,
        COUNT(*) AS n_obs,
        MIN(month_start) AS first_month,
        MAX(month_start) AS last_month,
        SUM(CASE WHEN ward IS NOT NULL THEN 1 ELSE 0 END) AS n_with_ward,
        SUM(CASE WHEN community_area IS NOT NULL THEN 1 ELSE 0 END) AS n_with_community_area,
        COUNT(DISTINCT ward) AS n_wards,
        COUNT(DISTINCT community_area) AS n_community_areas,
        AVG(duplicate_rows) AS mean_duplicate_rows,
        '%s' AS trim_spec
      FROM renthub_clean_geo
      ",
      trim_spec
    )
  )),
  as.data.table(dbGetQuery(
    con,
    "
    SELECT
      'dwellsy' AS source,
      COUNT(*) AS n_obs,
      MIN(month_start) AS first_month,
      MAX(month_start) AS last_month,
      SUM(CASE WHEN ward IS NOT NULL THEN 1 ELSE 0 END) AS n_with_ward,
      SUM(CASE WHEN community_area IS NOT NULL THEN 1 ELSE 0 END) AS n_with_community_area,
      COUNT(DISTINCT ward) AS n_wards,
      COUNT(DISTINCT community_area) AS n_community_areas,
      NULL::DOUBLE AS mean_duplicate_rows,
      NULL::VARCHAR AS trim_spec
    FROM dwellsy_geo
    "
  ))
), fill = TRUE)

coverage_summary[, share_with_ward := n_with_ward / n_obs]
coverage_summary[, share_with_community_area := n_with_community_area / n_obs]

message("Collecting exact map period summaries...")
period_summary <- rbindlist(list(
  collect_period_summary(con, "renthub_raw_geo", "renthub_raw", "2023_vs_2022_full_year", "full_year", "2023-01-01", "2023-12-01", "2022-01-01", "2022-12-01"),
  collect_period_summary(con, "renthub_clean_geo", "renthub_clean", "2023_vs_2022_full_year", "full_year", "2023-01-01", "2023-12-01", "2022-01-01", "2022-12-01"),
  collect_period_summary(con, "dwellsy_geo", "dwellsy", "2023_vs_2022_full_year", "full_year", "2023-01-01", "2023-12-01", "2022-01-01", "2022-12-01"),
  collect_period_summary(con, "renthub_raw_geo", "renthub_raw", "2024_vs_2023_full_year", "full_year", "2024-01-01", "2024-12-01", "2023-01-01", "2023-12-01"),
  collect_period_summary(con, "renthub_clean_geo", "renthub_clean", "2024_vs_2023_full_year", "full_year", "2024-01-01", "2024-12-01", "2023-01-01", "2023-12-01"),
  collect_period_summary(con, "dwellsy_geo", "dwellsy", "2024_vs_2023_full_year", "full_year", "2024-01-01", "2024-12-01", "2023-01-01", "2023-12-01"),
  collect_period_summary(
    con,
    "renthub_raw_geo",
    "renthub_raw",
    "2025_vs_2024_ytd",
    "matched_ytd",
    "2025-01-01",
    sprintf("2025-%02d-01", renthub_ytd_cutoff),
    "2024-01-01",
    sprintf("2024-%02d-01", renthub_ytd_cutoff),
    renthub_ytd_cutoff
  ),
  collect_period_summary(
    con,
    "renthub_clean_geo",
    "renthub_clean",
    "2025_vs_2024_ytd",
    "matched_ytd",
    "2025-01-01",
    sprintf("2025-%02d-01", renthub_ytd_cutoff),
    "2024-01-01",
    sprintf("2024-%02d-01", renthub_ytd_cutoff),
    renthub_ytd_cutoff
  ),
  collect_period_summary(
    con,
    "dwellsy_geo",
    "dwellsy",
    "2026_vs_2025_ytd",
    "matched_ytd",
    "2026-01-01",
    sprintf("2026-%02d-01", dwellsy_ytd_cutoff),
    "2025-01-01",
    sprintf("2025-%02d-01", dwellsy_ytd_cutoff),
    dwellsy_ytd_cutoff
  )
), fill = TRUE)

period_summary[, growth_pct := 100 * (current_median_rent_real_2024 / prior_median_rent_real_2024 - 1)]

write_csv(monthly_summary %>% arrange(source, geography_level, geography_id, month_start), monthly_output)
write_csv(yearly_summary %>% arrange(source, geography_level, geography_id, year), yearly_output)
write_csv(period_summary %>% arrange(source, period_label, geography_level, geography_id), period_output)
write_csv(coverage_summary %>% arrange(source), coverage_output)

message("Saved rent geography monthly summary: ", monthly_output)
message("Saved rent geography yearly summary: ", yearly_output)
message("Saved rent map period summary: ", period_output)
message("Saved rent coverage summary: ", coverage_output)
