source("../../setup_environment/code/packages.R")

library(DBI)
library(duckdb)
library(data.table)
library(ggplot2)
library(readr)
library(tidycensus)
library(zoo)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/renthub_data_audit/code")
# Rscript run_renthub_data_audit.R ../input/renthub_raw ../output ../temp
# raw_dir <- "../input/renthub_raw"
# out_dir <- "../output"
# temp_dir <- "../temp"
# =======================================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3) {
  raw_dir <- args[1]
  out_dir <- args[2]
  temp_dir <- args[3]
} else {
  if (!exists("raw_dir") || !exists("out_dir") || !exists("temp_dir")) {
    stop("FATAL: Script requires 3 args: <raw_dir> <out_dir> <temp_dir>", call. = FALSE)
  }
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

status_levels <- c("GREEN", "YELLOW", "RED")
status_rank <- setNames(seq_along(status_levels), status_levels)
status_fill <- c(GREEN = "#2a7f3b", YELLOW = "#d98e04", RED = "#b83a2f")

normalize_missing_like <- function(x) {
  txt <- toupper(trimws(as.character(x)))
  txt[txt %in% c("", "NA", "NAN", "NULL", "NONE")] <- NA_character_
  txt
}

days_in_month <- function(x) {
  x <- as.Date(format(as.Date(x), "%Y-%m-01"))
  next_month <- as.Date(format(x + 32, "%Y-%m-01"))
  as.integer(format(next_month - 1, "%d"))
}

month_end <- function(x) {
  x <- as.Date(format(as.Date(x), "%Y-%m-01"))
  as.Date(format(x + 32, "%Y-%m-01")) - 1
}

month_floor <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

previous_trailing_median <- function(x, n = 12L) {
  zoo::rollapplyr(
    data.table::shift(x, n = 1L, type = "lag"),
    width = n,
    FUN = function(y) median(y, na.rm = TRUE),
    fill = NA_real_,
    partial = FALSE
  )
}

pick_worst_status <- function(x) {
  x <- x[!is.na(x) & x %in% names(status_rank)]
  if (!length(x)) {
    return(NA_character_)
  }
  x[which.max(status_rank[x])]
}

classify_completeness_status <- function(observed_days, left_gap, right_gap, missing_month, abrupt_rows, abrupt_geo, abrupt_unit) {
  if (isTRUE(missing_month) || isTRUE(abrupt_rows) || isTRUE(abrupt_geo) || isTRUE(abrupt_unit)) {
    return("RED")
  }
  if (!is.finite(observed_days) || observed_days < 18 || max(left_gap, right_gap, na.rm = TRUE) > 7) {
    return("RED")
  }
  if (observed_days <= 24 || max(left_gap, right_gap, na.rm = TRUE) >= 4) {
    return("YELLOW")
  }
  "GREEN"
}

classify_coverage_status <- function(share_rows_with_unit_id, distinct_unit_ids) {
  if (!is.finite(share_rows_with_unit_id) || !is.finite(distinct_unit_ids)) {
    return("RED")
  }
  if (share_rows_with_unit_id < 0.05 || distinct_unit_ids < 2000) {
    return("RED")
  }
  if (share_rows_with_unit_id >= 0.10 && distinct_unit_ids >= 5000) {
    return("GREEN")
  }
  "YELLOW"
}

classify_frequency_status <- function(p95_unit_month_rows, share_unit_month_gt14) {
  if (!is.finite(p95_unit_month_rows) || !is.finite(share_unit_month_gt14)) {
    return("RED")
  }
  if (p95_unit_month_rows <= 7 && share_unit_month_gt14 <= 0.02) {
    return("GREEN")
  }
  if (p95_unit_month_rows <= 14 && share_unit_month_gt14 <= 0.05) {
    return("YELLOW")
  }
  "RED"
}

classify_same_day_status <- function(share_unit_day_multi_rent) {
  if (!is.finite(share_unit_day_multi_rent)) {
    return("RED")
  }
  if (share_unit_day_multi_rent <= 0.05) {
    return("GREEN")
  }
  if (share_unit_day_multi_rent <= 0.10) {
    return("YELLOW")
  }
  "RED"
}

classify_volatility_status <- function(share_abs_change_gt25, share_abs_change_gt50) {
  if (!is.finite(share_abs_change_gt25) || !is.finite(share_abs_change_gt50)) {
    return("RED")
  }
  if (share_abs_change_gt25 <= 0.01 && share_abs_change_gt50 <= 0.0025) {
    return("GREEN")
  }
  if (share_abs_change_gt25 <= 0.025 && share_abs_change_gt50 <= 0.0075) {
    return("YELLOW")
  }
  "RED"
}

classify_benchmark_status <- function(corr_zillow, gap_zillow_pp, corr_fred) {
  if (!is.finite(corr_zillow) || !is.finite(gap_zillow_pp) || !is.finite(corr_fred)) {
    return(NA_character_)
  }
  if (corr_zillow >= 0.8 && gap_zillow_pp <= 5 && corr_fred >= 0.7) {
    return("GREEN")
  }
  if (corr_zillow >= 0.6 && gap_zillow_pp <= 8 && corr_fred >= 0.5) {
    return("YELLOW")
  }
  "RED"
}

assert_unique <- function(dt, cols, label) {
  dup_n <- dt[, .N, by = cols][N > 1, sum(N)]
  if (dup_n > 0) {
    stop(sprintf("%s has duplicate keys on %s.", label, paste(cols, collapse = ", ")), call. = FALSE)
  }
}

approx_span_ft <- function(min_lat, max_lat, min_lon, max_lon, avg_lat) {
  lat_ft <- (max_lat - min_lat) * 364000
  lon_ft <- (max_lon - min_lon) * 288200 * cos(avg_lat * pi / 180)
  sqrt(lat_ft^2 + lon_ft^2)
}

rolling_compare_metrics <- function(dt, x_col, y_col, lags = 0L, window_n = 24L, min_obs = 12L) {
  out <- data.table(
    month_start = dt$month_start,
    n_pairs = NA_integer_,
    best_corr = NA_real_,
    best_lag = NA_integer_,
    median_abs_gap_pp = NA_real_
  )

  for (i in seq_len(nrow(dt))) {
    lo <- max(1L, i - window_n + 1L)
    window_dt <- copy(dt[lo:i])

    best_corr <- -Inf
    best_gap <- NA_real_
    best_lag <- NA_integer_
    best_n <- 0L

    for (lag_k in lags) {
      y_shift <- data.table::shift(window_dt[[y_col]], n = lag_k, type = "lead")
      keep <- is.finite(window_dt[[x_col]]) & is.finite(y_shift)
      n_keep <- sum(keep)
      if (n_keep < min_obs) {
        next
      }

      corr_val <- suppressWarnings(cor(window_dt[[x_col]][keep], y_shift[keep]))
      gap_val <- median(abs(window_dt[[x_col]][keep] - y_shift[keep]), na.rm = TRUE)

      if (is.finite(corr_val) && (corr_val > best_corr || (isTRUE(all.equal(corr_val, best_corr)) && lag_k < best_lag))) {
        best_corr <- corr_val
        best_gap <- gap_val
        best_lag <- lag_k
        best_n <- n_keep
      }
    }

    if (is.finite(best_corr)) {
      out[i, `:=`(
        n_pairs = best_n,
        best_corr = best_corr,
        best_lag = best_lag,
        median_abs_gap_pp = best_gap
      )]
    }
  }

  out
}

compute_period_metrics <- function(dt, x_col, y_col, lags = 0L, min_obs = 12L) {
  best_corr <- -Inf
  best_gap <- NA_real_
  best_lag <- NA_integer_
  best_n <- 0L

  for (lag_k in lags) {
    y_shift <- data.table::shift(dt[[y_col]], n = lag_k, type = "lead")
    keep <- is.finite(dt[[x_col]]) & is.finite(y_shift)
    n_keep <- sum(keep)
    if (n_keep < min_obs) {
      next
    }
    corr_val <- suppressWarnings(cor(dt[[x_col]][keep], y_shift[keep]))
    gap_val <- median(abs(dt[[x_col]][keep] - y_shift[keep]), na.rm = TRUE)
    if (is.finite(corr_val) && (corr_val > best_corr || (isTRUE(all.equal(corr_val, best_corr)) && lag_k < best_lag))) {
      best_corr <- corr_val
      best_gap <- gap_val
      best_lag <- lag_k
      best_n <- n_keep
    }
  }

  data.table(
    n_pairs = if (best_n > 0) best_n else NA_integer_,
    best_corr = if (is.finite(best_corr)) best_corr else NA_real_,
    best_lag = best_lag,
    median_abs_gap_pp = best_gap
  )
}

longest_run <- function(x) {
  if (!length(x)) {
    return(0L)
  }
  r <- rle(x)
  if (!any(r$values)) {
    return(0L)
  }
  max(r$lengths[r$values])
}

recommended_window <- function(dt) {
  usable <- dt$overall_verdict != "RED"
  if (!any(usable, na.rm = TRUE)) {
    return(data.table(window_start = as.Date(NA), window_end = as.Date(NA), n_months = 0L, share_green = NA_real_))
  }

  r <- rle(usable)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$values)
  candidate <- data.table(
    start_idx = starts[keep],
    end_idx = ends[keep]
  )
  candidate[, n_months := end_idx - start_idx + 1L]
  candidate[, share_green := vapply(seq_len(.N), function(i) {
    mean(dt$overall_verdict[start_idx[i]:end_idx[i]] == "GREEN")
  }, numeric(1))]
  setorder(candidate, -n_months, -share_green, start_idx)
  top <- candidate[1]
  data.table(
    window_start = dt$month_start[top$start_idx],
    window_end = dt$month_start[top$end_idx],
    n_months = top$n_months,
    share_green = top$share_green
  )
}

write_csv_dt <- function(dt, path) {
  fwrite(dt, path, na = "")
}

build_plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

fetch_fred_series <- function(series_id) {
  fred_url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", series_id)
  message(sprintf("Fetching FRED series %s...", series_id))
  old_http_ua <- getOption("HTTPUserAgent")
  on.exit(options(HTTPUserAgent = old_http_ua), add = TRUE)
  options(HTTPUserAgent = paste0("curl/", curl::curl_version()$version))
  raw <- read_csv(fred_url, show_col_types = FALSE)
  if (!all(c("observation_date", series_id) %in% names(raw))) {
    stop(sprintf("FRED response missing expected columns for %s.", series_id), call. = FALSE)
  }
  dt <- as.data.table(raw)
  dt[, month_start := month_floor(observation_date)]
  dt[, value := suppressWarnings(as.numeric(get(series_id)))]
  dt <- dt[month_start >= as.Date("2014-01-01") & month_start <= as.Date("2025-12-01"), .(month_start, fred_chi_rent_cpi = value)]
  dt
}

fetch_zillow_series <- function(url, region_name, state = NULL, value_name) {
  message(sprintf("Fetching Zillow benchmark %s...", value_name))
  raw <- as.data.table(read_csv(url, show_col_types = FALSE))
  if (!"RegionName" %in% names(raw)) {
    stop(sprintf("Zillow file at %s is missing RegionName.", url), call. = FALSE)
  }
  keep <- raw$RegionName == region_name
  if (!is.null(state) && "State" %in% names(raw)) {
    keep <- keep & raw$State == state
  }
  dt <- raw[keep]
  if (nrow(dt) != 1L) {
    stop(sprintf("Expected one Zillow row for %s.", region_name), call. = FALSE)
  }
  date_cols <- names(dt)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(dt))]
  long <- melt(dt, measure.vars = date_cols, variable.name = "period_end", value.name = value_name)
  long[, month_start := month_floor(period_end)]
  long[, (value_name) := as.numeric(get(value_name))]
  long <- long[month_start >= as.Date("2014-01-01") & month_start <= as.Date("2025-12-01"), .(month_start, value = get(value_name))]
  setnames(long, "value", value_name)
  long
}

fetch_acs_series <- function() {
  acs_max_year <- min(2024L, as.integer(format(Sys.Date(), "%Y")) - 2L)
  years <- 2014L:acs_max_year
  out <- rbindlist(lapply(years, function(y) {
    tryCatch({
      raw <- tidycensus::get_acs(
        geography = "place",
        variables = "B25064_001",
        state = "IL",
        year = y,
        survey = "acs1",
        geometry = FALSE,
        cache_table = TRUE
      )
      row <- raw[grepl("^Chicago city", raw$NAME), ]
      if (nrow(row) == 0L) {
        stop(sprintf("Chicago city not found in ACS %d.", y))
      }
      data.table(
        year = y,
        month_start = as.Date(sprintf("%d-07-01", y)),
        acs_chicago_median_gross_rent = as.numeric(row$estimate[1]),
        status = "ok"
      )
    }, error = function(e) {
      data.table(
        year = y,
        month_start = as.Date(sprintf("%d-07-01", y)),
        acs_chicago_median_gross_rent = NA_real_,
        status = paste("error:", conditionMessage(e))
      )
    })
  }), fill = TRUE)
  out
}

run_internal_tests <- function() {
  test_results <- data.table(
    test_name = character(),
    passed = logical(),
    detail = character()
  )

  norm_test <- normalize_missing_like(c("NA", "null", "", "  abc "))
  test_results <- rbind(
    test_results,
    data.table(
      test_name = "normalize_missing_like",
      passed = all(is.na(norm_test[1:3])) && identical(norm_test[4], "ABC"),
      detail = "literal missing-like tokens convert to NA"
    )
  )

  synthetic <- data.table(
    unit_id = c("U1", "U1", "U1", "U1"),
    id = c("A", "B", "B", "B"),
    file_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-05", "2020-01-20")),
    posted_date = as.Date(c("2019-12-20", "2019-12-29", "2020-01-04", "2020-01-18")),
    scraped_timestamp = as.POSIXct(c("2020-01-01 08:00:00", "2020-01-01 09:00:00", "2020-01-05 08:00:00", "2020-01-20 08:00:00"), tz = "UTC"),
    rent_price = c(1000, 1100, 1700, 1700),
    latitude = c(41.8800, 41.8800, 41.8800, 41.8830),
    longitude = c(-87.6300, -87.6300, -87.6300, -87.6200)
  )

  same_day <- synthetic[, .(n_ids = uniqueN(id), n_rents = uniqueN(rent_price)), by = .(unit_id, file_date)]
  test_results <- rbind(
    test_results,
    data.table(
      test_name = "same_day_multi_id_and_rent",
      passed = same_day[file_date == as.Date("2020-01-01"), n_ids] == 2L &&
        same_day[file_date == as.Date("2020-01-01"), n_rents] == 2L,
      detail = "same unit-day with multiple IDs and rents is detected"
    )
  )

  synthetic <- synthetic[order(unit_id, scraped_timestamp)]
  synthetic[, prev_posted_date := shift(posted_date), by = unit_id]
  synthetic[, prev_rent := shift(rent_price), by = unit_id]
  synthetic[, prev_file_date := shift(file_date), by = unit_id]
  synthetic[, gap_days := as.integer(file_date - prev_file_date)]
  synthetic[, posted_delta_days := as.integer(posted_date - prev_posted_date)]
  synthetic[, pct_change := rent_price / prev_rent - 1]

  test_results <- rbind(
    test_results,
    data.table(
      test_name = "repost_reset_flag_logic",
      passed = any(synthetic$gap_days <= 14 & synthetic$posted_delta_days >= 7, na.rm = TRUE),
      detail = "posted-date reset within an active spell is flagged"
    )
  )

  test_results <- rbind(
    test_results,
    data.table(
      test_name = "large_finite_rent_change",
      passed = any(is.finite(synthetic$pct_change) & abs(synthetic$pct_change) > 0.5, na.rm = TRUE),
      detail = "large but finite rent changes survive and can be flagged"
    )
  )

  span_ft <- approx_span_ft(
    min(synthetic$latitude),
    max(synthetic$latitude),
    min(synthetic$longitude),
    max(synthetic$longitude),
    mean(synthetic$latitude)
  )
  test_results <- rbind(
    test_results,
    data.table(
      test_name = "geo_jump_span",
      passed = is.finite(span_ft) && span_ft > 500,
      detail = "geo-span calculation flags large location jumps"
    )
  )

  test_results
}

message("Running internal audit checks...")
internal_test_results <- run_internal_tests()
write_csv_dt(internal_test_results, file.path(out_dir, "internal_test_results.csv"))
if (!all(internal_test_results$passed)) {
  stop("Internal tests failed. See internal_test_results.csv.", call. = FALSE)
}

raw_dir_abs <- normalizePath(raw_dir, mustWork = TRUE)
parquet_glob <- file.path(raw_dir_abs, "*.parquet")

message("Opening raw RentHub parquet files via DuckDB...")
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA threads=4")

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
    CAST(ZIP AS VARCHAR) AS zip,
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
  gsub("'", "''", parquet_glob)
)
dbExecute(con, view_sql)

collect_query <- function(sql) {
  as.data.table(dbGetQuery(con, sql))
}

message("Checking Chicago raw row count...")
raw_count <- collect_query("SELECT COUNT(*) AS n_rows FROM chicago_raw")
if (raw_count$n_rows[1] == 0L) {
  stop("No Chicago rows were found in the raw RentHub files.", call. = FALSE)
}

message("Building monthly skeleton...")
monthly_skeleton <- data.table(month_start = seq(as.Date("2014-01-01"), as.Date("2025-12-01"), by = "month"))
monthly_skeleton[, month_end := month_end(month_start)]
monthly_skeleton[, year := as.integer(format(month_start, "%Y"))]
monthly_skeleton[, year_month := format(month_start, "%Y-%m")]
stopifnot(nrow(monthly_skeleton) == 144L)

message("Computing monthly identifier coverage...")
monthly_identifier_coverage <- collect_query(
  "
  SELECT
    month_start,
    COUNT(*) AS total_rows,
    COUNT(DISTINCT id) AS distinct_ids,
    COUNT(DISTINCT property_id) FILTER (WHERE property_id IS NOT NULL) AS distinct_property_ids,
    COUNT(DISTINCT unit_id) FILTER (WHERE unit_id IS NOT NULL) AS distinct_unit_ids,
    AVG(CASE WHEN property_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_property_id,
    AVG(CASE WHEN unit_id IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_unit_id,
    COUNT(DISTINCT fingerprint_key) FILTER (WHERE fingerprint_key IS NOT NULL) AS distinct_missing_id_fingerprints,
    AVG(CASE WHEN fingerprint_key IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_missing_id_fingerprint,
    COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) AS distinct_addresses,
    COUNT(DISTINCT coord4) FILTER (WHERE coord4 IS NOT NULL) AS distinct_coord4,
    COUNT(DISTINCT company_norm) FILTER (WHERE company_norm IS NOT NULL) AS distinct_companies,
    AVG(CASE WHEN rent_price > 0 THEN 1.0 ELSE 0.0 END) AS share_rows_with_positive_rent,
    AVG(CASE WHEN beds IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_beds,
    AVG(CASE WHEN baths IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_baths,
    AVG(CASE WHEN sqft IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_rows_with_sqft,
    COUNT(DISTINCT file_date) AS observed_scrape_days,
    MIN(file_date) AS first_file_date,
    MAX(file_date) AS last_file_date
  FROM chicago_raw
  GROUP BY 1
  ORDER BY 1
  "
)
monthly_identifier_coverage[, month_start := as.Date(month_start)]
monthly_identifier_coverage[, first_file_date := as.Date(first_file_date)]
monthly_identifier_coverage[, last_file_date := as.Date(last_file_date)]

message("Computing completeness and outage metrics...")
monthly_completeness_outages <- merge(monthly_skeleton, monthly_identifier_coverage, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_completeness_outages[is.na(total_rows), `:=`(
  total_rows = 0,
  distinct_ids = 0,
  distinct_property_ids = 0,
  distinct_unit_ids = 0,
  distinct_missing_id_fingerprints = 0,
  distinct_addresses = 0,
  distinct_coord4 = 0,
  distinct_companies = 0,
  share_rows_with_property_id = 0,
  share_rows_with_unit_id = 0,
  share_rows_with_missing_id_fingerprint = 0,
  share_rows_with_positive_rent = 0,
  share_rows_with_beds = 0,
  share_rows_with_baths = 0,
  share_rows_with_sqft = 0,
  observed_scrape_days = 0
)]
monthly_completeness_outages[, missing_month := total_rows == 0]
monthly_completeness_outages[, left_edge_gap_days := fifelse(missing_month, NA_integer_, as.integer(first_file_date - month_start))]
monthly_completeness_outages[, right_edge_gap_days := fifelse(missing_month, NA_integer_, as.integer(month_end - last_file_date))]
monthly_completeness_outages[, partial_month := !missing_month & (left_edge_gap_days > 0 | right_edge_gap_days > 0)]
monthly_completeness_outages[, trailing12_total_rows_median := previous_trailing_median(total_rows)]
monthly_completeness_outages[, trailing12_coord4_median := previous_trailing_median(distinct_coord4)]
monthly_completeness_outages[, trailing12_unit_share_median := previous_trailing_median(share_rows_with_unit_id)]
monthly_completeness_outages[, abrupt_observation_collapse := is.finite(trailing12_total_rows_median) & total_rows < 0.5 * trailing12_total_rows_median]
monthly_completeness_outages[, abrupt_geography_collapse := is.finite(trailing12_coord4_median) & distinct_coord4 < 0.5 * trailing12_coord4_median]
monthly_completeness_outages[, abrupt_unit_id_coverage_collapse := is.finite(trailing12_unit_share_median) & share_rows_with_unit_id < 0.5 * trailing12_unit_share_median]
monthly_completeness_outages[, completeness_status := vapply(seq_len(.N), function(i) {
  classify_completeness_status(
    observed_days = observed_scrape_days[i],
    left_gap = left_edge_gap_days[i],
    right_gap = right_edge_gap_days[i],
    missing_month = missing_month[i],
    abrupt_rows = abrupt_observation_collapse[i],
    abrupt_geo = abrupt_geography_collapse[i],
    abrupt_unit = abrupt_unit_id_coverage_collapse[i]
  )
}, character(1))]

message("Computing native-unit repeat frequency...")
native_unit_repeat_frequency <- collect_query(
  "
  WITH native_unit_month AS (
    SELECT
      month_start,
      unit_id,
      COUNT(*) AS unit_month_rows,
      COUNT(DISTINCT file_date) AS unit_month_days
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
    GROUP BY 1, 2
  ),
  native_unit_day AS (
    SELECT
      month_start,
      unit_id,
      file_date,
      COUNT(*) AS unit_day_rows,
      COUNT(DISTINCT id) AS unit_day_ids,
      COUNT(DISTINCT rent_price) FILTER (WHERE rent_price > 0) AS unit_day_rents
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
    GROUP BY 1, 2, 3
  )
  SELECT
    m.month_start,
    COUNT(*) AS n_unit_month_cells,
    QUANTILE_CONT(unit_month_rows, 0.50) AS p50_unit_month_rows,
    QUANTILE_CONT(unit_month_rows, 0.95) AS p95_unit_month_rows,
    MAX(unit_month_rows) AS max_unit_month_rows,
    AVG(CASE WHEN unit_month_rows > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_month_gt1,
    AVG(CASE WHEN unit_month_rows > 7 THEN 1.0 ELSE 0.0 END) AS share_unit_month_gt7,
    AVG(CASE WHEN unit_month_rows > 14 THEN 1.0 ELSE 0.0 END) AS share_unit_month_gt14,
    AVG(CASE WHEN unit_month_rows > 21 THEN 1.0 ELSE 0.0 END) AS share_unit_month_gt21,
    d.n_unit_day_cells,
    d.p50_unit_day_rows,
    d.p95_unit_day_rows,
    d.max_unit_day_rows,
    d.share_unit_day_gt1,
    d.share_unit_day_multi_id,
    d.share_unit_day_multi_rent
  FROM native_unit_month m
  LEFT JOIN (
    SELECT
      month_start,
      COUNT(*) AS n_unit_day_cells,
      QUANTILE_CONT(unit_day_rows, 0.50) AS p50_unit_day_rows,
      QUANTILE_CONT(unit_day_rows, 0.95) AS p95_unit_day_rows,
      MAX(unit_day_rows) AS max_unit_day_rows,
      AVG(CASE WHEN unit_day_rows > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_day_gt1,
      AVG(CASE WHEN unit_day_ids > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_day_multi_id,
      AVG(CASE WHEN unit_day_rents > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_day_multi_rent
    FROM native_unit_day
    GROUP BY 1
  ) d
    ON m.month_start = d.month_start
  GROUP BY 1, 10, 11, 12, 13, 14, 15, 16
  ORDER BY 1
  "
)
native_unit_repeat_frequency[, month_start := as.Date(month_start)]

message("Computing building-layer repeat diagnostics...")
building_layer_repeat_frequency <- collect_query(
  "
  WITH building_month AS (
    SELECT
      month_start,
      property_id,
      COUNT(*) AS property_month_rows
    FROM chicago_raw
    WHERE property_id IS NOT NULL
    GROUP BY 1, 2
  )
  SELECT
    month_start,
    COUNT(*) AS n_property_month_cells,
    QUANTILE_CONT(property_month_rows, 0.50) AS p50_property_month_rows,
    QUANTILE_CONT(property_month_rows, 0.95) AS p95_property_month_rows,
    MAX(property_month_rows) AS max_property_month_rows,
    AVG(CASE WHEN property_month_rows > 1 THEN 1.0 ELSE 0.0 END) AS share_property_month_gt1,
    AVG(CASE WHEN property_month_rows > 7 THEN 1.0 ELSE 0.0 END) AS share_property_month_gt7,
    AVG(CASE WHEN property_month_rows > 14 THEN 1.0 ELSE 0.0 END) AS share_property_month_gt14,
    AVG(CASE WHEN property_month_rows > 21 THEN 1.0 ELSE 0.0 END) AS share_property_month_gt21
  FROM building_month
  GROUP BY 1
  ORDER BY 1
  "
)
building_layer_repeat_frequency[, month_start := as.Date(month_start)]

message("Computing missing-ID fingerprint diagnostics...")
missing_id_fingerprint_frequency <- collect_query(
  "
  WITH missing_month AS (
    SELECT
      month_start,
      fingerprint_key,
      COUNT(*) AS fingerprint_month_rows
    FROM chicago_raw
    WHERE unit_id IS NULL
      AND fingerprint_key IS NOT NULL
    GROUP BY 1, 2
  )
  SELECT
    month_start,
    COUNT(*) AS n_fingerprint_month_cells,
    QUANTILE_CONT(fingerprint_month_rows, 0.50) AS p50_fingerprint_month_rows,
    QUANTILE_CONT(fingerprint_month_rows, 0.95) AS p95_fingerprint_month_rows,
    MAX(fingerprint_month_rows) AS max_fingerprint_month_rows,
    AVG(CASE WHEN fingerprint_month_rows > 1 THEN 1.0 ELSE 0.0 END) AS share_fingerprint_month_gt1,
    AVG(CASE WHEN fingerprint_month_rows > 7 THEN 1.0 ELSE 0.0 END) AS share_fingerprint_month_gt7,
    AVG(CASE WHEN fingerprint_month_rows > 14 THEN 1.0 ELSE 0.0 END) AS share_fingerprint_month_gt14,
    AVG(CASE WHEN fingerprint_month_rows > 21 THEN 1.0 ELSE 0.0 END) AS share_fingerprint_month_gt21
  FROM missing_month
  GROUP BY 1
  ORDER BY 1
  "
)
missing_id_fingerprint_frequency[, month_start := as.Date(month_start)]

message("Computing same-day multi-rent conflicts...")
same_day_multi_rent_conflicts <- collect_query(
  "
  WITH native_unit_day AS (
    SELECT
      month_start,
      unit_id,
      file_date,
      COUNT(*) AS unit_day_rows,
      COUNT(DISTINCT id) AS unit_day_ids,
      COUNT(DISTINCT rent_price) FILTER (WHERE rent_price > 0) AS unit_day_rents
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
    GROUP BY 1, 2, 3
  )
  SELECT
    month_start,
    COUNT(*) AS n_unit_day_cells,
    SUM(CASE WHEN unit_day_ids > 1 THEN 1 ELSE 0 END) AS n_unit_days_multi_id,
    SUM(CASE WHEN unit_day_rents > 1 THEN 1 ELSE 0 END) AS n_unit_days_multi_rent,
    AVG(CASE WHEN unit_day_ids > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_day_multi_id,
    AVG(CASE WHEN unit_day_rents > 1 THEN 1.0 ELSE 0.0 END) AS share_unit_day_multi_rent,
    MAX(unit_day_rows) AS max_unit_day_rows,
    MAX(unit_day_rents) AS max_unit_day_rents
  FROM native_unit_day
  GROUP BY 1
  ORDER BY 1
  "
)
same_day_multi_rent_conflicts[, month_start := as.Date(month_start)]

message("Computing within-unit rent-change flags...")
rent_change_flags <- collect_query(
  "
  WITH ordered AS (
    SELECT
      month_start,
      unit_id,
      id,
      file_date,
      scraped_timestamp,
      rent_price,
      LAG(rent_price) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_rent,
      LAG(file_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_file_date
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
      AND rent_price > 0
  ),
  changes AS (
    SELECT
      month_start,
      CASE
        WHEN DATE_DIFF('day', prev_file_date, file_date) <= 1 THEN '1d'
        WHEN DATE_DIFF('day', prev_file_date, file_date) <= 7 THEN '7d'
        WHEN DATE_DIFF('day', prev_file_date, file_date) <= 31 THEN '31d'
        ELSE NULL
      END AS change_window,
      ABS(rent_price / prev_rent - 1.0) AS abs_pct_change,
      rent_price / prev_rent - 1.0 AS pct_change
    FROM ordered
    WHERE prev_rent > 0
      AND prev_file_date IS NOT NULL
      AND DATE_DIFF('day', prev_file_date, file_date) BETWEEN 0 AND 31
  )
  SELECT
    month_start,
    change_window,
    COUNT(*) AS n_changes,
    QUANTILE_CONT(abs_pct_change, 0.50) AS median_abs_pct_change,
    QUANTILE_CONT(abs_pct_change, 0.95) AS p95_abs_pct_change,
    AVG(CASE WHEN abs_pct_change > 0.10 THEN 1.0 ELSE 0.0 END) AS share_abs_change_gt10,
    AVG(CASE WHEN abs_pct_change > 0.25 THEN 1.0 ELSE 0.0 END) AS share_abs_change_gt25,
    AVG(CASE WHEN abs_pct_change > 0.50 THEN 1.0 ELSE 0.0 END) AS share_abs_change_gt50,
    AVG(CASE WHEN abs_pct_change > 1.00 THEN 1.0 ELSE 0.0 END) AS share_abs_change_gt100,
    MAX(abs_pct_change) AS max_abs_pct_change
  FROM changes
  WHERE change_window IS NOT NULL
  GROUP BY 1, 2
  ORDER BY 1, 2
  "
)
rent_change_flags[, month_start := as.Date(month_start)]

message("Computing repost/reset flags...")
repost_reset_flags <- collect_query(
  "
  WITH ordered AS (
    SELECT
      month_start,
      unit_id,
      id,
      file_date,
      posted_date,
      scraped_timestamp,
      LAG(id) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_id,
      LAG(file_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_file_date,
      LAG(posted_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_posted_date
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
  )
  SELECT
    month_start,
    COUNT(*) FILTER (WHERE prev_id IS NOT NULL) AS n_transitions,
    AVG(CASE WHEN prev_id IS NOT NULL AND DATE_DIFF('day', prev_file_date, file_date) <= 14 AND id <> prev_id THEN 1.0 ELSE 0.0 END) AS share_id_churn_same_spell,
    AVG(CASE WHEN prev_id IS NOT NULL AND DATE_DIFF('day', prev_file_date, file_date) <= 14 AND DATE_DIFF('day', prev_posted_date, posted_date) >= 7 THEN 1.0 ELSE 0.0 END) AS share_posted_reset_same_spell,
    AVG(CASE WHEN prev_id IS NOT NULL AND DATE_DIFF('day', prev_file_date, file_date) > 14 AND DATE_DIFF('day', prev_posted_date, posted_date) >= 0 THEN 1.0 ELSE 0.0 END) AS share_reposted_after_gap,
    MAX(CASE WHEN prev_id IS NOT NULL THEN DATE_DIFF('day', prev_posted_date, posted_date) END) AS max_posted_delta_days
  FROM ordered
  GROUP BY 1
  ORDER BY 1
  "
)
repost_reset_flags[, month_start := as.Date(month_start)]

message("Computing geo/address instability flags...")
geo_address_flags <- collect_query(
  "
  WITH native_unit_geo AS (
    SELECT
      unit_id,
      MIN(latitude) AS min_lat,
      MAX(latitude) AS max_lat,
      MIN(longitude) AS min_lon,
      MAX(longitude) AS max_lon,
      AVG(latitude) AS avg_lat,
      COUNT(DISTINCT coord4) FILTER (WHERE coord4 IS NOT NULL) AS n_coords,
      COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) AS n_addresses
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
    GROUP BY 1
  ),
  unit_presence AS (
    SELECT DISTINCT month_start, unit_id
    FROM chicago_raw
    WHERE unit_id IS NOT NULL
  )
  SELECT
    p.month_start,
    COUNT(*) AS n_active_units,
    AVG(CASE WHEN n_addresses > 1 THEN 1.0 ELSE 0.0 END) AS share_units_multiple_addresses,
    AVG(
      CASE
        WHEN SQRT(
          POWER((g.max_lat - g.min_lat) * 364000, 2) +
          POWER((g.max_lon - g.min_lon) * 288200 * COS(g.avg_lat * PI() / 180.0), 2)
        ) > 100 THEN 1.0 ELSE 0.0
      END
    ) AS share_units_geo_span_gt100,
    AVG(
      CASE
        WHEN SQRT(
          POWER((g.max_lat - g.min_lat) * 364000, 2) +
          POWER((g.max_lon - g.min_lon) * 288200 * COS(g.avg_lat * PI() / 180.0), 2)
        ) > 500 THEN 1.0 ELSE 0.0
      END
    ) AS share_units_geo_span_gt500,
    MAX(
      SQRT(
        POWER((g.max_lat - g.min_lat) * 364000, 2) +
        POWER((g.max_lon - g.min_lon) * 288200 * COS(g.avg_lat * PI() / 180.0), 2)
      )
    ) AS max_geo_span_ft
  FROM unit_presence p
  LEFT JOIN native_unit_geo g
    ON p.unit_id = g.unit_id
  GROUP BY 1
  ORDER BY 1
  "
)
geo_address_flags[, month_start := as.Date(month_start)]

message("Computing company/building-type composition shifts...")
company_building_type_composition <- collect_query(
  "
  WITH company_counts AS (
    SELECT
      month_start,
      company_norm,
      COUNT(*) AS n_rows
    FROM chicago_raw
    WHERE company_norm IS NOT NULL
    GROUP BY 1, 2
  ),
  company_shares AS (
    SELECT
      month_start,
      company_norm,
      n_rows,
      SUM(n_rows) OVER (PARTITION BY month_start) AS month_total
    FROM company_counts
  ),
  company_summary AS (
    SELECT
      month_start,
      COUNT(*) AS distinct_named_companies,
      MAX(n_rows * 1.0 / month_total) AS top_company_share,
      SUM(POWER(n_rows * 1.0 / month_total, 2)) AS company_hhi
    FROM company_shares
    GROUP BY 1
  )
  SELECT
    c.month_start,
    COUNT(*) AS total_rows,
    AVG(CASE WHEN company_norm IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_company_nonmissing,
    AVG(CASE WHEN building_type_clean = 'multi_family' THEN 1.0 ELSE 0.0 END) AS share_multi_family,
    AVG(CASE WHEN building_type_clean = 'single_family' THEN 1.0 ELSE 0.0 END) AS share_single_family,
    AVG(CASE WHEN building_type_clean = 'condo' THEN 1.0 ELSE 0.0 END) AS share_condo,
    AVG(CASE WHEN building_type_clean = 'townhouse' THEN 1.0 ELSE 0.0 END) AS share_townhouse,
    AVG(CASE WHEN building_type_clean = 'other' THEN 1.0 ELSE 0.0 END) AS share_other_building_type,
    AVG(CASE WHEN beds IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_beds_nonmissing,
    AVG(CASE WHEN baths IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_baths_nonmissing,
    AVG(CASE WHEN sqft IS NOT NULL THEN 1.0 ELSE 0.0 END) AS share_sqft_nonmissing,
    s.distinct_named_companies,
    s.top_company_share,
    s.company_hhi
  FROM chicago_raw c
  LEFT JOIN company_summary s
    ON c.month_start = s.month_start
  GROUP BY 1, 12, 13, 14
  ORDER BY 1
  "
)
company_building_type_composition[, month_start := as.Date(month_start)]

message("Computing monthly rent series...")
monthly_rent_series <- collect_query(
  "
  WITH raw_month AS (
    SELECT
      month_start,
      AVG(rent_price) FILTER (WHERE rent_price > 0) AS raw_mean_rent,
      QUANTILE_CONT(rent_price, 0.50) FILTER (WHERE rent_price > 0) AS raw_median_rent,
      COUNT(*) FILTER (WHERE rent_price > 0) AS raw_positive_rent_rows
    FROM chicago_raw
    GROUP BY 1
  ),
  unit_day_last AS (
    SELECT
      month_start,
      AVG(rent_price) AS unit_day_last_mean_rent,
      QUANTILE_CONT(rent_price, 0.50) AS unit_day_last_median_rent,
      COUNT(*) AS n_unit_days
    FROM (
      SELECT
        month_start,
        unit_id,
        file_date,
        rent_price,
        ROW_NUMBER() OVER (PARTITION BY unit_id, file_date ORDER BY scraped_timestamp DESC, id DESC) AS rn
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
        AND rent_price > 0
    ) x
    WHERE rn = 1
    GROUP BY 1
  ),
  unit_month_last AS (
    SELECT
      month_start,
      AVG(rent_price) AS unit_month_last_mean_rent,
      QUANTILE_CONT(rent_price, 0.50) AS unit_month_last_median_rent,
      COUNT(*) AS n_unit_month_last_obs
    FROM (
      SELECT
        month_start,
        unit_id,
        rent_price,
        ROW_NUMBER() OVER (PARTITION BY unit_id, month_start ORDER BY scraped_timestamp DESC, id DESC) AS rn
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
        AND rent_price > 0
    ) x
    WHERE rn = 1
    GROUP BY 1
  ),
  unit_month_median AS (
    SELECT
      month_start,
      AVG(unit_month_median_rent) AS unit_month_median_mean_rent,
      QUANTILE_CONT(unit_month_median_rent, 0.50) AS unit_month_median_median_rent,
      COUNT(*) AS n_unit_month_median_obs
    FROM (
      SELECT
        month_start,
        unit_id,
        QUANTILE_CONT(rent_price, 0.50) AS unit_month_median_rent
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
        AND rent_price > 0
      GROUP BY 1, 2
    ) x
    GROUP BY 1
  )
  SELECT
    r.month_start,
    r.raw_mean_rent,
    r.raw_median_rent,
    r.raw_positive_rent_rows,
    d.unit_day_last_mean_rent,
    d.unit_day_last_median_rent,
    d.n_unit_days,
    l.unit_month_last_mean_rent,
    l.unit_month_last_median_rent,
    l.n_unit_month_last_obs,
    m.unit_month_median_mean_rent,
    m.unit_month_median_median_rent,
    m.n_unit_month_median_obs
  FROM raw_month r
  LEFT JOIN unit_day_last d ON r.month_start = d.month_start
  LEFT JOIN unit_month_last l ON r.month_start = l.month_start
  LEFT JOIN unit_month_median m ON r.month_start = m.month_start
  ORDER BY 1
  "
)
monthly_rent_series[, month_start := as.Date(month_start)]

message("Fetching external benchmarks...")
zillow_city <- fetch_zillow_series(
  url = "https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv",
  region_name = "Chicago",
  state = "IL",
  value_name = "zillow_city_zori"
)
zillow_metro <- fetch_zillow_series(
  url = "https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_month.csv",
  region_name = "Chicago, IL",
  value_name = "zillow_metro_zori"
)
fred_series <- fetch_fred_series("CUURA207SEHA")
acs_series <- fetch_acs_series()

benchmark_fetch_status <- rbindlist(list(
  data.table(source = "zillow_city_zori", status = "ok", detail = "City_zori_uc_sfrcondomfr_sm_month.csv"),
  data.table(source = "zillow_metro_zori", status = "ok", detail = "Metro_zori_uc_sfrcondomfr_sm_month.csv"),
  data.table(source = "fred_cuura207seha", status = "ok", detail = "Rent of Primary Residence in Chicago metro"),
  data.table(
    source = "acs_b25064",
    status = if (all(acs_series$status == "ok")) "ok" else "warning",
    detail = paste(unique(acs_series$status), collapse = " | ")
  )
), fill = TRUE)

message("Merging series and computing benchmark metrics...")
monthly_rent_series <- merge(monthly_skeleton[, .(month_start)], monthly_rent_series, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series <- merge(monthly_rent_series, zillow_city, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series <- merge(monthly_rent_series, zillow_metro, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series <- merge(monthly_rent_series, fred_series, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series <- merge(monthly_rent_series, acs_series[, .(month_start, acs_chicago_median_gross_rent)], by = "month_start", all.x = TRUE, sort = TRUE)

canonical_series_col <- "unit_month_median_mean_rent"
level_cols <- c(
  "raw_mean_rent",
  "unit_day_last_mean_rent",
  "unit_month_last_mean_rent",
  canonical_series_col,
  "zillow_city_zori",
  "zillow_metro_zori",
  "fred_chi_rent_cpi"
)

for (col in level_cols) {
  sm_col <- paste0(col, "_sm3")
  yoy_col <- paste0(col, "_yoy_pct")
  monthly_rent_series[, (sm_col) := zoo::rollapplyr(get(col), 3, mean, fill = NA_real_, partial = FALSE)]
  monthly_rent_series[, (yoy_col) := 100 * (get(sm_col) / shift(get(sm_col), 12) - 1)]
}

zillow_roll <- rolling_compare_metrics(
  dt = monthly_rent_series,
  x_col = paste0(canonical_series_col, "_yoy_pct"),
  y_col = "zillow_city_zori_yoy_pct",
  lags = 0L,
  window_n = 24L,
  min_obs = 12L
)
setnames(zillow_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("zillow_pairs_roll24", "zillow_corr_roll24", "zillow_best_lag_roll24", "zillow_median_abs_gap_pp_roll24"))

fred_roll <- rolling_compare_metrics(
  dt = monthly_rent_series,
  x_col = paste0(canonical_series_col, "_yoy_pct"),
  y_col = "fred_chi_rent_cpi_yoy_pct",
  lags = 0L:6L,
  window_n = 24L,
  min_obs = 12L
)
setnames(fred_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("fred_pairs_roll24", "fred_best_corr_roll24", "fred_best_lag_roll24", "fred_median_abs_gap_pp_roll24"))

monthly_rent_series <- merge(monthly_rent_series, zillow_roll, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series <- merge(monthly_rent_series, fred_roll, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_rent_series[, benchmark_status := vapply(seq_len(.N), function(i) {
  classify_benchmark_status(
    corr_zillow = zillow_corr_roll24[i],
    gap_zillow_pp = zillow_median_abs_gap_pp_roll24[i],
    corr_fred = fred_best_corr_roll24[i]
  )
}, character(1))]

external_benchmark_comparison <- monthly_rent_series[, .(
  month_start,
  unit_month_median_mean_rent,
  unit_month_median_mean_rent_yoy_pct,
  zillow_city_zori,
  zillow_city_zori_yoy_pct,
  zillow_metro_zori,
  zillow_metro_zori_yoy_pct,
  fred_chi_rent_cpi,
  fred_chi_rent_cpi_yoy_pct,
  acs_chicago_median_gross_rent,
  zillow_pairs_roll24,
  zillow_corr_roll24,
  zillow_median_abs_gap_pp_roll24,
  fred_pairs_roll24,
  fred_best_corr_roll24,
  fred_best_lag_roll24,
  benchmark_status
)]

message("Building monthly verdict scorecard...")
coverage_status <- monthly_completeness_outages[, .(
  month_start,
  distinct_unit_ids,
  share_rows_with_unit_id,
  unit_coverage_status = vapply(seq_len(.N), function(i) classify_coverage_status(share_rows_with_unit_id[i], distinct_unit_ids[i]), character(1))
)]

frequency_status <- native_unit_repeat_frequency[, .(
  month_start,
  p95_unit_month_rows,
  share_unit_month_gt14,
  frequency_status = vapply(seq_len(.N), function(i) classify_frequency_status(p95_unit_month_rows[i], share_unit_month_gt14[i]), character(1))
)]

same_day_status <- same_day_multi_rent_conflicts[, .(
  month_start,
  share_unit_day_multi_rent,
  same_day_conflict_status = vapply(seq_len(.N), function(i) classify_same_day_status(share_unit_day_multi_rent[i]), character(1))
)]

volatility_status <- rent_change_flags[change_window == "31d", .(
  month_start,
  share_abs_change_gt25,
  share_abs_change_gt50,
  volatility_status = vapply(seq_len(.N), function(i) classify_volatility_status(share_abs_change_gt25[i], share_abs_change_gt50[i]), character(1))
)]

monthly_verdict_scorecard <- merge(monthly_completeness_outages[, .(
  month_start,
  year,
  year_month,
  total_rows,
  observed_scrape_days,
  first_file_date,
  last_file_date,
  left_edge_gap_days,
  right_edge_gap_days,
  partial_month,
  missing_month,
  abrupt_observation_collapse,
  abrupt_geography_collapse,
  abrupt_unit_id_coverage_collapse,
  completeness_status
)], coverage_status, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_verdict_scorecard <- merge(monthly_verdict_scorecard, frequency_status, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_verdict_scorecard <- merge(monthly_verdict_scorecard, same_day_status, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_verdict_scorecard <- merge(monthly_verdict_scorecard, volatility_status, by = "month_start", all.x = TRUE, sort = TRUE)
monthly_verdict_scorecard <- merge(monthly_verdict_scorecard, external_benchmark_comparison[, .(
  month_start,
  benchmark_status,
  zillow_corr_roll24,
  zillow_median_abs_gap_pp_roll24,
  fred_best_corr_roll24,
  fred_best_lag_roll24
)], by = "month_start", all.x = TRUE, sort = TRUE)

monthly_verdict_scorecard[is.na(unit_coverage_status), unit_coverage_status := "RED"]
monthly_verdict_scorecard[is.na(frequency_status), frequency_status := "RED"]
monthly_verdict_scorecard[is.na(same_day_conflict_status), same_day_conflict_status := "RED"]
monthly_verdict_scorecard[is.na(volatility_status), volatility_status := "RED"]

monthly_verdict_scorecard[, overall_verdict := vapply(seq_len(.N), function(i) {
  if (completeness_status[i] == "RED" || unit_coverage_status[i] == "RED") {
    return("RED")
  }
  pick_worst_status(c(
    frequency_status[i],
    same_day_conflict_status[i],
    volatility_status[i],
    benchmark_status[i]
  ))
}, character(1))]

monthly_verdict_scorecard[is.na(overall_verdict), overall_verdict := "RED"]

message("Computing subperiod and full-panel verdicts...")
period_lookup <- data.table(
  period = c("2014-2017", "2018-2020", "2021-2025", "2014-2025"),
  start_month = as.Date(c("2014-01-01", "2018-01-01", "2021-01-01", "2014-01-01")),
  end_month = as.Date(c("2017-12-01", "2020-12-01", "2025-12-01", "2025-12-01"))
)

subperiod_verdict_scorecard <- rbindlist(lapply(seq_len(nrow(period_lookup)), function(i) {
  p <- period_lookup[i]
  period_months <- monthly_verdict_scorecard[month_start >= p$start_month & month_start <= p$end_month]
  period_bench <- external_benchmark_comparison[month_start >= p$start_month & month_start <= p$end_month]

  zillow_metrics <- compute_period_metrics(period_bench, paste0(canonical_series_col, "_yoy_pct"), "zillow_city_zori_yoy_pct", lags = 0L, min_obs = 12L)
  fred_metrics <- compute_period_metrics(period_bench, paste0(canonical_series_col, "_yoy_pct"), "fred_chi_rent_cpi_yoy_pct", lags = 0L:6L, min_obs = 12L)
  bench_status <- classify_benchmark_status(zillow_metrics$best_corr[1], zillow_metrics$median_abs_gap_pp[1], fred_metrics$best_corr[1])

  red_streak <- longest_run(period_months$overall_verdict == "RED")
  red_streak_post2019 <- longest_run(period_months[month_start >= as.Date("2019-01-01"), overall_verdict == "RED"])
  share_nonred <- mean(period_months$overall_verdict != "RED")
  n_nonred <- sum(period_months$overall_verdict != "RED")

  verdict <- if (
    share_nonred >= 0.80 &&
      (p$period != "2014-2025" || red_streak_post2019 <= 2) &&
      identical(bench_status, "GREEN")
  ) {
    "GREEN"
  } else if (
    share_nonred >= 0.50 &&
      n_nonred >= 12 &&
      !identical(bench_status, "RED")
  ) {
    "YELLOW"
  } else {
    "RED"
  }

  data.table(
    period = p$period,
    start_month = p$start_month,
    end_month = p$end_month,
    n_months = nrow(period_months),
    n_green = sum(period_months$overall_verdict == "GREEN"),
    n_yellow = sum(period_months$overall_verdict == "YELLOW"),
    n_red = sum(period_months$overall_verdict == "RED"),
    share_nonred = share_nonred,
    longest_red_streak = red_streak,
    longest_red_streak_post2019 = red_streak_post2019,
    zillow_corr = zillow_metrics$best_corr[1],
    zillow_median_abs_gap_pp = zillow_metrics$median_abs_gap_pp[1],
    fred_best_corr = fred_metrics$best_corr[1],
    fred_best_lag = fred_metrics$best_lag[1],
    benchmark_status = bench_status,
    verdict = verdict
  )
}), fill = TRUE)

full_panel_verdict <- subperiod_verdict_scorecard[period == "2014-2025"]
recommended_usable_window <- recommended_window(monthly_verdict_scorecard)

message("Collecting manual-review cases...")
manual_review_cases <- rbindlist(list(
  collect_query(
    "
    WITH native_unit_day AS (
      SELECT
        month_start,
        unit_id,
        file_date,
        MIN(address_norm) AS sample_address,
        MIN(company_norm) AS sample_company,
        COUNT(*) AS n_rows,
        COUNT(DISTINCT id) AS n_ids,
        COUNT(DISTINCT rent_price) FILTER (WHERE rent_price > 0) AS n_rents
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
      GROUP BY 1, 2, 3
    )
    SELECT
      'same_day_multi_rent' AS issue_type,
      month_start,
      unit_id AS entity_key,
      file_date AS event_date,
      sample_address,
      sample_company,
      n_rows AS metric_primary,
      n_rents AS metric_secondary,
      NULL AS extra_detail
    FROM native_unit_day
    WHERE n_rents > 1
    ORDER BY n_rents DESC, n_rows DESC
    LIMIT 50
    "
  ),
  collect_query(
    "
    WITH ordered AS (
      SELECT
        month_start,
        unit_id,
        file_date,
        address_norm,
        company_norm,
        rent_price,
        LAG(rent_price) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_rent,
        LAG(file_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_file_date
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
        AND rent_price > 0
    )
    SELECT
      'rent_change_outlier' AS issue_type,
      month_start,
      unit_id AS entity_key,
      file_date AS event_date,
      address_norm AS sample_address,
      company_norm AS sample_company,
      ABS(rent_price / prev_rent - 1.0) AS metric_primary,
      DATE_DIFF('day', prev_file_date, file_date) AS metric_secondary,
      CAST(prev_rent AS VARCHAR) || ' -> ' || CAST(rent_price AS VARCHAR) AS extra_detail
    FROM ordered
    WHERE prev_rent > 0
      AND prev_file_date IS NOT NULL
      AND DATE_DIFF('day', prev_file_date, file_date) BETWEEN 0 AND 31
    ORDER BY metric_primary DESC
    LIMIT 50
    "
  ),
  collect_query(
    "
    WITH native_unit_geo AS (
      SELECT
        unit_id,
        MIN(month_start) AS month_start,
        MIN(address_norm) AS sample_address,
        MIN(company_norm) AS sample_company,
        SQRT(
          POWER((MAX(latitude) - MIN(latitude)) * 364000, 2) +
          POWER((MAX(longitude) - MIN(longitude)) * 288200 * COS(AVG(latitude) * PI() / 180.0), 2)
        ) AS geo_span_ft,
        COUNT(DISTINCT address_norm) FILTER (WHERE address_norm IS NOT NULL) AS n_addresses
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
      GROUP BY 1
    )
    SELECT
      'geo_address_instability' AS issue_type,
      month_start,
      unit_id AS entity_key,
      NULL AS event_date,
      sample_address,
      sample_company,
      geo_span_ft AS metric_primary,
      n_addresses AS metric_secondary,
      NULL AS extra_detail
    FROM native_unit_geo
    WHERE geo_span_ft > 100 OR n_addresses > 1
    ORDER BY geo_span_ft DESC, n_addresses DESC
    LIMIT 50
    "
  ),
  collect_query(
    "
    WITH ordered AS (
      SELECT
        month_start,
        unit_id,
        file_date,
        address_norm,
        company_norm,
        posted_date,
        LAG(posted_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_posted_date,
        LAG(file_date) OVER (PARTITION BY unit_id ORDER BY scraped_timestamp, id) AS prev_file_date
      FROM chicago_raw
      WHERE unit_id IS NOT NULL
    )
    SELECT
      'posted_date_reset' AS issue_type,
      month_start,
      unit_id AS entity_key,
      file_date AS event_date,
      address_norm AS sample_address,
      company_norm AS sample_company,
      DATE_DIFF('day', prev_posted_date, posted_date) AS metric_primary,
      DATE_DIFF('day', prev_file_date, file_date) AS metric_secondary,
      NULL AS extra_detail
    FROM ordered
    WHERE prev_posted_date IS NOT NULL
      AND prev_file_date IS NOT NULL
      AND DATE_DIFF('day', prev_file_date, file_date) <= 14
      AND DATE_DIFF('day', prev_posted_date, posted_date) >= 7
    ORDER BY metric_primary DESC
    LIMIT 50
    "
  ),
  collect_query(
    "
    WITH missing_month AS (
      SELECT
        month_start,
        fingerprint_key,
        MIN(address_norm) AS sample_address,
        MIN(company_norm) AS sample_company,
        COUNT(*) AS n_rows
      FROM chicago_raw
      WHERE unit_id IS NULL
        AND fingerprint_key IS NOT NULL
      GROUP BY 1, 2
    )
    SELECT
      'missing_id_fingerprint_cluster' AS issue_type,
      month_start,
      fingerprint_key AS entity_key,
      NULL AS event_date,
      sample_address,
      sample_company,
      n_rows AS metric_primary,
      NULL AS metric_secondary,
      NULL AS extra_detail
    FROM missing_month
    ORDER BY n_rows DESC
    LIMIT 50
    "
  )
), fill = TRUE)
manual_review_cases[, month_start := as.Date(month_start)]
manual_review_cases[, event_date := as.Date(event_date)]

message("Writing CSV outputs...")
assert_unique(monthly_identifier_coverage, "month_start", "monthly_identifier_coverage")
assert_unique(monthly_completeness_outages, "month_start", "monthly_completeness_outages")
assert_unique(native_unit_repeat_frequency, "month_start", "native_unit_repeat_frequency")
assert_unique(building_layer_repeat_frequency, "month_start", "building_layer_repeat_frequency")
assert_unique(missing_id_fingerprint_frequency, "month_start", "missing_id_fingerprint_frequency")
assert_unique(same_day_multi_rent_conflicts, "month_start", "same_day_multi_rent_conflicts")
assert_unique(repost_reset_flags, "month_start", "repost_reset_flags")
assert_unique(geo_address_flags, "month_start", "geo_address_flags")
assert_unique(company_building_type_composition, "month_start", "company_building_type_composition")
assert_unique(monthly_rent_series, "month_start", "monthly_rent_series")
assert_unique(external_benchmark_comparison, "month_start", "external_benchmark_comparison")
assert_unique(monthly_verdict_scorecard, "month_start", "monthly_verdict_scorecard")
assert_unique(subperiod_verdict_scorecard, "period", "subperiod_verdict_scorecard")

write_csv_dt(monthly_identifier_coverage, file.path(out_dir, "monthly_identifier_coverage.csv"))
write_csv_dt(monthly_completeness_outages, file.path(out_dir, "monthly_completeness_outages.csv"))
write_csv_dt(native_unit_repeat_frequency, file.path(out_dir, "native_unit_repeat_frequency.csv"))
write_csv_dt(building_layer_repeat_frequency, file.path(out_dir, "building_layer_repeat_frequency.csv"))
write_csv_dt(missing_id_fingerprint_frequency, file.path(out_dir, "missing_id_fingerprint_frequency.csv"))
write_csv_dt(same_day_multi_rent_conflicts, file.path(out_dir, "same_day_multi_rent_conflicts.csv"))
write_csv_dt(rent_change_flags, file.path(out_dir, "rent_change_flags.csv"))
write_csv_dt(repost_reset_flags, file.path(out_dir, "repost_reset_flags.csv"))
write_csv_dt(geo_address_flags, file.path(out_dir, "geo_address_flags.csv"))
write_csv_dt(company_building_type_composition, file.path(out_dir, "company_building_type_composition.csv"))
write_csv_dt(monthly_rent_series, file.path(out_dir, "monthly_rent_series.csv"))
write_csv_dt(benchmark_fetch_status, file.path(out_dir, "benchmark_fetch_status.csv"))
write_csv_dt(external_benchmark_comparison, file.path(out_dir, "external_benchmark_comparison.csv"))
write_csv_dt(monthly_verdict_scorecard, file.path(out_dir, "monthly_verdict_scorecard.csv"))
write_csv_dt(subperiod_verdict_scorecard, file.path(out_dir, "subperiod_verdict_scorecard.csv"))
write_csv_dt(full_panel_verdict, file.path(out_dir, "full_panel_verdict.csv"))
write_csv_dt(recommended_usable_window, file.path(out_dir, "recommended_usable_window.csv"))
write_csv_dt(manual_review_cases, file.path(out_dir, "manual_review_top_cases.csv"))

message("Rendering figures...")
heatmap_dt <- melt(
  monthly_verdict_scorecard[, .(
    month_start,
    completeness_status,
    unit_coverage_status,
    frequency_status,
    same_day_conflict_status,
    volatility_status,
    benchmark_status,
    overall_verdict
  )],
  id.vars = "month_start",
  variable.name = "dimension",
  value.name = "status"
)
heatmap_dt[, dimension := factor(
  dimension,
  levels = c(
    "overall_verdict",
    "completeness_status",
    "unit_coverage_status",
    "frequency_status",
    "same_day_conflict_status",
    "volatility_status",
    "benchmark_status"
  ),
  labels = c(
    "Overall",
    "Completeness",
    "Native Unit Coverage",
    "Listing Frequency",
    "Same-Day Conflicts",
    "31-Day Volatility",
    "External Agreement"
  )
)]

p_heatmap <- ggplot(heatmap_dt, aes(x = month_start, y = dimension, fill = status)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = c(status_fill, "NA" = "#d9d9d9"), na.value = "#d9d9d9", drop = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, fill = NULL, title = "RentHub Monthly Audit Verdict Heatmap") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_monthly_verdict_heatmap.pdf"), p_heatmap, width = 14, height = 4.8, bg = "white")

completeness_plot_dt <- monthly_completeness_outages[, .(
  month_start,
  observed_scrape_days,
  total_rows_index = {
    max_total_rows <- max(total_rows, na.rm = TRUE)
    if (is.finite(max_total_rows) && max_total_rows > 0) {
      100 * total_rows / max_total_rows
    } else {
      rep(NA_real_, .N)
    }
  }
)]
completeness_long <- melt(
  completeness_plot_dt,
  id.vars = "month_start",
  variable.name = "series",
  value.name = "value"
)
completeness_long[, series := factor(series, levels = c("observed_scrape_days", "total_rows_index"), labels = c("Observed Scrape Days", "Rows (Index = 100 at Max)"))]
p_completeness <- ggplot(completeness_long, aes(month_start, value, color = series)) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, color = NULL, title = "Monthly Scrape Completeness and Volume") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_scrape_day_completeness_timeline.pdf"), p_completeness, width = 13, height = 4.6, bg = "white")

p_unit_coverage <- ggplot(monthly_identifier_coverage, aes(month_start)) +
  geom_line(aes(y = 100 * share_rows_with_unit_id, color = "Share of Rows with UNIT_ID"), linewidth = 0.8) +
  geom_line(aes(y = distinct_unit_ids / 100, color = "Distinct UNIT_IDs / 100"), linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, color = NULL, title = "Native UNIT_ID Coverage Over Time") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_unit_id_coverage_timeline.pdf"), p_unit_coverage, width = 13, height = 4.6, bg = "white")

duplicate_plot_dt <- native_unit_repeat_frequency[, .(
  month_start,
  p95_unit_month_rows,
  share_unit_month_gt14 = 100 * share_unit_month_gt14,
  share_unit_day_multi_rent = 100 * share_unit_day_multi_rent
)]
duplicate_long <- melt(
  duplicate_plot_dt,
  id.vars = "month_start",
  variable.name = "series",
  value.name = "value"
)
duplicate_long[, series := factor(
  series,
  levels = c("p95_unit_month_rows", "share_unit_month_gt14", "share_unit_day_multi_rent"),
  labels = c("P95 Unit-Month Rows", "Share Unit-Month > 14 (%)", "Share Unit-Day Multi-Rent (%)")
)]
p_duplicate <- ggplot(duplicate_long, aes(month_start, value, color = series)) +
  geom_line(linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, color = NULL, title = "Duplicate and Repeat-Listing Intensity") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_duplicate_frequency_timeline.pdf"), p_duplicate, width = 13, height = 4.6, bg = "white")

volatility_plot_dt <- rent_change_flags[change_window == "31d", .(
  month_start,
  share_abs_change_gt25 = 100 * share_abs_change_gt25,
  share_abs_change_gt50 = 100 * share_abs_change_gt50,
  share_abs_change_gt100 = 100 * share_abs_change_gt100
)]
volatility_long <- melt(
  volatility_plot_dt,
  id.vars = "month_start",
  variable.name = "series",
  value.name = "value"
)
volatility_long[, series := factor(
  series,
  levels = c("share_abs_change_gt25", "share_abs_change_gt50", "share_abs_change_gt100"),
  labels = c("Share > 25%", "Share > 50%", "Share > 100%")
)]
p_volatility <- ggplot(volatility_long, aes(month_start, value, color = series)) +
  geom_line(linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL, color = NULL, title = "31-Day Within-Unit Rent Volatility") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_rent_volatility_timeline.pdf"), p_volatility, width = 13, height = 4.6, bg = "white")

overlay_dt <- data.table(
  month_start = monthly_rent_series$month_start,
  native_unit_month_median = monthly_rent_series[[canonical_series_col]],
  zillow_city = monthly_rent_series$zillow_city_zori,
  zillow_metro = monthly_rent_series$zillow_metro_zori,
  fred_rent_cpi = monthly_rent_series$fred_chi_rent_cpi
)
for (col in names(overlay_dt)[-1]) {
  base_idx <- which(is.finite(overlay_dt[[col]]))[1]
  if (length(base_idx) == 1L && is.finite(overlay_dt[[col]][base_idx]) && overlay_dt[[col]][base_idx] != 0) {
    overlay_dt[, (col) := 100 * get(col) / get(col)[base_idx]]
  } else {
    overlay_dt[, (col) := NA_real_]
  }
}
overlay_long <- melt(overlay_dt, id.vars = "month_start", variable.name = "series", value.name = "index_value")
overlay_long[, series := factor(
  series,
  levels = c("native_unit_month_median", "zillow_city", "zillow_metro", "fred_rent_cpi"),
  labels = c("RentHub Native Unit", "Zillow City ZORI", "Zillow Metro ZORI", "FRED Rent CPI")
)]
acs_overlay <- acs_series[is.finite(acs_chicago_median_gross_rent)]
if (nrow(acs_overlay) > 0) {
  acs_base <- acs_overlay$acs_chicago_median_gross_rent[which(is.finite(acs_overlay$acs_chicago_median_gross_rent))[1]]
  acs_overlay[, acs_index := 100 * acs_chicago_median_gross_rent / acs_base]
}
p_overlay <- ggplot(overlay_long, aes(month_start, index_value, color = series)) +
  geom_line(linewidth = 0.8) +
  {
    if (nrow(acs_overlay) > 0) geom_point(data = acs_overlay, aes(month_start, acs_index), color = "#555555", size = 2, inherit.aes = FALSE) else NULL
  } +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Index (first observed = 100)", color = NULL, title = "RentHub Versus External Chicago Rent Benchmarks") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_normalized_rent_overlay.pdf"), p_overlay, width = 13, height = 4.8, bg = "white")

full_fred_metrics <- compute_period_metrics(
  external_benchmark_comparison,
  paste0(canonical_series_col, "_yoy_pct"),
  "fred_chi_rent_cpi_yoy_pct",
  lags = 0L:6L,
  min_obs = 12L
)
best_fred_lag_full <- if (is.finite(full_fred_metrics$best_lag[1])) full_fred_metrics$best_lag[1] else 0L
yoy_plot_dt <- monthly_rent_series[, .(
  month_start,
  native_unit_yoy = get(paste0(canonical_series_col, "_yoy_pct")),
  zillow_city_yoy = zillow_city_zori_yoy_pct,
  fred_yoy_shifted = data.table::shift(fred_chi_rent_cpi_yoy_pct, n = best_fred_lag_full, type = "lead")
)]
yoy_long <- melt(yoy_plot_dt, id.vars = "month_start", variable.name = "series", value.name = "yoy_pct")
yoy_long[, series := factor(
  series,
  levels = c("native_unit_yoy", "zillow_city_yoy", "fred_yoy_shifted"),
  labels = c("RentHub Native Unit YoY", "Zillow City ZORI YoY", sprintf("FRED Rent CPI YoY (lead %d)", best_fred_lag_full))
)]
p_yoy <- ggplot(yoy_long, aes(month_start, yoy_pct, color = series)) +
  geom_line(linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "YoY Growth (%)", color = NULL, title = "YoY Growth Comparison: RentHub, Zillow, and FRED") +
  build_plot_theme()
ggsave(file.path(out_dir, "fig_monthly_yoy_comparison.pdf"), p_yoy, width = 13, height = 4.6, bg = "white")

message("Audit task complete. Outputs written to:")
message(sprintf("  - %s", file.path(out_dir, "monthly_verdict_scorecard.csv")))
message(sprintf("  - %s", file.path(out_dir, "subperiod_verdict_scorecard.csv")))
message(sprintf("  - %s", file.path(out_dir, "full_panel_verdict.csv")))
