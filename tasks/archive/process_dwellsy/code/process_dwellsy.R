source("../../setup_environment/code/packages.R")

sf_use_s2(FALSE)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_dwellsy/code")
# raw_dir <- "../input/dwellsy_raw"
# boundary_file <- "../input/chicago_boundary.geojson"
# zillow_city_file <- "../input/zillow_city_zori.csv"
# zillow_metro_file <- "../input/zillow_metro_zori.csv"
# fred_file <- "../input/fred_chi_rent_cpi.csv"
# cpi_file <- "../input/fred_chi_cpi_all_items.csv"
# output_dir <- "../output"
# temp_dir <- "../temp"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- tryCatch(
    c(
      raw_dir,
      boundary_file,
      zillow_city_file,
      zillow_metro_file,
      fred_file,
      cpi_file,
      output_dir,
      temp_dir
    ),
    error = function(e) {
      stop(
        "FATAL: process_dwellsy.R requires 8 args, or uncomment the top interactive block before sourcing in RStudio.",
        call. = FALSE
      )
    }
  )
}

if (length(cli_args) != 8) {
  stop(
    "FATAL: process_dwellsy.R requires 8 args: <raw_dir> <boundary_file> <zillow_city_file> <zillow_metro_file> <fred_file> <cpi_file> <output_dir> <temp_dir>",
    call. = FALSE
  )
}

raw_dir <- cli_args[1]
boundary_file <- cli_args[2]
zillow_city_file <- cli_args[3]
zillow_metro_file <- cli_args[4]
fred_file <- cli_args[5]
cpi_file <- cli_args[6]
output_dir <- cli_args[7]
temp_dir <- cli_args[8]

crs_projected <- 3435
real_base_year <- 2024L

classify_benchmark_status <- function(corr_zillow, gap_zillow_pp, corr_fred) {
  if (!is.finite(corr_zillow) || !is.finite(gap_zillow_pp) || !is.finite(corr_fred)) {
    return("insufficient_data")
  }
  if (corr_zillow >= 0.8 && gap_zillow_pp <= 5 && corr_fred >= 0.7) {
    return("strong")
  }
  if (corr_zillow >= 0.6 && gap_zillow_pp <= 8 && corr_fred >= 0.5) {
    return("usable")
  }
  "weak"
}

rolling_compare_metrics <- function(dt, x_col, y_col, lags = 0L:6L, min_obs = 12L) {
  dt <- copy(dt)
  dt <- dt[is.finite(get(x_col)) & is.finite(get(y_col))]
  if (nrow(dt) == 0) {
    return(data.table(
      month_start = as.Date(character()),
      n_pairs = integer(),
      best_corr = numeric(),
      best_lag = integer(),
      median_abs_gap_pp = numeric()
    ))
  }

  out <- vector("list", nrow(dt))
  for (i in seq_len(nrow(dt))) {
    window_dt <- dt[pmax(1L, i - 23L):i]
    best_corr <- NA_real_
    best_lag <- NA_integer_
    best_gap <- NA_real_
    best_n <- 0L

    for (lag_value in lags) {
      lagged_y <- data.table::shift(window_dt[[y_col]], n = lag_value, type = "lag")
      ok <- is.finite(window_dt[[x_col]]) & is.finite(lagged_y)
      if (sum(ok) < min_obs) {
        next
      }

      corr_value <- suppressWarnings(cor(window_dt[[x_col]][ok], lagged_y[ok]))
      gap_value <- median(abs(window_dt[[x_col]][ok] - lagged_y[ok]), na.rm = TRUE)
      if (!is.finite(corr_value)) {
        next
      }

      if (!is.finite(best_corr) || corr_value > best_corr) {
        best_corr <- corr_value
        best_lag <- lag_value
        best_gap <- gap_value
        best_n <- sum(ok)
      }
    }

    out[[i]] <- data.table(
      month_start = window_dt$month_start[nrow(window_dt)],
      n_pairs = best_n,
      best_corr = best_corr,
      best_lag = best_lag,
      median_abs_gap_pp = best_gap
    )
  }

  rbindlist(out)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  median(x)
}

parse_amenities <- function(x) {
  if (is.na(x) || !nzchar(str_trim(x))) {
    return(character())
  }

  txt <- str_trim(as.character(x))
  tokens <- tryCatch(
    {
      if (startsWith(txt, "[")) {
        parsed <- jsonlite::fromJSON(txt)
        if (is.null(parsed)) {
          character()
        } else {
          as.character(parsed)
        }
      } else {
        strsplit(txt, ";", fixed = TRUE)[[1]]
      }
    },
    error = function(e) {
      strsplit(txt, ";", fixed = TRUE)[[1]]
    }
  )

  tokens <- str_squish(tokens)
  tokens <- tokens[!is.na(tokens) & tokens != ""]
  unique(tokens)
}

make_amenity_base_name <- function(x) {
  cleaned <- tolower(x)
  cleaned <- gsub("&", " and ", cleaned, fixed = TRUE)
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  cleaned[cleaned == ""] <- "unknown"
  paste0("amenity_", cleaned)
}

build_amenity_dictionary <- function(amenity_long) {
  dictionary <- unique(amenity_long[, .(amenity_raw)])[order(amenity_raw)]
  dictionary[, amenity_base := make_amenity_base_name(amenity_raw)]
  dictionary[, amenity_name_index := seq_len(.N), by = amenity_base]
  dictionary[, amenity_column := fifelse(
    amenity_name_index == 1L,
    amenity_base,
    paste0(amenity_base, "_", amenity_name_index)
  )]
  dictionary[, amenity_name_index := NULL]
  dictionary
}

fetch_fred_series <- function(fred_file) {
  raw <- suppressWarnings(read_csv(fred_file, show_col_types = FALSE, progress = FALSE))
  setDT(raw)
  setnames(raw, names(raw), c("date", "value"))
  raw[, month_start := as.Date(date)]
  raw[, fred_chi_rent_cpi := as.numeric(value)]
  raw[month_start >= as.Date("2020-09-01"), .(month_start, fred_chi_rent_cpi)]
}

fetch_zillow_series <- function(zillow_file, region_name, state = NULL, value_name) {
  raw <- suppressWarnings(read_csv(zillow_file, show_col_types = FALSE, progress = FALSE))
  id_cols <- c("RegionID", "SizeRank", "RegionName", "RegionType", "StateName", "State", "Metro", "CountyName")
  month_cols <- setdiff(names(raw), id_cols)

  dt <- raw %>%
    filter(RegionName == region_name) %>%
    { if (is.null(state)) . else filter(., StateName == state) } %>%
    pivot_longer(cols = all_of(month_cols), names_to = "month_start", values_to = value_name) %>%
    mutate(month_start = lubridate::floor_date(as.Date(month_start), unit = "month")) %>%
    select(month_start, all_of(value_name))

  setDT(dt)
  dt[month_start >= as.Date("2020-09-01")]
}

load_cpi_deflator <- function(cpi_file, start_month, end_month, base_year) {
  raw <- suppressWarnings(read_csv(cpi_file, show_col_types = FALSE, progress = FALSE))
  if (ncol(raw) < 2) {
    stop("CPI input must have at least two columns.", call. = FALSE)
  }

  raw <- raw[, 1:2]
  setDT(raw)
  setnames(raw, names(raw), c("date", "value"))
  raw[, month_start := as.Date(date)]
  raw[, rent_price_cpi_chi_all_items := as.numeric(value)]
  raw <- raw[!is.na(month_start), .(month_start, rent_price_cpi_chi_all_items)]

  month_grid <- data.table(month_start = seq.Date(
    from = as.Date(format(start_month, "%Y-%m-01")),
    to = as.Date(format(end_month, "%Y-%m-01")),
    by = "month"
  ))
  cpi_dt <- merge(month_grid, raw, by = "month_start", all.x = TRUE, sort = TRUE)

  n_missing_pre <- cpi_dt[!is.finite(rent_price_cpi_chi_all_items), .N]
  if (n_missing_pre > 0) {
    idx_known <- which(is.finite(cpi_dt$rent_price_cpi_chi_all_items))
    if (length(idx_known) < 2) {
      stop("Not enough non-missing CPI observations to interpolate.", call. = FALSE)
    }
    cpi_interp <- approx(
      x = idx_known,
      y = cpi_dt$rent_price_cpi_chi_all_items[idx_known],
      xout = seq_len(nrow(cpi_dt)),
      method = "linear",
      rule = 2
    )$y
    cpi_dt[, rent_price_cpi_chi_all_items := fifelse(
      is.finite(rent_price_cpi_chi_all_items),
      rent_price_cpi_chi_all_items,
      cpi_interp
    )]
  }

  if (cpi_dt[!is.finite(rent_price_cpi_chi_all_items), .N] > 0) {
    stop("CPI input has unresolved missing values after interpolation.", call. = FALSE)
  }

  base_cpi <- cpi_dt[format(month_start, "%Y") == as.character(base_year), mean(rent_price_cpi_chi_all_items)]
  if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop(sprintf("Unable to compute base CPI for %d.", base_year), call. = FALSE)
  }

  cpi_dt[, rent_price_deflator_to_2024 := base_cpi / rent_price_cpi_chi_all_items]
  cpi_dt
}

fetch_acs_series <- function() {
  acs_max_year <- min(2025L, as.integer(format(Sys.Date(), "%Y")) - 1L)
  years <- 2020L:acs_max_year
  out <- vector("list", length(years))

  for (i in seq_along(years)) {
    year_value <- years[i]
    acs_row <- tryCatch(
      {
        tidycensus::get_acs(
          geography = "place",
          variables = "B25064_001",
          state = "IL",
          year = year_value,
          survey = "acs1",
          cache_table = TRUE
        ) %>%
          filter(NAME == "Chicago city, Illinois") %>%
          transmute(
            month_start = as.Date(sprintf("%d-01-01", year_value)),
            acs_chicago_median_gross_rent = as.numeric(estimate)
          )
      },
      error = function(e) {
        tibble(
          month_start = as.Date(sprintf("%d-01-01", year_value)),
          acs_chicago_median_gross_rent = NA_real_
        )
      }
    )
    out[[i]] <- acs_row
  }

  rbindlist(out)
}

message("Reading raw Dwellsy parquet shards...")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

raw_files <- list.files(raw_dir, pattern = "\\.parquet$", full.names = TRUE)
if (length(raw_files) == 0) {
  stop(sprintf("FATAL: No parquet shards found in %s", raw_dir), call. = FALSE)
}
raw_files_sql <- paste(sprintf("'%s'", raw_files), collapse = ", ")

raw_query <- sprintf(
  paste(
    "SELECT",
    "  CAST(LISTING_ID AS VARCHAR) AS listing_id,",
    "  CAST(PROPERTY_ID AS VARCHAR) AS property_id,",
    "  DWELLSY_ADDRESS_ID AS dwellsy_address_id,",
    "  CAST(ADDRESS1_ID AS VARCHAR) AS address1_id,",
    "  ADDRESS2_ID AS address2_id,",
    "  ADDRESS_1 AS address_1,",
    "  ADDRESS_2 AS address_2,",
    "  ADDRESS_CITY AS address_city,",
    "  ADDRESS_STATE AS address_state,",
    "  ADDRESS_ZIP AS address_zip,",
    "  ADDRESS_TYPE AS address_type,",
    "  ADDRESS_COUNTRY AS address_country,",
    "  AMENITIES AS amenities,",
    "  CAST(BEDROOMS AS DOUBLE) AS beds,",
    "  TRY_CAST(FULL_BATHS AS DOUBLE) AS full_baths,",
    "  TRY_CAST(HALF_BATHS AS DOUBLE) AS half_baths,",
    "  RENT_AMOUNT AS rent_price,",
    "  SQUARE_FEET AS sqft,",
    "  TRY_CAST(YEAR_BUILT AS DOUBLE) AS year_built,",
    "  LATITUDE AS latitude,",
    "  LONGITUDE AS longitude,",
    "  CREATION_TS AS creation_ts,",
    "  TRY_CAST(DEACTIVATION_TIME AS TIMESTAMP) AS deactivation_ts,",
    "  DEACTIVATION_TIME AS deactivation_time_raw,",
    "  PROPERTY_LISTING_STATUS AS listing_status,",
    "  COMMUNITY_ID AS community_id,",
    "  COMMUNITY_NAME AS community_name,",
    "  COMPANY_NAME AS company_name,",
    "  SS_RAW_PRIMARY_NUMBER AS ss_raw_primary_number,",
    "  SS_RAW_STREET_NAME AS ss_raw_street_name,",
    "  SS_RAW_STREET_SUFFIX AS ss_raw_street_suffix,",
    "  SS_RAW_STREET_PREDIRECTION AS ss_raw_street_predirection,",
    "  SS_RAW_STREET_POSTDIRECTION AS ss_raw_street_postdirection,",
    "  SS_RAW_SECONDARY_DESIGNATOR AS ss_raw_secondary_designator,",
    "  SS_RAW_SECONDARY_NUMBER AS ss_raw_secondary_number,",
    "  SS_RAW_ZIPCODE AS ss_raw_zipcode,",
    "  filename AS source_filename",
    "FROM read_parquet([%s], union_by_name = TRUE, filename = TRUE)",
    sep = "\n"
  ),
  raw_files_sql
)

raw_dt <- as.data.table(DBI::dbGetQuery(con, raw_query))
raw_dt[, source_filename := basename(source_filename)]
raw_dt[, creation_ts := as.POSIXct(creation_ts, tz = "UTC")]
raw_dt[, deactivation_ts := as.POSIXct(deactivation_ts, tz = "UTC")]
raw_dt[, creation_date := as.Date(creation_ts)]
raw_dt[, creation_month := lubridate::floor_date(creation_date, unit = "month")]
raw_dt[, deactivation_date := as.Date(deactivation_ts)]
raw_dt[, deactivation_month := lubridate::floor_date(deactivation_date, unit = "month")]
raw_dt[, listing_status := str_to_lower(str_trim(coalesce(listing_status, "")))]
raw_dt[, address_city := str_squish(address_city)]
raw_dt[, address_state := str_to_upper(str_squish(address_state))]
raw_dt[, city_label_chicago := str_to_upper(coalesce(address_city, "")) == "CHICAGO"]
raw_dt[, baths_total := fifelse(
  is.na(full_baths) & is.na(half_baths),
  NA_real_,
  coalesce(full_baths, 0) + 0.5 * coalesce(half_baths, 0)
)]

raw_dt[, missing_latlon := !is.finite(latitude) | !is.finite(longitude)]
raw_dt[, zero_latlon := !missing_latlon & (latitude == 0 | longitude == 0)]
raw_dt[, outside_bbox := !missing_latlon & !zero_latlon & !(latitude >= 41 & latitude <= 43 & longitude >= -89 & longitude <= -87)]
raw_dt[, nonpositive_rent := !is.finite(rent_price) | rent_price <= 0]
raw_dt[, bad_sqft := !is.finite(sqft) | sqft <= 0]
raw_dt[, missing_bedrooms := !is.finite(beds)]
raw_dt[, missing_baths := !is.finite(baths_total)]

message("Checking Chicago boundary membership...")
chicago_boundary <- st_read(boundary_file, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_projected)

candidate_points <- raw_dt[!missing_latlon & !zero_latlon & !outside_bbox]
# Create points in 4326 from raw lon/lat, then immediately project to 3435
# for Chicago boundary membership checks.
candidate_points_sf <- st_as_sf(candidate_points, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_projected)
inside_boundary <- lengths(st_within(candidate_points_sf, chicago_boundary)) > 0
candidate_points[, inside_city_boundary := inside_boundary]
raw_dt[candidate_points[, .(listing_id, creation_ts, source_filename, inside_city_boundary)],
       on = .(listing_id, creation_ts, source_filename),
       inside_city_boundary := i.inside_city_boundary]
raw_dt[is.na(inside_city_boundary), inside_city_boundary := FALSE]
raw_dt[, chicago_label_outside_boundary := city_label_chicago & !inside_city_boundary]
raw_dt[, nonchicago_label_inside_boundary := !city_label_chicago & inside_city_boundary]

kept_dt <- copy(raw_dt[inside_city_boundary == TRUE])
kept_dt[, source_row_order := .I]
setorder(kept_dt, listing_id, creation_ts, deactivation_ts, rent_price, source_filename, source_row_order)

canonical_cols <- c(
  "listing_id", "property_id", "dwellsy_address_id", "address1_id", "address2_id",
  "address_1", "address_2", "address_city", "address_state", "address_zip",
  "address_type", "address_country", "amenities", "beds", "full_baths",
  "half_baths", "baths_total", "rent_price", "sqft", "year_built", "latitude",
  "longitude", "creation_ts", "creation_date", "creation_month", "deactivation_ts",
  "deactivation_time_raw", "deactivation_date", "deactivation_month", "listing_status",
  "community_id", "community_name", "company_name", "ss_raw_primary_number",
  "ss_raw_street_name", "ss_raw_street_suffix", "ss_raw_street_predirection",
  "ss_raw_street_postdirection", "ss_raw_secondary_designator",
  "ss_raw_secondary_number", "ss_raw_zipcode", "city_label_chicago"
)

message("Building exact-duplicate signature...")
kept_dt[, canonical_signature := do.call(paste, c(.SD, sep = "||")), .SDcols = canonical_cols]
kept_dt[, duplicate_exact_group := duplicated(canonical_signature) | duplicated(canonical_signature, fromLast = TRUE)]
dedup_dt <- unique(kept_dt, by = "canonical_signature")
zero_sqft_dropped <- copy(dedup_dt[is.finite(sqft) & sqft == 0])
final_dt <- dedup_dt[!(is.finite(sqft) & sqft == 0)]
setorder(final_dt, listing_id, creation_ts, deactivation_ts, rent_price, source_filename, source_row_order)
final_dt[, dwellsy_row_id := .I]
final_dt[, nonpositive_rent := !is.finite(rent_price) | rent_price <= 0]
final_dt[, bad_sqft := !is.finite(sqft) | sqft <= 0]
final_dt[, missing_bedrooms := !is.finite(beds)]
final_dt[, missing_baths := !is.finite(baths_total)]
final_dt[, negative_sqft := is.finite(sqft) & sqft < 0]
final_dt[, zero_sqft := is.finite(sqft) & sqft == 0]
final_dt[, very_small_sqft := is.finite(sqft) & sqft > 0 & sqft < 200]
final_dt[, very_large_sqft := is.finite(sqft) & sqft > 5000]
final_dt[, rent_per_sqft := fifelse(
  is.finite(rent_price) & rent_price > 0 & is.finite(sqft) & sqft > 0,
  rent_price / sqft,
  NA_real_
)]
final_dt[, extreme_rent_per_sqft_high := is.finite(rent_per_sqft) & rent_per_sqft > 20]
final_dt[, hedonic_completeness := as.integer(!is.na(beds)) +
  as.integer(!is.na(baths_total)) +
  as.integer(is.finite(sqft) & sqft > 0) +
  as.integer(!is.na(year_built))]

message("Loading Chicago CPI-U all-items deflator...")
cpi_deflator <- load_cpi_deflator(
  cpi_file = cpi_file,
  start_month = min(final_dt$creation_month, na.rm = TRUE),
  end_month = max(c(final_dt$creation_month, final_dt$deactivation_month), na.rm = TRUE),
  base_year = real_base_year
)
final_dt[cpi_deflator, on = .(creation_month = month_start), `:=`(
  rent_price_cpi_chi_all_items = i.rent_price_cpi_chi_all_items,
  rent_price_deflator_to_2024 = i.rent_price_deflator_to_2024
)]
if (final_dt[!is.na(creation_month) & !is.finite(rent_price_deflator_to_2024), .N] > 0) {
  stop("Missing CPI deflator for one or more Dwellsy creation months.", call. = FALSE)
}
final_dt[, rent_price_real_2024 := fifelse(
  is.finite(rent_price),
  rent_price * rent_price_deflator_to_2024,
  NA_real_
)]

history_first_rows <- copy(final_dt)
setorder(history_first_rows, listing_id, creation_ts, deactivation_ts, rent_price, source_filename, source_row_order, dwellsy_row_id)
history_first_rows <- history_first_rows[, .SD[1], by = listing_id]

analysis_sample_candidates <- copy(final_dt)
analysis_sample_candidates[, preferred_active := listing_status == "active"]
analysis_sample_candidates[, preferred_open_ended := is.na(deactivation_ts)]
analysis_sample_candidates[, deactivation_sort_num := fifelse(
  is.na(deactivation_ts),
  -Inf,
  as.numeric(deactivation_ts)
)]
setorder(
  analysis_sample_candidates,
  listing_id,
  -preferred_active,
  -preferred_open_ended,
  -hedonic_completeness,
  creation_ts,
  -deactivation_sort_num,
  -rent_price,
  dwellsy_row_id
)
analysis_sample_pretrim <- analysis_sample_candidates[, .SD[1], by = listing_id]
setorder(analysis_sample_pretrim, listing_id)

analysis_sample_pretrim[, positive_rent := is.finite(rent_price) & rent_price > 0]
analysis_sample_pretrim[, positive_sqft := is.finite(sqft) & sqft > 0]
analysis_sample_rent_trim_bounds <- quantile(
  analysis_sample_pretrim[positive_rent == TRUE, rent_price],
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  type = 7
)
analysis_sample_sqft_trim_bounds <- quantile(
  analysis_sample_pretrim[positive_sqft == TRUE, sqft],
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  type = 7
)
analysis_sample_pretrim[, rent_outside_trim := positive_rent & (
  rent_price < analysis_sample_rent_trim_bounds[[1]] |
    rent_price > analysis_sample_rent_trim_bounds[[2]]
)]
analysis_sample_pretrim[, sqft_outside_trim := positive_sqft & (
  sqft < analysis_sample_sqft_trim_bounds[[1]] |
    sqft > analysis_sample_sqft_trim_bounds[[2]]
)]
analysis_sample_pretrim[, missing_or_nonpositive_rent := !positive_rent]
analysis_sample_pretrim[, missing_or_nonpositive_sqft := !positive_sqft]
analysis_sample_pretrim[, rent_trim_keep := positive_rent & !rent_outside_trim]
analysis_sample_pretrim[, sqft_trim_keep := positive_sqft & !sqft_outside_trim]
analysis_sample_pretrim[, sqft_mass_100 := positive_sqft & sqft == 100]
analysis_sample_pretrim[, pre_rent_per_sqft_trim_keep := rent_trim_keep & sqft_trim_keep & !sqft_mass_100]
analysis_sample_rent_per_sqft_trim_bounds <- quantile(
  analysis_sample_pretrim[pre_rent_per_sqft_trim_keep == TRUE, rent_per_sqft],
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  type = 7
)
analysis_sample_pretrim[, rent_per_sqft_outside_trim := pre_rent_per_sqft_trim_keep & (
  rent_per_sqft < analysis_sample_rent_per_sqft_trim_bounds[[1]] |
    rent_per_sqft > analysis_sample_rent_per_sqft_trim_bounds[[2]]
)]
analysis_sample_pretrim[, analysis_trim_keep := pre_rent_per_sqft_trim_keep & !rent_per_sqft_outside_trim]

analysis_sample_trimmed_out <- copy(analysis_sample_pretrim[analysis_trim_keep == FALSE])
analysis_sample_trimmed_out[, issue_type := fifelse(
  rent_per_sqft_outside_trim == TRUE,
  "analysis_sample_outside_rent_per_sqft_1_99",
  fifelse(
  sqft_mass_100 == TRUE & rent_trim_keep == TRUE & sqft_trim_keep == TRUE,
  "analysis_sample_sqft_100_mass",
  fifelse(
    missing_or_nonpositive_rent == TRUE & missing_or_nonpositive_sqft == TRUE,
    "analysis_sample_missing_or_nonpositive_rent_and_sqft",
    fifelse(
      missing_or_nonpositive_rent == TRUE,
      "analysis_sample_missing_or_nonpositive_rent",
      fifelse(
        missing_or_nonpositive_sqft == TRUE,
        "analysis_sample_missing_or_nonpositive_sqft",
        fifelse(
          rent_outside_trim == TRUE & sqft_outside_trim == TRUE,
          "analysis_sample_outside_rent_and_sqft_1_99",
          fifelse(
            rent_outside_trim == TRUE,
            "analysis_sample_outside_rent_1_99",
            "analysis_sample_outside_sqft_1_99"
          )
        )
      )
    )
  )
  )
)]

analysis_sample <- copy(analysis_sample_pretrim[analysis_trim_keep == TRUE])
setorder(analysis_sample, listing_id)

repeat_listing_resolution <- final_dt[, .(
  n_rows = .N,
  n_rent = uniqueN(rent_price),
  n_status = uniqueN(listing_status),
  n_creation_ts = uniqueN(creation_ts),
  n_deactivation_ts = uniqueN(deactivation_ts),
  n_source_files = uniqueN(source_filename),
  any_active = any(listing_status == "active"),
  any_missing_deactivation = any(is.na(deactivation_ts)),
  address_1 = first(address_1),
  address_city = first(address_city),
  latitude = first(latitude),
  longitude = first(longitude)
), by = listing_id][n_rows > 1]

repeat_listing_resolution <- merge(
  repeat_listing_resolution,
  history_first_rows[, .(
    listing_id,
    first_dwellsy_row_id = dwellsy_row_id,
    first_listing_status = listing_status,
    first_rent_price = rent_price,
    first_deactivation_ts = deactivation_ts
  )],
  by = "listing_id",
  all.x = TRUE,
  sort = FALSE
)
repeat_listing_resolution <- merge(
  repeat_listing_resolution,
  analysis_sample_pretrim[, .(
    listing_id,
    preferred_dwellsy_row_id = dwellsy_row_id,
    preferred_listing_status = listing_status,
    preferred_rent_price = rent_price,
    preferred_deactivation_ts = deactivation_ts,
    preferred_hedonic_completeness = hedonic_completeness,
    preferred_row_kept_in_trimmed_analysis_sample = analysis_trim_keep
  )],
  by = "listing_id",
  all.x = TRUE,
  sort = FALSE
)
repeat_listing_resolution[, resolution_changed_from_first := first_dwellsy_row_id != preferred_dwellsy_row_id]
repeat_listing_resolution[, first_deactivation_key := fifelse(
  is.na(first_deactivation_ts),
  "",
  format(first_deactivation_ts, "%Y-%m-%d %H:%M:%S")
)]
repeat_listing_resolution[, preferred_deactivation_key := fifelse(
  is.na(preferred_deactivation_ts),
  "",
  format(preferred_deactivation_ts, "%Y-%m-%d %H:%M:%S")
)]
repeat_listing_resolution[, resolution_change_type := fifelse(
  resolution_changed_from_first == FALSE,
  "same_as_first",
  fifelse(
    first_listing_status != preferred_listing_status & first_rent_price != preferred_rent_price,
    "status_and_rent_change",
    fifelse(
      first_listing_status != preferred_listing_status,
      "status_change_only",
      fifelse(
        first_rent_price != preferred_rent_price,
        "rent_change_only",
        fifelse(
          first_deactivation_key != preferred_deactivation_key,
          "deactivation_change_only",
          "other_change"
        )
      )
    )
  )
)]
repeat_listing_resolution[, c("first_deactivation_key", "preferred_deactivation_key") := NULL]
setorder(repeat_listing_resolution, -n_rows, -resolution_changed_from_first, listing_id)

sample_ladder <- data.table(
  step = c(
    "raw_all_rows",
    "raw_rows_with_valid_coordinates",
    "rows_inside_broad_chicago_bbox",
    "rows_inside_city_boundary",
    "rows_after_exact_dedup",
    "rows_after_zero_sqft_drop",
    "analysis_sample_rows_pretrim",
    "analysis_sample_rows_posttrim"
  ),
  n_rows = c(
    nrow(raw_dt),
    raw_dt[missing_latlon == FALSE & zero_latlon == FALSE, .N],
    raw_dt[missing_latlon == FALSE & zero_latlon == FALSE & outside_bbox == FALSE, .N],
    kept_dt[, .N],
    dedup_dt[, .N],
    final_dt[, .N],
    analysis_sample_pretrim[, .N],
    analysis_sample[, .N]
  )
)
sample_ladder[, dropped_from_previous := shift(n_rows, fill = n_rows[1]) - n_rows]
sample_ladder[, retained_share_raw := n_rows / n_rows[step == "raw_all_rows"]]

message("Building duplicate listing diagnostics...")
duplicate_listing_issues <- kept_dt[, .(
  n_rows_pre_dedup = .N,
  n_rows_post_dedup = uniqueN(canonical_signature),
  n_exact_duplicate_rows = .N - uniqueN(canonical_signature),
  n_status = uniqueN(listing_status),
  n_rent = uniqueN(rent_price),
  n_creation_ts = uniqueN(creation_ts),
  n_deactivation_ts = uniqueN(deactivation_ts),
  min_creation_ts = min(creation_ts, na.rm = TRUE),
  max_creation_ts = max(creation_ts, na.rm = TRUE),
  city_label_chicago = any(city_label_chicago),
  address_1 = first(address_1),
  address_city = first(address_city),
  latitude = first(latitude),
  longitude = first(longitude)
), by = listing_id][n_rows_pre_dedup > 1]

duplicate_listing_issues[, issue_type := fifelse(
  n_rows_post_dedup == 1 & n_exact_duplicate_rows > 0,
  "exact_duplicate_only",
  fifelse(
    n_status > 1,
    "status_change",
    fifelse(
      n_rent > 1,
      "rent_change",
      fifelse(
        n_creation_ts > 1 | n_deactivation_ts > 1,
        "timestamp_only_repeat",
        "other_repeat"
      )
    )
  )
)]
setorder(duplicate_listing_issues, -n_rows_pre_dedup, listing_id)

coordinate_hotspots <- raw_dt[
  !missing_latlon & !zero_latlon,
  .(hotspot_count = .N),
  by = .(latitude, longitude)
][hotspot_count >= 10]

coordinate_issues <- rbindlist(
  list(
    raw_dt[missing_latlon == TRUE, .(
      issue_type = "missing_latlon",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count = NA_integer_
    )],
    raw_dt[zero_latlon == TRUE, .(
      issue_type = "zero_latlon",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count = NA_integer_
    )],
    raw_dt[outside_bbox == TRUE, .(
      issue_type = "outside_broad_chicago_bbox",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count = NA_integer_
    )],
    raw_dt[chicago_label_outside_boundary == TRUE, .(
      issue_type = "chicago_label_outside_boundary",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count = NA_integer_
    )],
    raw_dt[nonchicago_label_inside_boundary == TRUE, .(
      issue_type = "inside_boundary_nonchicago_label",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count = NA_integer_
    )],
    raw_dt[coordinate_hotspots, on = .(latitude, longitude), nomatch = 0L][, .(
      issue_type = "exact_coordinate_hotspot",
      listing_id,
      property_id,
      address_1,
      address_city,
      latitude,
      longitude,
      creation_ts,
      source_filename,
      hotspot_count
    )]
  ),
  use.names = TRUE,
  fill = TRUE
)

message("Parsing amenities and building indicator dictionary...")
amenity_rows <- final_dt[!is.na(amenities) & str_trim(amenities) != "", .(dwellsy_row_id, amenities)]
amenity_long_list <- vector("list", nrow(amenity_rows))
if (nrow(amenity_rows) > 0) {
  for (i in seq_len(nrow(amenity_rows))) {
    amenity_tokens <- parse_amenities(amenity_rows$amenities[[i]])
    if (length(amenity_tokens) == 0) {
      next
    }
    amenity_long_list[[i]] <- data.table(
      dwellsy_row_id = amenity_rows$dwellsy_row_id[[i]],
      amenity_raw = amenity_tokens
    )
  }
}
amenity_long <- rbindlist(amenity_long_list, use.names = TRUE, fill = TRUE)
if (nrow(amenity_long) > 0) {
  amenity_long <- unique(amenity_long)
  amenity_dictionary <- build_amenity_dictionary(amenity_long)
  amenity_dictionary <- merge(
    amenity_dictionary,
    amenity_long[, .(n_rows_with_amenity = .N), by = amenity_raw],
    by = "amenity_raw",
    all.x = TRUE,
    sort = FALSE
  )
  amenity_dictionary[, share_rows_with_amenity := n_rows_with_amenity / nrow(final_dt)]
  setorder(amenity_dictionary, -n_rows_with_amenity, amenity_raw)
  amenity_long <- merge(
    amenity_long,
    amenity_dictionary[, .(amenity_raw, amenity_column)],
    by = "amenity_raw",
    all.x = TRUE,
    sort = FALSE
  )
} else {
  amenity_dictionary <- data.table(
    amenity_raw = character(),
    amenity_base = character(),
    amenity_column = character(),
    n_rows_with_amenity = integer(),
    share_rows_with_amenity = numeric()
  )
}

message("Building row-level value integrity diagnostics...")
value_integrity_issues <- rbindlist(
  list(
    analysis_sample_trimmed_out[, .(
      issue_type,
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    zero_sqft_dropped[, .(
      issue_type = "zero_sqft_dropped",
      dwellsy_row_id = NA_integer_,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft = NA_real_,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    final_dt[negative_sqft == TRUE, .(
      issue_type = "negative_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    final_dt[very_small_sqft == TRUE, .(
      issue_type = "very_small_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    final_dt[very_large_sqft == TRUE, .(
      issue_type = "very_large_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    final_dt[extreme_rent_per_sqft_high == TRUE, .(
      issue_type = "extreme_rent_per_sqft_high",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )],
    head(final_dt[order(-rent_price), .(
      issue_type = "top_rent",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )], 100L),
    head(final_dt[is.finite(sqft)][order(-sqft), .(
      issue_type = "top_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )], 100L),
    head(final_dt[is.finite(sqft) & sqft > 0][order(sqft), .(
      issue_type = "bottom_positive_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )], 100L),
    head(final_dt[is.finite(rent_per_sqft)][order(-rent_per_sqft), .(
      issue_type = "top_rent_per_sqft",
      dwellsy_row_id,
      listing_id,
      address_1,
      address_city,
      rent_price,
      sqft,
      rent_per_sqft,
      beds,
      baths_total,
      year_built,
      listing_status,
      creation_ts
    )], 100L)
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(value_integrity_issues, issue_type, -rent_per_sqft, -rent_price, dwellsy_row_id)

message("Building ID integrity checks...")
id_integrity <- rbindlist(
  list(
    data.table(
      check_type = "uniqueness",
      id_field = "listing_id",
      related_field = NA_character_,
      n_missing = final_dt[listing_id == "" | is.na(listing_id), .N],
      n_unique = uniqueN(final_dt$listing_id),
      n_rows_with_duplicates = final_dt[, .N, by = listing_id][N > 1 & !is.na(listing_id) & listing_id != "", sum(N)],
      max_rows_per_id = final_dt[, max(.N), by = listing_id][, max(V1)]
    ),
    data.table(
      check_type = "uniqueness",
      id_field = "property_id",
      related_field = NA_character_,
      n_missing = final_dt[property_id == "" | is.na(property_id), .N],
      n_unique = uniqueN(final_dt$property_id),
      n_rows_with_duplicates = final_dt[, .N, by = property_id][N > 1 & !is.na(property_id) & property_id != "", sum(N)],
      max_rows_per_id = final_dt[, max(.N), by = property_id][, max(V1)]
    ),
    data.table(
      check_type = "uniqueness",
      id_field = "dwellsy_address_id",
      related_field = NA_character_,
      n_missing = final_dt[dwellsy_address_id == "" | is.na(dwellsy_address_id), .N],
      n_unique = uniqueN(final_dt$dwellsy_address_id),
      n_rows_with_duplicates = final_dt[, .N, by = dwellsy_address_id][N > 1 & !is.na(dwellsy_address_id) & dwellsy_address_id != "", sum(N)],
      max_rows_per_id = final_dt[, max(.N), by = dwellsy_address_id][, max(V1)]
    ),
    data.table(
      check_type = "uniqueness",
      id_field = "address1_id",
      related_field = NA_character_,
      n_missing = final_dt[address1_id == "" | is.na(address1_id), .N],
      n_unique = uniqueN(final_dt$address1_id),
      n_rows_with_duplicates = final_dt[, .N, by = address1_id][N > 1 & !is.na(address1_id) & address1_id != "", sum(N)],
      max_rows_per_id = final_dt[, max(.N), by = address1_id][, max(V1)]
    ),
    final_dt[property_id != "" & !is.na(property_id), .(
      check_type = "nesting",
      id_field = "property_id",
      related_field = "dwellsy_address_id",
      n_missing = NA_integer_,
      n_unique = uniqueN(property_id),
      n_rows_with_duplicates = NA_integer_,
      max_rows_per_id = NA_real_,
      n_ids_multi_related = sum(uniqueN(dwellsy_address_id) > 1),
      max_related_per_id = max(uniqueN(dwellsy_address_id))
    ), by = property_id][, .(
      check_type = first(check_type),
      id_field = first(id_field),
      related_field = first(related_field),
      n_missing = NA_integer_,
      n_unique = uniqueN(property_id),
      n_rows_with_duplicates = NA_integer_,
      max_rows_per_id = NA_real_,
      n_ids_multi_related = sum(n_ids_multi_related > 0),
      max_related_per_id = max(max_related_per_id)
    )],
    final_dt[dwellsy_address_id != "" & !is.na(dwellsy_address_id), .(
      check_type = "nesting",
      id_field = "dwellsy_address_id",
      related_field = "property_id",
      n_missing = NA_integer_,
      n_unique = uniqueN(dwellsy_address_id),
      n_rows_with_duplicates = NA_integer_,
      max_rows_per_id = NA_real_,
      n_ids_multi_related = sum(uniqueN(property_id) > 1),
      max_related_per_id = max(uniqueN(property_id))
    ), by = dwellsy_address_id][, .(
      check_type = first(check_type),
      id_field = first(id_field),
      related_field = first(related_field),
      n_missing = NA_integer_,
      n_unique = uniqueN(dwellsy_address_id),
      n_rows_with_duplicates = NA_integer_,
      max_rows_per_id = NA_real_,
      n_ids_multi_related = sum(n_ids_multi_related > 0),
      max_related_per_id = max(max_related_per_id)
    )]
  ),
  use.names = TRUE,
  fill = TRUE
)

quality_summary <- data.table(
  metric = c(
    "raw_rows",
    "raw_chicago_label_rows",
    "raw_missing_latlon_rows",
    "raw_zero_latlon_rows",
    "raw_outside_bbox_rows",
    "rows_inside_city_boundary",
    "rows_after_exact_dedup",
    "rows_dropped_zero_sqft_after_dedup",
    "rows_after_zero_sqft_drop",
    "analysis_sample_rows_pretrim",
    "analysis_sample_rows_trimmed_out_any",
    "analysis_sample_rows_posttrim",
    "analysis_sample_rows_missing_or_nonpositive_sqft",
    "analysis_sample_rows_outside_rent_1_99",
    "analysis_sample_rows_outside_sqft_1_99",
    "analysis_sample_rows_sqft_100_mass",
    "analysis_sample_rows_outside_rent_per_sqft_1_99",
    "rows_with_chicago_label_outside_boundary",
    "rows_inside_boundary_nonchicago_label",
    "rows_with_nonpositive_rent",
    "rows_with_bad_sqft",
    "rows_with_negative_sqft",
    "rows_with_zero_sqft",
    "rows_with_very_small_sqft",
    "rows_with_very_large_sqft",
    "rows_with_extreme_rent_per_sqft_high",
    "rows_with_missing_bedrooms",
    "rows_with_missing_baths",
    "duplicate_listing_rows_pre_dedup",
    "exact_duplicate_rows_pre_dedup",
    "repeated_listing_ids_post_dedup",
    "rows_in_repeated_listing_ids_post_dedup",
    "repeated_listing_ids_changed_under_preferred_rule",
    "outside_city_boundary_rows",
    "exact_coordinate_hotspot_rows"
  ),
  n_rows = c(
    nrow(raw_dt),
    raw_dt[city_label_chicago == TRUE, .N],
    raw_dt[missing_latlon == TRUE, .N],
    raw_dt[zero_latlon == TRUE, .N],
    raw_dt[outside_bbox == TRUE, .N],
    kept_dt[, .N],
    dedup_dt[, .N],
    zero_sqft_dropped[, .N],
    final_dt[, .N],
    analysis_sample_pretrim[, .N],
    analysis_sample_trimmed_out[, .N],
    analysis_sample[, .N],
    analysis_sample_pretrim[missing_or_nonpositive_sqft == TRUE, .N],
    analysis_sample_pretrim[rent_outside_trim == TRUE, .N],
    analysis_sample_pretrim[sqft_outside_trim == TRUE, .N],
    analysis_sample_pretrim[sqft_mass_100 == TRUE, .N],
    analysis_sample_pretrim[rent_per_sqft_outside_trim == TRUE, .N],
    raw_dt[chicago_label_outside_boundary == TRUE, .N],
    raw_dt[nonchicago_label_inside_boundary == TRUE, .N],
    final_dt[nonpositive_rent == TRUE, .N],
    final_dt[bad_sqft == TRUE, .N],
    final_dt[negative_sqft == TRUE, .N],
    final_dt[zero_sqft == TRUE, .N],
    final_dt[very_small_sqft == TRUE, .N],
    final_dt[very_large_sqft == TRUE, .N],
    final_dt[extreme_rent_per_sqft_high == TRUE, .N],
    final_dt[missing_bedrooms == TRUE, .N],
    final_dt[missing_baths == TRUE, .N],
    kept_dt[, .N] - uniqueN(kept_dt$listing_id),
    kept_dt[, .N] - dedup_dt[, .N],
    final_dt[, .N, by = listing_id][N > 1, .N],
    final_dt[, .N, by = listing_id][N > 1, sum(N)],
    repeat_listing_resolution[resolution_changed_from_first == TRUE, .N],
    raw_dt[inside_city_boundary == FALSE, .N],
    coordinate_issues[issue_type == "exact_coordinate_hotspot", .N]
  )
)
quality_summary[, share_raw := n_rows / quality_summary[metric == "raw_rows", n_rows]]

message("Building monthly Dwellsy series...")
analysis_end_month <- max(c(final_dt$creation_month, final_dt$deactivation_month), na.rm = TRUE)

listing_first <- copy(history_first_rows)
listing_last <- final_dt[order(listing_id, creation_ts, deactivation_ts, source_row_order, dwellsy_row_id)][, .SD[.N], by = listing_id]
listing_span <- final_dt[, .(
  creation_month = min(creation_month, na.rm = TRUE),
  deactivation_month = {
    non_missing <- deactivation_month[is.finite(as.numeric(deactivation_month))]
    if (length(non_missing) == 0) as.Date(NA) else max(non_missing)
  }
), by = listing_id]

listing_primary <- merge(
  listing_first[, .(
    listing_id,
    creation_month,
    rent_price_first = rent_price,
    listing_status_first = listing_status
  )],
  listing_last[, .(
    listing_id,
    rent_price_last = rent_price,
    listing_status_last = listing_status,
    deactivation_month_last = deactivation_month
  )],
  by = "listing_id",
  all = TRUE
)
listing_primary <- merge(listing_primary, listing_span, by = c("listing_id", "creation_month"), all.x = TRUE)
listing_primary[is.na(deactivation_month), deactivation_month := deactivation_month_last]
listing_primary[, interval_end_month := fifelse(
  is.na(deactivation_month),
  analysis_end_month,
  pmax(deactivation_month, creation_month)
)]
listing_primary[, open_ended := is.na(deactivation_month)]

created_variants <- rbindlist(
  list(
    listing_primary[, .(
      month_start = creation_month,
      variant_id = "created_listing_first",
      variant_label = "Created month, first row per listing",
      family = "created_month",
      rent_price = rent_price_first,
      support_unit = listing_id
    )],
    listing_primary[listing_status_last == "active", .(
      month_start = creation_month,
      variant_id = "created_listing_first_active_status",
      variant_label = "Created month, first row per listing, latest status active",
      family = "created_month",
      rent_price = rent_price_first,
      support_unit = listing_id
    )],
    final_dt[, .(
      month_start = creation_month,
      variant_id = "created_all_history_rows",
      variant_label = "Created month, all cleaned history rows",
      family = "created_month",
      rent_price = rent_price,
      support_unit = dwellsy_row_id
    )]
  ),
  use.names = TRUE
)

expand_active_months <- function(dt, variant_id, variant_label, rent_col, status_filter = NULL) {
  use_dt <- copy(dt)
  if (!is.null(status_filter)) {
    use_dt <- use_dt[get("listing_status_last") %in% status_filter]
  }
  use_dt <- use_dt[!is.na(creation_month) & !is.na(interval_end_month) & interval_end_month >= creation_month]
  if (nrow(use_dt) == 0) {
    return(data.table())
  }

  use_dt[, start_month_index := lubridate::year(creation_month) * 12L + lubridate::month(creation_month) - 1L]
  use_dt[, end_month_index := lubridate::year(interval_end_month) * 12L + lubridate::month(interval_end_month) - 1L]
  use_dt[, n_months := end_month_index - start_month_index + 1L]
  month_offset <- sequence(use_dt$n_months) - 1L
  expanded <- use_dt[rep(seq_len(.N), n_months)]
  expanded[, month_offset := month_offset]
  expanded[, month_index := start_month_index + month_offset]
  expanded[, month_start := as.Date(sprintf(
    "%04d-%02d-01",
    month_index %/% 12L,
    (month_index %% 12L) + 1L
  ))]
  expanded <- expanded[, .(
    month_start,
    variant_id = variant_id,
    variant_label = variant_label,
    family = "active_month",
    rent_price = get(rent_col),
    support_unit = listing_id,
    open_ended = open_ended
  )]
  expanded
}

active_variants <- rbindlist(
  list(
    expand_active_months(
      dt = listing_primary,
      variant_id = "active_listing_span_first",
      variant_label = "Active month, first-row rent over listing span",
      rent_col = "rent_price_first"
    ),
    expand_active_months(
      dt = listing_primary,
      variant_id = "active_listing_span_last",
      variant_label = "Active month, last-row rent over listing span",
      rent_col = "rent_price_last"
    ),
    expand_active_months(
      dt = listing_primary,
      variant_id = "active_listing_span_active_status",
      variant_label = "Active month, latest status active only",
      rent_col = "rent_price_first",
      status_filter = "active"
    )
  ),
  use.names = TRUE,
  fill = TRUE
)

monthly_variants <- rbindlist(list(created_variants, active_variants), use.names = TRUE, fill = TRUE)
monthly_variants <- monthly_variants[is.finite(rent_price) & rent_price > 0]
monthly_variants[cpi_deflator, on = "month_start", `:=`(
  rent_price_cpi_chi_all_items = i.rent_price_cpi_chi_all_items,
  rent_price_deflator_to_2024 = i.rent_price_deflator_to_2024
)]
if (monthly_variants[!is.finite(rent_price_deflator_to_2024), .N] > 0) {
  stop("Missing CPI deflator for one or more Dwellsy monthly-series months.", call. = FALSE)
}
monthly_variants[, rent_price_real_2024 := rent_price * rent_price_deflator_to_2024]

monthly_series_variants <- monthly_variants[, .(
  n_obs = uniqueN(support_unit),
  mean_rent = mean(rent_price, na.rm = TRUE),
  median_rent = median(rent_price, na.rm = TRUE),
  mean_rent_real_2024 = mean(rent_price_real_2024, na.rm = TRUE),
  median_rent_real_2024 = median(rent_price_real_2024, na.rm = TRUE),
  open_ended_share = mean(open_ended %in% TRUE, na.rm = TRUE)
), by = .(month_start, variant_id, variant_label, family)]
setorder(monthly_series_variants, variant_id, month_start)
monthly_series_variants[, median_rent_sm3 := zoo::rollapplyr(median_rent, 3, mean, fill = NA_real_, partial = FALSE), by = variant_id]
monthly_series_variants[, median_rent_yoy_pct := 100 * (median_rent_sm3 / shift(median_rent_sm3, 12) - 1), by = variant_id]
monthly_series_variants[, median_rent_real_2024_sm3 := zoo::rollapplyr(median_rent_real_2024, 3, mean, fill = NA_real_, partial = FALSE), by = variant_id]
monthly_series_variants[, median_rent_real_2024_yoy_pct := 100 * (median_rent_real_2024_sm3 / shift(median_rent_real_2024_sm3, 12) - 1), by = variant_id]

monthly_series <- merge(
  monthly_series_variants[variant_id == "created_listing_first", .(
    month_start,
    created_month_n_listings = n_obs,
    created_month_mean_rent = mean_rent,
    created_month_median_rent = median_rent,
    created_month_median_rent_sm3 = median_rent_sm3,
    created_month_median_rent_yoy_pct = median_rent_yoy_pct,
    created_month_mean_rent_real_2024 = mean_rent_real_2024,
    created_month_median_rent_real_2024 = median_rent_real_2024,
    created_month_median_rent_real_2024_sm3 = median_rent_real_2024_sm3,
    created_month_median_rent_real_2024_yoy_pct = median_rent_real_2024_yoy_pct
  )],
  monthly_series_variants[variant_id == "active_listing_span_first", .(
    month_start,
    active_month_n_listings = n_obs,
    active_month_mean_rent = mean_rent,
    active_month_median_rent = median_rent,
    active_month_median_rent_sm3 = median_rent_sm3,
    active_month_median_rent_yoy_pct = median_rent_yoy_pct,
    active_month_mean_rent_real_2024 = mean_rent_real_2024,
    active_month_median_rent_real_2024 = median_rent_real_2024,
    active_month_median_rent_real_2024_sm3 = median_rent_real_2024_sm3,
    active_month_median_rent_real_2024_yoy_pct = median_rent_real_2024_yoy_pct,
    active_month_open_ended_share = open_ended_share
  )],
  by = "month_start",
  all = TRUE
)
setorder(monthly_series, month_start)

message("Fetching external benchmark series...")
zillow_city <- fetch_zillow_series(
  zillow_file = zillow_city_file,
  region_name = "Chicago",
  state = "IL",
  value_name = "zillow_city_zori"
)
zillow_metro <- fetch_zillow_series(
  zillow_file = zillow_metro_file,
  region_name = "Chicago, IL",
  value_name = "zillow_metro_zori"
)
fred_series <- fetch_fred_series(fred_file)
acs_series <- fetch_acs_series()

benchmark_months <- data.table(month_start = seq.Date(
  from = as.Date("2020-09-01"),
  to = max(monthly_series$month_start, na.rm = TRUE),
  by = "month"
))
benchmark_base <- Reduce(
  function(x, y) merge(x, y, by = "month_start", all = TRUE, sort = TRUE),
  list(benchmark_months, zillow_city, zillow_metro, fred_series, acs_series)
)
setorder(benchmark_base, month_start)
benchmark_base[cpi_deflator, on = "month_start", `:=`(
  rent_price_cpi_chi_all_items = i.rent_price_cpi_chi_all_items,
  rent_price_deflator_to_2024 = i.rent_price_deflator_to_2024
)]
benchmark_base[, zillow_city_zori_real_2024 := zillow_city_zori * rent_price_deflator_to_2024]
benchmark_base[, zillow_metro_zori_real_2024 := zillow_metro_zori * rent_price_deflator_to_2024]
benchmark_base[, fred_chi_rent_cpi_real_2024 := fred_chi_rent_cpi * rent_price_deflator_to_2024]
for (series_col in c("zillow_city_zori", "zillow_metro_zori", "fred_chi_rent_cpi")) {
  benchmark_base[, paste0(series_col, "_sm3") := zoo::rollapplyr(get(series_col), 3, mean, fill = NA_real_, partial = FALSE)]
  benchmark_base[, paste0(series_col, "_yoy_pct") := 100 * (get(paste0(series_col, "_sm3")) / shift(get(paste0(series_col, "_sm3")), 12) - 1)]
}
for (series_col in c("zillow_city_zori_real_2024", "zillow_metro_zori_real_2024", "fred_chi_rent_cpi_real_2024")) {
  benchmark_base[, paste0(series_col, "_sm3") := zoo::rollapplyr(get(series_col), 3, mean, fill = NA_real_, partial = FALSE)]
  benchmark_base[, paste0(series_col, "_yoy_pct") := 100 * (get(paste0(series_col, "_sm3")) / shift(get(paste0(series_col, "_sm3")), 12) - 1)]
}

benchmark_comparison <- rbindlist(lapply(unique(monthly_series_variants$variant_id), function(v) {
  variant_dt <- monthly_series_variants[variant_id == v]
  variant_dt <- merge(variant_dt, benchmark_base, by = "month_start", all.x = TRUE, sort = TRUE)
  zillow_roll <- rolling_compare_metrics(variant_dt, "median_rent_yoy_pct", "zillow_city_zori_yoy_pct", lags = 0L, min_obs = 12L)
  fred_roll <- rolling_compare_metrics(variant_dt, "median_rent_yoy_pct", "fred_chi_rent_cpi_yoy_pct", lags = 0L:6L, min_obs = 12L)
  setnames(zillow_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("zillow_pairs_roll24", "zillow_corr_roll24", "zillow_best_lag_roll24", "zillow_median_abs_gap_pp_roll24"))
  setnames(fred_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("fred_pairs_roll24", "fred_best_corr_roll24", "fred_best_lag_roll24", "fred_median_abs_gap_pp_roll24"))
  variant_dt <- merge(variant_dt, zillow_roll, by = "month_start", all.x = TRUE, sort = TRUE)
  variant_dt <- merge(variant_dt, fred_roll, by = "month_start", all.x = TRUE, sort = TRUE)
  variant_dt[, benchmark_status := vapply(
    seq_len(.N),
    function(i) classify_benchmark_status(
      corr_zillow = zillow_corr_roll24[i],
      gap_zillow_pp = zillow_median_abs_gap_pp_roll24[i],
      corr_fred = fred_best_corr_roll24[i]
    ),
    character(1)
  )]
  variant_dt
}), use.names = TRUE, fill = TRUE)

benchmark_scorecard <- benchmark_comparison[, .(
  first_month = min(month_start, na.rm = TRUE),
  last_month = max(month_start, na.rm = TRUE),
  n_months = .N,
  n_months_with_yoy = sum(is.finite(median_rent_yoy_pct)),
  median_monthly_support = as.numeric(median(n_obs, na.rm = TRUE)),
  max_monthly_support = safe_max(n_obs),
  share_months_strong = as.numeric(mean(benchmark_status == "strong", na.rm = TRUE)),
  share_months_usable = as.numeric(mean(benchmark_status %in% c("strong", "usable"), na.rm = TRUE)),
  best_zillow_corr = safe_max(zillow_corr_roll24),
  median_zillow_gap_pp = safe_median(zillow_median_abs_gap_pp_roll24),
  best_fred_corr = safe_max(fred_best_corr_roll24),
  median_open_ended_share = safe_median(open_ended_share),
  overall_benchmark_verdict = classify_benchmark_status(
    corr_zillow = safe_median(zillow_corr_roll24),
    gap_zillow_pp = safe_median(zillow_median_abs_gap_pp_roll24),
    corr_fred = safe_median(fred_best_corr_roll24)
  )
), by = .(variant_id, variant_label, family)]

benchmark_comparison_real <- rbindlist(lapply(unique(monthly_series_variants$variant_id), function(v) {
  variant_dt <- monthly_series_variants[variant_id == v]
  variant_dt <- merge(variant_dt, benchmark_base, by = "month_start", all.x = TRUE, sort = TRUE)
  zillow_roll <- rolling_compare_metrics(variant_dt, "median_rent_real_2024_yoy_pct", "zillow_city_zori_real_2024_yoy_pct", lags = 0L, min_obs = 12L)
  fred_roll <- rolling_compare_metrics(variant_dt, "median_rent_real_2024_yoy_pct", "fred_chi_rent_cpi_real_2024_yoy_pct", lags = 0L:6L, min_obs = 12L)
  setnames(zillow_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("zillow_pairs_roll24", "zillow_corr_roll24", "zillow_best_lag_roll24", "zillow_median_abs_gap_pp_roll24"))
  setnames(fred_roll, c("n_pairs", "best_corr", "best_lag", "median_abs_gap_pp"), c("fred_pairs_roll24", "fred_best_corr_roll24", "fred_best_lag_roll24", "fred_median_abs_gap_pp_roll24"))
  variant_dt <- merge(variant_dt, zillow_roll, by = "month_start", all.x = TRUE, sort = TRUE)
  variant_dt <- merge(variant_dt, fred_roll, by = "month_start", all.x = TRUE, sort = TRUE)
  variant_dt[, benchmark_status := vapply(
    seq_len(.N),
    function(i) classify_benchmark_status(
      corr_zillow = zillow_corr_roll24[i],
      gap_zillow_pp = zillow_median_abs_gap_pp_roll24[i],
      corr_fred = fred_best_corr_roll24[i]
    ),
    character(1)
  )]
  variant_dt
}), use.names = TRUE, fill = TRUE)

benchmark_scorecard_real <- benchmark_comparison_real[, .(
  first_month = min(month_start, na.rm = TRUE),
  last_month = max(month_start, na.rm = TRUE),
  n_months = .N,
  n_months_with_yoy = sum(is.finite(median_rent_real_2024_yoy_pct)),
  median_monthly_support = as.numeric(median(n_obs, na.rm = TRUE)),
  max_monthly_support = safe_max(n_obs),
  share_months_strong = as.numeric(mean(benchmark_status == "strong", na.rm = TRUE)),
  share_months_usable = as.numeric(mean(benchmark_status %in% c("strong", "usable"), na.rm = TRUE)),
  best_zillow_corr = safe_max(zillow_corr_roll24),
  median_zillow_gap_pp = safe_median(zillow_median_abs_gap_pp_roll24),
  best_fred_corr = safe_max(fred_best_corr_roll24),
  median_open_ended_share = safe_median(open_ended_share),
  overall_benchmark_verdict = classify_benchmark_status(
    corr_zillow = safe_median(zillow_corr_roll24),
    gap_zillow_pp = safe_median(zillow_median_abs_gap_pp_roll24),
    corr_fred = safe_median(fred_best_corr_roll24)
  )
), by = .(variant_id, variant_label, family)]

overall_verdict <- "usable_with_caution"
if (quality_summary[metric == "rows_with_chicago_label_outside_boundary", n_rows] <= 100 &&
    quality_summary[metric == "rows_inside_boundary_nonchicago_label", n_rows] <= 100 &&
    benchmark_scorecard_real[variant_id == "active_listing_span_first", overall_benchmark_verdict] %in% c("strong", "usable")) {
  overall_verdict <- "promising_for_2023_designs"
}
if (benchmark_scorecard_real[variant_id == "active_listing_span_first", overall_benchmark_verdict] == "weak") {
  overall_verdict <- "not_ready_for_causal_use"
}

quality_memo <- c(
  "# Dwellsy Quality Memo",
  "",
  sprintf("- Raw Illinois rows scanned: %s", format(nrow(raw_dt), big.mark = ",")),
  sprintf("- Chicago rows kept by spatial boundary: %s", format(kept_dt[, .N], big.mark = ",")),
  sprintf("- Chicago rows after exact dedupe: %s", format(dedup_dt[, .N], big.mark = ",")),
  sprintf("- Zero-sqft rows dropped after exact dedupe: %s", format(zero_sqft_dropped[, .N], big.mark = ",")),
  sprintf("- Final Chicago rows after exact dedupe and zero-sqft drop: %s", format(final_dt[, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows before trim (one preferred row per listing): %s", format(analysis_sample_pretrim[, .N], big.mark = ",")),
  sprintf("- Analysis-sample rent trim bounds (1st, 99th percentile): %.0f to %.0f", analysis_sample_rent_trim_bounds[[1]], analysis_sample_rent_trim_bounds[[2]]),
  sprintf("- Analysis-sample sqft trim bounds (1st, 99th percentile): %.0f to %.0f", analysis_sample_sqft_trim_bounds[[1]], analysis_sample_sqft_trim_bounds[[2]]),
  sprintf("- Analysis-sample rent-per-sqft trim bounds (1st, 99th percentile): %.3f to %.3f", analysis_sample_rent_per_sqft_trim_bounds[[1]], analysis_sample_rent_per_sqft_trim_bounds[[2]]),
  sprintf("- Analysis-sample rows with missing or nonpositive sqft: %s", format(analysis_sample_pretrim[missing_or_nonpositive_sqft == TRUE, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows outside rent 1/99 trim bounds: %s", format(analysis_sample_pretrim[rent_outside_trim == TRUE, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows outside sqft 1/99 trim bounds: %s", format(analysis_sample_pretrim[sqft_outside_trim == TRUE, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows at the 100 sqft mass: %s", format(analysis_sample_pretrim[sqft_mass_100 == TRUE, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows outside rent-per-sqft 1/99 trim bounds: %s", format(analysis_sample_pretrim[rent_per_sqft_outside_trim == TRUE, .N], big.mark = ",")),
  sprintf("- Analysis-sample rows after rent/sqft/rent-per-sqft trim and 100 sqft drop: %s", format(analysis_sample[, .N], big.mark = ",")),
  sprintf("- Rent deflated to real %d dollars using the Chicago CPI-U all-items series", real_base_year),
  sprintf("- Exact duplicate rows dropped: %s", format(kept_dt[, .N] - dedup_dt[, .N], big.mark = ",")),
  sprintf("- Repeated listing IDs after dedupe: %s", format(final_dt[, .N, by = listing_id][N > 1, .N], big.mark = ",")),
  sprintf("- Repeated listing IDs whose preferred row differs from the first history row: %s", format(repeat_listing_resolution[resolution_changed_from_first == TRUE, .N], big.mark = ",")),
  sprintf("- Chicago-label rows outside the city boundary: %s", format(raw_dt[chicago_label_outside_boundary == TRUE, .N], big.mark = ",")),
  sprintf("- Non-Chicago-label rows inside the city boundary: %s", format(raw_dt[nonchicago_label_inside_boundary == TRUE, .N], big.mark = ",")),
  sprintf("- Outside-bbox rows: %s", format(raw_dt[outside_bbox == TRUE, .N], big.mark = ",")),
  sprintf("- Rows with negative sqft: %s", format(final_dt[negative_sqft == TRUE, .N], big.mark = ",")),
  sprintf("- Rows with zero sqft in cleaned outputs: %s", format(final_dt[zero_sqft == TRUE, .N], big.mark = ",")),
  sprintf("- Rows with very small sqft (<200): %s", format(final_dt[very_small_sqft == TRUE, .N], big.mark = ",")),
  sprintf("- Rows with very large sqft (>5000): %s", format(final_dt[very_large_sqft == TRUE, .N], big.mark = ",")),
  sprintf("- Rows with extreme rent per sqft (>20): %s", format(final_dt[extreme_rent_per_sqft_high == TRUE, .N], big.mark = ",")),
  sprintf("- Parsed amenity indicators: %s", format(nrow(amenity_dictionary), big.mark = ",")),
  sprintf("- Active-month real benchmark verdict: %s", benchmark_scorecard_real[variant_id == "active_listing_span_first", overall_benchmark_verdict]),
  sprintf("- Active-month best real Zillow rolling correlation: %.3f", benchmark_scorecard_real[variant_id == "active_listing_span_first", best_zillow_corr]),
  sprintf("- Active-month best real FRED rolling correlation: %.3f", benchmark_scorecard_real[variant_id == "active_listing_span_first", best_fred_corr]),
  sprintf("- Active-month median real Zillow YoY gap (pp): %.3f", benchmark_scorecard_real[variant_id == "active_listing_span_first", median_zillow_gap_pp]),
  "",
  sprintf("Overall verdict: `%s`", overall_verdict),
  "",
  "Interpretation:",
  "- The cleaned Chicago history file looks spatially usable: coordinate coverage is complete, the city-boundary filter is tight, and exact duplicates are removed deterministically.",
  "- Zero-sqft rows are now dropped from the cleaned history and analysis sample, while remaining visible in the value-integrity diagnostics for audit purposes.",
  "- Repeated listing IDs remain preserved in the history file, and the analysis sample now keeps one preferred row per listing before trimming to the rent, sqft, and rent-per-sqft 1/99 bounds and dropping the 100 sqft mass.",
  "- Amenities now parse from both semicolon-delimited strings and JSON-like array strings before expanding to indicator columns in a separate file.",
  "- Rent variables now include a real 2024-dollar version using the Chicago CPI-U all-items series; the nominal rent columns are still preserved alongside them.",
  "- Repeated exact coordinates are descriptive only and should not be treated as automatic geocode failures; many are likely building-level listing clusters.",
  "- Sqft and rent-per-sqft outliers are still flagged explicitly in diagnostics, but the downstream analysis sample now trims rent, sqft, and rent-per-sqft tails and excludes the 100 sqft mass.",
  "- Aggregate monthly series should be treated as listing-history summaries rather than a fully observed listing-month panel.",
  "- The main limitation is trend validation: even in real 2024 dollars, Dwellsy's Chicago monthly rent series does not track Zillow/FRED cleanly enough to treat this source as causal-design-ready yet."
)

message("Writing outputs...")
final_output <- copy(final_dt[, c(
  "dwellsy_row_id",
  canonical_cols,
  "rent_price_cpi_chi_all_items",
  "rent_price_deflator_to_2024",
  "rent_price_real_2024",
  "source_filename"
), with = FALSE])
analysis_output <- copy(analysis_sample[, c(
  "dwellsy_row_id",
  canonical_cols,
  "rent_price_cpi_chi_all_items",
  "rent_price_deflator_to_2024",
  "rent_price_real_2024",
  "source_filename"
), with = FALSE])

message("Writing amenity indicator parquet...")
amenity_indicator_file <- file.path(output_dir, "dwellsy_amenity_indicators.parquet")
if (nrow(amenity_long) == 0) {
  write_parquet(as_tibble(final_dt[, .(dwellsy_row_id)]), amenity_indicator_file)
} else {
  amenity_row_ids_file <- file.path(temp_dir, "dwellsy_amenity_row_ids.parquet")
  amenity_long_file <- file.path(temp_dir, "dwellsy_amenity_long.parquet")
  write_parquet(as_tibble(final_dt[, .(dwellsy_row_id)]), amenity_row_ids_file)
  write_parquet(
    as_tibble(amenity_long[, .(dwellsy_row_id, amenity_column, indicator = 1L)]),
    amenity_long_file
  )

  amenity_projection <- vapply(amenity_dictionary$amenity_column, function(col_name) {
    col_literal <- as.character(DBI::dbQuoteString(con, col_name))
    col_identifier <- as.character(DBI::dbQuoteIdentifier(con, col_name))
    sprintf(
      "COALESCE(MAX(CASE WHEN l.amenity_column = %s THEN l.indicator END), 0) AS %s",
      col_literal,
      col_identifier
    )
  }, character(1))

  amenity_copy_sql <- sprintf(
    paste(
      "COPY (",
      "SELECT",
      "  %s",
      "FROM read_parquet(%s) r",
      "LEFT JOIN read_parquet(%s) l USING (dwellsy_row_id)",
      "GROUP BY r.dwellsy_row_id",
      "ORDER BY r.dwellsy_row_id",
      ")",
      "TO %s (FORMAT PARQUET)",
      sep = "\n"
    ),
    paste(c("r.dwellsy_row_id", amenity_projection), collapse = ",\n  "),
    as.character(DBI::dbQuoteString(con, amenity_row_ids_file)),
    as.character(DBI::dbQuoteString(con, amenity_long_file)),
    as.character(DBI::dbQuoteString(con, amenity_indicator_file))
  )
  DBI::dbExecute(con, amenity_copy_sql)
}

write_parquet(as_tibble(final_output), file.path(output_dir, "chicago_dwellsy_listing_history.parquet"))
write_parquet(as_tibble(analysis_output), file.path(output_dir, "chicago_dwellsy_listing_analysis_sample.parquet"))
write_csv(sample_ladder, file.path(output_dir, "dwellsy_processing_sample_ladder.csv"))
write_csv(quality_summary, file.path(output_dir, "dwellsy_quality_summary.csv"))
write_csv(duplicate_listing_issues, file.path(output_dir, "dwellsy_duplicate_listing_issues.csv"))
write_csv(coordinate_issues, file.path(output_dir, "dwellsy_coordinate_issues.csv"))
write_csv(id_integrity, file.path(output_dir, "dwellsy_id_integrity.csv"))
write_csv(repeat_listing_resolution, file.path(output_dir, "dwellsy_repeat_listing_resolution.csv"))
write_csv(value_integrity_issues, file.path(output_dir, "dwellsy_value_integrity_issues.csv"))
write_csv(amenity_dictionary, file.path(output_dir, "dwellsy_amenity_dictionary.csv"))
write_csv(monthly_series, file.path(output_dir, "dwellsy_monthly_series.csv"))
write_csv(monthly_series_variants, file.path(output_dir, "dwellsy_monthly_series_variants.csv"))
write_csv(benchmark_comparison, file.path(output_dir, "dwellsy_benchmark_comparison.csv"))
write_csv(benchmark_scorecard, file.path(output_dir, "dwellsy_benchmark_scorecard.csv"))
write_csv(benchmark_comparison_real, file.path(output_dir, "dwellsy_benchmark_comparison_real.csv"))
write_csv(benchmark_scorecard_real, file.path(output_dir, "dwellsy_benchmark_scorecard_real.csv"))
writeLines(quality_memo, file.path(output_dir, "dwellsy_quality_memo.md"))

message("process_dwellsy complete.")
