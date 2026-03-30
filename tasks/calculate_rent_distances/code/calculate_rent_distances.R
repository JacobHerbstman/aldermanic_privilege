# calculate_rent_distances.R
# Calculates unsigned distance to nearest ward boundary for rental listings.
# Optimized for large datasets using batching and spatial indexing.
# Score/sign merge happens in merge_event_study_scores.

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

# 1. SETUP & ARGUMENTS

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_rent_distances/code")
# sample <- TRUE

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sample)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 args: <sample>", call. = FALSE)
}
sample <- cli_args[1]
run_sample <- as.logical(sample)

load_cpi_deflator <- function(start_date,
                              end_date,
                              base_year = 2022L,
                              series_id = "CUURA207SA0L2") {
  fred_url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", series_id)
  message(sprintf("Fetching CPI series %s from FRED...", series_id))
  old_http_ua <- getOption("HTTPUserAgent")
  on.exit(options(HTTPUserAgent = old_http_ua), add = TRUE)
  # As of March 16, 2026, FRED's edge responds poorly to R's default UA
  # over libcurl HTTP/2, while the same URL works with a curl-style UA.
  options(HTTPUserAgent = paste0("curl/", curl::curl_version()$version))
  cpi_raw <- read_csv(fred_url, show_col_types = FALSE)
  if (!all(c("observation_date", series_id) %in% names(cpi_raw))) {
    stop(sprintf("FRED response missing expected columns for series %s.", series_id), call. = FALSE)
  }

  cpi <- cpi_raw %>%
    transmute(
      observation_date = as.Date(observation_date),
      cpi_value = suppressWarnings(as.numeric(.data[[series_id]]))
    ) %>%
    filter(!is.na(observation_date))

  start_month <- as.Date(format(start_date, "%Y-%m-01"))
  end_month <- as.Date(format(end_date, "%Y-%m-01"))
  month_grid <- tibble(observation_date = seq(start_month, end_month, by = "month"))

  cpi <- month_grid %>%
    left_join(cpi, by = "observation_date") %>%
    arrange(observation_date)

  n_missing_pre <- sum(is.na(cpi$cpi_value))
  if (n_missing_pre > 0) {
    idx_known <- which(!is.na(cpi$cpi_value))
    if (length(idx_known) < 2) {
      stop("Not enough non-missing CPI observations to interpolate.", call. = FALSE)
    }
    cpi_interp <- approx(
      x = idx_known,
      y = cpi$cpi_value[idx_known],
      xout = seq_len(nrow(cpi)),
      method = "linear",
      rule = 1
    )$y
    cpi$cpi_value <- if_else(is.na(cpi$cpi_value), cpi_interp, cpi$cpi_value)
  }

  if (anyNA(cpi$cpi_value)) {
    stop("CPI has unresolved endpoint gaps after interpolation.", call. = FALSE)
  }

  base_vals <- cpi %>%
    filter(format(observation_date, "%Y") == as.character(base_year)) %>%
    pull(cpi_value)

  if (length(base_vals) == 0 || !all(is.finite(base_vals))) {
    stop(sprintf("Unable to compute base CPI for year %d.", base_year), call. = FALSE)
  }

  base_cpi <- mean(base_vals)
  if (!is.finite(base_cpi) || base_cpi <= 0) {
    stop(sprintf("Computed invalid base CPI for year %d.", base_year), call. = FALSE)
  }

  message(sprintf(
    "CPI window %s to %s | base (%d avg) = %.3f | interpolated %d month(s)",
    format(start_month, "%Y-%m"),
    format(end_month, "%Y-%m"),
    base_year,
    base_cpi,
    n_missing_pre
  ))

  cpi %>%
    transmute(
      rent_year_month = format(observation_date, "%Y-%m"),
      rent_price_cpi_chi_ex_shelter = cpi_value,
      rent_price_deflator_to_2022 = base_cpi / cpi_value
    )
}

# Core CRS for distance calc (Illinois East ftUS)
crs_projected <- 3435

# -----------------------------------------------------------------------------
# 2. LOAD & PREP ANCILLARY DATA
# -----------------------------------------------------------------------------
message("Loading ancillary data...")

# Wards
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(crs_projected)
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)
canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

# Alderman Data
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

# -----------------------------------------------------------------------------
# 3. RENT PROCESSING FUNCTION
# -----------------------------------------------------------------------------

process_batch <- function(df_batch) {
  n_input <- nrow(df_batch)
  n_valid_coords <- sum(!is.na(df_batch$latitude) & !is.na(df_batch$longitude) & !is.na(df_batch$file_date))

  df_batch <- df_batch %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(file_date))

  if (nrow(df_batch) == 0) {
    return(list(
      data = NULL,
      diagnostics = tibble(
        n_input = n_input,
        n_valid_coords = n_valid_coords,
        n_with_ward_pair = 0L
      )
    ))
  }

  pts <- st_as_sf(df_batch, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected) %>%
    mutate(
      boundary_year = canonical_boundary_year_from_date(file_date),
      era = canonical_era_from_date(file_date, allow_pre_2003 = FALSE)
    )

  boundary_assignments <- assign_points_to_boundaries(
    points_sf = pts,
    era_values = pts$era,
    ward_maps = canonical_ward_maps,
    boundary_lines = canonical_boundaries,
    chunk_n = 5000L
  )

  out <- bind_cols(pts, boundary_assignments) %>%
    filter(!is.na(ward), !is.na(ward_pair_id))

  list(
    data = out,
    diagnostics = tibble(
      n_input = n_input,
      n_valid_coords = n_valid_coords,
      n_with_ward_pair = nrow(out)
    )
  )
}

# -----------------------------------------------------------------------------
# 5. EXECUTE (STREAMING)
# -----------------------------------------------------------------------------
input_file <- "../input/chicago_rent_panel.parquet"

# Open dataset
ds <- arrow::open_dataset(input_file)

years <- 2014:2025
results_list <- list()
batch_diag_list <- list()

if (run_sample) {
  message("RUNNING ON SAMPLE MODE (1% by year)")
} else {
  message("RUNNING FULL DATASET (Batch Mode)")
}

# Set up progress bar for BOTH modes
pb <- txtProgressBar(min = 0, max = length(years), style = 3)

for (i in seq_along(years)) {
  yr <- years[i]

  # Read chunk for the year
  df_chunk <- ds %>%
    filter(year(file_date) == yr) %>%
    select(id, rent_price, building_type, beds, baths, sqft, laundry, gym, year_built, available_date, file_date, latitude, longitude) %>%
    collect()

  # Apply sampling if needed
  if (run_sample) {
    df_chunk <- df_chunk %>% slice_sample(prop = 0.01)
  }

  # Process chunk
  if (nrow(df_chunk) > 0) {
    res <- process_batch(df_chunk)
    if (!is.null(res$data)) {
      results_list[[length(results_list) + 1]] <- res$data
    }
    batch_diag_list[[length(batch_diag_list) + 1]] <- mutate(res$diagnostics, year = yr)
  }

  gc()
  setTxtProgressBar(pb, i)
}
close(pb)

results_sf <- bind_rows(results_list)
batch_diagnostics <- bind_rows(batch_diag_list)

# -----------------------------------------------------------------------------
# 6. POST-PROCESS: ALDERMAN LOOKUPS (PRE-SCORES)
# -----------------------------------------------------------------------------
message("Attaching Alderman data (pre-scores output)...")

# We convert back to tibble for the merge
# Extract lat/lon before dropping geometry (transform back to WGS84 first)
final_df <- results_sf %>%
  st_transform(4326) %>% # Back to WGS84 for proper lat/lon
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  as_tibble()

# 6a. Attach Alderman (Own & Neighbor)
# We need to lookup who was alderman at [ward, file_date]
# Use the alderman panel.
# Optimized lookup: Join on Ward + Month-Year

final_df <- final_df %>%
  mutate(month_join = as.yearmon(file_date))

ald_lookup <- alderman_panel %>%
  select(ward, month, alderman)

# Join Own
final_df <- final_df %>%
  left_join(ald_lookup, by = c("ward" = "ward", "month_join" = "month")) %>%
  rename(alderman_own = alderman)

# Join Neighbor
final_df <- final_df %>%
  left_join(ald_lookup, by = c("neighbor_ward" = "ward", "month_join" = "month")) %>%
  rename(alderman_neighbor = alderman)

# 6b. Deflate rent prices to 2022 dollars
final_df <- final_df %>%
  mutate(rent_year_month = format(file_date, "%Y-%m"))

cpi_deflator <- load_cpi_deflator(
  start_date = min(final_df$file_date, na.rm = TRUE),
  end_date = max(final_df$file_date, na.rm = TRUE),
  base_year = 2022L
)

final_df <- final_df %>%
  left_join(cpi_deflator, by = "rent_year_month")

n_missing_deflator <- sum(!is.finite(final_df$rent_price_deflator_to_2022))
if (n_missing_deflator > 0) {
  stop(sprintf(
    "Missing/invalid rent CPI deflator for %d observations.",
    n_missing_deflator
  ), call. = FALSE)
}

final_df <- final_df %>%
  mutate(
    rent_price_nominal = rent_price,
    rent_price = rent_price_nominal * rent_price_deflator_to_2022
  )

# -----------------------------------------------------------------------------
# 7. CLEAN BUILDING TYPES
# -----------------------------------------------------------------------------
message("Cleaning building types...")

final_df <- final_df %>%
  mutate(
    building_type_clean = case_when(
      # Single Family
      str_detect(building_type, regex("SFR|Single[- ]?family|house", ignore_case = TRUE)) ~ "single_family",

      # Multi Family (APT, Apartment, Multi-family, duplex, triplex, etc.)
      str_detect(building_type, regex("APT|Apartment|Multi[- ]?family|duplex|triplex|fourplex", ignore_case = TRUE)) ~ "multi_family",

      # Condo (CON, Condo, Condominium)
      str_detect(building_type, regex("CON|Condo|Condominium", ignore_case = TRUE)) ~ "condo",

      # Townhouse (TH, Townhouse)
      str_detect(building_type, regex("TH|Townhouse", ignore_case = TRUE)) ~ "townhouse",

      # Other/Unknown (COMM, NA, unknown)
      TRUE ~ "other"
    )
  )


# -----------------------------------------------------------------------------
# 8. SAVE
# -----------------------------------------------------------------------------
# Dynamically determine output filename
suffix <- if (run_sample) "_sample" else "_full"
output_path <- sprintf("../output/rent_pre_scores%s.parquet", suffix)
diag_path <- sprintf("../output/rent_geometry_diagnostics%s.csv", suffix)

write_parquet(final_df, output_path)

rent_geometry_diagnostics <- bind_rows(
  tibble(
    scope = "overall",
    boundary_year = NA_integer_,
    metric = c(
      "n_rows_read",
      "n_valid_coords",
      "n_with_ward_pair"
    ),
    value = c(
      sum(batch_diagnostics$n_input),
      sum(batch_diagnostics$n_valid_coords),
      sum(batch_diagnostics$n_with_ward_pair)
    )
  ),
  batch_diagnostics %>%
    summarise(
      n_rows_read = sum(n_input),
      n_valid_coords = sum(n_valid_coords),
      n_with_ward_pair = sum(n_with_ward_pair),
      .by = year
    ) %>%
    pivot_longer(
      cols = c(n_rows_read, n_valid_coords, n_with_ward_pair),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(scope = "calendar_year", boundary_year = NA_integer_) %>%
    select(scope, boundary_year, year, metric, value),
  final_df %>%
    summarise(
      n_obs = n(),
      mean_dist_ft = mean(dist_ft, na.rm = TRUE),
      median_dist_ft = median(dist_ft, na.rm = TRUE),
      .by = boundary_year
    ) %>%
    pivot_longer(
      cols = c(n_obs, mean_dist_ft, median_dist_ft),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(scope = "boundary_year", year = NA_integer_) %>%
    select(scope, boundary_year, year, metric, value)
)

write_csv(rent_geometry_diagnostics, diag_path)

message("Done! Saved ", nrow(final_df), " rows to ", output_path)
message("Saved diagnostics to ", diag_path)
