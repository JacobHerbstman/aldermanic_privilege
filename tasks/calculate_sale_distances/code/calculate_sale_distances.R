# calculate_sale_distances.R
# Calculates unsigned distance to nearest ward boundary for home sales.
# Includes all four ward map eras (1998, 2003, 2015, 2024).
# Score/sign merge happens in merge_event_study_scores.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_sale_distances/code")
source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

# -----------------------------------------------------------------------------
# 1. SETUP & ARGUMENTS
# -----------------------------------------------------------------------------
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_sale_distances/code")
# sample <- "FALSE"
# Rscript calculate_sale_distances.R "FALSE"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 1) {
  sample <- cli_args[1]
} else {
  if (!exists("sample")) {
    stop("FATAL: Script requires 1 args: <sample>", call. = FALSE)
  }
}
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
            sale_year_month = format(observation_date, "%Y-%m"),
            sale_price_cpi_chi_ex_shelter = cpi_value,
            sale_price_deflator_to_2022 = base_cpi / cpi_value
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
# 3. LOAD AND CLEAN SALES DATA
# -----------------------------------------------------------------------------
message("Loading sales data...")

sales_raw <- fread("../input/Assessor_-_Parcel_Sales_20251123.csv")
message(sprintf("Loaded %s sales records", format(nrow(sales_raw), big.mark = ",")))

# Filter to market transactions
message("Filtering to market transactions...")

sales <- sales_raw %>%
    # Filter for residential classes (single-family + small multi-family, no condos)
    # 202: one-story <1000 sqft
    # 203: one-story 1000-1800 sqft
    # 204: one-story 1800+ sqft
    # 205-208: two-story single-family (various sizes)
    # 209: split-level
    # 210: old-style row house
    # 211: 2-6 unit apartment buildings
    # Excluding: 212 (mixed-use), 299/399 (condos)
    filter(class %in% c(202, 203, 204, 205, 206, 207, 208, 209, 210, 211)) %>%
    # Clean Price
    mutate(
        sale_price_nominal = as.numeric(gsub("[$,]", "", sale_price)),
        year = as.numeric(year),
        pin = as.character(pin),
        # Parse sale date
        sale_date = as.Date(sale_date, format = "%B %d, %Y")
    ) %>%
    # Filter valid prices and years
    filter(!is.na(sale_price_nominal), sale_price_nominal > 10000, !is.na(year)) %>%
    # Filter to 1999-2025
    filter(year >= 1999, year <= 2025) %>%
    # Market transactions: Warranty or Trustee deeds
    filter(sale_deed_type %in% c("Warranty", "Trustee")) %>%
    # Exclude land-only sales
    filter(sale_type != "LAND") %>%
    # Exclude transfers where seller/buyer names are problematic
    filter(!sale_seller_name %in% c("", "-", "UNKNOWN", "..")) %>%
    filter(sale_seller_name != sale_buyer_name) %>%
    # Single-parcel sales only
    filter(num_parcels_sale == 1) %>%
    mutate(
        sale_date_for_price = if_else(
            !is.na(sale_date),
            sale_date,
            as.Date(paste0(as.integer(year), "-06-15"))
        ),
        sale_year_month = format(sale_date_for_price, "%Y-%m")
    )

message(sprintf("Filtered to %s market sales", format(nrow(sales), big.mark = ",")))

# Deflate to 2022 dollars (monthly CPI) before winsorization
cpi_deflator <- load_cpi_deflator(
    start_date = min(sales$sale_date_for_price, na.rm = TRUE),
    end_date = max(sales$sale_date_for_price, na.rm = TRUE),
    base_year = 2022L
)

sales <- sales %>%
    left_join(cpi_deflator, by = "sale_year_month")

n_missing_deflator <- sum(!is.finite(sales$sale_price_deflator_to_2022))
if (n_missing_deflator > 0) {
    stop(sprintf(
        "Missing/invalid sale CPI deflator for %d observations.",
        n_missing_deflator
    ), call. = FALSE)
}

sales <- sales %>%
    mutate(
        sale_price_real_2022_raw = sale_price_nominal * sale_price_deflator_to_2022
    )

# Winsorize real prices at 1st and 99th percentiles to handle outliers
p01 <- quantile(sales$sale_price_real_2022_raw, 0.01, na.rm = TRUE)
p99 <- quantile(sales$sale_price_real_2022_raw, 0.99, na.rm = TRUE)
message(sprintf("Winsorizing real 2022 prices: p1 = $%s, p99 = $%s",
                format(p01, big.mark = ","), format(p99, big.mark = ",")))

sales <- sales %>%
    mutate(sale_price = pmin(pmax(sale_price_real_2022_raw, p01), p99))

message(sprintf("After real-price winsorizing: %s sales", format(nrow(sales), big.mark = ",")))

# Apply sampling if needed
if (run_sample) {
    message("SAMPLING: Taking 5% of data for testing")
    sales <- sales %>% slice_sample(prop = 0.05)
    message(sprintf("Sampled to %s sales", format(nrow(sales), big.mark = ",")))
}

# -----------------------------------------------------------------------------
# 4. GEOCODE SALES
# -----------------------------------------------------------------------------
message("Loading parcel universe for geocoding...")

parcels <- fread("../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv",
    select = c("pin", "pin10", "latitude", "longitude")
)
message(sprintf("Loaded %s parcel records", format(nrow(parcels), big.mark = ",")))

# Ensure parcel PINs are character
parcels[, `:=`(pin = as.character(pin), pin10 = as.character(pin10))]

message("Geocoding sales...")

# Join on full PIN first
sales_geo <- sales %>%
    left_join(parcels, by = "pin")

# Identify missing coordinates
missing_coords <- sum(is.na(sales_geo$latitude))
message(sprintf("Initial match missing coordinates: %s", format(missing_coords, big.mark = ",")))

# If missing, try matching on PIN10 (first 10 digits of sales PIN)
if (missing_coords > 0) {
    # Create PIN10 for sales
    sales_geo <- sales_geo %>%
        mutate(pin10_sales = substr(pin, 1, 10))

    # Separate missing and found
    sales_found <- sales_geo %>% filter(!is.na(latitude))
    sales_missing <- sales_geo %>%
        filter(is.na(latitude)) %>%
        select(-latitude, -longitude, -pin10)

    # Join missing on PIN10
    parcels_pin10 <- parcels %>%
        select(pin10, latitude, longitude) %>%
        distinct(pin10, .keep_all = TRUE)

    sales_missing_fixed <- sales_missing %>%
        left_join(parcels_pin10, by = c("pin10_sales" = "pin10"))

    # Recombine
    sales_geo <- bind_rows(sales_found, sales_missing_fixed)

    remaining_missing <- sum(is.na(sales_geo$latitude))
    message(sprintf("After PIN10 fallback, still missing: %s", format(remaining_missing, big.mark = ",")))
}

# Filter out any remaining missing coordinates and convert to SF
sales_sf <- sales_geo %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected)

message(sprintf("Final geolocated sales: %s", format(nrow(sales_sf), big.mark = ",")))

n_sales_raw <- nrow(sales_raw)
n_sales_market_filtered <- nrow(sales)
n_geolocated_sales <- nrow(sales_sf)

# Clean up memory
rm(sales_raw, sales, sales_geo, parcels)
gc()

# -----------------------------------------------------------------------------
# 5. CANONICAL WARD ASSIGNMENT AND DISTANCE
# -----------------------------------------------------------------------------

message("Assigning canonical ward pairs and distances for sales...")

sales_sf <- sales_sf %>%
    mutate(
        sale_date_use = sale_date_for_price,
        boundary_year = canonical_boundary_year_from_date(sale_date_use),
        era = canonical_era_from_boundary_year(boundary_year)
    )

boundary_assignments <- assign_points_to_boundaries(
    points_sf = sales_sf,
    era_values = sales_sf$era,
    ward_maps = canonical_ward_maps,
    boundary_lines = canonical_boundaries,
    chunk_n = 5000L
)

results_sf <- bind_cols(sales_sf, boundary_assignments) %>%
    filter(!is.na(ward), !is.na(ward_pair_id))

message(sprintf(
    "Canonical assignment coverage: %s of %s geolocated sales have ward-pair assignment",
    format(nrow(results_sf), big.mark = ","),
    format(nrow(sales_sf), big.mark = ",")
))

# -----------------------------------------------------------------------------
# 7. POST-PROCESS: ALDERMAN LOOKUPS (PRE-SCORES)
# -----------------------------------------------------------------------------
message("Attaching Alderman data (pre-scores output)...")

# Extract lat/lon before dropping geometry
final_df <- results_sf %>%
    st_transform(4326) %>%
    mutate(
        longitude = st_coordinates(.)[, 1],
        latitude = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry() %>%
    as_tibble()

# Attach Alderman (Own & Neighbor)
final_df <- final_df %>%
    mutate(month_join = as.yearmon(sale_date_use))

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

message(sprintf("Pre-score sales rows: %s", format(nrow(final_df), big.mark = ",")))

# -----------------------------------------------------------------------------
# 8. SELECT FINAL COLUMNS AND SAVE
# -----------------------------------------------------------------------------
message("Preparing final output...")

final_output <- final_df %>%
    select(
        # Identifiers
        pin, year,
        sale_date = sale_date_use,
        boundary_year,
        # Sale info
        sale_price,
        sale_price_nominal,
        sale_price_real_2022_raw,
        sale_price_cpi_chi_ex_shelter,
        sale_price_deflator_to_2022,
        class,
        # Location
        latitude, longitude, ward, neighbor_ward, ward_pair_id,
        # Distance (unsigned)
        dist_ft,
        # Alderman info
        alderman_own, alderman_neighbor
    )

# Output path
suffix <- if (run_sample) "_sample" else ""
output_path <- sprintf("../output/sales_pre_scores%s.csv", suffix)
diag_path <- sprintf("../output/sales_geometry_diagnostics%s.csv", suffix)

write_csv(final_output, output_path)

sales_geometry_diagnostics <- bind_rows(
    tibble(
        scope = "overall",
        boundary_year = NA_integer_,
        metric = c(
            "n_sales_raw",
            "n_sales_market_filtered",
            "n_missing_coords_after_pin",
            "n_missing_coords_after_pin10",
            "n_geolocated_sales",
            "n_with_ward_pair"
        ),
        value = c(
            n_sales_raw,
            n_sales_market_filtered,
            missing_coords,
            if (exists("remaining_missing")) remaining_missing else missing_coords,
            n_geolocated_sales,
            nrow(final_output)
        )
    ),
    final_output %>%
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
        mutate(scope = "boundary_year") %>%
        select(scope, boundary_year, metric, value)
)

write_csv(sales_geometry_diagnostics, diag_path)

message(sprintf("Done! Saved %s rows to %s", format(nrow(final_output), big.mark = ","), output_path))
message(sprintf("Saved diagnostics to %s", diag_path))

# Summary stats
summary_stats <- final_output %>%
    group_by(year) %>%
    summarise(
        n_sales = n(),
        mean_price = mean(sale_price, na.rm = TRUE),
        mean_dist_ft = mean(dist_ft, na.rm = TRUE),
        .groups = "drop"
    )

message("\nSummary by year:")
print(summary_stats)
