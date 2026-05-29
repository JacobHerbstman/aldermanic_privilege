# calculate_sale_distances.R
# Calculates unsigned distance to nearest ward boundary for home sales.
# Includes all four ward map eras (1998, 2003, 2015, 2024).
# Score/sign merge happens in merge_event_study_scores.

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

# 1. SETUP & ARGUMENTS

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("tasks/calculate_sale_distances/code")
# sample <- FALSE

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(sample)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <sample>", call. = FALSE)
}
sample <- cli_args[1]
run_sample <- as.logical(sample)

load_cpi_deflator <- function(start_date,
                              end_date,
                              base_year = 2022L,
                              cpi_csv = "../input/fred_cpi_cuura207sa0.csv",
                              series_id = "CUURA207SA0") {
    message(sprintf("Reading CPI series %s from %s...", series_id, cpi_csv))
    cpi_raw <- read_csv(cpi_csv, col_types = cols(.default = "c"), show_col_types = FALSE)
    if (!all(c("observation_date", series_id) %in% names(cpi_raw))) {
        stop(sprintf("CPI input missing expected columns for series %s.", series_id), call. = FALSE)
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
            sale_price_cpi_chi_all_items = cpi_value,
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

sales_raw <- fread("../input/parcel_sales.csv")
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
        sale_date = coalesce(
            as.Date(as.character(sale_date), format = "%B %d, %Y"),
            as.Date(substr(as.character(sale_date), 1, 10), format = "%Y-%m-%d")
        )
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

# Create points in 4326 from geocoded lon/lat, then immediately project to 3435
# for all Chicago spatial work.
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

message("Running sales geometry contract audit inside 500ft...")
contract_sf <- results_sf[
    is.finite(results_sf$dist_m) &
        results_sf$dist_m <= 500 * 0.3048,
]
if (nrow(contract_sf) > 0) {
    ward_hit_details <- bind_rows(lapply(sort(unique(as.character(contract_sf$era))), function(era_i) {
        idx <- which(as.character(contract_sf$era) == era_i)
        ward_sf <- canonical_ward_maps[[era_i]]
        hits <- st_within(contract_sf[idx, ], ward_sf)
        ward_vals <- st_drop_geometry(ward_sf)$ward
        tibble(
            pin = contract_sf$pin[idx],
            sale_date = contract_sf$sale_date_for_price[idx],
            era = era_i,
            ward = contract_sf$ward[idx],
            neighbor_ward = contract_sf$neighbor_ward[idx],
            ward_pair_id = contract_sf$ward_pair_id[idx],
            dist_m = contract_sf$dist_m[idx],
            n_ward_hits = lengths(hits),
            hit_wards = vapply(
                hits,
                function(v) paste(sort(ward_vals[v]), collapse = ";"),
                character(1)
            ),
            hit_pair_id = vapply(
                hits,
                function(v) paste(sort(ward_vals[v]), collapse = "_"),
                character(1)
            )
        )
    })) %>%
        mutate(
            flag_multiple_hit_pair_consistent = n_ward_hits > 1L &
                normalize_pair_dash(hit_pair_id) == normalize_pair_dash(ward_pair_id),
            flag_multiple_hit_pair_inconsistent = n_ward_hits > 1L &
                normalize_pair_dash(hit_pair_id) != normalize_pair_dash(ward_pair_id)
        )

    ward_hit_audit <- ward_hit_details %>%
        summarise(
            n_rows = n(),
            n_zero_ward_hits = sum(n_ward_hits == 0L, na.rm = TRUE),
            n_multiple_ward_hits = sum(n_ward_hits > 1L, na.rm = TRUE),
            n_multiple_hit_pair_consistent = sum(flag_multiple_hit_pair_consistent, na.rm = TRUE),
            n_multiple_hit_pair_inconsistent = sum(flag_multiple_hit_pair_inconsistent, na.rm = TRUE),
            max_ward_hits = max(n_ward_hits, na.rm = TRUE),
            .by = era
        ) %>%
        arrange(era)

    ward_hit_detail <- ward_hit_details %>%
        filter(n_ward_hits != 1L) %>%
        arrange(era, sale_date, pin)

    recomputed_assignment <- assign_points_to_boundaries(
        points_sf = contract_sf,
        era_values = contract_sf$era,
        ward_maps = canonical_ward_maps,
        boundary_lines = canonical_boundaries,
        chunk_n = 5000L
    ) %>%
        transmute(
            recomputed_ward = ward,
            recomputed_neighbor_ward = neighbor_ward,
            recomputed_ward_pair_id = ward_pair_id,
            recomputed_dist_m = dist_m
        )

    geometry_contract_detail <- bind_cols(st_drop_geometry(contract_sf), recomputed_assignment) %>%
        transmute(
            pin,
            sale_date = sale_date_for_price,
            era,
            ward,
            recomputed_ward,
            neighbor_ward,
            recomputed_neighbor_ward,
            ward_pair_id,
            recomputed_ward_pair_id,
            expected_ward_pair_id = normalize_pair_id(ward, neighbor_ward, sep = "_"),
            dist_m,
            recomputed_dist_m,
            abs_dist_diff_m = abs(dist_m - recomputed_dist_m),
            flag_ward_mismatch = ward != recomputed_ward,
            flag_neighbor_mismatch = neighbor_ward != recomputed_neighbor_ward,
            flag_pair_mismatch = normalize_pair_dash(ward_pair_id) != normalize_pair_dash(recomputed_ward_pair_id),
            flag_pair_endpoint_mismatch = normalize_pair_dash(ward_pair_id) != normalize_pair_dash(expected_ward_pair_id),
            flag_dist_mismatch = !is.finite(abs_dist_diff_m) | abs_dist_diff_m > 0.05
        )

    geometry_contract_audit <- geometry_contract_detail %>%
        summarise(
            n_rows = n(),
            n_ward_mismatch = sum(flag_ward_mismatch, na.rm = TRUE),
            n_neighbor_mismatch = sum(flag_neighbor_mismatch, na.rm = TRUE),
            n_pair_mismatch = sum(flag_pair_mismatch, na.rm = TRUE),
            n_pair_endpoint_mismatch = sum(flag_pair_endpoint_mismatch, na.rm = TRUE),
            n_dist_mismatch = sum(flag_dist_mismatch, na.rm = TRUE),
            max_abs_dist_diff_m = max(abs_dist_diff_m, na.rm = TRUE),
            .by = era
        ) %>%
        arrange(era)
} else {
    ward_hit_audit <- tibble(
        era = character(),
        n_rows = integer(),
        n_zero_ward_hits = integer(),
        n_multiple_ward_hits = integer(),
        n_multiple_hit_pair_consistent = integer(),
        n_multiple_hit_pair_inconsistent = integer(),
        max_ward_hits = integer()
    )
    ward_hit_detail <- tibble(
        pin = character(),
        sale_date = as.Date(character()),
        era = character(),
        ward = integer(),
        neighbor_ward = integer(),
        ward_pair_id = character(),
        dist_m = numeric(),
        n_ward_hits = integer(),
        hit_wards = character(),
        hit_pair_id = character(),
        flag_multiple_hit_pair_consistent = logical(),
        flag_multiple_hit_pair_inconsistent = logical()
    )
    geometry_contract_audit <- tibble(
        era = character(),
        n_rows = integer(),
        n_ward_mismatch = integer(),
        n_neighbor_mismatch = integer(),
        n_pair_mismatch = integer(),
        n_pair_endpoint_mismatch = integer(),
        n_dist_mismatch = integer(),
        max_abs_dist_diff_m = numeric()
    )
}

if (sum(ward_hit_audit$n_zero_ward_hits, na.rm = TRUE) > 0 ||
    sum(ward_hit_audit$n_multiple_hit_pair_inconsistent, na.rm = TRUE) > 0) {
    stop("Sales ward-hit multiplicity audit failed inside 500ft.", call. = FALSE)
}
if (sum(geometry_contract_audit$n_ward_mismatch, na.rm = TRUE) > 0 ||
    sum(geometry_contract_audit$n_neighbor_mismatch, na.rm = TRUE) > 0 ||
    sum(geometry_contract_audit$n_pair_mismatch, na.rm = TRUE) > 0 ||
    sum(geometry_contract_audit$n_pair_endpoint_mismatch, na.rm = TRUE) > 0 ||
    sum(geometry_contract_audit$n_dist_mismatch, na.rm = TRUE) > 0) {
    stop("Sales geometry contract audit failed inside 500ft.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 7. POST-PROCESS: ALDERMAN LOOKUPS (PRE-SCORES)
# -----------------------------------------------------------------------------
message("Attaching Alderman data (pre-scores output)...")

# After all spatial work in 3435, extract lat/lon only for the flat CSV output.
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
        sale_price_cpi_chi_all_items,
        sale_price_deflator_to_2022,
        class,
        # Location
        latitude, longitude, ward, neighbor_ward, ward_pair_id,
        # Distance (unsigned)
        dist_m,
        # Alderman info
        alderman_own, alderman_neighbor
    )

# Output path
suffix <- if (run_sample) "_sample" else ""
output_path <- sprintf("../output/sales_pre_scores%s.csv", suffix)

write_csv(final_output, output_path)

message(sprintf("Done! Saved %s rows to %s", format(nrow(final_output), big.mark = ","), output_path))
