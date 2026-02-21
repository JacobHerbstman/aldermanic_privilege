# calculate_sale_distances.R
# Calculates unsigned distance to nearest ward boundary for home sales.
# Includes all four ward map eras (1998, 2003, 2015, 2024).
# Score/sign merge happens in merge_event_study_scores.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_sale_distances/code")
source("../../setup_environment/code/packages.R")

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

# Core CRS for distance calc (Illinois East ftUS)
crs_projected <- 3435

# Map Change Dates (Crucial for correct assignment)
# Map 1 (1998-2003): Ends May 2003
# Map 2 (2003-2015): Ends May 2015
# Map 3 (2015-2023): Ends May 2023
# Map 4 (2023-Present): Starts May 2023
date_switch_2003 <- as.Date("2003-05-01")
date_switch_2015 <- as.Date("2015-05-18")
date_switch_2023 <- as.Date("2023-05-15")

# -----------------------------------------------------------------------------
# 2. LOAD & PREP ANCILLARY DATA
# -----------------------------------------------------------------------------
message("Loading ancillary data...")

# Wards
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    st_transform(crs_projected)

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
        sale_price = as.numeric(gsub("[$,]", "", sale_price)),
        year = as.numeric(year),
        pin = as.character(pin),
        # Parse sale date
        sale_date = as.Date(sale_date, format = "%B %d, %Y")
    ) %>%
    # Filter valid prices and years
    filter(!is.na(sale_price), sale_price > 10000, !is.na(year)) %>%
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
    filter(num_parcels_sale == 1)

message(sprintf("Filtered to %s market sales", format(nrow(sales), big.mark = ",")))

# Winsorize prices at 1st and 99th percentiles to handle outliers
p01 <- quantile(sales$sale_price, 0.01, na.rm = TRUE)
p99 <- quantile(sales$sale_price, 0.99, na.rm = TRUE)
message(sprintf("Winsorizing prices: p1 = $%s, p99 = $%s", 
                format(p01, big.mark = ","), format(p99, big.mark = ",")))

sales <- sales %>%
    mutate(sale_price = pmin(pmax(sale_price, p01), p99))

message(sprintf("After winsorizing: %s sales", format(nrow(sales), big.mark = ",")))

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

# Clean up memory
rm(sales_raw, sales, sales_geo, parcels)
gc()

# -----------------------------------------------------------------------------
# 5. BUILD BOUNDARY LINES FOR EACH MAP ERA
# -----------------------------------------------------------------------------

get_boundaries <- function(ward_sf) {
    # Buffer 0 to fix topology
    ward_sf <- st_buffer(ward_sf, 0)

    # Find touching pairs
    adj <- st_touches(ward_sf)

    edges <- imap_dfr(adj, function(nb, i) {
        if (length(nb) == 0) {
            return(NULL)
        }

        # Iterate through neighbors (only j > i to avoid duplicates)
        nb_valid <- nb[nb > i]
        if (length(nb_valid) == 0) {
            return(NULL)
        }

        map_dfr(nb_valid, function(j) {
            geom_i <- st_geometry(ward_sf[i, ])
            geom_j <- st_geometry(ward_sf[j, ])

            # Intersection is the shared border
            shared <- st_intersection(geom_i, geom_j)

            # Keep only lines (ignore points if corners touch)
            if (st_dimension(shared) != 1) {
                return(NULL)
            }

            tibble(
                ward_a = ward_sf$ward[i],
                ward_b = ward_sf$ward[j],
                geometry = shared
            )
        })
    }) %>%
        st_as_sf(crs = st_crs(ward_sf))

    return(edges)
}

message("Preparing ward maps for all four eras...")

# Map 1: 1998 (for sales before May 2003)
# Find closest available year to 1998 in the panel
available_years <- unique(ward_panel$year)
if (1998 %in% available_years) {
    map_1998_poly <- ward_panel %>% filter(year == 1998)
} else {
    # Use earliest available
    earliest_year <- min(available_years)
    message(sprintf("1998 not in ward panel, using %d instead", earliest_year))
    map_1998_poly <- ward_panel %>% filter(year == earliest_year)
}
map_1998_lines <- get_boundaries(map_1998_poly)

# Map 2: 2003-2014 era (use 2003 or closest)
if (2003 %in% available_years) {
    map_2003_poly <- ward_panel %>% filter(year == 2003)
} else if (2004 %in% available_years) {
    map_2003_poly <- ward_panel %>% filter(year == 2004)
} else {
    # Find closest year after 2003
    map_2003_poly <- ward_panel %>% filter(year == min(available_years[available_years >= 2003]))
}
map_2003_lines <- get_boundaries(map_2003_poly)

# Map 3: 2015-2023 era
if (2015 %in% available_years) {
    map_2015_poly <- ward_panel %>% filter(year == 2015)
} else if (2016 %in% available_years) {
    map_2015_poly <- ward_panel %>% filter(year == 2016)
} else {
    map_2015_poly <- ward_panel %>% filter(year == min(available_years[available_years >= 2015]))
}
map_2015_lines <- get_boundaries(map_2015_poly)

# Map 4: 2024+ era
if (2024 %in% available_years) {
    map_2024_poly <- ward_panel %>% filter(year == 2024)
} else {
    map_2024_poly <- ward_panel %>% filter(year == max(available_years))
}
map_2024_lines <- get_boundaries(map_2024_poly)

message(sprintf(
    "Generated boundaries: 1998 (%d), 2003 (%d), 2015 (%d), 2024 (%d)",
    nrow(map_1998_lines), nrow(map_2003_lines),
    nrow(map_2015_lines), nrow(map_2024_lines)
))

# -----------------------------------------------------------------------------
# 6. PROCESS SALES: ASSIGN WARD AND CALCULATE DISTANCE
# -----------------------------------------------------------------------------

calc_dist <- function(points, polys, lines) {
    if (nrow(points) == 0) {
        return(NULL)
    }

    n_in <- nrow(points)

    # A. Assign Ward (Point in Polygon)
    joined <- st_join(points, polys %>% select(ward), join = st_within)

    # Drop points outside Chicago wards
    n_outside <- sum(is.na(joined$ward))
    if (n_outside > 0) {
        message(sprintf("  Dropped %d points outside ward boundaries (%.1f%%)",
                        n_outside, 100 * n_outside / n_in))
    }
    joined <- joined %>% filter(!is.na(ward))
    if (nrow(joined) == 0) {
        return(NULL)
    }

    # B. Distance to Nearest Border touching assigned ward
    joined$dist_ft <- NA_real_
    joined$ward_pair_a <- NA_integer_
    joined$ward_pair_b <- NA_integer_

    ward_vals <- sort(unique(joined$ward))
    for (w in ward_vals) {
        idx <- which(joined$ward == w)
        edges_w <- lines[lines$ward_a == w | lines$ward_b == w, ]

        if (length(idx) == 0 || nrow(edges_w) == 0) {
            next
        }

        nearest_idx <- st_nearest_feature(joined[idx, ], edges_w)
        nearest_geoms <- edges_w[nearest_idx, ]
        dists <- st_distance(joined[idx, ], nearest_geoms, by_element = TRUE)

        joined$dist_ft[idx] <- as.numeric(dists)
        joined$ward_pair_a[idx] <- as.integer(nearest_geoms$ward_a)
        joined$ward_pair_b[idx] <- as.integer(nearest_geoms$ward_b)
    }

    n_no_border <- sum(is.na(joined$ward_pair_a) | is.na(joined$ward_pair_b))
    if (n_no_border > 0) {
        message(sprintf("  Dropped %d points with no nearest border (%.1f%% of ward-assigned)",
                        n_no_border, 100 * n_no_border / nrow(joined)))
    }
    joined <- joined %>% filter(!is.na(ward_pair_a), !is.na(ward_pair_b))
    if (nrow(joined) == 0) {
        return(NULL)
    }

    # Identify neighbor ward
    joined <- joined %>%
        mutate(
            neighbor_ward = if_else(ward == ward_pair_a, ward_pair_b, ward_pair_a, missing = NA_integer_),
            ward_pair_id = if_else(
                !is.na(neighbor_ward),
                paste(pmin(ward, neighbor_ward), pmax(ward, neighbor_ward), sep = "-"),
                NA_character_
            )
        ) %>%
        select(-ward_pair_a, -ward_pair_b)

    return(joined)
}

message("Processing sales by era...")

# Split by era based on sale_date
# Handle NA sale_date by using year to create approximate date
sales_sf <- sales_sf %>%
    mutate(
        sale_date_use = if_else(is.na(sale_date),
            as.Date(paste0(year, "-06-15")),
            sale_date
        )
    )

pts_era1 <- sales_sf %>% filter(sale_date_use < date_switch_2003)
pts_era2 <- sales_sf %>% filter(sale_date_use >= date_switch_2003, sale_date_use < date_switch_2015)
pts_era3 <- sales_sf %>% filter(sale_date_use >= date_switch_2015, sale_date_use < date_switch_2023)
pts_era4 <- sales_sf %>% filter(sale_date_use >= date_switch_2023)

message(sprintf(
    "Era splits: Pre-2003: %d, 2003-2015: %d, 2015-2023: %d, 2023+: %d",
    nrow(pts_era1), nrow(pts_era2), nrow(pts_era3), nrow(pts_era4)
))

# Process each era
message("Processing Era 1 (pre-2003)...")
res1 <- calc_dist(pts_era1, map_1998_poly, map_1998_lines)

message("Processing Era 2 (2003-2015)...")
res2 <- calc_dist(pts_era2, map_2003_poly, map_2003_lines)

message("Processing Era 3 (2015-2023)...")
res3 <- calc_dist(pts_era3, map_2015_poly, map_2015_lines)

message("Processing Era 4 (2023+)...")
res4 <- calc_dist(pts_era4, map_2024_poly, map_2024_lines)

# Combine results
results_sf <- bind_rows(res1, res2, res3, res4)
message(sprintf("Combined results: %s sales with ward assignments", format(nrow(results_sf), big.mark = ",")))

# Clean up
rm(pts_era1, pts_era2, pts_era3, pts_era4, res1, res2, res3, res4, sales_sf)
gc()

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
        # Sale info
        sale_price, class,
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

write_csv(final_output, output_path)

message(sprintf("Done! Saved %s rows to %s", format(nrow(final_output), big.mark = ","), output_path))

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