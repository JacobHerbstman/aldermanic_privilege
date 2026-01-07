# create_listing_panel.R
# Creates listing-level panel for disaggregate event study analysis of rental prices
# Output: rental_listing_panel.parquet - listing-level panel ready for event study estimation
#
# Key difference from create_event_study_data.R:
# - Keeps individual listings instead of aggregating to block-year
# - Enables hedonic controls (beds, baths, gym, etc.)
# - Uses 500ft distance restriction (tighter than block-level version)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_rental_data_disaggregate/code")
source("../../setup_environment/code/packages.R")

# Disable s2 spherical geometry to avoid validation errors with census block geometries
sf_use_s2(FALSE)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading rental data with ward distances...")
rentals <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
    filter(!is.na(rent_price), rent_price > 0) %>%
    mutate(
        file_date = as.Date(file_date),
        year = year(file_date),
        month = month(file_date),
        quarter = ceiling(month / 3),
        year_month = paste(year, sprintf("%02d", month), sep = "-"),
        year_quarter = paste(year, sprintf("Q%d", quarter), sep = "-")
    )
message(sprintf("Loaded %s rental listings", format(nrow(rentals), big.mark = ",")))

message("Loading census blocks...")
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id)) %>%
    # Deduplicate - raw data has duplicate block_ids
    distinct(block_id, .keep_all = TRUE)
message(sprintf("Loaded %s census blocks", format(nrow(census_blocks), big.mark = ",")))

message("Loading treatment panel...")
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id))

# =============================================================================
# 2. ASSIGN RENTALS TO CENSUS BLOCKS
# =============================================================================
message("Assigning rentals to census blocks via spatial join...")

# Transform to projected CRS for spatial operations
crs_projected <- 3435 # Illinois East State Plane (feet)

rentals_sf <- rentals %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected)

census_blocks_proj <- census_blocks %>%
    st_transform(crs_projected)

# Spatial join: assign each rental to a census block
rentals_with_blocks <- st_join(rentals_sf, census_blocks_proj %>% select(block_id), join = st_within) %>%
    st_drop_geometry() %>%
    filter(!is.na(block_id))

message(sprintf("Rentals assigned to blocks: %s", format(nrow(rentals_with_blocks), big.mark = ",")))

# Clean up spatial objects to free memory
rm(rentals_sf, census_blocks_proj, census_blocks)
gc()

# =============================================================================
# 3. PREPARE TREATMENT DATA BY COHORT
# =============================================================================
message("Preparing treatment data...")

# Get treatment info for 2015 cohort
treatment_2015 <- treatment_panel %>%
    filter(cohort == "2015") %>%
    select(
        block_id,
        ward_origin_2015 = ward_origin,
        ward_dest_2015 = ward_dest,
        switched_2015 = switched,
        strictness_origin_2015 = strictness_origin,
        strictness_dest_2015 = strictness_dest,
        strictness_change_2015 = strictness_change,
        switch_type_2015 = switch_type,
        valid_2015 = valid
    )

# Get treatment info for 2023 cohort
treatment_2023 <- treatment_panel %>%
    filter(cohort == "2023") %>%
    select(
        block_id,
        ward_origin_2023 = ward_origin,
        ward_dest_2023 = ward_dest,
        switched_2023 = switched,
        strictness_origin_2023 = strictness_origin,
        strictness_dest_2023 = strictness_dest,
        strictness_change_2023 = strictness_change,
        switch_type_2023 = switch_type,
        valid_2023 = valid
    )

n_switching_blocks_2015 <- sum(treatment_2015$switched_2015, na.rm = TRUE)
n_switching_blocks_2023 <- sum(treatment_2023$switched_2023, na.rm = TRUE)
message(sprintf("Blocks that switched in 2015: %d", n_switching_blocks_2015))
message(sprintf("Blocks that switched in 2023: %d", n_switching_blocks_2023))

# =============================================================================
# 4. PREPARE HEDONIC CONTROLS
# =============================================================================
message("Preparing hedonic controls...")

# Available variables from calculate_rent_distances:
# beds, baths, sqft, gym, laundry, building_type_clean

rentals_with_controls <- rentals_with_blocks %>%
    mutate(
        # Sqft: log transform with missing indicator
        sqft_missing = is.na(sqft) | sqft <= 0,
        sqft_clean = if_else(sqft_missing, median(sqft[!sqft_missing], na.rm = TRUE), as.numeric(sqft)),
        log_sqft = log(sqft_clean),

        # Beds: cap at 4+ and create factor
        beds_clean = pmin(coalesce(beds, 0L), 4L),
        beds_factor = factor(beds_clean, levels = 0:4, labels = c("0", "1", "2", "3", "4+")),
        beds_missing = is.na(beds),

        # Baths: cap at 3+ and create factor
        baths_clean = pmin(coalesce(baths, 1), 3),
        baths_factor = factor(floor(baths_clean), levels = 0:3, labels = c("0", "1", "2", "3+")),
        baths_missing = is.na(baths),

        # Amenities: convert to binary (0/1)
        has_gym = coalesce(as.integer(gym), 0L),
        has_laundry = coalesce(as.integer(laundry), 0L),

        # Building type: clean and factor
        building_type_factor = factor(
            coalesce(building_type_clean, "other"),
            levels = c("multi_family", "condo", "single_family", "townhouse", "other")
        )
    )

# =============================================================================
# 5. CREATE 2015 COHORT LISTING PANEL
# =============================================================================
message("Creating 2015 cohort listing panel...")

cohort_2015 <- rentals_with_controls %>%
    # Time window: 2010-2020
    filter(year >= 2010, year <= 2020) %>%
    # Join treatment info
    left_join(treatment_2015, by = "block_id") %>%
    # Drop blocks without treatment info
    filter(!is.na(switched_2015)) %>%
    # Drop contaminated controls
    filter(valid_2015) %>%
    # Apply 1000ft distance restriction
    filter(!is.na(ward_pair_id), dist_ft < 1000) %>%
    mutate(
        # Cohort identifier
        cohort = "2015",

        # Time variables
        relative_year = year - 2015,
        relative_year_capped = pmax(pmin(relative_year, 5), -5),
        relative_quarter = (year - 2015) * 4 + (quarter - 2),
        relative_quarter_capped = pmax(pmin(relative_quarter, 16), -8),

        # Treatment variables (from block panel)
        treat = as.integer(switched_2015),
        switched = switched_2015,
        strictness_change = strictness_change_2015,
        ward_origin = ward_origin_2015,
        treatment_continuous = strictness_change_2015,

        # Create treat_stricter and treat_lenient for binary direction
        treat_stricter = as.integer(strictness_change > 0),
        treat_lenient = as.integer(strictness_change < 0),

        # FE variables
        # ward_pair_id comes from rental data (already computed)
        # ward_origin comes from treatment panel (post spatial join)
        ward_pair_side = paste(ward_pair_id, ward_origin, sep = "_")
    ) %>%
    select(
        # Identifiers
        id, block_id, cohort,

        # Time
        file_date, year, month, quarter, year_month, year_quarter,
        relative_year, relative_year_capped,
        relative_quarter, relative_quarter_capped,

        # Outcome
        rent_price,

        # Unit characteristics (hedonic controls)
        sqft, log_sqft, sqft_missing,
        beds, beds_factor, beds_missing,
        baths, baths_factor, baths_missing,
        has_gym, has_laundry,
        building_type_clean, building_type_factor,

        # Geography/FEs (ward_pair_id from rental data)
        ward_pair_id, ward_origin, ward_pair_side,
        dist_ft, signed_dist,

        # Treatment (from block panel)
        treat, switched, strictness_change, treatment_continuous,
        treat_stricter, treat_lenient
    )

message(sprintf("2015 cohort: %s listings", format(nrow(cohort_2015), big.mark = ",")))

# =============================================================================
# 6. CREATE 2023 COHORT LISTING PANEL
# =============================================================================
message("Creating 2023 cohort listing panel...")

cohort_2023 <- rentals_with_controls %>%
    # Time window: 2018-2025
    filter(year >= 2018, year <= 2025) %>%
    # Join treatment info
    left_join(treatment_2023, by = "block_id") %>%
    # Drop blocks without treatment info
    filter(!is.na(switched_2023)) %>%
    # Drop contaminated controls
    filter(valid_2023) %>%
    # Apply 1000ft distance restriction
    filter(!is.na(ward_pair_id), dist_ft < 1000) %>%
    mutate(
        # Cohort identifier
        cohort = "2023",

        # Time variables
        relative_year = year - 2023,
        relative_year_capped = pmax(pmin(relative_year, 5), -5),
        relative_quarter = (year - 2023) * 4 + (quarter - 2),
        relative_quarter_capped = pmax(pmin(relative_quarter, 16), -8),

        # Treatment variables (from block panel)
        treat = as.integer(switched_2023),
        switched = switched_2023,
        strictness_change = strictness_change_2023,
        ward_origin = ward_origin_2023,
        treatment_continuous = strictness_change_2023,

        # Create treat_stricter and treat_lenient for binary direction
        treat_stricter = as.integer(strictness_change > 0),
        treat_lenient = as.integer(strictness_change < 0),

        # FE variables
        ward_pair_side = paste(ward_pair_id, ward_origin, sep = "_")
    ) %>%
    select(
        # Identifiers
        id, block_id, cohort,

        # Time
        file_date, year, month, quarter, year_month, year_quarter,
        relative_year, relative_year_capped,
        relative_quarter, relative_quarter_capped,

        # Outcome
        rent_price,

        # Unit characteristics (hedonic controls)
        sqft, log_sqft, sqft_missing,
        beds, beds_factor, beds_missing,
        baths, baths_factor, baths_missing,
        has_gym, has_laundry,
        building_type_clean, building_type_factor,

        # Geography/FEs
        ward_pair_id, ward_origin, ward_pair_side,
        dist_ft, signed_dist,

        # Treatment
        treat, switched, strictness_change, treatment_continuous,
        treat_stricter, treat_lenient
    )

message(sprintf("2023 cohort: %s listings", format(nrow(cohort_2023), big.mark = ",")))

# =============================================================================
# 7. STACK COHORTS AND CREATE FINAL PANEL
# =============================================================================
message("Stacking cohorts...")

listing_panel <- bind_rows(cohort_2015, cohort_2023) %>%
    mutate(
        # Cohort-specific identifiers for FEs
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_"),
        cohort_ward_pair_side = paste(cohort, ward_pair_id, ward_origin, sep = "_")
    )

message(sprintf(
    "Stacked panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(listing_panel), big.mark = ","),
    format(nrow(cohort_2015), big.mark = ","),
    format(nrow(cohort_2023), big.mark = ",")
))

# =============================================================================
# 8. DIAGNOSTICS
# =============================================================================
message("\n=== PANEL DIAGNOSTICS ===")

message("\nTotal listings in panel:")
message(sprintf("  %s", format(nrow(listing_panel), big.mark = ",")))

message("\nListings by cohort and treatment status:")
listing_panel %>%
    group_by(cohort, treat) %>%
    summarise(
        n_listings = n(),
        n_blocks = n_distinct(block_id),
        mean_rent = mean(rent_price, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    print()

message("\nShare with missing hedonic controls:")
message(sprintf("  Sqft missing: %.1f%%", 100 * mean(listing_panel$sqft_missing)))
message(sprintf("  Beds missing: %.1f%%", 100 * mean(listing_panel$beds_missing)))
message(sprintf("  Baths missing: %.1f%%", 100 * mean(listing_panel$baths_missing)))
message(sprintf("  Has gym: %.1f%%", 100 * mean(listing_panel$has_gym)))
message(sprintf("  Has laundry: %.1f%%", 100 * mean(listing_panel$has_laundry)))

message("\nBuilding type distribution:")
table(listing_panel$building_type_factor) %>% print()

message("\nMean/median listings per block-year:")
listings_per_block_year <- listing_panel %>%
    group_by(cohort, block_id, year) %>%
    summarise(n = n(), .groups = "drop")
message(sprintf("  Mean: %.1f", mean(listings_per_block_year$n)))
message(sprintf("  Median: %.0f", median(listings_per_block_year$n)))

message("\nDistance to boundary (ft):")
message(sprintf("  Mean: %.0f", mean(listing_panel$dist_ft)))
message(sprintf("  Median: %.0f", median(listing_panel$dist_ft)))
message(sprintf("  Max: %.0f", max(listing_panel$dist_ft)))

message("\nStrictness change distribution (treated units):")
listing_panel %>%
    filter(treat == 1) %>%
    summarise(
        mean_change = mean(strictness_change, na.rm = TRUE),
        sd_change = sd(strictness_change, na.rm = TRUE),
        min_change = min(strictness_change, na.rm = TRUE),
        max_change = max(strictness_change, na.rm = TRUE)
    ) %>%
    print()

# =============================================================================
# 9. SAVE OUTPUT
# =============================================================================
message("\nSaving output...")

write_parquet(listing_panel, "../output/rental_listing_panel.parquet")
message(sprintf("Saved stacked listing panel: %s rows", format(nrow(listing_panel), big.mark = ",")))

# Also save cohort-specific panels for unstacked analysis
write_parquet(cohort_2015, "../output/rental_listing_panel_2015.parquet")
message(sprintf("Saved 2015 cohort panel: %s rows", format(nrow(cohort_2015), big.mark = ",")))

write_parquet(cohort_2023, "../output/rental_listing_panel_2023.parquet")
message(sprintf("Saved 2023 cohort panel: %s rows", format(nrow(cohort_2023), big.mark = ",")))

message("\nDone!")
