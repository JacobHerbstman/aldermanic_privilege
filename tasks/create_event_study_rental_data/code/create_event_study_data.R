# create_event_study_data.R
# Creates block-level panels for event study analysis of rental prices
# Outputs: stacked cohort panels (yearly and quarterly) for 2015+2023 redistrictings

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_rental_data/code")
source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading rental data...")
rentals <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
    filter(!is.na(rent_price), rent_price > 0) %>%
    mutate(
        file_date = as.Date(file_date),
        year = year(file_date),
        month = month(file_date),
        quarter = ceiling(month / 3)
    )
message(sprintf("Loaded %s rental listings", format(nrow(rentals), big.mark = ",")))

message("Loading ward panel...")
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

message("Loading census blocks...")
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id)) %>%
    # Deduplicate - raw data has duplicate block_ids
    distinct(block_id, .keep_all = TRUE)

message("Loading treatment panel...")
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
    mutate(block_id = as.character(block_id))

message("Loading block group controls...")
block_group_controls <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE) %>%
    mutate(GEOID = as.character(GEOID)) %>%
    rename(
        share_white = percent_white,
        share_black = percent_black,
        median_hh_income_1000s = median_income
    ) %>%
    mutate(
        median_hh_income_1000s = median_hh_income_1000s / 1000
    ) %>%
    select(GEOID, year, homeownership_rate, share_white, share_black, share_bach_plus, median_hh_income_1000s)

# =============================================================================
# 2. ASSIGN RENTALS TO CENSUS BLOCKS
# =============================================================================
message("Assigning rentals to census blocks...")

rentals_sf <- rentals %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(st_crs(census_blocks))

rentals_with_blocks <- st_join(rentals_sf, census_blocks %>% select(block_id), join = st_within) %>%
    st_drop_geometry() %>%
    filter(!is.na(block_id))

message(sprintf("Rentals assigned to blocks: %s", format(nrow(rentals_with_blocks), big.mark = ",")))

# =============================================================================
# 3. CREATE BLOCK-YEAR PANEL USING TREATMENT PANEL
# =============================================================================
message("Creating block-year panel using treatment panel...")

# Get unique years in data
year_range <- range(rentals_with_blocks$year)
all_years <- seq(year_range[1], year_range[2])

# Get treatment info for 2015 cohort (uses 2010 blocks)
treatment_2015 <- treatment_panel %>%
    filter(cohort == "2015") %>%
    select(
        block_id,
        ward_pre_2015 = ward_origin,
        ward_post_2015 = ward_dest,
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
        ward_post_2023 = ward_dest,
        switched_2023 = switched,
        strictness_origin_2023 = strictness_origin,
        strictness_dest_2023 = strictness_dest,
        strictness_change_2023 = strictness_change,
        switch_type_2023 = switch_type,
        valid_2023 = valid
    )

# Create block-year base panel from 2015 treatment data
block_year_base <- treatment_2015 %>%
    tidyr::crossing(year = all_years) %>%
    mutate(
        ward = case_when(
            year < 2015 ~ ward_pre_2015,
            year >= 2015 ~ ward_post_2015
        ),
        relative_year_2015 = year - 2015
    ) %>%
    # Join 2023 treatment info
    left_join(treatment_2023, by = "block_id") %>%
    mutate(
        relative_year_2023 = year - 2023,
        switched_2023 = replace_na(switched_2023, FALSE),
        valid_2023 = replace_na(valid_2023, TRUE)
    ) %>%
    filter(!is.na(ward))

# Create ward assignments for joining to rentals
ward_assignments <- treatment_2015 %>%
    select(block_id, ward_pre_2015, ward_post_2015) %>%
    left_join(treatment_2023 %>% select(block_id, ward_post_2023), by = "block_id")

n_switching_blocks_2015 <- sum(treatment_2015$switched_2015, na.rm = TRUE)
n_switching_blocks_2023 <- sum(treatment_2023$switched_2023, na.rm = TRUE)
message(sprintf("Blocks that switched in 2015: %d", n_switching_blocks_2015))
message(sprintf("Blocks that switched in 2023: %d", n_switching_blocks_2023))

# (Old Section 3-4 code removed - now using pre-computed treatment panel)
# Diagnostics are printed by create_block_treatment_panel task

# =============================================================================
# 4. AGGREGATE RENTALS TO BLOCK-YEAR
# =============================================================================
message("Aggregating rentals to block-year...")

# Calculate mode helper function
get_mode <- function(x) {
    if (all(is.na(x))) {
        return(NA)
    }
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
}

# Aggregate yearly rentals
rental_block_year <- rentals_with_blocks %>%
    group_by(block_id, year) %>%
    summarise(
        n_listings = n(),
        mean_rent = mean(rent_price, na.rm = TRUE),
        median_rent = median(rent_price, na.rm = TRUE),
        sd_rent = sd(rent_price, na.rm = TRUE),
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(as.numeric(signed_dist)), na.rm = TRUE),
        .groups = "drop"
    )

# Merge with treatment panel
block_year_panel <- block_year_base %>%
    left_join(rental_block_year, by = c("block_id", "year")) %>%
    mutate(
        n_listings = replace_na(n_listings, 0),
        has_listings = n_listings > 0
    )

# Derive block group ID from block ID (first 12 characters of 15-char block GEOID)
block_year_panel <- block_year_panel %>%
    mutate(block_group_id = substr(block_id, 1, 12))

# Merge demographic controls
block_year_panel <- block_year_panel %>%
    left_join(
        block_group_controls %>% rename(block_group_id = GEOID),
        by = c("block_group_id", "year")
    )

message(sprintf("Block-year panel: %s rows", format(nrow(block_year_panel), big.mark = ",")))

# =============================================================================
# 5. AGGREGATE RENTALS TO BLOCK-QUARTER
# =============================================================================
message("Aggregating rentals to block-quarter...")

rental_block_quarter <- rentals_with_blocks %>%
    mutate(year_quarter = paste(year, sprintf("Q%d", quarter), sep = "-")) %>%
    group_by(block_id, year, quarter, year_quarter) %>%
    summarise(
        n_listings = n(),
        mean_rent = mean(rent_price, na.rm = TRUE),
        median_rent = median(rent_price, na.rm = TRUE),
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(as.numeric(signed_dist)), na.rm = TRUE),
        .groups = "drop"
    )

# =============================================================================
# 6. CREATE STACKED YEARLY COHORT PANEL
# =============================================================================
message("Creating stacked cohort panel...")

# 2015 cohort: valid units only
cohort_2015 <- block_year_panel %>%
    filter(year >= 2010, year <= 2020) %>%
    filter(valid_2015) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        redistricted = switched_2015,
        relative_year = relative_year_2015,
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    ) %>%
    select(
        block_id, year, cohort, treat, redistricted, relative_year, switch_type, strictness_change,
        n_listings, mean_rent, median_rent, has_listings,
        ward_pair_id, mean_dist_to_boundary,
        homeownership_rate, share_white, share_black, share_bach_plus, median_hh_income_1000s
    )

# 2023 cohort: valid units only
cohort_2023 <- block_year_panel %>%
    filter(year >= 2018, year <= 2025) %>%
    filter(valid_2023) %>% # DROP CONTAMINATED CONTROLS
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        redistricted = switched_2023,
        relative_year = relative_year_2023,
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023
    ) %>%
    select(
        block_id, year, cohort, treat, redistricted, relative_year, switch_type, strictness_change,
        n_listings, mean_rent, median_rent, has_listings,
        ward_pair_id, mean_dist_to_boundary,
        homeownership_rate, share_white, share_black, share_bach_plus, median_hh_income_1000s
    )

# Stack cohorts
stacked_panel <- bind_rows(cohort_2015, cohort_2023) %>%
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_panel), big.mark = ","),
    format(nrow(cohort_2015), big.mark = ","),
    format(nrow(cohort_2023), big.mark = ",")
))

# =============================================================================
# 7. CREATE STACKED QUARTERLY PANEL
# =============================================================================
message("Creating stacked quarterly panel...")

# 2015 cohort quarterly
cohort_2015_quarterly <- rental_block_quarter %>%
    filter(year >= 2010, year <= 2020) %>%
    left_join(
        treatment_2015 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2015), valid_2015) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        relative_quarter = (year - 2015) * 4 + (quarter - 2),
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015,
        has_listings = TRUE,
        block_group_id = substr(block_id, 1, 12)
    ) %>%
    left_join(
        block_group_controls %>% rename(block_group_id = GEOID),
        by = c("block_group_id", "year")
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change, n_listings, mean_rent, median_rent, has_listings,
        ward_pair_id, mean_dist_to_boundary,
        homeownership_rate, share_white, share_black, share_bach_plus, median_hh_income_1000s
    )

# 2023 cohort quarterly
cohort_2023_quarterly <- rental_block_quarter %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(
        treatment_2023 %>% distinct(block_id, .keep_all = TRUE),
        by = "block_id"
    ) %>%
    filter(!is.na(switched_2023), valid_2023) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        relative_quarter = (year - 2023) * 4 + (quarter - 2),
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023,
        has_listings = TRUE,
        block_group_id = substr(block_id, 1, 12)
    ) %>%
    left_join(
        block_group_controls %>% rename(block_group_id = GEOID),
        by = c("block_group_id", "year")
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change, n_listings, mean_rent, median_rent, has_listings,
        ward_pair_id, mean_dist_to_boundary,
        homeownership_rate, share_white, share_black, share_bach_plus, median_hh_income_1000s
    )

# Stack quarterly cohorts
stacked_quarterly_panel <- bind_rows(cohort_2015_quarterly, cohort_2023_quarterly) %>%
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked quarterly panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_quarterly_panel), big.mark = ","),
    format(nrow(cohort_2015_quarterly), big.mark = ","),
    format(nrow(cohort_2023_quarterly), big.mark = ",")
))

# =============================================================================
# 8. SAVE OUTPUTS
# =============================================================================
message("\nSaving outputs...")

write_csv(stacked_panel, "../output/rental_stacked_panel.csv")
message(sprintf("Saved stacked cohort panel: %s rows", format(nrow(stacked_panel), big.mark = ",")))

# Save unstacked cohort panels (same generic column structure as stacked)
write_csv(cohort_2015, "../output/rental_unstacked_2015_panel.csv")
message(sprintf("Saved unstacked 2015 panel: %s rows", format(nrow(cohort_2015), big.mark = ",")))

write_csv(cohort_2023, "../output/rental_unstacked_2023_panel.csv")
message(sprintf("Saved unstacked 2023 panel: %s rows", format(nrow(cohort_2023), big.mark = ",")))

write_csv(stacked_quarterly_panel, "../output/rental_stacked_quarterly_panel.csv")
message(sprintf("Saved stacked quarterly panel: %s rows", format(nrow(stacked_quarterly_panel), big.mark = ",")))

# Summary stats
message("\nSummary by cohort and treatment status:")
stacked_panel %>%
    filter(n_listings > 0) %>%
    group_by(cohort, treat) %>%
    summarise(
        n_block_years = n(),
        total_listings = sum(n_listings),
        mean_rent = weighted.mean(mean_rent, n_listings, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    print()

message("\nDone!")
