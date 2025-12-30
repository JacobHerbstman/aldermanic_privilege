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
    mutate(block_id = as.character(block_id))

message("Loading alderman data...")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
    filter(month(month_date) == 6) %>%
    mutate(year = year(month_date)) %>%
    select(year, ward, alderman)

strictness <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE) %>%
    select(alderman, strictness_index)

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
# 3. CREATE WARD PANEL FOR BLOCKS
# =============================================================================
message("Creating block-ward panel across redistricting events...")

year_range <- range(rentals_with_blocks$year)
all_years <- seq(year_range[1], year_range[2])

block_centroids <- st_centroid(census_blocks)

# Spatial join blocks to ward maps
ward_assignments <- tibble(block_id = census_blocks$block_id)

map_years <- list(pre_2015 = 2014, post_2015 = 2015, post_2023 = 2024)

for (map_name in names(map_years)) {
    map_year <- map_years[[map_name]]
    message(sprintf("  Joining blocks to %d ward map...", map_year))

    ward_polys <- ward_panel %>% filter(year == map_year)
    if (nrow(ward_polys) == 0) {
        available_years <- unique(ward_panel$year)
        map_year <- available_years[which.min(abs(available_years - map_year))]
        ward_polys <- ward_panel %>% filter(year == map_year)
    }

    joined <- st_join(block_centroids, ward_polys, join = st_within) %>%
        st_drop_geometry() %>%
        select(block_id, ward) %>%
        distinct(block_id, .keep_all = TRUE)

    names(joined)[names(joined) == "ward"] <- paste0("ward_", map_name)
    ward_assignments <- ward_assignments %>% left_join(joined, by = "block_id")
}

# Expand to full block-year panel
block_year_base <- ward_assignments %>%
    tidyr::crossing(year = all_years) %>%
    mutate(
        ward = case_when(
            year < 2015 ~ ward_pre_2015,
            year >= 2015 & year < 2023 ~ ward_post_2015,
            year >= 2023 ~ ward_post_2023
        ),
        switched_2015 = (ward_pre_2015 != ward_post_2015) & !is.na(ward_pre_2015) & !is.na(ward_post_2015),
        switched_2023 = (ward_post_2015 != ward_post_2023) & !is.na(ward_post_2015) & !is.na(ward_post_2023),
        relative_year_2015 = year - 2015,
        relative_year_2023 = year - 2023
    ) %>%
    filter(!is.na(ward))

message(sprintf(
    "Blocks that switched in 2015: %d",
    block_year_base %>% filter(switched_2015) %>% distinct(block_id) %>% nrow()
))
message(sprintf(
    "Blocks that switched in 2023: %d",
    block_year_base %>% filter(switched_2023) %>% distinct(block_id) %>% nrow()
))

# =============================================================================
# 4. MERGE ALDERMAN AND STRICTNESS DATA
# =============================================================================
message("Merging alderman and strictness data...")

alderman_scores <- alderman_panel %>% left_join(strictness, by = "alderman")
block_year_base <- block_year_base %>% left_join(alderman_scores, by = c("ward", "year"))

# 2015 treatment
block_treatment_2015 <- block_year_base %>%
    filter(year %in% c(2014, 2015)) %>%
    select(block_id, year, strictness_index) %>%
    pivot_wider(names_from = year, values_from = strictness_index, names_prefix = "strictness_") %>%
    mutate(
        strictness_change_2015 = strictness_2015 - strictness_2014,
        switch_type_2015 = case_when(
            is.na(strictness_change_2015) ~ "No Data",
            strictness_change_2015 > 0.1 ~ "Moved to Stricter",
            strictness_change_2015 < -0.1 ~ "Moved to More Lenient",
            TRUE ~ "No Significant Change"
        )
    ) %>%
    select(block_id, strictness_2014, strictness_2015, strictness_change_2015, switch_type_2015)

# 2023 treatment
block_treatment_2023 <- block_year_base %>%
    filter(year %in% c(2022, 2023)) %>%
    select(block_id, year, strictness_index) %>%
    pivot_wider(names_from = year, values_from = strictness_index, names_prefix = "strictness_") %>%
    mutate(
        strictness_change_2023 = strictness_2023 - strictness_2022,
        switch_type_2023 = case_when(
            is.na(strictness_change_2023) ~ "No Data",
            strictness_change_2023 > 0.1 ~ "Moved to Stricter",
            strictness_change_2023 < -0.1 ~ "Moved to More Lenient",
            TRUE ~ "No Significant Change"
        )
    ) %>%
    select(block_id, strictness_2022, strictness_2023, strictness_change_2023, switch_type_2023)

block_year_base <- block_year_base %>%
    left_join(block_treatment_2015, by = "block_id") %>%
    left_join(block_treatment_2023, by = "block_id")

# =============================================================================
# 5. HELPER FUNCTION
# =============================================================================
get_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
        return(NA_character_)
    }
    names(sort(table(x), decreasing = TRUE))[1]
}

# =============================================================================
# 6. AGGREGATE RENTALS TO BLOCK-YEAR
# =============================================================================
message("Aggregating rentals to block-year...")

rental_block_year <- rentals_with_blocks %>%
    group_by(block_id, year) %>%
    summarise(
        n_listings = n(),
        mean_rent = mean(rent_price, na.rm = TRUE),
        median_rent = median(rent_price, na.rm = TRUE),
        ward_pair_id = get_mode(ward_pair_id),
        mean_dist_to_boundary = mean(abs(dist_ft), na.rm = TRUE),
        .groups = "drop"
    )

# Merge with block panel
block_year_panel <- block_year_base %>%
    left_join(rental_block_year, by = c("block_id", "year")) %>%
    mutate(n_listings = replace_na(n_listings, 0)) %>%
    select(-starts_with("ward_pre"), -starts_with("ward_post"))

# =============================================================================
# 7. AGGREGATE RENTALS TO BLOCK-QUARTER
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
        mean_dist_to_boundary = mean(abs(dist_ft), na.rm = TRUE),
        .groups = "drop"
    )

# =============================================================================
# 8. CREATE STACKED YEARLY PANEL
# =============================================================================
message("Creating stacked yearly panel...")

# 2015 cohort: 2014-2020 (limited pre-period since data starts 2014)
cohort_2015 <- block_year_panel %>%
    filter(year >= 2014, year <= 2020) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        relative_year = relative_year_2015,
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    ) %>%
    select(
        block_id, year, cohort, treat, relative_year, switch_type, strictness_change,
        n_listings, mean_rent, median_rent, ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort: 2018-2025
cohort_2023 <- block_year_panel %>%
    filter(year >= 2018, year <= 2025) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        relative_year = relative_year_2023,
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023
    ) %>%
    select(
        block_id, year, cohort, treat, relative_year, switch_type, strictness_change,
        n_listings, mean_rent, median_rent, ward_pair_id, mean_dist_to_boundary
    )

stacked_panel <- bind_rows(cohort_2015, cohort_2023) %>%
    mutate(
        cohort_block_id = paste(cohort, block_id, sep = "_"),
        cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    )

message(sprintf(
    "Stacked yearly panel: %s rows (%s from 2015, %s from 2023)",
    format(nrow(stacked_panel), big.mark = ","),
    format(nrow(cohort_2015), big.mark = ","),
    format(nrow(cohort_2023), big.mark = ",")
))

# =============================================================================
# 9. CREATE STACKED QUARTERLY PANEL
# =============================================================================
message("Creating stacked quarterly panel...")

# 2015 cohort quarterly
cohort_2015_quarterly <- rental_block_quarter %>%
    filter(year >= 2014, year <= 2020) %>%
    left_join(block_treatment_2015, by = "block_id") %>%
    left_join(block_year_base %>% filter(year == 2015) %>% distinct(block_id, switched_2015), by = "block_id") %>%
    filter(!is.na(switched_2015)) %>%
    mutate(
        cohort = "2015",
        treat = as.integer(switched_2015),
        relative_quarter = (year - 2015) * 4 + (quarter - 2),
        switch_type = switch_type_2015,
        strictness_change = strictness_change_2015
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change, n_listings, mean_rent, median_rent,
        ward_pair_id, mean_dist_to_boundary
    )

# 2023 cohort quarterly
cohort_2023_quarterly <- rental_block_quarter %>%
    filter(year >= 2018, year <= 2025) %>%
    left_join(block_treatment_2023, by = "block_id") %>%
    left_join(block_year_base %>% filter(year == 2023) %>% distinct(block_id, switched_2023), by = "block_id") %>%
    filter(!is.na(switched_2023)) %>%
    mutate(
        cohort = "2023",
        treat = as.integer(switched_2023),
        relative_quarter = (year - 2023) * 4 + (quarter - 2),
        switch_type = switch_type_2023,
        strictness_change = strictness_change_2023
    ) %>%
    select(
        block_id, year, quarter, year_quarter, cohort, treat, relative_quarter,
        switch_type, strictness_change, n_listings, mean_rent, median_rent,
        ward_pair_id, mean_dist_to_boundary
    )

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
# 10. SAVE OUTPUTS
# =============================================================================
message("Saving outputs...")

write_csv(stacked_panel, "../output/rental_stacked_panel.csv")
message(sprintf("Saved stacked yearly panel: %s rows", format(nrow(stacked_panel), big.mark = ",")))

write_csv(stacked_quarterly_panel, "../output/rental_stacked_quarterly_panel.csv")
message(sprintf("Saved stacked quarterly panel: %s rows", format(nrow(stacked_quarterly_panel), big.mark = ",")))

# Summary
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
