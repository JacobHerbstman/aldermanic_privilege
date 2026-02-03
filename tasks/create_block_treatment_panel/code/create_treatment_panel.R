# create_treatment_panel.R
# Creates master block-level treatment panel for event study analysis
# Output: block_treatment_pre_scores.csv with block-to-ward treatment geometry.
# Score merge happens in merge_event_study_scores.

source("../../setup_environment/code/packages.R")

# Disable s2 for geometry operations
sf_use_s2(FALSE)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading data...")

# Ward panel
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

# Alderman panel
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
    mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
    filter(month(month_date) == 6) %>%
    mutate(year = year(month_date)) %>%
    select(year, ward, alderman)

# 2010 Census blocks (for 2015 cohort)
message("Loading 2010 census blocks...")
blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID10) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)

message(sprintf("  2010 blocks: %s", format(nrow(blocks_2010), big.mark = ",")))

# 2020 Census blocks (for 2023 cohort)
message("Loading 2020 census blocks...")
blocks_2020 <- read_csv("../input/census_blocks_2020.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(st_crs(ward_panel)) %>%
    rename(block_id = GEOID20) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)

message(sprintf("  2020 blocks: %s", format(nrow(blocks_2020), big.mark = ",")))

# =============================================================================
# 2. ASSIGN 2010 BLOCKS TO WARDS (FOR 2015 COHORT)
# =============================================================================
message("\nAssigning 2010 blocks to ward maps...")

block_centroids_2010 <- st_centroid(blocks_2010)

# Pre-2015 ward map (2014)
ward_2014 <- ward_panel %>% filter(year == 2014)
joined_2014 <- st_join(block_centroids_2010, ward_2014, join = st_within) %>%
    st_drop_geometry() %>%
    select(block_id, ward) %>%
    distinct(block_id, .keep_all = TRUE) %>%
    rename(ward_pre_2015 = ward)

# Post-2015 ward map (2015)
ward_2015 <- ward_panel %>% filter(year == 2015)
joined_2015 <- st_join(block_centroids_2010, ward_2015, join = st_within) %>%
    st_drop_geometry() %>%
    select(block_id, ward) %>%
    distinct(block_id, .keep_all = TRUE) %>%
    rename(ward_post_2015 = ward)

# Combine 2010 block assignments
assignments_2010 <- tibble(block_id = blocks_2010$block_id) %>%
    left_join(joined_2014, by = "block_id") %>%
    left_join(joined_2015, by = "block_id") %>%
    mutate(
        switched_2015 = (ward_pre_2015 != ward_post_2015) &
            !is.na(ward_pre_2015) & !is.na(ward_post_2015),
        block_vintage = "2010"
    )

message(sprintf("  2010 blocks switching in 2015: %d", sum(assignments_2010$switched_2015, na.rm = TRUE)))

# =============================================================================
# 3. ASSIGN 2020 BLOCKS TO WARDS (FOR 2023 COHORT)
# =============================================================================
message("\nAssigning 2020 blocks to ward maps...")

block_centroids_2020 <- st_centroid(blocks_2020)

# Post-2015 ward map (use 2022 as reference for origin)
ward_2022 <- ward_panel %>% filter(year == 2022)
if (nrow(ward_2022) == 0) ward_2022 <- ward_panel %>% filter(year == 2021)
joined_2022 <- st_join(block_centroids_2020, ward_2022, join = st_within) %>%
    st_drop_geometry() %>%
    select(block_id, ward) %>%
    distinct(block_id, .keep_all = TRUE) %>%
    rename(ward_post_2015 = ward) # Origin ward for 2023 event

# Post-2023 ward map (2024)
ward_2024 <- ward_panel %>% filter(year == 2024)
if (nrow(ward_2024) == 0) ward_2024 <- ward_panel %>% filter(year == max(ward_panel$year))
joined_2024 <- st_join(block_centroids_2020, ward_2024, join = st_within) %>%
    st_drop_geometry() %>%
    select(block_id, ward) %>%
    distinct(block_id, .keep_all = TRUE) %>%
    rename(ward_post_2023 = ward)

# Combine 2020 block assignments
assignments_2020 <- tibble(block_id = blocks_2020$block_id) %>%
    left_join(joined_2022, by = "block_id") %>%
    left_join(joined_2024, by = "block_id") %>%
    mutate(
        switched_2023 = (ward_post_2015 != ward_post_2023) &
            !is.na(ward_post_2015) & !is.na(ward_post_2023),
        block_vintage = "2020"
    )

message(sprintf("  2020 blocks switching in 2023: %d", sum(assignments_2020$switched_2023, na.rm = TRUE)))

# =============================================================================
# 4. IDENTIFY WARD TURNOVER (FOR CONTROL GROUP FILTERING)
# =============================================================================
message("Identifying wards with electoral turnover...")

# 2015: wards with turnover between 2014 and 2015
ward_turnover_2015 <- alderman_panel %>%
    filter(year %in% c(2014, 2015)) %>%
    select(ward, year, alderman) %>%
    pivot_wider(names_from = year, values_from = alderman, names_prefix = "alderman_") %>%
    mutate(ward_had_turnover_2015 = alderman_2014 != alderman_2015) %>%
    select(ward, ward_had_turnover_2015)

# 2023: wards with turnover between 2022 and 2023
ward_turnover_2023 <- alderman_panel %>%
    filter(year %in% c(2022, 2023)) %>%
    select(ward, year, alderman) %>%
    pivot_wider(names_from = year, values_from = alderman, names_prefix = "alderman_") %>%
    mutate(ward_had_turnover_2023 = alderman_2022 != alderman_2023) %>%
    select(ward, ward_had_turnover_2023)

# Add turnover flags and compute valid flags
treatment_2015 <- assignments_2010 %>%
    left_join(ward_turnover_2015, by = c("ward_pre_2015" = "ward")) %>%
    mutate(
        valid_2015 = switched_2015 | (!switched_2015 & !ward_had_turnover_2015),
        valid_2015 = replace_na(valid_2015, FALSE)
    )

treatment_2023 <- assignments_2020 %>%
    left_join(ward_turnover_2023, by = c("ward_post_2015" = "ward")) %>%
    mutate(
        valid_2023 = switched_2023 | (!switched_2023 & !ward_had_turnover_2023),
        valid_2023 = replace_na(valid_2023, FALSE)
    )

# =============================================================================
# 5. COMBINE AND SAVE PRE-SCORES PANEL
# =============================================================================
message("\nCombining treatment panels (pre-scores)...")

# Select columns for 2015 cohort output
panel_2015 <- treatment_2015 %>%
    select(
        block_id, block_vintage,
        ward_pre_2015, ward_post_2015,
        switched_2015,
        ward_had_turnover_2015, valid_2015
    ) %>%
    mutate(cohort = "2015")

# Select columns for 2023 cohort output
panel_2023 <- treatment_2023 %>%
    select(
        block_id, block_vintage,
        ward_post_2015, ward_post_2023,
        switched_2023,
        ward_had_turnover_2023, valid_2023
    ) %>%
    mutate(cohort = "2023")

# Rename columns for stacking
panel_2015_renamed <- panel_2015 %>%
    rename(
        ward_origin = ward_pre_2015,
        ward_dest = ward_post_2015,
        switched = switched_2015,
        ward_had_turnover = ward_had_turnover_2015,
        valid = valid_2015
    )

panel_2023_renamed <- panel_2023 %>%
    rename(
        ward_origin = ward_post_2015,
        ward_dest = ward_post_2023,
        switched = switched_2023,
        ward_had_turnover = ward_had_turnover_2023,
        valid = valid_2023
    )

# Stack cohorts
block_treatment_pre_scores <- bind_rows(panel_2015_renamed, panel_2023_renamed)

# =============================================================================
# 6. DIAGNOSTICS
# =============================================================================
message("\n=== TREATMENT PRE-SCORES DIAGNOSTICS ===")

for (coh in c("2015", "2023")) {
    cohort_data <- block_treatment_pre_scores %>% filter(cohort == coh)

    n_total <- nrow(cohort_data)
    n_treated <- sum(cohort_data$switched, na.rm = TRUE)
    n_valid <- sum(cohort_data$valid, na.rm = TRUE)
    n_valid_control <- sum(!cohort_data$switched & cohort_data$valid, na.rm = TRUE)
    n_contaminated <- sum(!cohort_data$switched & !cohort_data$valid, na.rm = TRUE)

    message(sprintf("\n%s Cohort:", coh))
    message(sprintf("  Total blocks: %s", format(n_total, big.mark = ",")))
    message(sprintf("  Treated (redistricted): %s", format(n_treated, big.mark = ",")))
    message(sprintf("  Valid controls: %s", format(n_valid_control, big.mark = ",")))
    message(sprintf("  Contaminated controls (dropped): %s", format(n_contaminated, big.mark = ",")))
}

# =============================================================================
# 7. SAVE OUTPUT
# =============================================================================
message("\nSaving output...")

write_csv(block_treatment_pre_scores, "../output/block_treatment_pre_scores.csv")

message(sprintf(
    "Saved: ../output/block_treatment_pre_scores.csv (%s rows)",
    format(nrow(block_treatment_pre_scores), big.mark = ",")
))

message("\nDone!")
