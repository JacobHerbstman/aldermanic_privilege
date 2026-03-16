# create_listing_panel.R
# Creates listing-level panel for disaggregate event study analysis of rental prices
# Output: rental_listing_panel.parquet - listing-level panel ready for event study estimation
#
# Key features:
# - Keeps individual listings instead of aggregating to block-year
# - Enables hedonic controls (beds, baths, gym, etc.)
# - NO IMPUTATION - missing hedonics remain as NA and will be dropped in regression
# - FIX: For treated blocks, ward_pair_id is constructed from ward_origin/ward_dest
#        to ensure stability across pre/post periods

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

# Disable s2 spherical geometry to avoid validation errors with census block geometries
sf_use_s2(FALSE)

assign_cohort_segments <- function(df, segment_layers, era_label, cohort_label, chunk_n = 80000L) {
  if (nrow(df) == 0) {
    return(df %>%
      mutate(
        segment_id_cohort = NA_character_,
        segment_side = NA_character_,
        cohort_segment = NA_character_,
        cohort_segment_side = NA_character_
      ))
  }

  out <- df
  pts <- st_as_sf(
    data.frame(longitude = out$longitude, latitude = out$latitude),
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  seg_id <- assign_points_to_segments(
    points_sf = pts,
    era_values = rep(era_label, nrow(out)),
    pair_values = out$ward_pair_id,
    segment_layers = segment_layers,
    chunk_n = chunk_n
  )

  out <- out %>%
    mutate(
      segment_id_cohort = seg_id,
      segment_side = if_else(!is.na(segment_id_cohort) & !is.na(ward_origin), paste(segment_id_cohort, ward_origin, sep = "_"), NA_character_),
      cohort_segment = if_else(!is.na(segment_id_cohort), paste(cohort_label, segment_id_cohort, sep = "_"), NA_character_),
      cohort_segment_side = if_else(!is.na(segment_side), paste(cohort_label, segment_side, sep = "_"), NA_character_)
    )

  message(sprintf(
    "[%s] segment coverage within cohort panel: %.2f%%",
    cohort_label,
    100 * mean(!is.na(out$segment_id_cohort))
  ))

  out
}

summarize_event_support <- function(df, panel_mode, filter_stage) {
  if (nrow(df) == 0) {
    return(tibble(
      panel_mode = character(),
      filter_stage = character(),
      cohort = character(),
      event_time = integer(),
      n_listings = integer(),
      n_blocks = integer(),
      n_treated = integer(),
      n_control = integer()
    ))
  }

  df %>%
    mutate(
      panel_mode = panel_mode,
      filter_stage = filter_stage
    ) %>%
    group_by(panel_mode, filter_stage, cohort, event_time = relative_year_capped) %>%
    summarise(
      n_listings = n(),
      n_blocks = n_distinct(block_id),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(panel_mode, filter_stage, cohort, event_time)
}

summarize_calendar_support <- function(df, panel_mode) {
  if (nrow(df) == 0) {
    return(tibble(
      panel_mode = character(),
      cohort = character(),
      year = integer(),
      treat = integer(),
      n_listings = integer(),
      n_blocks = integer()
    ))
  }

  df %>%
    mutate(panel_mode = panel_mode) %>%
    group_by(panel_mode, cohort, year, treat) %>%
    summarise(
      n_listings = n(),
      n_blocks = n_distinct(block_id),
      .groups = "drop"
    ) %>%
    arrange(panel_mode, cohort, year, treat)
}

summarize_repeat_listings <- function(df, panel_mode) {
  if (nrow(df) == 0) {
    return(tibble(
      panel_mode = panel_mode,
      n_rows = 0L,
      n_unique_ids = 0L,
      mean_obs_per_id = NA_real_,
      median_obs_per_id = NA_real_,
      p90_obs_per_id = NA_real_,
      p99_obs_per_id = NA_real_,
      max_obs_per_id = NA_real_,
      share_repeated_ids = NA_real_
    ))
  }

  listing_counts <- df %>%
    count(id, name = "n_obs")

  tibble(
    panel_mode = panel_mode,
    n_rows = nrow(df),
    n_unique_ids = nrow(listing_counts),
    mean_obs_per_id = mean(listing_counts$n_obs),
    median_obs_per_id = median(listing_counts$n_obs),
    p90_obs_per_id = as.numeric(quantile(listing_counts$n_obs, 0.9)),
    p99_obs_per_id = as.numeric(quantile(listing_counts$n_obs, 0.99)),
    max_obs_per_id = max(listing_counts$n_obs),
    share_repeated_ids = mean(listing_counts$n_obs > 1)
  )
}

summarize_assignment_stability <- function(df, panel_mode) {
  treated_blocks <- df %>%
    filter(treat == 1) %>%
    group_by(block_id) %>%
    summarise(
      n_ward_pairs = n_distinct(ward_pair_id),
      n_segments = n_distinct(segment_id_cohort[!is.na(segment_id_cohort)]),
      .groups = "drop"
    )

  tibble(
    panel_mode = panel_mode,
    n_rows = nrow(df),
    n_treated_blocks = nrow(treated_blocks),
    segment_coverage_pct = 100 * mean(!is.na(df$segment_id_cohort)),
    n_missing_segment_rows = sum(is.na(df$segment_id_cohort)),
    n_blocks_multi_ward_pair = sum(treated_blocks$n_ward_pairs > 1),
    pct_blocks_multi_ward_pair = 100 * mean(treated_blocks$n_ward_pairs > 1),
    n_blocks_multi_segment = sum(treated_blocks$n_segments > 1),
    pct_blocks_multi_segment = 100 * mean(treated_blocks$n_segments > 1)
  )
}

add_hedonic_controls <- function(df) {
  df %>%
    mutate(
      log_sqft = if_else(!is.na(sqft) & sqft > 0, log(sqft), NA_real_),
      log_beds = if_else(!is.na(beds) & beds > 0, log(beds), NA_real_),
      log_baths = if_else(!is.na(baths) & baths > 0, log(baths), NA_real_),
      has_gym = coalesce(as.integer(gym), 0L),
      has_laundry = coalesce(as.integer(laundry), 0L),
      building_type_factor = factor(
        coalesce(building_type_clean, "other"),
        levels = c("multi_family", "condo", "single_family", "townhouse", "other")
      )
    )
}

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading rental data with ward distances...")
rentals <- read_parquet(
  "../input/rent_with_ward_distances.parquet",
  col_select = c(
    "id", "rent_price", "beds", "baths", "sqft", "laundry", "gym",
    "file_date", "ward", "dist_ft", "ward_pair_id", "longitude", "latitude",
    "building_type_clean", "signed_dist"
  )
) %>%
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
  distinct(block_id, .keep_all = TRUE)
message(sprintf("Loaded %s census blocks", format(nrow(census_blocks), big.mark = ",")))

message("Loading treatment panel...")
treatment_panel <- read_csv("../input/block_treatment_panel.csv", show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))

message("Loading segment layers for cohort-baseline assignment...")
segment_layers_1000 <- load_segment_layers("../input/boundary_segments_1320ft.gpkg", buffer_ft = 1000)

# =============================================================================
# 2. ASSIGN RENTALS TO CENSUS BLOCKS
# =============================================================================
message("Assigning rentals to census blocks via spatial join...")

crs_projected <- 3435 # Illinois East State Plane (feet)

rentals_sf <- rentals %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_projected)

census_blocks_proj <- census_blocks %>%
  st_transform(crs_projected)

rentals_with_blocks <- st_join(rentals_sf, census_blocks_proj %>% select(block_id), join = st_within) %>%
  st_drop_geometry() %>%
  select(
    id, file_date, year, month, quarter, year_month, year_quarter,
    rent_price, beds, baths, sqft, laundry, gym, building_type_clean,
    ward, dist_ft, ward_pair_id, longitude, latitude, signed_dist, block_id
  ) %>%
  filter(!is.na(block_id))

message(sprintf("Rentals assigned to blocks: %s", format(nrow(rentals_with_blocks), big.mark = ",")))

rm(rentals, rentals_sf, census_blocks_proj, census_blocks)
gc()

# =============================================================================
# 3. PREPARE TREATMENT DATA BY COHORT
# =============================================================================
message("Preparing treatment data...")

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
# 4. PREPARE HEDONIC CONTROLS (NO IMPUTATION)
# =============================================================================
message("Preparing hedonic controls (no imputation)...")

# Report hedonic coverage
message("\nHedonic variable coverage (% non-missing):")
message(sprintf("  sqft: %.1f%%", 100 * mean(!is.na(rentals_with_blocks$sqft) & rentals_with_blocks$sqft > 0)))
message(sprintf("  beds: %.1f%%", 100 * mean(!is.na(rentals_with_blocks$beds) & rentals_with_blocks$beds > 0)))
message(sprintf("  baths: %.1f%%", 100 * mean(!is.na(rentals_with_blocks$baths) & rentals_with_blocks$baths > 0)))

rental_support_parts <- list()

# =============================================================================
# 5. CREATE 2015 COHORT LISTING PANEL
# =============================================================================
message("\nCreating 2015 cohort listing panel...")

cohort_2015_work <- rentals_with_blocks %>%
  filter(year >= 2010, year <= 2020) %>%
  left_join(treatment_2015, by = "block_id") %>%
  mutate(
    cohort = "2015",
    relative_year = year - 2015,
    relative_year_capped = pmax(pmin(relative_year, 5), -5),
    relative_quarter = (year - 2015) * 4 + (quarter - 2),
    relative_quarter_capped = pmax(pmin(relative_quarter, 16), -8),
    treat = as.integer(switched_2015)
  )

rental_support_parts[["cohort_2015_post_join"]] <- summarize_event_support(
  cohort_2015_work, "cohort_2015", "post_join_treatment"
)

cohort_2015_work <- cohort_2015_work %>%
  filter(!is.na(switched_2015))

rental_support_parts[["cohort_2015_post_nonmissing"]] <- summarize_event_support(
  cohort_2015_work, "cohort_2015", "post_drop_missing_treatment"
)

cohort_2015_work <- cohort_2015_work %>%
  filter(valid_2015)

rental_support_parts[["cohort_2015_post_valid"]] <- summarize_event_support(
  cohort_2015_work, "cohort_2015", "post_valid_treatment_assignment"
)

cohort_2015_work <- cohort_2015_work %>%
  filter(!is.na(ward_pair_id), !is.na(ward), dist_ft <= 2000)

rental_support_parts[["cohort_2015_post_distance"]] <- summarize_event_support(
  cohort_2015_work, "cohort_2015", "post_distance_filter"
)

cohort_2015_work <- cohort_2015_work %>%
  mutate(
    switched = switched_2015,
    strictness_change = strictness_change_2015,
    ward_origin = if_else(switched_2015, ward_origin_2015, ward),
    ward_dest = if_else(switched_2015, ward_dest_2015, ward),
    treatment_continuous = strictness_change_2015,
    treat_stricter = as.integer(strictness_change > 0),
    treat_lenient = as.integer(strictness_change < 0),
    ward_pair_id_fixed = if_else(
      switched_2015,
      paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-"),
      ward_pair_id
    ),
    ward_pair_side = paste(ward_pair_id_fixed, ward_origin, sep = "_")
  ) %>%
  mutate(
    ward_pair_id = ward_pair_id_fixed
  ) %>%
  add_hedonic_controls()

cohort_2015 <- cohort_2015_work %>%
  assign_cohort_segments(segment_layers_1000, "2003_2014", "2015") %>%
  select(
    id, block_id, cohort,
    file_date, year, month, quarter, year_month, year_quarter,
    relative_year, relative_year_capped,
    relative_quarter, relative_quarter_capped,
    rent_price,
    log_sqft, log_beds, log_baths,
    has_gym, has_laundry,
    building_type_clean, building_type_factor,
    sqft, beds, baths,
    ward_pair_id, ward_origin, ward_dest, ward_pair_side,
    segment_id_cohort, segment_side, cohort_segment, cohort_segment_side,
    dist_ft, signed_dist,
    treat, switched, strictness_change, treatment_continuous,
    treat_stricter, treat_lenient
  )

rental_support_parts[["cohort_2015_final"]] <- summarize_event_support(
  cohort_2015, "cohort_2015", "final_assigned_segments"
)

rm(cohort_2015_work)
gc()

message(sprintf("2015 cohort: %s listings", format(nrow(cohort_2015), big.mark = ",")))

# Diagnostic: check ward_pair_id stability for treated blocks
treated_wp_check_2015 <- cohort_2015 %>%
  filter(treat == 1) %>%
  group_by(block_id) %>%
  summarise(
    n_ward_pairs = n_distinct(ward_pair_id),
    .groups = "drop"
  )
message(sprintf(
  "2015 treated blocks with multiple ward_pair_ids: %d (%.1f%%)",
  sum(treated_wp_check_2015$n_ward_pairs > 1),
  100 * mean(treated_wp_check_2015$n_ward_pairs > 1)
))

# =============================================================================
# 6. CREATE 2023 COHORT LISTING PANEL
# =============================================================================
message("\nCreating 2023 cohort listing panel...")

cohort_2023_work <- rentals_with_blocks %>%
  filter(year >= 2018, year <= 2025) %>%
  left_join(treatment_2023, by = "block_id") %>%
  mutate(
    cohort = "2023",
    relative_year = year - 2023,
    relative_year_capped = pmax(pmin(relative_year, 5), -5),
    relative_quarter = (year - 2023) * 4 + (quarter - 2),
    relative_quarter_capped = pmax(pmin(relative_quarter, 16), -8),
    treat = as.integer(switched_2023)
  )

rental_support_parts[["cohort_2023_post_join"]] <- summarize_event_support(
  cohort_2023_work, "cohort_2023", "post_join_treatment"
)

cohort_2023_work <- cohort_2023_work %>%
  filter(!is.na(switched_2023))

rental_support_parts[["cohort_2023_post_nonmissing"]] <- summarize_event_support(
  cohort_2023_work, "cohort_2023", "post_drop_missing_treatment"
)

cohort_2023_work <- cohort_2023_work %>%
  filter(valid_2023)

rental_support_parts[["cohort_2023_post_valid"]] <- summarize_event_support(
  cohort_2023_work, "cohort_2023", "post_valid_treatment_assignment"
)

cohort_2023_work <- cohort_2023_work %>%
  filter(!is.na(ward_pair_id), !is.na(ward), dist_ft <= 2000)

rental_support_parts[["cohort_2023_post_distance"]] <- summarize_event_support(
  cohort_2023_work, "cohort_2023", "post_distance_filter"
)

cohort_2023_work <- cohort_2023_work %>%
  mutate(
    switched = switched_2023,
    strictness_change = strictness_change_2023,
    ward_origin = if_else(switched_2023, ward_origin_2023, ward),
    ward_dest = if_else(switched_2023, ward_dest_2023, ward),
    treatment_continuous = strictness_change_2023,
    treat_stricter = as.integer(strictness_change > 0),
    treat_lenient = as.integer(strictness_change < 0),
    ward_pair_id_fixed = if_else(
      switched_2023,
      paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-"),
      ward_pair_id
    ),
    ward_pair_side = paste(ward_pair_id_fixed, ward_origin, sep = "_")
  ) %>%
  mutate(
    ward_pair_id = ward_pair_id_fixed
  ) %>%
  add_hedonic_controls()

cohort_2023 <- cohort_2023_work %>%
  assign_cohort_segments(segment_layers_1000, "2015_2023", "2023") %>%
  select(
    id, block_id, cohort,
    file_date, year, month, quarter, year_month, year_quarter,
    relative_year, relative_year_capped,
    relative_quarter, relative_quarter_capped,
    rent_price,
    log_sqft, log_beds, log_baths,
    has_gym, has_laundry,
    building_type_clean, building_type_factor,
    sqft, beds, baths,
    ward_pair_id, ward_origin, ward_dest, ward_pair_side,
    segment_id_cohort, segment_side, cohort_segment, cohort_segment_side,
    dist_ft, signed_dist,
    treat, switched, strictness_change, treatment_continuous,
    treat_stricter, treat_lenient
  )

rental_support_parts[["cohort_2023_final"]] <- summarize_event_support(
  cohort_2023, "cohort_2023", "final_assigned_segments"
)

rm(cohort_2023_work, rentals_with_blocks, treatment_2015, treatment_2023)
gc()

message(sprintf("2023 cohort: %s listings", format(nrow(cohort_2023), big.mark = ",")))

# Diagnostic: check ward_pair_id stability for treated blocks
treated_wp_check_2023 <- cohort_2023 %>%
  filter(treat == 1) %>%
  group_by(block_id) %>%
  summarise(
    n_ward_pairs = n_distinct(ward_pair_id),
    .groups = "drop"
  )
message(sprintf(
  "2023 treated blocks with multiple ward_pair_ids: %d (%.1f%%)",
  sum(treated_wp_check_2023$n_ward_pairs > 1),
  100 * mean(treated_wp_check_2023$n_ward_pairs > 1)
))

# =============================================================================
# 7. STACK COHORTS AND CREATE FINAL PANEL
# =============================================================================
message("\nStacking cohorts...")

listing_panel <- bind_rows(cohort_2015, cohort_2023) %>%
  mutate(
    # Cohort-specific identifiers for FEs
    cohort_block_id = paste(cohort, block_id, sep = "_"),
    cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_"),
    cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
    cohort_segment = if_else(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
    cohort_segment_side = if_else(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
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

message("\nHedonic variable coverage in final panel (% non-missing):")
message(sprintf("  log_sqft: %.1f%%", 100 * mean(!is.na(listing_panel$log_sqft))))
message(sprintf("  log_beds: %.1f%%", 100 * mean(!is.na(listing_panel$log_beds))))
message(sprintf("  log_baths: %.1f%%", 100 * mean(!is.na(listing_panel$log_baths))))
message(sprintf("  has_gym: %.1f%% (of listings)", 100 * mean(listing_panel$has_gym)))
message(sprintf("  has_laundry: %.1f%% (of listings)", 100 * mean(listing_panel$has_laundry)))

# Complete hedonic cases
n_complete <- sum(!is.na(listing_panel$log_sqft) &
  !is.na(listing_panel$log_beds) &
  !is.na(listing_panel$log_baths))
message(sprintf(
  "\nListings with complete hedonics (sqft, beds, baths): %s (%.1f%%)",
  format(n_complete, big.mark = ","),
  100 * n_complete / nrow(listing_panel)
))

message("\nBuilding type distribution:")
table(listing_panel$building_type_factor) %>% print()

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

# Final ward_pair_id stability check
message("\n=== WARD_PAIR_ID STABILITY CHECK ===")
wp_stability <- listing_panel %>%
  filter(treat == 1) %>%
  group_by(cohort, block_id) %>%
  summarise(
    n_ward_pairs = n_distinct(ward_pair_id),
    n_listings = n(),
    .groups = "drop"
  )
message(sprintf("Total treated blocks: %d", nrow(wp_stability)))
message(sprintf(
  "Treated blocks with unstable ward_pair_id: %d (%.1f%%)",
  sum(wp_stability$n_ward_pairs > 1),
  100 * mean(wp_stability$n_ward_pairs > 1)
))

rental_support_parts[["stacked_final"]] <- summarize_event_support(
  listing_panel, "stacked_implementation", "final_assigned_segments"
)

rental_support_by_event_time <- bind_rows(rental_support_parts)

rental_support_by_calendar_time <- bind_rows(
  summarize_calendar_support(cohort_2015, "cohort_2015"),
  summarize_calendar_support(cohort_2023, "cohort_2023"),
  summarize_calendar_support(listing_panel, "stacked_implementation")
)

rental_repeat_listing_diagnostics <- bind_rows(
  summarize_repeat_listings(cohort_2015, "cohort_2015"),
  summarize_repeat_listings(cohort_2023, "cohort_2023"),
  summarize_repeat_listings(listing_panel, "stacked_implementation")
)

rental_assignment_stability <- bind_rows(
  summarize_assignment_stability(cohort_2015, "cohort_2015"),
  summarize_assignment_stability(cohort_2023, "cohort_2023"),
  summarize_assignment_stability(listing_panel, "stacked_implementation")
)

# =============================================================================
# 9. SAVE OUTPUT
# =============================================================================
message("\nSaving output...")

write_parquet(listing_panel, "../output/rental_listing_panel.parquet")
message(sprintf("Saved stacked listing panel: %s rows", format(nrow(listing_panel), big.mark = ",")))

write_parquet(cohort_2015, "../output/rental_listing_panel_2015.parquet")
message(sprintf("Saved 2015 cohort panel: %s rows", format(nrow(cohort_2015), big.mark = ",")))

write_parquet(cohort_2023, "../output/rental_listing_panel_2023.parquet")
message(sprintf("Saved 2023 cohort panel: %s rows", format(nrow(cohort_2023), big.mark = ",")))

write_csv(rental_support_by_event_time, "../output/rental_listing_panel_support_by_event_time.csv")
message("Saved rental event-time support diagnostics")

write_csv(rental_support_by_calendar_time, "../output/rental_listing_panel_support_by_calendar_time.csv")
message("Saved rental calendar-time support diagnostics")

write_csv(rental_repeat_listing_diagnostics, "../output/rental_listing_panel_repeat_listing_diagnostics.csv")
message("Saved rental repeat-listing diagnostics")

write_csv(rental_assignment_stability, "../output/rental_listing_panel_assignment_stability.csv")
message("Saved rental assignment-stability diagnostics")

message("\nDone!")
