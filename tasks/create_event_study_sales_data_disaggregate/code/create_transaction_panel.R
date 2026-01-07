# create_transaction_panel.R
# Creates transaction-level panel for disaggregate sales event study
# Key feature: temporal matching of sales to property characteristics via rolling join
# NO IMPUTATION - missing hedonics remain as NA and will be dropped in regression

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading sales data...")
sales <- fread("../input/sales_with_ward_distances.csv")
sales[, `:=`(
  pin = as.character(pin),
  sale_date = as.Date(sale_date),
  sale_year = year(as.Date(sale_date))
)]
message(sprintf("Loaded %s sales", format(nrow(sales), big.mark = ",")))

message("Loading residential improvements panel...")
improvements <- read_parquet("../input/residential_improvements_panel.parquet")
setDT(improvements)
message(sprintf(
  "Loaded %s improvement records for %s unique PINs",
  format(nrow(improvements), big.mark = ","),
  format(uniqueN(improvements$pin), big.mark = ",")
))

message("Loading census blocks...")
# Disable S2 spherical geometry to avoid validation errors on some blocks
sf_use_s2(FALSE)
census_blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

message("Loading treatment panel...")
treatment_panel <- fread("../input/block_treatment_panel.csv")
treatment_panel[, block_id := as.character(block_id)]

# =============================================================================
# 2. TEMPORAL MERGE: SALES TO HEDONICS (ROLLING JOIN)
# =============================================================================
message("\n=== TEMPORAL MATCHING VIA ROLLING JOIN ===")

# Ensure improvements is keyed for rolling join
setkey(improvements, pin, tax_year)

# Rolling join: for each sale, find most recent improvements record
# roll = TRUE: find max tax_year where tax_year <= sale_year
# rollends = c(TRUE, FALSE):
#   - TRUE: if sale is before first record, use first record (better than NA)
#   - FALSE: if sale is after last record, return NA (don't use stale data)
message("Performing rolling join...")

sales_with_hedonics <- improvements[
  sales,
  on = .(pin, tax_year = sale_year),
  roll = TRUE,
  rollends = c(TRUE, FALSE)
]

# The joined tax_year column now contains the matched hedonic year
# Rename for clarity and recover sale_year
setnames(sales_with_hedonics, "tax_year", "hedonic_tax_year")
sales_with_hedonics[, sale_year := year(sale_date)]

# Diagnostics on match quality
n_total <- nrow(sales_with_hedonics)
n_matched <- sum(!is.na(sales_with_hedonics$building_sqft))
message(sprintf(
  "\nMatch rate: %s of %s sales (%.1f%%) matched to hedonics",
  format(n_matched, big.mark = ","),
  format(n_total, big.mark = ","),
  n_matched / n_total * 100
))

# Check temporal gap between sale and matched assessment
sales_with_hedonics[, years_gap := sale_year - hedonic_tax_year]
message("\nYears between sale and matched assessment:")
message(sprintf("  Median: %d", median(sales_with_hedonics$years_gap, na.rm = TRUE)))
message(sprintf("  Mean: %.1f", mean(sales_with_hedonics$years_gap, na.rm = TRUE)))
message(sprintf("  Max: %d", max(sales_with_hedonics$years_gap, na.rm = TRUE)))

# Distribution of gaps
message("\nGap distribution:")
gap_dist <- sales_with_hedonics[!is.na(years_gap), .N, by = years_gap][order(years_gap)]
print(head(gap_dist, 10))

# =============================================================================
# 3. SPATIAL MERGE: ASSIGN SALES TO CENSUS BLOCKS
# =============================================================================
message("\n=== SPATIAL ASSIGNMENT TO BLOCKS ===")

# Convert to sf for spatial join
sales_sf <- sales_with_hedonics %>%
  as_tibble() %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(census_blocks))

message(sprintf("Sales with valid coordinates: %s", format(nrow(sales_sf), big.mark = ",")))

# Spatial join to get block_id
sales_with_blocks <- st_join(sales_sf, census_blocks %>% select(block_id), join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(block_id))

message(sprintf("Sales assigned to blocks: %s", format(nrow(sales_with_blocks), big.mark = ",")))

# Convert back to data.table for efficiency
setDT(sales_with_blocks)

# =============================================================================
# 4. MERGE TREATMENT INFORMATION
# =============================================================================
message("\n=== MERGING TREATMENT INFO ===")

# 2015 cohort treatment info
treatment_2015 <- treatment_panel[cohort == "2015", .(
  block_id,
  ward_origin_2015 = ward_origin,
  ward_dest_2015 = ward_dest,
  switched_2015 = switched,
  strictness_change_2015 = strictness_change,
  valid_2015 = valid
)]

# 2023 cohort treatment info
treatment_2023 <- treatment_panel[cohort == "2023", .(
  block_id,
  ward_origin_2023 = ward_origin,
  ward_dest_2023 = ward_dest,
  switched_2023 = switched,
  strictness_change_2023 = strictness_change,
  valid_2023 = valid
)]

# Merge treatment info
sales_with_treatment <- merge(sales_with_blocks, treatment_2015, by = "block_id", all.x = TRUE)
sales_with_treatment <- merge(sales_with_treatment, treatment_2023, by = "block_id", all.x = TRUE)

message(sprintf("Sales with treatment info: %s", format(nrow(sales_with_treatment), big.mark = ",")))

# =============================================================================
# 5. CREATE HEDONIC CONTROL VARIABLES (NO IMPUTATION)
# =============================================================================
message("\n=== CREATING HEDONIC CONTROLS (NO IMPUTATION) ===")

sales_with_treatment[, `:=`(
  # Building age at time of sale
  building_age = sale_year - year_built,

  # Total bathrooms
  baths_total = num_full_baths + 0.5 * fifelse(is.na(num_half_baths), 0, num_half_baths),

  # Garage indicator
  has_garage = as.integer(garage_size > 0 & !is.na(garage_size))
)]

# Fix negative building ages (data errors) - set to NA
sales_with_treatment[building_age < 0, building_age := NA]

# Create log transformations (keep NAs as NAs - no imputation)
sales_with_treatment[, `:=`(
  log_sqft = log(building_sqft),
  log_land_sqft = log(land_sqft),
  log_building_age = log(building_age),
  log_bedrooms = log(num_bedrooms),
  log_baths = log(baths_total)
)]

# Report hedonic coverage
message("\nHedonic variable coverage (% non-missing):")
message(sprintf("  building_sqft: %.1f%%", 100 * mean(!is.na(sales_with_treatment$building_sqft))))
message(sprintf("  land_sqft: %.1f%%", 100 * mean(!is.na(sales_with_treatment$land_sqft))))
message(sprintf("  building_age: %.1f%%", 100 * mean(!is.na(sales_with_treatment$building_age))))
message(sprintf("  num_bedrooms: %.1f%%", 100 * mean(!is.na(sales_with_treatment$num_bedrooms))))
message(sprintf("  baths_total: %.1f%%", 100 * mean(!is.na(sales_with_treatment$baths_total))))
message(sprintf("  has_garage: %.1f%%", 100 * mean(!is.na(sales_with_treatment$has_garage))))

# Report complete cases for core hedonics
core_hedonics_complete <- sales_with_treatment[
  !is.na(log_sqft) & !is.na(log_land_sqft) & !is.na(log_building_age) &
    !is.na(log_bedrooms) & !is.na(log_baths)
]
message(sprintf(
  "\nComplete hedonic cases: %s of %s (%.1f%%)",
  format(nrow(core_hedonics_complete), big.mark = ","),
  format(nrow(sales_with_treatment), big.mark = ","),
  100 * nrow(core_hedonics_complete) / nrow(sales_with_treatment)
))

# =============================================================================
# 6. CREATE STACKED COHORT PANELS
# =============================================================================
message("\n=== CREATING STACKED COHORT PANEL ===")

# 2015 cohort: sales from 2010-2020
cohort_2015 <- sales_with_treatment[
  sale_year >= 2010 & sale_year <= 2020 &
    valid_2015 == TRUE &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) < 1000
][, `:=`(
  cohort = "2015",
  relative_year = sale_year - 2015,
  relative_year_capped = pmax(pmin(sale_year - 2015, 5), -5),
  treat = as.integer(switched_2015),
  strictness_change = strictness_change_2015,
  ward_origin = ward_origin_2015,
  ward_dest = ward_dest_2015
)]

# FIX: For treated blocks, use the boundary they crossed (time-invariant)
# This ensures ward_pair_id doesn't change pre vs post treatment
cohort_2015[
  switched_2015 == TRUE,
  ward_pair_id := paste(pmin(ward_origin_2015, ward_dest_2015),
    pmax(ward_origin_2015, ward_dest_2015),
    sep = "-"
  )
]

# Now construct ward_pair_side from the (potentially fixed) ward_pair_id
cohort_2015[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]

message(sprintf("2015 cohort: %s transactions", format(nrow(cohort_2015), big.mark = ",")))

# 2023 cohort: sales from 2018-2025
cohort_2023 <- sales_with_treatment[
  sale_year >= 2018 & sale_year <= 2025 &
    valid_2023 == TRUE &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) < 1000
][, `:=`(
  cohort = "2023",
  relative_year = sale_year - 2023,
  relative_year_capped = pmax(pmin(sale_year - 2023, 5), -5),
  treat = as.integer(switched_2023),
  strictness_change = strictness_change_2023,
  ward_origin = ward_origin_2023,
  ward_dest = ward_dest_2023
)]

# FIX: For treated blocks, use the boundary they crossed (time-invariant)
cohort_2023[
  switched_2023 == TRUE,
  ward_pair_id := paste(pmin(ward_origin_2023, ward_dest_2023),
    pmax(ward_origin_2023, ward_dest_2023),
    sep = "-"
  )
]

# Now construct ward_pair_side from the (potentially fixed) ward_pair_id
cohort_2023[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]

message(sprintf("2023 cohort: %s transactions", format(nrow(cohort_2023), big.mark = ",")))

# Stack cohorts
stacked_panel <- rbindlist(list(cohort_2015, cohort_2023), fill = TRUE)

# Create cohort-specific identifiers for FEs
stacked_panel[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_")
)]

message(sprintf("\nStacked panel: %s total transactions", format(nrow(stacked_panel), big.mark = ",")))

# =============================================================================
# 7. SELECT FINAL COLUMNS
# =============================================================================
message("\n=== SELECTING FINAL COLUMNS ===")

final_panel <- stacked_panel[, .(
  # Identifiers
  pin,
  block_id,
  cohort,
  cohort_block_id,

  # Time
  sale_date,
  sale_year,
  relative_year,
  relative_year_capped,

  # Outcome
  sale_price,

  # Hedonic controls (log-transformed, NO imputation)
  log_sqft,
  log_land_sqft,
  log_building_age,
  log_bedrooms,
  log_baths,
  has_garage,

  # Raw hedonics (for diagnostics)
  building_sqft,
  land_sqft,
  year_built,
  building_age,
  num_bedrooms,
  num_full_baths,
  baths_total,
  garage_size,
  hedonic_tax_year,
  years_gap,

  # Geography/FEs
  ward,
  ward_pair_id,
  ward_origin,
  ward_pair_side,
  cohort_ward_pair_side,
  signed_dist,

  # Treatment
  treat,
  strictness_change
)]

# =============================================================================
# 8. DIAGNOSTICS
# =============================================================================
message("\n=== FINAL PANEL DIAGNOSTICS ===")

message("\nBy cohort and treatment:")
cohort_treat_stats <- final_panel[, .(
  n_transactions = .N,
  n_blocks = uniqueN(block_id),
  mean_price = as.double(mean(sale_price, na.rm = TRUE)),
  median_price = as.double(median(sale_price, na.rm = TRUE))
), by = .(cohort, treat)]
print(cohort_treat_stats)

message("\nHedonic variable coverage in final panel (% non-missing):")
hedonic_coverage <- final_panel[, .(
  log_sqft = mean(!is.na(log_sqft)) * 100,
  log_land_sqft = mean(!is.na(log_land_sqft)) * 100,
  log_building_age = mean(!is.na(log_building_age)) * 100,
  log_bedrooms = mean(!is.na(log_bedrooms)) * 100,
  log_baths = mean(!is.na(log_baths)) * 100,
  has_garage = mean(!is.na(has_garage)) * 100
)]
print(hedonic_coverage)

message("\nYears gap between sale and hedonic assessment:")
gap_stats <- final_panel[, .(
  mean_gap = mean(years_gap, na.rm = TRUE),
  median_gap = median(years_gap, na.rm = TRUE),
  pct_exact_match = mean(years_gap == 0, na.rm = TRUE) * 100,
  pct_within_2yrs = mean(years_gap <= 2, na.rm = TRUE) * 100
)]
print(gap_stats)

message("\nTransactions by relative year:")
rel_year_counts <- final_panel[, .N, by = .(cohort, relative_year_capped)][order(cohort, relative_year_capped)]
rel_year_wide <- dcast(rel_year_counts, relative_year_capped ~ cohort, value.var = "N")
print(rel_year_wide)

# Complete cases for regression
n_complete <- final_panel[
  !is.na(log_sqft) & !is.na(log_land_sqft) & !is.na(log_building_age) &
    !is.na(log_bedrooms) & !is.na(log_baths) & !is.na(has_garage), .N
]
message(sprintf(
  "\nTransactions with complete hedonics (regression sample): %s (%.1f%%)",
  format(n_complete, big.mark = ","),
  100 * n_complete / nrow(final_panel)
))

# =============================================================================
# 9. SAVE
# =============================================================================
message("\n=== SAVING ===")

write_parquet(final_panel, "../output/sales_transaction_panel.parquet")

message(sprintf(
  "Saved: ../output/sales_transaction_panel.parquet (%s rows)",
  format(nrow(final_panel), big.mark = ",")
))

message("\nDone!")
