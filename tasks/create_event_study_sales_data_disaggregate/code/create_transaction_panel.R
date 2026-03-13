# create_transaction_panel.R
# Creates transaction-level panel for disaggregate sales event study
# Key feature: temporal matching of sales to property characteristics via rolling join
# NO IMPUTATION - missing hedonics remain as NA and will be dropped in regression

source("../../setup_environment/code/packages.R")

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  if (!any(ok)) return(out)
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

assign_cohort_segments_dt <- function(dt, seg_sf, cohort_label, chunk_n = 50000L) {
  if (nrow(dt) == 0) {
    dt[, `:=`(
      segment_id_cohort = NA_character_,
      segment_side = NA_character_,
      cohort_segment = NA_character_,
      cohort_segment_side = NA_character_
    )]
    return(dt)
  }

  dt <- copy(dt)
  pair_dash <- normalize_pair_dash(dt$ward_pair_id)
  seg_id <- rep(NA_character_, nrow(dt))
  valid <- !is.na(pair_dash) & is.finite(dt$longitude) & is.finite(dt$latitude)
  valid_pairs <- intersect(unique(pair_dash[valid]), unique(seg_sf$pair_dash))

  for (j in seq_along(valid_pairs)) {
    pair_j <- valid_pairs[j]
    idx_pair <- which(valid & pair_dash == pair_j)
    seg_pair <- seg_sf[seg_sf$pair_dash == pair_j, ]
    if (length(idx_pair) == 0 || nrow(seg_pair) == 0) next

    starts <- seq(1L, length(idx_pair), by = chunk_n)
    for (s in starts) {
      e <- min(s + chunk_n - 1L, length(idx_pair))
      chunk_idx <- idx_pair[s:e]

      pts <- st_as_sf(
        data.frame(
          longitude = dt$longitude[chunk_idx],
          latitude = dt$latitude[chunk_idx]
        ),
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      if (st_crs(pts) != st_crs(seg_pair)) {
        pts <- st_transform(pts, st_crs(seg_pair))
      }
      hits <- st_within(pts, seg_pair)
      hit_idx <- vapply(hits, function(v) {
        if (length(v) == 0) return(NA_integer_)
        v[1]
      }, integer(1))
      ok <- !is.na(hit_idx)
      if (any(ok)) {
        seg_id[chunk_idx[ok]] <- as.character(seg_pair$segment_id[hit_idx[ok]])
      }
    }

    if (j %% 25L == 0L || j == length(valid_pairs)) {
      message(sprintf("[%s] segment assignment progress: %d/%d pairs", cohort_label, j, length(valid_pairs)))
    }
  }

  dt[, segment_id_cohort := seg_id]
  dt[, segment_side := fifelse(!is.na(segment_id_cohort) & !is.na(ward_origin), paste(segment_id_cohort, ward_origin, sep = "_"), NA_character_)]
  dt[, cohort_segment := fifelse(!is.na(segment_id_cohort), paste(cohort_label, segment_id_cohort, sep = "_"), NA_character_)]
  dt[, cohort_segment_side := fifelse(!is.na(segment_side), paste(cohort_label, segment_side, sep = "_"), NA_character_)]

  message(sprintf(
    "[%s] segment coverage within cohort panel: %.2f%%",
    cohort_label,
    100 * mean(!is.na(dt$segment_id_cohort))
  ))

  dt
}

summarize_event_support_dt <- function(dt, panel_mode, filter_stage) {
  if (nrow(dt) == 0) {
    return(data.table(
      panel_mode = character(),
      filter_stage = character(),
      cohort = character(),
      event_time = integer(),
      n_transactions = integer(),
      n_blocks = integer(),
      n_treated = integer(),
      n_control = integer()
    ))
  }

  out <- dt[, .(
    n_transactions = .N,
    n_blocks = uniqueN(block_id),
    n_treated = sum(treat == 1, na.rm = TRUE),
    n_control = sum(treat == 0, na.rm = TRUE)
  ), by = .(cohort, event_time = relative_year_capped)]
  out[, `:=`(panel_mode = panel_mode, filter_stage = filter_stage)]
  setcolorder(out, c("panel_mode", "filter_stage", "cohort", "event_time", "n_transactions", "n_blocks", "n_treated", "n_control"))
  out[order(panel_mode, filter_stage, cohort, event_time)]
}

summarize_calendar_support_dt <- function(dt, panel_mode) {
  if (nrow(dt) == 0) {
    return(data.table(
      panel_mode = character(),
      cohort = character(),
      year = integer(),
      treat = integer(),
      n_transactions = integer(),
      n_blocks = integer(),
      n_pins = integer()
    ))
  }

  out <- dt[, .(
    n_transactions = .N,
    n_blocks = uniqueN(block_id),
    n_pins = uniqueN(pin)
  ), by = .(cohort, year = sale_year, treat)]
  out[, panel_mode := panel_mode]
  setcolorder(out, c("panel_mode", "cohort", "year", "treat", "n_transactions", "n_blocks", "n_pins"))
  out[order(panel_mode, cohort, year, treat)]
}

summarize_assignment_stability_dt <- function(dt, panel_mode) {
  treated_blocks <- dt[treat == 1, .(
    n_ward_pairs = uniqueN(ward_pair_id),
    n_segments = uniqueN(segment_id_cohort[!is.na(segment_id_cohort)])
  ), by = block_id]

  data.table(
    panel_mode = panel_mode,
    n_rows = nrow(dt),
    n_treated_blocks = nrow(treated_blocks),
    segment_coverage_pct = 100 * mean(!is.na(dt$segment_id_cohort)),
    n_missing_segment_rows = sum(is.na(dt$segment_id_cohort)),
    n_blocks_multi_ward_pair = sum(treated_blocks$n_ward_pairs > 1),
    pct_blocks_multi_ward_pair = 100 * mean(treated_blocks$n_ward_pairs > 1),
    n_blocks_multi_segment = sum(treated_blocks$n_segments > 1),
    pct_blocks_multi_segment = 100 * mean(treated_blocks$n_segments > 1)
  )
}

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

message("Loading segment layers for cohort-baseline assignment...")
segments_2003 <- st_read("../input/boundary_segments_1320ft.gpkg", layer = "2003_2014_bw1000", quiet = TRUE)
segments_2003$pair_dash <- normalize_pair_dash(segments_2003$ward_pair_id)
segments_2003 <- segments_2003[!is.na(segments_2003$pair_dash), ]

segments_2015 <- st_read("../input/boundary_segments_1320ft.gpkg", layer = "2015_2023_bw1000", quiet = TRUE)
segments_2015$pair_dash <- normalize_pair_dash(segments_2015$ward_pair_id)
segments_2015 <- segments_2015[!is.na(segments_2015$pair_dash), ]

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
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
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
# 6. CREATE COHORT PANELS
# =============================================================================

# =============================================================================
# 6a. CREATE 2012 COHORT (Anticipation Analysis)
# =============================================================================
# The 2012 cohort uses the SAME treatment definition as 2015 (which blocks switched
# in the 2015 redistricting), but centers the event study on the announcement date
# (2012) rather than implementation (May 2015).
#
# This allows testing whether market effects began at announcement or implementation.
# Uses sales from 2007-2017 for a balanced 5-year pre/post window around 2012.

message("\n=== CREATING 2012 COHORT (Anticipation) ===")

cohort_2012_window <- copy(sales_with_treatment[
  sale_year >= 2007 & sale_year <= 2017 &
    !is.na(ward) &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) <= 2000
])
cohort_2012_window[, `:=`(
  cohort = "2012",
  relative_year = sale_year - 2012,
  relative_year_capped = pmax(pmin(sale_year - 2012, 5), -5),
  treat = as.integer(switched_2015)
)]

cohort_2012_nonmissing <- cohort_2012_window[!is.na(switched_2015)]
cohort_2012_valid <- cohort_2012_nonmissing[valid_2015 == TRUE]
cohort_2012_pre_segment <- copy(cohort_2012_valid)
cohort_2012_pre_segment[, `:=`(
  strictness_change = strictness_change_2015,
  ward_origin = fifelse(switched_2015 == TRUE, ward_origin_2015, ward),
  ward_dest = fifelse(switched_2015 == TRUE, ward_dest_2015, ward),
  dist_ft = abs(as.numeric(signed_dist))
)]
cohort_2012_pre_segment[
  switched_2015 == TRUE,
  ward_pair_id := paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-")
]
cohort_2012_pre_segment[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]
cohort_2012 <- assign_cohort_segments_dt(cohort_2012_pre_segment, segments_2003, "2012")

message(sprintf("2012 cohort: %s transactions", format(nrow(cohort_2012), big.mark = ",")))
message(sprintf("  Year range: %d to %d", min(cohort_2012$sale_year), max(cohort_2012$sale_year)))
message(sprintf("  Treated transactions: %s", format(sum(cohort_2012$treat == 1), big.mark = ",")))
message(sprintf("  Control transactions: %s", format(sum(cohort_2012$treat == 0), big.mark = ",")))

# =============================================================================
# 6b. CREATE 2022 COHORT (Anticipation for 2023 Redistricting)
# =============================================================================
# The 2022 cohort uses the SAME treatment definition as 2023 (which blocks switched
# in the 2023 redistricting), but centers the event study on the announcement date
# (2022) rather than implementation (May 2023).
#
# Uses sales from 2017-2025 for pre/post window around 2022.

message("\n=== CREATING 2022 COHORT (Anticipation for 2023 Redistricting) ===")

cohort_2022_window <- copy(sales_with_treatment[
  sale_year >= 2017 & sale_year <= 2025 &
    !is.na(ward) &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) <= 2000
])
cohort_2022_window[, `:=`(
  cohort = "2022",
  relative_year = sale_year - 2022,
  relative_year_capped = pmax(pmin(sale_year - 2022, 5), -5),
  treat = as.integer(switched_2023)
)]

cohort_2022_nonmissing <- cohort_2022_window[!is.na(switched_2023)]
cohort_2022_valid <- cohort_2022_nonmissing[valid_2023 == TRUE]
cohort_2022_pre_segment <- copy(cohort_2022_valid)
cohort_2022_pre_segment[, `:=`(
  strictness_change = strictness_change_2023,
  ward_origin = fifelse(switched_2023 == TRUE, ward_origin_2023, ward),
  ward_dest = fifelse(switched_2023 == TRUE, ward_dest_2023, ward),
  dist_ft = abs(as.numeric(signed_dist))
)]
cohort_2022_pre_segment[
  switched_2023 == TRUE,
  ward_pair_id := paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-")
]
cohort_2022_pre_segment[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]
cohort_2022 <- assign_cohort_segments_dt(cohort_2022_pre_segment, segments_2015, "2022")

message(sprintf("2022 cohort: %s transactions", format(nrow(cohort_2022), big.mark = ",")))
message(sprintf("  Year range: %d to %d", min(cohort_2022$sale_year), max(cohort_2022$sale_year)))
message(sprintf("  Treated transactions: %s", format(sum(cohort_2022$treat == 1), big.mark = ",")))
message(sprintf("  Control transactions: %s", format(sum(cohort_2022$treat == 0), big.mark = ",")))

# =============================================================================
# 6c. CREATE 2015 COHORT (Implementation)
# =============================================================================
# The 2015 cohort centers on the actual implementation date (May 2015).
# Uses sales from 2010-2020.

message("\n=== CREATING 2015 COHORT (Implementation) ===")

# 2015 cohort: sales from 2010-2020
cohort_2015_window <- copy(sales_with_treatment[
  sale_year >= 2010 & sale_year <= 2020 &
    !is.na(ward) &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) <= 2000
])
cohort_2015_window[, `:=`(
  cohort = "2015",
  relative_year = sale_year - 2015,
  relative_year_capped = pmax(pmin(sale_year - 2015, 5), -5),
  treat = as.integer(switched_2015)
)]

cohort_2015_nonmissing <- cohort_2015_window[!is.na(switched_2015)]
cohort_2015_valid <- cohort_2015_nonmissing[valid_2015 == TRUE]
cohort_2015_pre_segment <- copy(cohort_2015_valid)
cohort_2015_pre_segment[, `:=`(
  strictness_change = strictness_change_2015,
  ward_origin = fifelse(switched_2015 == TRUE, ward_origin_2015, ward),
  ward_dest = fifelse(switched_2015 == TRUE, ward_dest_2015, ward),
  dist_ft = abs(as.numeric(signed_dist))
)]
cohort_2015_pre_segment[
  switched_2015 == TRUE,
  ward_pair_id := paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-")
]
cohort_2015_pre_segment[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]
cohort_2015 <- assign_cohort_segments_dt(cohort_2015_pre_segment, segments_2003, "2015")

message(sprintf("2015 cohort: %s transactions", format(nrow(cohort_2015), big.mark = ",")))
# =============================================================================
# 6d. CREATE 2023 COHORT
# =============================================================================
message("\n=== CREATING 2023 COHORT ===")

# 2023 cohort: sales from 2018-2025
cohort_2023_window <- copy(sales_with_treatment[
  sale_year >= 2018 & sale_year <= 2025 &
    !is.na(ward) &
    !is.na(ward_pair_id) &
    abs(as.numeric(signed_dist)) <= 2000
])
cohort_2023_window[, `:=`(
  cohort = "2023",
  relative_year = sale_year - 2023,
  relative_year_capped = pmax(pmin(sale_year - 2023, 5), -5),
  treat = as.integer(switched_2023)
)]

cohort_2023_nonmissing <- cohort_2023_window[!is.na(switched_2023)]
cohort_2023_valid <- cohort_2023_nonmissing[valid_2023 == TRUE]
cohort_2023_pre_segment <- copy(cohort_2023_valid)
cohort_2023_pre_segment[, `:=`(
  strictness_change = strictness_change_2023,
  ward_origin = fifelse(switched_2023 == TRUE, ward_origin_2023, ward),
  ward_dest = fifelse(switched_2023 == TRUE, ward_dest_2023, ward),
  dist_ft = abs(as.numeric(signed_dist))
)]
cohort_2023_pre_segment[
  switched_2023 == TRUE,
  ward_pair_id := paste(pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest), sep = "-")
]
cohort_2023_pre_segment[, ward_pair_side := paste(ward_pair_id, ward_origin, sep = "_")]
cohort_2023 <- assign_cohort_segments_dt(cohort_2023_pre_segment, segments_2015, "2023")

message(sprintf("2023 cohort: %s transactions", format(nrow(cohort_2023), big.mark = ",")))

# Add cohort-specific identifiers for all cohorts
cohort_2012[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]

cohort_2022[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]

cohort_2015[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]

cohort_2023[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]

# =============================================================================
# 7. CREATE STACKED PANELS
# =============================================================================
message("\n=== CREATING STACKED PANELS ===")

# Announcement timing: 2012 + 2022
stacked_announcement <- rbindlist(list(cohort_2012, cohort_2022), fill = TRUE)
stacked_announcement[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]
message(sprintf(
  "Stacked (announcement): %s transactions (2012 + 2022)",
  format(nrow(stacked_announcement), big.mark = ",")
))

# Implementation timing: 2015 + 2023
stacked_implementation <- rbindlist(list(cohort_2015, cohort_2023), fill = TRUE)
stacked_implementation[, `:=`(
  cohort_block_id = paste(cohort, block_id, sep = "_"),
  cohort_ward_pair_side = paste(cohort, ward_pair_side, sep = "_"),
  cohort_segment = fifelse(!is.na(segment_id_cohort), paste(cohort, segment_id_cohort, sep = "_"), NA_character_),
  cohort_segment_side = fifelse(!is.na(segment_side), paste(cohort, segment_side, sep = "_"), NA_character_)
)]
message(sprintf(
  "Stacked (implementation): %s transactions (2015 + 2023)",
  format(nrow(stacked_implementation), big.mark = ",")
))

# =============================================================================
# 8. SELECT FINAL COLUMNS
# =============================================================================
message("\n=== SELECTING FINAL COLUMNS ===")

# Define column selection function to avoid repetition
select_final_cols <- function(dt) {
  dt[, .(
    pin, block_id, cohort, cohort_block_id,
    sale_date, sale_year, relative_year, relative_year_capped,
    sale_price,
    log_sqft, log_land_sqft, log_building_age, log_bedrooms, log_baths, has_garage,
    building_sqft, land_sqft, year_built, building_age, num_bedrooms,
    num_full_baths, baths_total, garage_size, hedonic_tax_year, years_gap,
    ward, ward_pair_id, ward_origin, ward_pair_side, cohort_ward_pair_side,
    segment_id_cohort, segment_side, cohort_segment, cohort_segment_side,
    signed_dist, dist_ft,
    treat, strictness_change
  )]
}

# Apply to all panels
cohort_2012_final <- select_final_cols(cohort_2012)
cohort_2022_final <- select_final_cols(cohort_2022)
cohort_2015_final <- select_final_cols(cohort_2015)
cohort_2023_final <- select_final_cols(cohort_2023)
stacked_announcement_final <- select_final_cols(stacked_announcement)
stacked_implementation_final <- select_final_cols(stacked_implementation)

# Use implementation as "final_panel" for backwards compatibility with diagnostics
final_panel <- stacked_implementation_final

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

sales_support_by_event_time <- rbindlist(list(
  summarize_event_support_dt(cohort_2012_window, "cohort_2012", "post_time_and_geo_filter"),
  summarize_event_support_dt(cohort_2012_nonmissing, "cohort_2012", "post_drop_missing_treatment"),
  summarize_event_support_dt(cohort_2012_valid, "cohort_2012", "post_valid_controls"),
  summarize_event_support_dt(cohort_2012_final, "cohort_2012", "final_assigned_segments"),
  summarize_event_support_dt(cohort_2022_window, "cohort_2022", "post_time_and_geo_filter"),
  summarize_event_support_dt(cohort_2022_nonmissing, "cohort_2022", "post_drop_missing_treatment"),
  summarize_event_support_dt(cohort_2022_valid, "cohort_2022", "post_valid_controls"),
  summarize_event_support_dt(cohort_2022_final, "cohort_2022", "final_assigned_segments"),
  summarize_event_support_dt(cohort_2015_window, "cohort_2015", "post_time_and_geo_filter"),
  summarize_event_support_dt(cohort_2015_nonmissing, "cohort_2015", "post_drop_missing_treatment"),
  summarize_event_support_dt(cohort_2015_valid, "cohort_2015", "post_valid_controls"),
  summarize_event_support_dt(cohort_2015_final, "cohort_2015", "final_assigned_segments"),
  summarize_event_support_dt(cohort_2023_window, "cohort_2023", "post_time_and_geo_filter"),
  summarize_event_support_dt(cohort_2023_nonmissing, "cohort_2023", "post_drop_missing_treatment"),
  summarize_event_support_dt(cohort_2023_valid, "cohort_2023", "post_valid_controls"),
  summarize_event_support_dt(cohort_2023_final, "cohort_2023", "final_assigned_segments"),
  summarize_event_support_dt(stacked_announcement_final, "stacked_announcement", "final_assigned_segments"),
  summarize_event_support_dt(stacked_implementation_final, "stacked_implementation", "final_assigned_segments")
), fill = TRUE)

sales_support_by_calendar_time <- rbindlist(list(
  summarize_calendar_support_dt(cohort_2012_final, "cohort_2012"),
  summarize_calendar_support_dt(cohort_2022_final, "cohort_2022"),
  summarize_calendar_support_dt(cohort_2015_final, "cohort_2015"),
  summarize_calendar_support_dt(cohort_2023_final, "cohort_2023"),
  summarize_calendar_support_dt(stacked_announcement_final, "stacked_announcement"),
  summarize_calendar_support_dt(stacked_implementation_final, "stacked_implementation")
), fill = TRUE)

sales_assignment_stability <- rbindlist(list(
  summarize_assignment_stability_dt(cohort_2012_final, "cohort_2012"),
  summarize_assignment_stability_dt(cohort_2022_final, "cohort_2022"),
  summarize_assignment_stability_dt(cohort_2015_final, "cohort_2015"),
  summarize_assignment_stability_dt(cohort_2023_final, "cohort_2023"),
  summarize_assignment_stability_dt(stacked_announcement_final, "stacked_announcement"),
  summarize_assignment_stability_dt(stacked_implementation_final, "stacked_implementation")
), fill = TRUE)

# =============================================================================
# 10. SAVE ALL PANELS
# =============================================================================
message("\n=== SAVING ===")

# Stacked panels
write_parquet(stacked_implementation_final, "../output/sales_transaction_panel.parquet")
message(sprintf(
  "Saved stacked (implementation 2015+2023): %s rows",
  format(nrow(stacked_implementation_final), big.mark = ",")
))

write_parquet(stacked_announcement_final, "../output/sales_transaction_panel_announcement.parquet")
message(sprintf(
  "Saved stacked (announcement 2012+2022): %s rows",
  format(nrow(stacked_announcement_final), big.mark = ",")
))

# Individual cohort panels (for unstacked analysis)
write_parquet(cohort_2012_final, "../output/sales_transaction_panel_2012.parquet")
message(sprintf("Saved 2012 cohort: %s rows", format(nrow(cohort_2012_final), big.mark = ",")))

write_parquet(cohort_2022_final, "../output/sales_transaction_panel_2022.parquet")
message(sprintf("Saved 2022 cohort: %s rows", format(nrow(cohort_2022_final), big.mark = ",")))

write_parquet(cohort_2015_final, "../output/sales_transaction_panel_2015.parquet")
message(sprintf("Saved 2015 cohort: %s rows", format(nrow(cohort_2015_final), big.mark = ",")))

write_parquet(cohort_2023_final, "../output/sales_transaction_panel_2023.parquet")
message(sprintf("Saved 2023 cohort: %s rows", format(nrow(cohort_2023_final), big.mark = ",")))

write_csv(as_tibble(sales_support_by_event_time), "../output/sales_transaction_panel_support_by_event_time.csv")
message("Saved sales event-time support diagnostics")

write_csv(as_tibble(sales_support_by_calendar_time), "../output/sales_transaction_panel_support_by_calendar_time.csv")
message("Saved sales calendar-time support diagnostics")

write_csv(as_tibble(sales_assignment_stability), "../output/sales_transaction_panel_assignment_stability.csv")
message("Saved sales assignment-stability diagnostics")

message("\nDone!")
