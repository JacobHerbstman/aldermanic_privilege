# create_transaction_panel.R
# Creates transaction-level panel for disaggregate sales event study
# Key feature: temporal matching of sales to property characteristics via rolling join
# NO IMPUTATION - missing hedonics remain as NA and will be dropped in regression

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")
source("../../_lib/amenity_distance_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_event_study_sales_data_disaggregate/code")
# segment_buffer_m <- 304.8
# panel_max_distance_m <- 800

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_buffer_m, panel_max_distance_m)
}

if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <segment_buffer_m> <panel_max_distance_m>", call. = FALSE)
}
segment_buffer_m <- as.numeric(cli_args[1])
panel_max_distance_m <- as.numeric(cli_args[2])
if (!is.finite(segment_buffer_m) || segment_buffer_m <= 0) {
  stop("segment_buffer_m must be positive.", call. = FALSE)
}
if (!is.finite(panel_max_distance_m) || panel_max_distance_m <= 0) {
  stop("panel_max_distance_m must be positive.", call. = FALSE)
}
crs_projected <- 3435

assign_cohort_segments_dt <- function(dt, segment_layers, era_label, cohort_label, chunk_n = 50000L) {
  if (nrow(dt) == 0) {
    dt[, `:=`(
      segment_id_cohort = NA_character_,
      segment_side = NA_character_,
      cohort_segment = NA_character_,
      cohort_segment_side = NA_character_,
      segment_length_ft_cohort = NA_real_,
      segment_lt500ft_cohort = NA,
      segment_lt1000ft_cohort = NA
    )]
    return(dt)
  }

  dt <- copy(dt)
  segment_meta <- segment_metadata_from_layers(segment_layers)
  pts <- st_as_sf(
    data.frame(longitude = dt$longitude, latitude = dt$latitude),
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  seg_id <- assign_points_to_nearest_segments(
    points_sf = pts,
    era_values = rep(era_label, nrow(dt)),
    pair_values = dt$ward_pair_id,
    segment_layers = segment_layers,
    max_distance = units::set_units(segment_buffer_m, "m"),
    chunk_n = chunk_n
  )

  assert_event_segment_contract(
    points_sf = pts,
    era_values = rep(era_label, nrow(dt)),
    pair_values = dt$ward_pair_id,
    segment_layers = segment_layers,
    segment_id = seg_id,
    boundary_dist_m = dt$dist_m,
    max_distance_m = segment_buffer_m,
    context = sprintf("sales %s", cohort_label),
    chunk_n = chunk_n
  )

  segment_match <- match(
    paste(era_label, seg_id, sep = "\r"),
    paste(segment_meta$era, segment_meta$segment_id, sep = "\r")
  )

  dt[, segment_id_cohort := seg_id]
  dt[, segment_length_ft_cohort := segment_meta$segment_length_ft[segment_match]]
  dt[, segment_lt500ft_cohort := segment_meta$segment_lt500ft[segment_match]]
  dt[, segment_lt1000ft_cohort := segment_meta$segment_lt1000ft[segment_match]]
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

add_event_geometry_dt <- function(dt, era_label, switched_col, origin_col, dest_col, cohort_label) {
  if (nrow(dt) == 0) {
    return(dt)
  }

  dt <- copy(dt)
  pts <- st_as_sf(
    data.frame(longitude = dt$longitude, latitude = dt$latitude),
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  control_assign <- assign_points_to_boundaries(
    points_sf = pts,
    era_values = rep(era_label, nrow(dt)),
    ward_maps = ward_maps,
    boundary_lines = boundary_layers,
    chunk_n = 5000L
  )

  switched <- dt[[switched_col]] == TRUE
  origin_ward <- suppressWarnings(as.integer(dt[[origin_col]]))
  dest_ward <- suppressWarnings(as.integer(dt[[dest_col]]))
  control_pair <- normalize_pair_dash(control_assign$ward_pair_id)
  switched_pair <- normalize_pair_id(origin_ward, dest_ward, sep = "-")
  event_pair <- ifelse(switched, switched_pair, control_pair)

  event_dist_m <- distance_to_boundary_pair_m(
    points_sf = pts,
    pair_values = event_pair,
    boundary_sf = boundary_layers[[era_label]],
    chunk_n = 5000L
  )

  dt[, `:=`(
    pre_score_ward = ward,
    pre_score_ward_pair_id = normalize_pair_dash(ward_pair_id),
    pre_score_dist_m = dist_m,
    pre_score_signed_dist_m = signed_dist_m,
    nearest_pre_boundary_ward = control_assign$ward,
    nearest_pre_boundary_neighbor_ward = control_assign$neighbor_ward,
    nearest_pre_boundary_pair_id = control_pair,
    nearest_pre_boundary_dist_m = control_assign$dist_m,
    nearest_pre_boundary_dist_ft = control_assign$dist_ft,
    event_control_ward = control_assign$ward,
    event_control_neighbor_ward = control_assign$neighbor_ward,
    event_dist_missing = is.na(event_dist_m) & !is.na(event_pair),
    event_point_origin_mismatch = !is.na(control_assign$ward) &
      !is.na(origin_ward) &
      control_assign$ward != origin_ward
  )]
  dt[, `:=`(
    ward_origin = origin_ward,
    ward_dest = dest_ward,
    event_neighbor_ward = fifelse(switched, dest_ward, control_assign$neighbor_ward),
    ward_pair_id = event_pair,
    dist_m = event_dist_m
  )]
  dt[, ward := ward_origin]
  dt[, ward_pair_side := fifelse(
    !is.na(ward_pair_id) & !is.na(ward_origin),
    paste(ward_pair_id, ward_origin, sep = "_"),
    NA_character_
  )]

  missing_switched <- dt[switched == TRUE & event_dist_missing == TRUE, .N]
  if (missing_switched > 0) {
    message(sprintf(
      "[%s] %s switched transactions have no pre-era origin-destination boundary and will be excluded.",
      cohort_label,
      format(missing_switched, big.mark = ",")
    ))
  }

  dt
}

summarize_event_geometry_dt <- function(dt, panel_mode, stage_label) {
  if (nrow(dt) == 0 || !"pre_score_dist_m" %in% names(dt)) {
    return(data.table())
  }

  dt[, .(
    rows = .N,
    blocks = uniqueN(block_id),
    old_pair_differs_from_event_pair = sum(
      normalize_pair_dash(pre_score_ward_pair_id) != normalize_pair_dash(ward_pair_id),
      na.rm = TRUE
    ),
    missing_event_dist = sum(is.na(dist_m)),
    point_origin_mismatch = sum(event_point_origin_mismatch, na.rm = TRUE),
    old_le_1000ft = sum(pre_score_dist_m <= 304.8, na.rm = TRUE),
    event_le_1000ft = sum(dist_m <= 304.8, na.rm = TRUE),
    old_event_1000ft_disagree = sum(
      (pre_score_dist_m <= 304.8) != (dist_m <= 304.8),
      na.rm = TRUE
    ),
    p50_abs_dist_change_m = as.numeric(quantile(abs(pre_score_dist_m - dist_m), 0.50, na.rm = TRUE)),
    p90_abs_dist_change_m = as.numeric(quantile(abs(pre_score_dist_m - dist_m), 0.90, na.rm = TRUE)),
    p99_abs_dist_change_m = as.numeric(quantile(abs(pre_score_dist_m - dist_m), 0.99, na.rm = TRUE)),
    max_abs_dist_change_m = max(abs(pre_score_dist_m - dist_m), na.rm = TRUE),
    panel_mode = panel_mode,
    stage = stage_label
  ), by = .(cohort, treat)]
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
  sale_year = year(as.Date(sale_date)),
  ward_pair_id = normalize_pair_dash(ward_pair_id)
)]
if (anyNA(sales$ward_pair_id)) {
  stop("Sales input contains invalid ward_pair_id values after normalization.", call. = FALSE)
}
message(sprintf("Loaded %s sales", format(nrow(sales), big.mark = ",")))

message("Loading residential improvements panel...")
improvements <- read_parquet("../input/residential_improvements_panel.parquet")
setDT(improvements)
message(sprintf(
  "Loaded %s improvement records for %s unique PINs",
  format(nrow(improvements), big.mark = ","),
  format(uniqueN(improvements$pin), big.mark = ",")
))

n_pre_1999_improvements <- sum(!is.na(improvements$year_built) & improvements$year_built < 1999)
message(sprintf(
  "Pre-1999 improvement records available for sales hedonics: %s",
  format(n_pre_1999_improvements, big.mark = ",")
))
if (n_pre_1999_improvements == 0) {
  stop("Sales hedonics input has no pre-1999 improvement records; this is likely the new-construction-only panel.", call. = FALSE)
}

message("Loading census blocks...")
# Disable S2 spherical geometry to avoid validation errors on some blocks
sf_use_s2(FALSE)
census_blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_transform(crs_projected) %>%
  st_make_valid() %>%
  rename(block_id_2010 = GEOID10) %>%
  mutate(block_id_2010 = as.character(block_id_2010)) %>%
  distinct(block_id_2010, .keep_all = TRUE)

census_blocks_2020 <- read_csv("../input/census_blocks_2020.csv", show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_transform(crs_projected) %>%
  st_make_valid() %>%
  rename(block_id_2020 = GEOID20) %>%
  mutate(block_id_2020 = as.character(block_id_2020)) %>%
  distinct(block_id_2020, .keep_all = TRUE)

message("Loading treatment panel...")
treatment_panel <- fread("../input/block_treatment_panel.csv")
treatment_panel[, block_id := as.character(block_id)]

message("Loading cohort geometry layers...")
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_layers <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

message(sprintf("Loading segment lines for %.0fm nearest-segment assignment...", segment_buffer_m))
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")

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
  mutate(sale_row_id = row_number()) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(census_blocks_2010))

message(sprintf("Sales with valid coordinates: %s", format(nrow(sales_sf), big.mark = ",")))

# Spatial join to get 2010 and 2020 block IDs. Each sale should hit at most one block per vintage.
sales_with_blocks_2010 <- st_join(
  sales_sf,
  census_blocks_2010 %>% select(block_id_2010),
  join = st_within
) %>%
  st_drop_geometry()

sales_with_blocks_2020 <- st_join(
  st_transform(sales_sf, st_crs(census_blocks_2020)),
  census_blocks_2020 %>% select(block_id_2020),
  join = st_within
) %>%
  st_drop_geometry() %>%
  select(sale_row_id, block_id_2020)

if (anyDuplicated(sales_with_blocks_2010$sale_row_id) > 0) {
  stop("2010 block assignment produced duplicate sale_row_id values; investigate overlapping blocks.", call. = FALSE)
}
if (anyDuplicated(sales_with_blocks_2020$sale_row_id) > 0) {
  stop("2020 block assignment produced duplicate sale_row_id values; investigate overlapping blocks.", call. = FALSE)
}

sales_with_blocks <- sales_with_blocks_2010 %>%
  left_join(sales_with_blocks_2020, by = "sale_row_id", relationship = "one-to-one")

message(sprintf("Sales assigned to 2010 blocks: %s", format(sum(!is.na(sales_with_blocks$block_id_2010)), big.mark = ",")))
message(sprintf("Sales assigned to 2020 blocks: %s", format(sum(!is.na(sales_with_blocks$block_id_2020)), big.mark = ",")))

# Convert back to data.table for efficiency
setDT(sales_with_blocks)

# =============================================================================
# 4. MERGE TREATMENT INFORMATION
# =============================================================================
message("\n=== MERGING TREATMENT INFO ===")

# 2015 cohort treatment info
treatment_2015 <- treatment_panel[cohort == "2015", .(
  block_id_2010 = block_id,
  ward_origin_2015 = ward_origin,
  ward_dest_2015 = ward_dest,
  switched_2015 = switched,
  strictness_change_2015 = strictness_change,
  valid_2015 = valid
)]
if (anyDuplicated(treatment_2015$block_id_2010) > 0) {
  stop("2015 treatment input must be unique by 2010 block.", call. = FALSE)
}

# 2023 cohort treatment info
treatment_2023 <- treatment_panel[cohort == "2023", .(
  block_id_2020 = block_id,
  ward_origin_2023 = ward_origin,
  ward_dest_2023 = ward_dest,
  switched_2023 = switched,
  strictness_change_2023 = strictness_change,
  valid_2023 = valid
)]
if (anyDuplicated(treatment_2023$block_id_2020) > 0) {
  stop("2023 treatment input must be unique by 2020 block.", call. = FALSE)
}

# Merge treatment info
n_sales_before_treatment_merge <- nrow(sales_with_blocks)
sales_with_treatment <- merge(sales_with_blocks, treatment_2015, by = "block_id_2010", all.x = TRUE)
sales_with_treatment <- merge(sales_with_treatment, treatment_2023, by = "block_id_2020", all.x = TRUE)
if (nrow(sales_with_treatment) != n_sales_before_treatment_merge) {
  stop("Sales treatment merge changed the row count.", call. = FALSE)
}

message(sprintf("Sales with treatment info: %s", format(nrow(sales_with_treatment), big.mark = ",")))

if (!"signed_dist_m" %in% names(sales_with_treatment) || !"dist_m" %in% names(sales_with_treatment)) {
  stop("Sales treatment input must include meter-native ward-boundary distances.", call. = FALSE)
}

# =============================================================================
# 5. CREATE AMENITY DISTANCE CONTROLS
# =============================================================================
message("\n=== CREATING AMENITY DISTANCE CONTROLS ===")

amenity_coordinates <- build_unique_coordinate_amenity_table(
  as_tibble(sales_with_treatment),
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  distance_units = "meters"
)

sales_with_treatment <- append_amenity_distances(
  sales_with_treatment,
  amenity_coordinates,
  "longitude",
  "latitude"
)

sales_amenity_diagnostics <- amenity_distance_diagnostics(
  sales_with_treatment,
  amenity_coordinates,
  "sales_event_study"
)

message("\nAmenity distance coverage (% non-missing):")
message(sprintf("  nearest_school_dist_m: %.1f%%", 100 * mean(!is.na(sales_with_treatment$nearest_school_dist_m))))
message(sprintf("  nearest_park_dist_m: %.1f%%", 100 * mean(!is.na(sales_with_treatment$nearest_park_dist_m))))
message(sprintf("  nearest_major_road_dist_m: %.1f%%", 100 * mean(!is.na(sales_with_treatment$nearest_major_road_dist_m))))
message(sprintf("  lake_michigan_dist_m: %.1f%%", 100 * mean(!is.na(sales_with_treatment$lake_michigan_dist_m))))

# =============================================================================
# 6. CREATE HEDONIC CONTROL VARIABLES (NO IMPUTATION)
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
  log_sqft = NA_real_,
  log_land_sqft = NA_real_,
  log_building_age = NA_real_,
  log_bedrooms = NA_real_,
  log_baths = NA_real_
)]
sales_with_treatment[!is.na(building_sqft) & building_sqft > 0, log_sqft := log(building_sqft)]
sales_with_treatment[!is.na(land_sqft) & land_sqft > 0, log_land_sqft := log(land_sqft)]
sales_with_treatment[!is.na(building_age) & building_age > 0, log_building_age := log(building_age)]
sales_with_treatment[!is.na(num_bedrooms) & num_bedrooms > 0, log_bedrooms := log(num_bedrooms)]
sales_with_treatment[!is.na(baths_total) & baths_total > 0, log_baths := log(baths_total)]

# Report hedonic coverage
message("\nHedonic variable coverage (% non-missing):")
message(sprintf("  building_sqft: %.1f%%", 100 * mean(!is.na(sales_with_treatment$building_sqft))))
message(sprintf("  land_sqft: %.1f%%", 100 * mean(!is.na(sales_with_treatment$land_sqft))))
message(sprintf("  building_age: %.1f%%", 100 * mean(!is.na(sales_with_treatment$building_age))))
message(sprintf("  num_bedrooms: %.1f%%", 100 * mean(!is.na(sales_with_treatment$num_bedrooms))))
message(sprintf("  baths_total: %.1f%%", 100 * mean(!is.na(sales_with_treatment$baths_total))))
message(sprintf("  has_garage: %.1f%%", 100 * mean(!is.na(sales_with_treatment$has_garage))))

# Report complete cases across the full sales universe before event-study filters.
core_hedonics_complete <- sales_with_treatment[
  is.finite(log_sqft) & is.finite(log_land_sqft) & is.finite(log_building_age) &
    is.finite(log_bedrooms) & is.finite(log_baths)
]
core_building_hedonics_rate <- mean(
  is.finite(sales_with_treatment$log_sqft) &
    is.finite(sales_with_treatment$log_building_age) &
    is.finite(sales_with_treatment$log_bedrooms) &
    is.finite(sales_with_treatment$log_baths)
)
core_hedonics_complete_rate <- nrow(core_hedonics_complete) / nrow(sales_with_treatment)
pre_1999_matched_sales <- sum(!is.na(sales_with_treatment$year_built) & sales_with_treatment$year_built < 1999)
message(sprintf(
  "\nComplete hedonic cases: %s of %s (%.1f%%)",
  format(nrow(core_hedonics_complete), big.mark = ","),
  format(nrow(sales_with_treatment), big.mark = ","),
  100 * core_hedonics_complete_rate
))
message(sprintf(
  "Sales matched to pre-1999 buildings: %s",
  format(pre_1999_matched_sales, big.mark = ",")
))
message(sprintf(
  "Core building-hedonic coverage excluding land_sqft: %.1f%%",
  100 * core_building_hedonics_rate
))

sales_hedonic_coverage_by_sale_year <- sales_with_treatment[, .(
  n_sales = .N,
  building_sqft = mean(!is.na(building_sqft)) * 100,
  land_sqft = mean(!is.na(land_sqft)) * 100,
  building_age = mean(!is.na(building_age)) * 100,
  num_bedrooms = mean(!is.na(num_bedrooms)) * 100,
  baths_total = mean(!is.na(baths_total)) * 100,
  complete_building_hedonics = mean(
    is.finite(log_sqft) & is.finite(log_building_age) &
      is.finite(log_bedrooms) & is.finite(log_baths)
  ) * 100,
  complete_hedonics_with_land = mean(
    is.finite(log_sqft) & is.finite(log_land_sqft) & is.finite(log_building_age) &
      is.finite(log_bedrooms) & is.finite(log_baths)
  ) * 100
), by = sale_year][order(sale_year)]

message("\nAll-sales hedonic coverage by sale year (% non-missing):")
print(sales_hedonic_coverage_by_sale_year)

if (pre_1999_matched_sales == 0) {
  stop("Sales hedonics merge has no pre-1999 matched buildings; check the residential improvements input.", call. = FALSE)
}
if (core_building_hedonics_rate < 0.90) {
  stop(sprintf(
    "Core building-hedonic coverage is %.1f%%, below the 90%% guardrail. Check that the all-buildings residential improvements panel is wired in.",
    100 * core_building_hedonics_rate
  ), call. = FALSE)
}

# =============================================================================
# 7. CREATE COHORT PANELS
# =============================================================================

# =============================================================================
# 7a. CREATE 2012 COHORT (Anticipation Analysis)
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
    !is.na(block_id_2010) &
    !is.na(longitude) &
    !is.na(latitude)
])
cohort_2012_window[, `:=`(
  cohort = "2012",
  block_id = block_id_2010,
  relative_year = sale_year - 2012,
  relative_year_capped = pmax(pmin(sale_year - 2012, 5), -5),
  treat = as.integer(switched_2015)
)]

cohort_2012_nonmissing <- cohort_2012_window[!is.na(switched_2015)]
cohort_2012_valid <- cohort_2012_nonmissing[valid_2015 == TRUE]
cohort_2012_pre_segment <- copy(cohort_2012_valid)
cohort_2012_pre_segment[, `:=`(
  block_id = block_id_2010,
  strictness_change = strictness_change_2015,
  treatment_continuous = strictness_change_2015
)]
cohort_2012_pre_segment <- add_event_geometry_dt(
  cohort_2012_pre_segment,
  "2003_2014",
  "switched_2015",
  "ward_origin_2015",
  "ward_dest_2015",
  "2012"
)
cohort_2012_event_geometry <- copy(cohort_2012_pre_segment)
cohort_2012_pre_segment <- cohort_2012_event_geometry[
  !is.na(ward_pair_id) &
    !is.na(ward_origin) &
    !is.na(dist_m) &
    event_point_origin_mismatch == FALSE &
    dist_m <= panel_max_distance_m
]
cohort_2012 <- assign_cohort_segments_dt(cohort_2012_pre_segment, segment_layers, "2003_2014", "2012")
if (any(grepl("_", cohort_2012$ward_pair_id, fixed = TRUE, useBytes = TRUE))) {
  stop("2012 cohort still contains underscore-form ward_pair_id values after normalization.", call. = FALSE)
}

message(sprintf("2012 cohort: %s transactions", format(nrow(cohort_2012), big.mark = ",")))
message(sprintf("  Year range: %d to %d", min(cohort_2012$sale_year), max(cohort_2012$sale_year)))
message(sprintf("  Treated transactions: %s", format(sum(cohort_2012$treat == 1), big.mark = ",")))
message(sprintf("  Control transactions: %s", format(sum(cohort_2012$treat == 0), big.mark = ",")))

# =============================================================================
# 7b. CREATE 2022 COHORT (Anticipation for 2023 Redistricting)
# =============================================================================
# The 2022 cohort uses the SAME treatment definition as 2023 (which blocks switched
# in the 2023 redistricting), but centers the event study on the announcement date
# (2022) rather than implementation (May 2023).
#
# Uses sales from 2017-2025 for pre/post window around 2022.

message("\n=== CREATING 2022 COHORT (Anticipation for 2023 Redistricting) ===")

cohort_2022_window <- copy(sales_with_treatment[
  sale_year >= 2017 & sale_year <= 2025 &
    !is.na(block_id_2020) &
    !is.na(longitude) &
    !is.na(latitude)
])
cohort_2022_window[, `:=`(
  cohort = "2022",
  block_id = block_id_2020,
  relative_year = sale_year - 2022,
  relative_year_capped = pmax(pmin(sale_year - 2022, 5), -5),
  treat = as.integer(switched_2023)
)]

cohort_2022_nonmissing <- cohort_2022_window[!is.na(switched_2023)]
cohort_2022_valid <- cohort_2022_nonmissing[valid_2023 == TRUE]
cohort_2022_pre_segment <- copy(cohort_2022_valid)
cohort_2022_pre_segment[, `:=`(
  block_id = block_id_2020,
  strictness_change = strictness_change_2023,
  treatment_continuous = strictness_change_2023
)]
cohort_2022_pre_segment <- add_event_geometry_dt(
  cohort_2022_pre_segment,
  "2015_2023",
  "switched_2023",
  "ward_origin_2023",
  "ward_dest_2023",
  "2022"
)
cohort_2022_event_geometry <- copy(cohort_2022_pre_segment)
cohort_2022_pre_segment <- cohort_2022_event_geometry[
  !is.na(ward_pair_id) &
    !is.na(ward_origin) &
    !is.na(dist_m) &
    event_point_origin_mismatch == FALSE &
    dist_m <= panel_max_distance_m
]
cohort_2022 <- assign_cohort_segments_dt(cohort_2022_pre_segment, segment_layers, "2015_2023", "2022")
if (any(grepl("_", cohort_2022$ward_pair_id, fixed = TRUE, useBytes = TRUE))) {
  stop("2022 cohort still contains underscore-form ward_pair_id values after normalization.", call. = FALSE)
}

message(sprintf("2022 cohort: %s transactions", format(nrow(cohort_2022), big.mark = ",")))
message(sprintf("  Year range: %d to %d", min(cohort_2022$sale_year), max(cohort_2022$sale_year)))
message(sprintf("  Treated transactions: %s", format(sum(cohort_2022$treat == 1), big.mark = ",")))
message(sprintf("  Control transactions: %s", format(sum(cohort_2022$treat == 0), big.mark = ",")))

# =============================================================================
# 7c. CREATE 2015 COHORT (Implementation)
# =============================================================================
# The 2015 cohort centers on the actual implementation date (May 2015).
# Uses sales from 2010-2020.

message("\n=== CREATING 2015 COHORT (Implementation) ===")

# 2015 cohort: sales from 2010-2020
cohort_2015_window <- copy(sales_with_treatment[
  sale_year >= 2010 & sale_year <= 2020 &
    !is.na(block_id_2010) &
    !is.na(longitude) &
    !is.na(latitude)
])
cohort_2015_window[, `:=`(
  cohort = "2015",
  block_id = block_id_2010,
  relative_year = sale_year - 2015,
  relative_year_capped = pmax(pmin(sale_year - 2015, 5), -5),
  treat = as.integer(switched_2015)
)]

cohort_2015_nonmissing <- cohort_2015_window[!is.na(switched_2015)]
cohort_2015_valid <- cohort_2015_nonmissing[valid_2015 == TRUE]
cohort_2015_pre_segment <- copy(cohort_2015_valid)
cohort_2015_pre_segment[, `:=`(
  block_id = block_id_2010,
  strictness_change = strictness_change_2015,
  treatment_continuous = strictness_change_2015
)]
cohort_2015_pre_segment <- add_event_geometry_dt(
  cohort_2015_pre_segment,
  "2003_2014",
  "switched_2015",
  "ward_origin_2015",
  "ward_dest_2015",
  "2015"
)
cohort_2015_event_geometry <- copy(cohort_2015_pre_segment)
cohort_2015_pre_segment <- cohort_2015_event_geometry[
  !is.na(ward_pair_id) &
    !is.na(ward_origin) &
    !is.na(dist_m) &
    event_point_origin_mismatch == FALSE &
    dist_m <= panel_max_distance_m
]
cohort_2015 <- assign_cohort_segments_dt(cohort_2015_pre_segment, segment_layers, "2003_2014", "2015")
if (any(grepl("_", cohort_2015$ward_pair_id, fixed = TRUE, useBytes = TRUE))) {
  stop("2015 cohort still contains underscore-form ward_pair_id values after normalization.", call. = FALSE)
}

message(sprintf("2015 cohort: %s transactions", format(nrow(cohort_2015), big.mark = ",")))
# =============================================================================
# 7d. CREATE 2023 COHORT
# =============================================================================
message("\n=== CREATING 2023 COHORT ===")

# 2023 cohort: sales from 2018-2025
cohort_2023_window <- copy(sales_with_treatment[
  sale_year >= 2018 & sale_year <= 2025 &
    !is.na(block_id_2020) &
    !is.na(longitude) &
    !is.na(latitude)
])
cohort_2023_window[, `:=`(
  cohort = "2023",
  block_id = block_id_2020,
  relative_year = sale_year - 2023,
  relative_year_capped = pmax(pmin(sale_year - 2023, 5), -5),
  treat = as.integer(switched_2023)
)]

cohort_2023_nonmissing <- cohort_2023_window[!is.na(switched_2023)]
cohort_2023_valid <- cohort_2023_nonmissing[valid_2023 == TRUE]
cohort_2023_pre_segment <- copy(cohort_2023_valid)
cohort_2023_pre_segment[, `:=`(
  block_id = block_id_2020,
  strictness_change = strictness_change_2023,
  treatment_continuous = strictness_change_2023
)]
cohort_2023_pre_segment <- add_event_geometry_dt(
  cohort_2023_pre_segment,
  "2015_2023",
  "switched_2023",
  "ward_origin_2023",
  "ward_dest_2023",
  "2023"
)
cohort_2023_event_geometry <- copy(cohort_2023_pre_segment)
cohort_2023_pre_segment <- cohort_2023_event_geometry[
  !is.na(ward_pair_id) &
    !is.na(ward_origin) &
    !is.na(dist_m) &
    event_point_origin_mismatch == FALSE &
    dist_m <= panel_max_distance_m
]
cohort_2023 <- assign_cohort_segments_dt(cohort_2023_pre_segment, segment_layers, "2015_2023", "2023")
if (any(grepl("_", cohort_2023$ward_pair_id, fixed = TRUE, useBytes = TRUE))) {
  stop("2023 cohort still contains underscore-form ward_pair_id values after normalization.", call. = FALSE)
}

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
# 8. CREATE STACKED PANELS
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
# 9. SELECT FINAL COLUMNS
# =============================================================================
message("\n=== SELECTING FINAL COLUMNS ===")

final_cols <- c(
  "pin", "block_id", "cohort", "cohort_block_id",
  "sale_date", "sale_year", "relative_year", "relative_year_capped",
  "sale_price",
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage",
  "nearest_school_dist_m", "nearest_park_dist_m", "nearest_major_road_dist_m", "lake_michigan_dist_m",
  "building_sqft", "land_sqft", "year_built", "building_age", "num_bedrooms",
  "num_full_baths", "baths_total", "garage_size", "hedonic_tax_year", "years_gap",
  "ward", "ward_pair_id", "ward_origin", "ward_dest", "event_neighbor_ward", "ward_pair_side", "cohort_ward_pair_side",
  "segment_id_cohort", "segment_side", "cohort_segment", "cohort_segment_side",
  "segment_length_ft_cohort", "segment_lt500ft_cohort", "segment_lt1000ft_cohort",
  "signed_dist_m", "dist_m",
  "treat", "strictness_change"
)

make_all_valid_panel <- function(dt) {
  out <- copy(dt)
  if (!"switched" %in% names(out)) {
    out[, switched := treat == 1]
  }
  missing_segment_cols <- setdiff(
    c(
      "segment_id_cohort", "segment_side", "cohort_segment", "cohort_segment_side",
      "segment_length_ft_cohort", "segment_lt500ft_cohort", "segment_lt1000ft_cohort"
    ),
    names(out)
  )
  for (col in missing_segment_cols) {
    out[, (col) := NA]
  }
  out[, `:=`(
    cohort_block_id = paste(cohort, block_id, sep = "_"),
    cohort_ward_pair_side = fifelse(!is.na(ward_pair_side), paste(cohort, ward_pair_side, sep = "_"), NA_character_),
    event_dist_le_1000ft = !is.na(dist_m) & dist_m <= 304.8,
    pre_score_dist_le_1000ft = !is.na(pre_score_dist_m) & pre_score_dist_m <= 304.8,
    nearest_pre_boundary_dist_le_1000ft = !is.na(nearest_pre_boundary_dist_m) & nearest_pre_boundary_dist_m <= 304.8,
    old_event_1000ft_disagree = (!is.na(pre_score_dist_m) & pre_score_dist_m <= 304.8) !=
      (!is.na(dist_m) & dist_m <= 304.8)
  )]
  out
}

cohort_2012_final <- cohort_2012[, ..final_cols]
cohort_2022_final <- cohort_2022[, ..final_cols]
cohort_2015_final <- cohort_2015[, ..final_cols]
cohort_2023_final <- cohort_2023[, ..final_cols]
stacked_announcement_final <- stacked_announcement[, ..final_cols]
stacked_implementation_final <- stacked_implementation[, ..final_cols]

all_valid_cols <- c(
  final_cols,
  "switched", "event_dist_missing", "event_point_origin_mismatch",
  "event_dist_le_1000ft", "pre_score_ward", "pre_score_ward_pair_id",
  "pre_score_dist_m", "pre_score_signed_dist_m", "pre_score_dist_le_1000ft",
  "nearest_pre_boundary_ward", "nearest_pre_boundary_neighbor_ward",
  "nearest_pre_boundary_pair_id", "nearest_pre_boundary_dist_m",
  "nearest_pre_boundary_dist_ft", "nearest_pre_boundary_dist_le_1000ft",
  "old_event_1000ft_disagree", "event_control_ward", "event_control_neighbor_ward"
)
cohort_2015_all_valid <- make_all_valid_panel(cohort_2015_event_geometry)[, ..all_valid_cols]

# Use implementation as "final_panel" for backwards compatibility with diagnostics
final_panel <- stacked_implementation_final
if (nrow(final_panel) == 0) {
  stop("Final sales event-study panel has zero rows.", call. = FALSE)
}

# =============================================================================
# 10. DIAGNOSTICS
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
  log_sqft = mean(is.finite(log_sqft)) * 100,
  log_land_sqft = mean(is.finite(log_land_sqft)) * 100,
  log_building_age = mean(is.finite(log_building_age)) * 100,
  log_bedrooms = mean(is.finite(log_bedrooms)) * 100,
  log_baths = mean(is.finite(log_baths)) * 100,
  has_garage = mean(!is.na(has_garage)) * 100
)]
print(hedonic_coverage)

final_hedonic_coverage_by_sale_year <- final_panel[, .(
  n_sales = .N,
  log_sqft = mean(is.finite(log_sqft)) * 100,
  log_land_sqft = mean(is.finite(log_land_sqft)) * 100,
  log_building_age = mean(is.finite(log_building_age)) * 100,
  log_bedrooms = mean(is.finite(log_bedrooms)) * 100,
  log_baths = mean(is.finite(log_baths)) * 100,
  has_garage = mean(!is.na(has_garage)) * 100,
  complete_building_hedonics = mean(
    is.finite(log_sqft) & is.finite(log_building_age) &
      is.finite(log_bedrooms) & is.finite(log_baths) & !is.na(has_garage)
  ) * 100,
  complete_hedonics_with_land = mean(
    is.finite(log_sqft) & is.finite(log_land_sqft) & is.finite(log_building_age) &
      is.finite(log_bedrooms) & is.finite(log_baths) & !is.na(has_garage)
  ) * 100
), by = .(cohort, sale_year)][order(cohort, sale_year)]

message("\nFinal panel hedonic coverage by cohort and sale year (% non-missing):")
print(final_hedonic_coverage_by_sale_year)

message("\nAmenity variable coverage in final panel (% non-missing):")
amenity_coverage <- final_panel[, .(
  nearest_school_dist_m = mean(!is.na(nearest_school_dist_m)) * 100,
  nearest_park_dist_m = mean(!is.na(nearest_park_dist_m)) * 100,
  nearest_major_road_dist_m = mean(!is.na(nearest_major_road_dist_m)) * 100,
  lake_michigan_dist_m = mean(!is.na(lake_michigan_dist_m)) * 100
)]
print(amenity_coverage)

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
  is.finite(log_sqft) & is.finite(log_land_sqft) & is.finite(log_building_age) &
    is.finite(log_bedrooms) & is.finite(log_baths) & !is.na(has_garage), .N
]
message(sprintf(
  "\nTransactions with complete hedonics (regression sample): %s (%.1f%%)",
  format(n_complete, big.mark = ","),
  100 * n_complete / nrow(final_panel)
))
final_complete_hedonics_rate <- n_complete / nrow(final_panel)
final_core_building_hedonics_rate <- final_panel[
  ,
  mean(is.finite(log_sqft) & is.finite(log_building_age) &
         is.finite(log_bedrooms) & is.finite(log_baths) & !is.na(has_garage))
]
final_land_sqft_rate <- mean(is.finite(final_panel$log_land_sqft))
final_pre_1999_matched_sales <- sum(!is.na(final_panel$year_built) & final_panel$year_built < 1999)

if (final_pre_1999_matched_sales == 0) {
  stop("Final sales event-study panel has no pre-1999 matched buildings; check the all-buildings hedonic input.", call. = FALSE)
}
if (final_core_building_hedonics_rate < 0.99) {
  stop(sprintf(
    "Final sales panel core building-hedonic coverage is %.1f%%, below the 99%% guardrail.",
    100 * final_core_building_hedonics_rate
  ), call. = FALSE)
}
if (final_land_sqft_rate < 0.99) {
  stop(sprintf(
    "Final sales panel land square-footage coverage is %.1f%%, below the 99%% guardrail.",
    100 * final_land_sqft_rate
  ), call. = FALSE)
}
if (final_complete_hedonics_rate < 0.99) {
  stop(sprintf(
    "Final sales panel complete-hedonic coverage is %.1f%%, below the 99%% guardrail.",
    100 * final_complete_hedonics_rate
  ), call. = FALSE)
}

n_complete_amenity <- final_panel[
  is.finite(log_sqft) & is.finite(log_land_sqft) & is.finite(log_building_age) &
    is.finite(log_bedrooms) & is.finite(log_baths) & !is.na(has_garage) &
    !is.na(nearest_school_dist_m) & !is.na(nearest_park_dist_m) &
    !is.na(nearest_major_road_dist_m) & !is.na(lake_michigan_dist_m), .N
]
message(sprintf(
  "Transactions with complete hedonics and amenity distances: %s (%.1f%%)",
  format(n_complete_amenity, big.mark = ","),
  100 * n_complete_amenity / nrow(final_panel)
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

sales_event_geometry_diagnostics <- rbindlist(list(
  summarize_event_geometry_dt(cohort_2012_event_geometry, "cohort_2012", "post_event_geometry"),
  summarize_event_geometry_dt(cohort_2012_pre_segment, "cohort_2012", "post_event_distance_filter"),
  summarize_event_geometry_dt(cohort_2022_event_geometry, "cohort_2022", "post_event_geometry"),
  summarize_event_geometry_dt(cohort_2022_pre_segment, "cohort_2022", "post_event_distance_filter"),
  summarize_event_geometry_dt(cohort_2015_event_geometry, "cohort_2015", "post_event_geometry"),
  summarize_event_geometry_dt(cohort_2015_pre_segment, "cohort_2015", "post_event_distance_filter"),
  summarize_event_geometry_dt(cohort_2023_event_geometry, "cohort_2023", "post_event_geometry"),
  summarize_event_geometry_dt(cohort_2023_pre_segment, "cohort_2023", "post_event_distance_filter")
), fill = TRUE)

# =============================================================================
# 11. SAVE ALL PANELS
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

write_parquet(cohort_2015_all_valid, "../output/sales_transaction_panel_2015_all_valid.parquet")
message(sprintf(
  "Saved 2015 all-valid diagnostic cohort: %s rows",
  format(nrow(cohort_2015_all_valid), big.mark = ",")
))

write_parquet(cohort_2023_final, "../output/sales_transaction_panel_2023.parquet")
message(sprintf("Saved 2023 cohort: %s rows", format(nrow(cohort_2023_final), big.mark = ",")))

write_csv(as_tibble(sales_amenity_diagnostics), "../output/sales_transaction_panel_amenity_distance_diagnostics.csv")
message("Saved sales amenity-distance diagnostics")

write_csv(as_tibble(sales_hedonic_coverage_by_sale_year), "../output/sales_transaction_panel_hedonic_coverage_by_sale_year.csv")
message("Saved all-sales hedonic-coverage diagnostics")

write_csv(as_tibble(final_hedonic_coverage_by_sale_year), "../output/sales_transaction_panel_final_hedonic_coverage_by_sale_year.csv")
message("Saved final-panel hedonic-coverage diagnostics")

write_csv(as_tibble(sales_support_by_event_time), "../output/sales_transaction_panel_support_by_event_time.csv")
message("Saved sales event-time support diagnostics")

write_csv(as_tibble(sales_support_by_calendar_time), "../output/sales_transaction_panel_support_by_calendar_time.csv")
message("Saved sales calendar-time support diagnostics")

write_csv(as_tibble(sales_assignment_stability), "../output/sales_transaction_panel_assignment_stability.csv")
message("Saved sales assignment-stability diagnostics")

write_csv(as_tibble(sales_event_geometry_diagnostics), "../output/sales_transaction_panel_event_geometry_diagnostics.csv")
message("Saved sales event-geometry diagnostics")

message("\nDone!")
