# This script calculates signed distances from parcels to ward boundaries
# and assigns aldermen based on construction year and redistricting events
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_ward_boundary_distances/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

cat("Loading parcel data...\n")
parcels <- st_read("../input/geocoded_residential_data.gpkg") %>%
  # Note: This filter excludes older multifamily buildings if they exist in the input
  filter(!is.na(yearbuilt), yearbuilt >= 1999 & yearbuilt <= 2025) %>%
  mutate(construction_date = as.Date(paste0(yearbuilt, "-06-15")))

cat("Loading ward boundaries...\n")
ward_panel <- st_read("../input/ward_panel.gpkg")
canonical_ward_maps <- load_canonical_ward_maps(ward_panel)

cat("Loading canonical ward-pair boundaries...\n")
canonical_boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

cat("Loading alderman panel...\n")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")

cat("Loading ward controls (homeownership rates)...\n")
ward_controls <- read_csv("../input/ward_controls.csv")

cat("Loading block group controls...\n")
bg_controls <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE)

if (st_crs(parcels) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match ward boundaries.")
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

cat("Loading zoning data...\n")
zoning_data <- st_read("../input/zoning_data_clean.gpkg")

if (st_crs(zoning_data) != st_crs(ward_panel)) {
  zoning_data <- st_transform(zoning_data, st_crs(ward_panel))
}

# Spatial join for zoning
# Note: This works for multifamily too, as they are now point geometries
parcels <- parcels %>%
  st_join(
    zoning_data %>% dplyr::select(
      zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height
    ),
    left = TRUE, largest = TRUE
  )

# -----------------------------------------------------------------------------
# 1.5. GEOCODE PARCELS TO CENSUS BLOCK GROUPS
# -----------------------------------------------------------------------------
cat("Loading canonical block-group geometries (ACS 2019)...\n")
block_groups <- st_read("../input/block_group_geometry_2019.gpkg", quiet = TRUE)

if (!("GEOID" %in% names(block_groups))) {
  stop("Block-group geometry input is missing GEOID.", call. = FALSE)
}
block_groups <- block_groups[, "GEOID", drop = FALSE]

geometry_column <- attr(block_groups, "sf_column")
if (is.null(geometry_column) || !(geometry_column %in% names(block_groups))) {
  stop("Block-group geometry input has no valid geometry column.", call. = FALSE)
}

if (nrow(block_groups) == 0) {
  stop("No block-group geometries found in ../input/block_group_geometry_2019.gpkg", call. = FALSE)
}
if (any(is.na(block_groups$GEOID) | block_groups$GEOID == "")) {
  stop("Block-group geometry input has missing GEOID values.", call. = FALSE)
}
if (any(duplicated(block_groups$GEOID))) {
  stop("Block-group geometry input GEOID values are not unique.", call. = FALSE)
}
if (any(st_is_empty(st_geometry(block_groups)))) {
  stop("Block-group geometry input contains empty geometries.", call. = FALSE)
}
if (st_crs(block_groups) != st_crs(parcels)) {
  block_groups <- st_transform(block_groups, st_crs(parcels))
}

cat("Spatial join: assigning parcels to block groups...\n")
parcels <- parcels %>%
  st_join(block_groups, left = TRUE)

cat(sprintf(
  "Block group assignment complete. %d of %d parcels have GEOID.\n",
  sum(!is.na(parcels$GEOID)), nrow(parcels)
))

# -----------------------------------------------------------------------------
# 2. CANONICAL WARD ASSIGNMENT AND BOUNDARY DISTANCES
# -----------------------------------------------------------------------------

cat("Assigning canonical ward pairs and distances to parcels...\n")

parcels <- parcels %>%
  mutate(
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

boundary_assignments <- assign_points_to_boundaries(
  points_sf = parcels,
  era_values = parcels$era,
  ward_maps = canonical_ward_maps,
  boundary_lines = canonical_boundaries,
  chunk_n = 2000L
) %>%
  rename(
    assigned_ward = ward,
    ward_pair = ward_pair_id,
    dist_to_boundary = dist_ft
  )

parcels_with_distances <- bind_cols(parcels, boundary_assignments)

cat(sprintf(
  "Canonical ward assignment coverage: %d of %d parcels have ward, %d have ward_pair.\n",
  sum(!is.na(parcels_with_distances$assigned_ward)),
  nrow(parcels_with_distances),
  sum(!is.na(parcels_with_distances$ward_pair))
))

# -----------------------------------------------------------------------------
# 6. ADD WARD PAIRS AND ALDERMAN INFO (EFFICIENT VERSION)
# -----------------------------------------------------------------------------

cat("Pre-processing alderman lookup tables...\n")
alderman_lookup <- alderman_panel %>%
  select(ward, month, alderman) %>%
  mutate(
    month_yearmon = as.yearmon(month, format = "%b %Y"),
    year = year(as.Date(month_yearmon)),
    yearmon_key = as.character(month_yearmon)
  )

# Create tenure lookup
alderman_tenure_lookup <- alderman_panel %>%
  mutate(month_yearmon = as.yearmon(month, format = "%b %Y")) %>%
  group_by(ward, alderman) %>%
  summarise(
    first_month = min(month_yearmon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(alderman_ward_key = paste(ward, alderman, sep = "_"))

# Efficient processing
final_dataset <- parcels_with_distances %>%
  mutate(
    construction_yearmon = as.yearmon(construction_date),
    yearmon_key = as.character(construction_yearmon)
  ) %>%
  left_join(alderman_lookup,
    by = c("assigned_ward" = "ward", "yearmon_key")
  ) %>%
  rename(alderman_own = alderman) %>%
  mutate(
    alderman_ward_key = paste(assigned_ward, alderman_own, sep = "_")
  ) %>%
  left_join(alderman_tenure_lookup,
    by = "alderman_ward_key"
  ) %>%
  mutate(
    alderman_tenure_months = ifelse(
      !is.na(first_month) & !is.na(construction_yearmon),
      as.numeric((construction_yearmon - first_month) * 12),
      NA_real_
    )
  ) %>%
  select(
    pin, geom, GEOID,
    construction_year = yearbuilt, construction_date, boundary_year, dist_to_boundary,
    ward = assigned_ward, ward_pair,
    alderman = alderman_own, alderman_tenure_months,
    # NOTE: These columns were kept in your previous script (res + multi)
    arealotsf, areabuilding, bedroomscount, unitscount, storiescount, residential,

    # NOTE: The following columns were DROPPED in the previous geocoding script.
    # I have commented them out to prevent crashes. If you need them,
    # add them to the select() in geocode_residential_data.R.
    # fullbathcount, halfbathcount, roomscount,
    # construction_quality, central_heating, central_air, single_v_multi_family,

    zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height,
    yearmon_key
  )

# -----------------------------------------------------------------------------
# 7. MERGE IN ALDERMAN STRICTNESS SCORES (DEPRECATED)
# -----------------------------------------------------------------------------
# final_dataset <- final_dataset %>%
#   left_join(alderman_scores, by = "alderman")

# -----------------------------------------------------------------------------
# 8. MAKE OUTCOME VARIABLES
# -----------------------------------------------------------------------------

final_dataset <- final_dataset %>%
  mutate(
    # Floor Area Ratio
    density_far = if_else(arealotsf > 0, areabuilding / arealotsf, NA_real_),
    # Lot Area Per Unit
    density_lapu = if_else(unitscount > 0, arealotsf / unitscount, NA_real_),

    # Building Coverage Ratio (requires stories; will be NA for multifamily where storiescount is NA)
    density_bcr = if_else(!is.na(storiescount) & storiescount > 0 & arealotsf > 0,
      (areabuilding / storiescount) / arealotsf, NA_real_
    ),
    # Lot Size Per Story (will be NA for multifamily)
    density_lps = if_else(!is.na(storiescount) & storiescount > 0, arealotsf / storiescount, NA_real_),

    # Square Feet Per Unit
    density_spu = if_else(unitscount > 0, areabuilding / unitscount, NA_real_),
    ## Dwelling units per acre (DUPAC)
    density_dupac = if_else(
      arealotsf > 0 & unitscount > 0,
      43560 * unitscount / arealotsf,
      NA_real_
    )
  )

# -----------------------------------------------------------------------------
# 8.5. SAVE GEOSPATIAL OUTPUT
# -----------------------------------------------------------------------------
cat("Saving geospatial output before dropping geometry...\n")
st_write(final_dataset, "../output/parcels_with_geometry.gpkg", delete_dsn = TRUE)

# -----------------------------------------------------------------------------
# 9. SIGN DISTANCES BASED ON HOMEOWNERSHIP RATES
# -----------------------------------------------------------------------------
final_dataset <- as_tibble(st_drop_geometry(final_dataset)) %>%
  rename(alderman_own = alderman)

# Prepare ward controls for joining
# We need homeownership_rate by ward and year
# Since parcels have construction_year, we'll try to match on that,
# or use the nearest available year if needed.
# For now, let's assume direct join on year is sufficient or we use boundary_year?
# The user said "ward by year data on homeonwer rates".
# Let's use construction_year to match the "treatment" at the time of construction.

# Ensure ward_controls has the columns we need
# Ensure ward_controls has the columns we need
ward_controls_clean <- ward_controls

final_dataset_signed <- final_dataset %>%
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a),
    match_year = construction_year
  ) %>%
  # --- JOIN 1: Own Ward Data ---
  # This adds columns like 'share_black', 'homeownership_rate', etc.
  left_join(ward_controls_clean, by = c("ward" = "ward", "match_year" = "year")) %>%
  # --- JOIN 2: Neighbor Ward Data (The Fix) ---
  # The 'suffix' argument tells dplyr:
  # "If you see a column name that already exists (from Join 1),
  #  rename the existing one with '_own' and the new one with '_neighbor'."
  left_join(
    ward_controls_clean,
    by = c("other_ward" = "ward", "match_year" = "year"),
    suffix = c("_own", "_neighbor")
  ) %>%
  # --- JOIN 3: Neighbor Alderman ---
  left_join(
    alderman_lookup %>% rename(alderman_neighbor = alderman),
    by = c("other_ward" = "ward", "yearmon_key")
  ) %>%
  # NOTE: Score merging moved to merge_in_scores task for faster iteration
  dplyr::select(-contains("wards_in_pair"), -match_year) %>%
  # --- JOIN 4: Block Group Demographics ---
  # Merge block group-level demographics by GEOID and construction_year
  # Ensure GEOID is character type in both datasets
  left_join(
    bg_controls %>%
      mutate(GEOID = as.character(GEOID)) %>%
      rename_with(~ paste0(., "_bg"), -c(GEOID, year)),
    by = c("GEOID", "construction_year" = "year")
  )

cat("Final Dataset Created!\n")
cat(sprintf(
  "Block group demographics merged: %d of %d parcels have block group data.\n",
  sum(!is.na(final_dataset_signed$percent_white_bg)), nrow(final_dataset_signed)
))

# -----------------------------------------------------------------------------
# 10. SAVE OUTPUT (Pre-scores - unsigned distances)
# -----------------------------------------------------------------------------

cat("Saving output (pre-scores, unsigned)...\n")

write_csv(final_dataset_signed, "../output/parcels_pre_scores.csv")

parcel_geometry_diagnostics <- final_dataset_signed %>%
  summarise(
    n_obs = n(),
    n_with_ward = sum(!is.na(ward)),
    n_with_ward_pair = sum(!is.na(ward_pair)),
    n_with_neighbor_alderman = sum(!is.na(alderman_neighbor)),
    mean_dist_to_boundary = mean(dist_to_boundary, na.rm = TRUE),
    median_dist_to_boundary = median(dist_to_boundary, na.rm = TRUE),
    .by = boundary_year
  ) %>%
  arrange(boundary_year)

write_csv(parcel_geometry_diagnostics, "../output/parcel_geometry_diagnostics.csv")

summary_stats <- final_dataset_signed %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman_own, na.rm = TRUE),
    mean_dist_to_boundary = mean(dist_to_boundary, na.rm = TRUE),
    median_dist_to_boundary = median(dist_to_boundary, na.rm = TRUE),
    .by = c(boundary_year, construction_year)
  ) %>%
  arrange(construction_year)

cat("Task completed successfully!\n")
print(summary_stats)

write_csv(summary_stats, "../output/boundary_distance_summary.csv")

cat("Task completed successfully!\n")
cat(sprintf(
  "Processed %d parcels across %d wards\n",
  nrow(final_dataset),
  n_distinct(final_dataset$ward)
))
cat(sprintf(
  "Found %d unique ward pairs\n",
  n_distinct(final_dataset$ward_pair, na.rm = TRUE)
))
