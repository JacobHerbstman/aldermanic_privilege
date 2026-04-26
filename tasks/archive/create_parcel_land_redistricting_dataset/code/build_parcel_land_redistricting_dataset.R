source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

options(dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_parcel_land_redistricting_dataset/code")
# in_panel <- "../input/parcel_land_value_panel.parquet"
# in_blocks_2010 <- "../input/census_blocks_2010.csv"
# in_ward_panel <- "../input/ward_panel.gpkg"
# in_boundaries <- "../input/ward_pair_boundaries.gpkg"
# in_segments <- "../input/boundary_segments_1320ft.gpkg"
# in_treatment <- "../input/block_treatment_panel.csv"
# out_panel <- "../output/parcel_land_redistricting_panel.parquet"
# out_diag <- "../output/parcel_land_redistricting_diagnostics.csv"
# out_support <- "../output/parcel_land_redistricting_support.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_panel,
    in_blocks_2010,
    in_ward_panel,
    in_boundaries,
    in_segments,
    in_treatment,
    out_panel,
    out_diag,
    out_support
  )
}

if (length(cli_args) != 9) {
  stop(
    "FATAL: Script requires 9 args: <panel_parquet> <census_blocks_2010_csv> <ward_panel_gpkg> <ward_pair_boundaries_gpkg> <boundary_segments_gpkg> <block_treatment_csv> <out_panel_parquet> <out_diag_csv> <out_support_csv>",
    call. = FALSE
  )
}

in_panel <- cli_args[1]
in_blocks_2010 <- cli_args[2]
in_ward_panel <- cli_args[3]
in_boundaries <- cli_args[4]
in_segments <- cli_args[5]
in_treatment <- cli_args[6]
out_panel <- cli_args[7]
out_diag <- cli_args[8]
out_support <- cli_args[9]

stopifnot(
  file.exists(in_panel),
  file.exists(in_blocks_2010),
  file.exists(in_ward_panel),
  file.exists(in_boundaries),
  file.exists(in_segments),
  file.exists(in_treatment)
)

assign_polygon_ids <- function(points_sf, polygons_sf, id_col) {
  hits <- sf::st_intersects(points_sf, polygons_sf)
  hit_n <- lengths(hits)

  if (any(hit_n > 1)) {
    stop(
      sprintf("Found %d parcels with multiple %s assignments.", sum(hit_n > 1), id_col),
      call. = FALSE
    )
  }

  out_id <- rep(NA_character_, nrow(points_sf))
  out_method <- rep(NA_character_, nrow(points_sf))

  within_idx <- which(hit_n == 1)
  if (length(within_idx) > 0) {
    out_id[within_idx] <- as.character(polygons_sf[[id_col]][unlist(hits[within_idx])])
    out_method[within_idx] <- "within"
  }

  missing_idx <- which(hit_n == 0)
  if (length(missing_idx) > 0) {
    nearest_idx <- sf::st_nearest_feature(points_sf[missing_idx, ], polygons_sf)
    out_id[missing_idx] <- as.character(polygons_sf[[id_col]][nearest_idx])
    out_method[missing_idx] <- "nearest"
  }

  tibble(
    assigned_id = out_id,
    assignment_method = out_method
  )
}

build_scope_diagnostics <- function(panel_scope, scope_label) {
  parcel_scope <- panel_scope %>%
    distinct(pin10, switched_2015, valid_control_2015, ward_pair_id, segment_id, block_origin_side_id)

  tax_year_diag <- panel_scope %>%
    group_by(tax_year, switched_2015, valid_control_2015) %>%
    summarise(
      n_rows = n(),
      n_pin10 = n_distinct(pin10),
      .groups = "drop"
    ) %>%
    mutate(
      diagnostic_group = "tax_year_treatment",
      scope = scope_label,
      ward_pair_id = NA_character_,
      segment_id = NA_character_,
      block_origin_side_id = NA_character_,
      n_switchers = NA_integer_,
      n_valid_controls = NA_integer_,
      has_mixed_support = NA
    )

  ward_pair_diag <- parcel_scope %>%
    group_by(ward_pair_id, switched_2015, valid_control_2015) %>%
    summarise(
      n_rows = n(),
      n_pin10 = n_distinct(pin10),
      .groups = "drop"
    ) %>%
    mutate(
      diagnostic_group = "ward_pair",
      scope = scope_label,
      tax_year = NA_integer_,
      segment_id = NA_character_,
      block_origin_side_id = NA_character_,
      n_switchers = NA_integer_,
      n_valid_controls = NA_integer_,
      has_mixed_support = NA
    )

  segment_diag <- parcel_scope %>%
    mutate(segment_id = if_else(is.na(segment_id), "__MISSING__", segment_id)) %>%
    group_by(segment_id) %>%
    summarise(
      n_rows = n(),
      n_pin10 = n_distinct(pin10),
      .groups = "drop"
    ) %>%
    mutate(
      diagnostic_group = "segment_coverage",
      scope = scope_label,
      tax_year = NA_integer_,
      switched_2015 = NA,
      valid_control_2015 = NA,
      ward_pair_id = NA_character_,
      block_origin_side_id = NA_character_,
      n_switchers = NA_integer_,
      n_valid_controls = NA_integer_,
      has_mixed_support = NA
    )

  block_diag <- parcel_scope %>%
    group_by(block_origin_side_id) %>%
    summarise(
      n_rows = n(),
      n_pin10 = n_distinct(pin10),
      n_switchers = sum(switched_2015, na.rm = TRUE),
      n_valid_controls = sum(valid_control_2015, na.rm = TRUE),
      has_mixed_support = n_switchers > 0 & n_valid_controls > 0,
      .groups = "drop"
    ) %>%
    mutate(
      diagnostic_group = "block_origin_side_support",
      scope = scope_label,
      tax_year = NA_integer_,
      switched_2015 = NA,
      valid_control_2015 = NA,
      ward_pair_id = NA_character_,
      segment_id = NA_character_
    )

  bind_rows(tax_year_diag, ward_pair_diag, segment_diag, block_diag) %>%
    select(
      diagnostic_group, scope, tax_year, switched_2015, valid_control_2015,
      ward_pair_id, segment_id, block_origin_side_id,
      n_rows, n_pin10, n_switchers, n_valid_controls, has_mixed_support
    )
}

message("Reading parcel land-value panel...")
panel <- arrow::open_dataset(in_panel) %>%
  filter(
    municipality_name == "CITY OF CHICAGO" | municipality_name == "" | is.na(municipality_name)
  ) %>%
  collect() %>%
  mutate(
    pin10 = as.character(pin10),
    tax_year = as.integer(tax_year),
    land_only = as.logical(land_only)
  )

if (nrow(panel) == 0) {
  stop("Filtered Chicago parcel land-value panel is empty.", call. = FALSE)
}

if (anyDuplicated(panel[c("pin10", "tax_year")]) > 0) {
  stop("Input parcel land-value panel has duplicate pin10 x tax_year rows.", call. = FALSE)
}

parcel_static <- panel %>%
  select(
    pin10, longitude, latitude, centroid_x_crs_3435, centroid_y_crs_3435,
    class_current,
    property_use_group_current, property_use_group_current_resolution,
    property_use_group_current_n_values, property_use_group_current_n_top_ties,
    property_use_standardized_current, property_use_standardized_current_resolution,
    property_use_standardized_current_n_values, property_use_standardized_current_n_top_ties,
    property_use_muni_current, property_use_muni_current_resolution,
    property_use_muni_current_n_values, property_use_muni_current_n_top_ties,
    zoned_code_local_current, zoned_code_local_current_resolution,
    zoned_code_local_current_n_values, zoned_code_local_current_n_top_ties,
    owner_type_description1_current, owner_type_description1_current_resolution,
    owner_type_description1_current_n_values, owner_type_description1_current_n_top_ties,
    any_exemption_current, any_exemption_current_n_values,
    triad_name, township_name, municipality_name
  ) %>%
  distinct()

if (nrow(parcel_static) != n_distinct(panel$pin10)) {
  stop("Static parcel table is not unique by pin10.", call. = FALSE)
}

message("Loading ward maps, boundary layers, segment layers, and 2010 blocks...")
ward_panel <- st_read(in_ward_panel, quiet = TRUE)
ward_map_2014 <- aggregate_ward_map(ward_panel, canonical_map_year_for_era("2003_2014"))
ward_map_2015 <- aggregate_ward_map(ward_panel, canonical_map_year_for_era("2015_2023"))
ward_maps_for_boundary <- list("2003_2014" = ward_map_2014)
boundary_lines <- load_boundary_layers(in_boundaries, eras = "2003_2014")
segment_layers <- load_segment_layers(in_segments, buffer_ft = 1000, eras = "2003_2014")

blocks_2010 <- read_csv(in_blocks_2010, show_col_types = FALSE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_transform(3435) %>%
  rename(block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  distinct(block_id, .keep_all = TRUE)

message("Assigning pre/post-2015 ward geography and boundary distance...")
parcel_points <- st_as_sf(
  parcel_static,
  coords = c("centroid_x_crs_3435", "centroid_y_crs_3435"),
  crs = 3435,
  remove = FALSE
)

parcel_static$ward_2014 <- assign_points_to_wards(parcel_points, ward_map_2014)
parcel_static$ward_2015 <- assign_points_to_wards(parcel_points, ward_map_2015)

boundary_match <- assign_points_to_boundaries(
  parcel_points,
  rep("2003_2014", nrow(parcel_points)),
  ward_maps_for_boundary,
  boundary_lines,
  chunk_n = 50000L
)

parcel_static <- parcel_static %>%
  mutate(
    boundary_side_ward_2014 = as.integer(boundary_match$ward),
    neighbor_ward_2014 = as.integer(boundary_match$neighbor_ward),
    ward_pair_id = as.character(boundary_match$ward_pair_id),
    dist_to_boundary_ft = as.numeric(boundary_match$dist_ft),
    in_500ft = !is.na(dist_to_boundary_ft) & dist_to_boundary_ft <= 500,
    in_1000ft = !is.na(dist_to_boundary_ft) & dist_to_boundary_ft <= 1000
  )

ward_mismatch <- parcel_static %>%
  filter(!is.na(ward_2014), !is.na(boundary_side_ward_2014), ward_2014 != boundary_side_ward_2014)
if (nrow(ward_mismatch) > 0) {
  stop(
    sprintf("Found %d parcels where direct pre-2015 ward assignment disagrees with boundary-side ward assignment.", nrow(ward_mismatch)),
    call. = FALSE
  )
}

parcel_near <- parcel_static %>%
  filter(in_1000ft)

if (nrow(parcel_near) == 0) {
  stop("No land-only Chicago parcels fall within 1000 feet of a pre-2015 ward boundary.", call. = FALSE)
}

message("Assigning segment IDs and 2010 census blocks for the near-boundary sample...")
parcel_near_idx <- match(parcel_near$pin10, parcel_static$pin10)
if (anyNA(parcel_near_idx)) {
  stop("Failed to align near-boundary parcel rows back to the full parcel geometry table.", call. = FALSE)
}

parcel_near_points <- parcel_points[parcel_near_idx, ]

parcel_near$segment_id <- assign_points_to_segments(
  parcel_near_points,
  rep("2003_2014", nrow(parcel_near_points)),
  parcel_near$ward_pair_id,
  segment_layers,
  chunk_n = 50000L
)

missing_segment_idx <- which(is.na(parcel_near$segment_id) & !is.na(parcel_near$ward_pair_id))
if (length(missing_segment_idx) > 0) {
  segment_2003_2014 <- segment_layers[["2003_2014"]]
  pair_dash_missing <- normalize_pair_dash(parcel_near$ward_pair_id[missing_segment_idx])

  for (pair_i in unique(pair_dash_missing[!is.na(pair_dash_missing)])) {
    idx_pair <- missing_segment_idx[pair_dash_missing == pair_i]
    seg_pair <- segment_2003_2014[segment_2003_2014$pair_dash == pair_i, ]
    if (length(idx_pair) == 0 || nrow(seg_pair) == 0) {
      next
    }

    nearest_idx <- sf::st_nearest_feature(parcel_near_points[idx_pair, ], seg_pair)
    parcel_near$segment_id[idx_pair] <- as.character(seg_pair$segment_id[nearest_idx])
  }
}

block_assignment <- assign_polygon_ids(parcel_near_points, blocks_2010, "block_id")
parcel_near <- parcel_near %>%
  mutate(
    block_id = as.character(block_assignment$assigned_id),
    block_assignment_method = block_assignment$assignment_method
  )

missing_geo <- parcel_near %>%
  filter(
    is.na(block_id) |
      is.na(ward_2014) |
      is.na(ward_2015) |
      is.na(ward_pair_id) |
      is.na(dist_to_boundary_ft) |
      is.na(segment_id)
  )
if (nrow(missing_geo) > 0) {
  stop(
    sprintf("Near-boundary parcel geometry assignment failed for %d parcels.", nrow(missing_geo)),
    call. = FALSE
  )
}

message("Preparing 2015 treatment and strictness lookups...")
treatment_2015 <- read_csv(in_treatment, show_col_types = FALSE) %>%
  mutate(
    cohort = as.integer(cohort),
    block_id = as.character(block_id),
    ward_origin = as.integer(ward_origin),
    ward_dest = as.integer(ward_dest),
    switched = as.logical(switched),
    strictness_origin = as.numeric(strictness_origin),
    strictness_dest = as.numeric(strictness_dest),
    strictness_change = as.numeric(strictness_change),
    ward_had_turnover = as.logical(ward_had_turnover),
    valid = as.logical(valid)
  ) %>%
  filter(cohort == 2015L)

if (nrow(treatment_2015) == 0) {
  stop("No 2015 rows found in block treatment panel.", call. = FALSE)
}

ward_turnover_lookup <- treatment_2015 %>%
  distinct(ward_origin, ward_had_turnover) %>%
  filter(!is.na(ward_origin))

if (anyDuplicated(ward_turnover_lookup$ward_origin) > 0) {
  stop("ward_had_turnover is not unique by origin ward in the 2015 treatment panel.", call. = FALSE)
}

ward_score_lookup <- bind_rows(
  treatment_2015 %>%
    transmute(ward = ward_origin, strictness = strictness_origin),
  treatment_2015 %>%
    transmute(ward = ward_dest, strictness = strictness_dest)
) %>%
  filter(!is.na(ward)) %>%
  distinct()

score_conflicts <- ward_score_lookup %>%
  group_by(ward) %>%
  summarise(n_scores = n_distinct(strictness), .groups = "drop") %>%
  filter(n_scores > 1)
if (nrow(score_conflicts) > 0) {
  stop("Strictness score lookup is not unique by ward in the 2015 treatment panel.", call. = FALSE)
}

ward_score_lookup <- ward_score_lookup %>%
  group_by(ward) %>%
  summarise(strictness = first(strictness), .groups = "drop")

block_treatment_lookup <- treatment_2015 %>%
  transmute(
    block_id,
    block_switched_2015 = switched,
    block_treatment_ward_origin = ward_origin,
    block_treatment_ward_dest = ward_dest,
    block_valid_control_2015 = valid,
    block_ward_had_turnover_2015 = ward_had_turnover
  )

parcel_near <- parcel_near %>%
  mutate(
    ward_origin = ward_2014,
    ward_dest = ward_2015,
    switched_2015 = ward_origin != ward_dest & !is.na(ward_origin) & !is.na(ward_dest),
    block_origin_side_id = paste(block_id, ward_2014, sep = "_")
  ) %>%
  left_join(
    ward_score_lookup %>% rename(ward_origin = ward, strictness_origin = strictness),
    by = "ward_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_score_lookup %>% rename(ward_dest = ward, strictness_dest = strictness),
    by = "ward_dest",
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_turnover_lookup %>% rename(ward_had_turnover_2015 = ward_had_turnover),
    by = "ward_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    block_treatment_lookup,
    by = "block_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    strictness_change = strictness_dest - strictness_origin,
    valid_control_2015 = !switched_2015 & !is.na(ward_had_turnover_2015) & !ward_had_turnover_2015,
    cohort = 2015L
  )

missing_treatment <- parcel_near %>%
  filter(
    is.na(strictness_origin) |
      is.na(strictness_dest) |
      is.na(ward_had_turnover_2015) |
      is.na(block_switched_2015) |
      is.na(block_valid_control_2015)
  )
if (nrow(missing_treatment) > 0) {
  stop(
    sprintf("Treatment or strictness fields are missing for %d near-boundary parcels.", nrow(missing_treatment)),
    call. = FALSE
  )
}

message("Joining fixed parcel geography back to the annual parcel panel...")
panel_final <- panel %>%
  semi_join(parcel_near %>% select(pin10), by = "pin10") %>%
  left_join(
    parcel_near %>%
      select(
        pin10, block_id, block_assignment_method, ward_2014, ward_2015, neighbor_ward_2014,
        ward_origin, ward_dest, ward_pair_id, segment_id, dist_to_boundary_ft,
        in_500ft, in_1000ft, block_origin_side_id, switched_2015, valid_control_2015,
        ward_had_turnover_2015, strictness_origin, strictness_dest, strictness_change,
        block_switched_2015, block_treatment_ward_origin, block_treatment_ward_dest,
        block_valid_control_2015, block_ward_had_turnover_2015, cohort
      ),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  arrange(pin10, tax_year)

if (anyDuplicated(panel_final[c("pin10", "tax_year")]) > 0) {
  stop("Final panel is not unique by pin10 x tax_year.", call. = FALSE)
}

fixed_geo_check <- panel_final %>%
  group_by(pin10) %>%
  summarise(
    n_block = n_distinct(block_id),
    n_ward_2014 = n_distinct(ward_2014),
    n_ward_2015 = n_distinct(ward_2015),
    n_dist = n_distinct(dist_to_boundary_ft),
    n_pair = n_distinct(ward_pair_id),
    n_segment = n_distinct(segment_id),
    .groups = "drop"
  ) %>%
  filter(n_block > 1 | n_ward_2014 > 1 | n_ward_2015 > 1 | n_dist > 1 | n_pair > 1 | n_segment > 1)

if (nrow(fixed_geo_check) > 0) {
  stop("Fixed parcel geography varies across tax years for at least one pin10.", call. = FALSE)
}

baseline_last_pre <- panel_final %>%
  filter(tax_year >= 2012L, tax_year <= 2014L) %>%
  group_by(pin10) %>%
  arrange(desc(tax_year), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  transmute(
    pin10,
    baseline_pre_year_2012_2014_last = tax_year,
    baseline_empty_last_pre_2012_2014 = bldg_sum == 0,
    baseline_land_sum = land_sum,
    baseline_bldg_sum = bldg_sum,
    baseline_land_psf = land_psf
  )

panel_final <- panel_final %>%
  left_join(
    baseline_last_pre,
    by = "pin10",
    relationship = "many-to-one"
  )

support <- panel_final %>%
  group_by(pin10) %>%
  summarise(
    observed_tax_years = paste(sort(unique(tax_year)), collapse = ";"),
    n_tax_years_observed = n_distinct(tax_year),
    first_tax_year = min(tax_year, na.rm = TRUE),
    last_tax_year = max(tax_year, na.rm = TRUE),
    has_2010 = any(tax_year == 2010L),
    has_2011 = any(tax_year == 2011L),
    has_2012 = any(tax_year == 2012L),
    has_2013 = any(tax_year == 2013L),
    has_2014 = any(tax_year == 2014L),
    has_2015 = any(tax_year == 2015L),
    has_2016 = any(tax_year == 2016L),
    has_2017 = any(tax_year == 2017L),
    has_2018 = any(tax_year == 2018L),
    has_2019 = any(tax_year == 2019L),
    has_2020 = any(tax_year == 2020L),
    baseline_pre_year_2012_2014_last = dplyr::first(baseline_pre_year_2012_2014_last),
    baseline_empty_last_pre_2012_2014 = dplyr::first(baseline_empty_last_pre_2012_2014),
    baseline_land_sum = dplyr::first(baseline_land_sum),
    baseline_bldg_sum = dplyr::first(baseline_bldg_sum),
    baseline_land_psf = dplyr::first(baseline_land_psf),
    .groups = "drop"
  ) %>%
  mutate(
    balanced_raw_2010_2013_2016_2019 = has_2010 & has_2013 & has_2016 & has_2019,
    balanced_raw_2011_2014_2017_2020 = has_2011 & has_2014 & has_2017 & has_2020,
    balanced_raw_2012_2015_2018 = has_2012 & has_2015 & has_2018
  ) %>%
  left_join(
    panel_final %>%
      distinct(
        pin10, block_id, block_origin_side_id, ward_origin, ward_dest, ward_pair_id,
        segment_id, switched_2015, valid_control_2015, strictness_origin, strictness_dest,
        strictness_change, in_500ft, in_1000ft, triad_name, township_name, class_current,
        property_use_group_current, property_use_group_current_resolution,
        property_use_standardized_current, property_use_standardized_current_resolution,
        property_use_muni_current, property_use_muni_current_resolution,
        zoned_code_local_current, zoned_code_local_current_resolution,
        owner_type_description1_current, owner_type_description1_current_resolution,
        any_exemption_current,
        lot_sqft_current, lot_sqft_resolution, lot_sqft_n_values, lot_sqft_n_top_ties
      ),
    by = "pin10",
    relationship = "one-to-one"
  ) %>%
  arrange(pin10)

diagnostics <- bind_rows(
  build_scope_diagnostics(panel_final, "1000ft"),
  build_scope_diagnostics(filter(panel_final, in_500ft), "500ft")
)

panel_final <- panel_final %>%
  select(
    pin10, tax_year, cohort,
    tax_year_source, tax_year_source_n,
    land_sum, bldg_sum, total_av, land_share_pin10, log_land_sum,
    lot_sqft_current, lot_sqft_resolution, lot_sqft_n_values, lot_sqft_n_top_ties,
    land_psf, log_land_psf, land_only,
    class_primary, class_n, n_pins, class_current,
    property_use_group_current, property_use_group_current_resolution,
    property_use_group_current_n_values, property_use_group_current_n_top_ties,
    property_use_standardized_current, property_use_standardized_current_resolution,
    property_use_standardized_current_n_values, property_use_standardized_current_n_top_ties,
    property_use_muni_current, property_use_muni_current_resolution,
    property_use_muni_current_n_values, property_use_muni_current_n_top_ties,
    zoned_code_local_current, zoned_code_local_current_resolution,
    zoned_code_local_current_n_values, zoned_code_local_current_n_top_ties,
    owner_type_description1_current, owner_type_description1_current_resolution,
    owner_type_description1_current_n_values, owner_type_description1_current_n_top_ties,
    any_exemption_current, any_exemption_current_n_values,
    triad_name, township_name, municipality_name,
    longitude, latitude, centroid_x_crs_3435, centroid_y_crs_3435,
    block_id, block_assignment_method, block_origin_side_id,
    ward_2014, ward_2015, neighbor_ward_2014, ward_origin, ward_dest, ward_pair_id, segment_id,
    dist_to_boundary_ft, in_500ft, in_1000ft,
    switched_2015, valid_control_2015, ward_had_turnover_2015,
    strictness_origin, strictness_dest, strictness_change,
    baseline_pre_year_2012_2014_last, baseline_empty_last_pre_2012_2014,
    baseline_land_sum, baseline_bldg_sum, baseline_land_psf,
    block_switched_2015, block_treatment_ward_origin, block_treatment_ward_dest,
    block_valid_control_2015, block_ward_had_turnover_2015
  )

message("Writing panel, diagnostics, and parcel support outputs...")
arrow::write_parquet(panel_final, out_panel, compression = "zstd")
write_csv(diagnostics, out_diag)
write_csv(support, out_support)

message(sprintf("Saved %s", out_panel))
message(sprintf("Saved %s", out_diag))
message(sprintf("Saved %s", out_support))
