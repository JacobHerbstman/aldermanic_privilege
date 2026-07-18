# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

sf_use_s2(FALSE)

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- build_canonical_boundary_list(ward_panel)

blocks <- read_csv(
  "../../../download_chicago_spatial_data/output/census_blocks_2010.csv",
  show_col_types = FALSE
) %>%
  rename(geometry = the_geom, block_id = GEOID10) %>%
  mutate(block_id = as.character(block_id)) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(st_crs(ward_panel)) %>%
  distinct(block_id, .keep_all = TRUE)

treatment <- read_csv(
  "../../../create_block_treatment_panel/output/block_treatment_pre_scores.csv",
  show_col_types = FALSE
) %>%
  filter(as.character(cohort) == "2015") %>%
  transmute(
    block_id = as.character(block_id),
    ward_origin,
    ward_dest,
    ward_origin_share,
    ward_dest_share,
    ward_origin_n_wards,
    ward_dest_n_wards,
    switched,
    ward_had_turnover,
    valid_current = valid,
    valid_all_turnover = has_complete_ward_assignment
  )

centroids <- st_centroid(blocks)
surface_points <- st_point_on_surface(blocks)
centroid_hits <- st_within(centroids, blocks)
centroid_inside_own_block <- vapply(
  seq_along(centroid_hits),
  function(i) i %in% centroid_hits[[i]],
  logical(1)
)

control_centroid <- assign_points_to_boundaries(
  centroids,
  rep("2003_2014", nrow(centroids)),
  ward_maps,
  boundary_lines,
  chunk_n = 5000L
) %>%
  transmute(
    block_id = blocks$block_id,
    centroid_ward = ward,
    control_neighbor_ward = neighbor_ward,
    control_pair_id = normalize_pair_dash(ward_pair_id),
    control_dist_m = dist_m
  )

control_surface <- assign_points_to_boundaries(
  surface_points,
  rep("2003_2014", nrow(surface_points)),
  ward_maps,
  boundary_lines,
  chunk_n = 5000L
) %>%
  transmute(
    block_id = blocks$block_id,
    surface_ward = ward,
    surface_control_neighbor_ward = neighbor_ward,
    surface_control_pair_id = normalize_pair_dash(ward_pair_id),
    surface_control_dist_m = dist_m
  )

spatial <- treatment %>%
  left_join(control_centroid, by = "block_id", relationship = "one-to-one") %>%
  left_join(control_surface, by = "block_id", relationship = "one-to-one") %>%
  mutate(
    centroid_inside_own_block = centroid_inside_own_block[match(block_id, blocks$block_id)],
    majority_centroid_mismatch = ward_origin != centroid_ward,
    treated_pair_id = normalize_pair_id(ward_origin, ward_dest, sep = "-"),
    event_pair_id = if_else(switched, treated_pair_id, control_pair_id)
  )

block_index <- match(spatial$block_id, blocks$block_id)
spatial$treated_dist_m <- distance_to_boundary_pair_m(
  centroids[block_index, ],
  spatial$treated_pair_id,
  boundary_lines[["2003_2014"]],
  chunk_n = 5000L
)
spatial$surface_treated_dist_m <- distance_to_boundary_pair_m(
  surface_points[block_index, ],
  spatial$treated_pair_id,
  boundary_lines[["2003_2014"]],
  chunk_n = 5000L
)
spatial <- spatial %>%
  mutate(
    event_dist_m = if_else(switched, treated_dist_m, control_dist_m),
    surface_origin_mismatch = ward_origin != surface_ward,
    surface_event_pair_id = if_else(switched, treated_pair_id, surface_control_pair_id),
    surface_event_dist_m = if_else(switched, surface_treated_dist_m, surface_control_dist_m),
    centroid_500ft_sample = valid_current & !majority_centroid_mismatch & event_dist_m <= 152.4,
    surface_500ft_sample = valid_current & !surface_origin_mismatch & surface_event_dist_m <= 152.4
  )

summary_rows <- bind_rows(
  tibble(
    statistic = c(
      "all_2010_blocks",
      "complete_ward_assignment",
      "current_valid_rule",
      "turnover_agnostic_valid_rule",
      "unchanged_controls_excluded_for_turnover",
      "majority_centroid_mismatch_complete",
      "centroid_outside_own_block_complete",
      "pre_map_multiward_blocks",
      "post_map_multiward_blocks",
      "current_valid_within_500ft_before_centroid_agreement",
      "current_valid_within_500ft_after_centroid_agreement",
      "all_turnover_within_500ft_after_centroid_agreement",
      "switched_within_500ft_after_centroid_agreement",
      "unchanged_within_500ft_current_rule_after_centroid_agreement",
      "surface_rule_within_500ft_after_surface_agreement",
      "blocks_switching_500ft_sample_centroid_vs_surface",
      "surface_500ft_blocks_with_centroid_distance_above_800m"
    ),
    value = c(
      nrow(spatial),
      sum(spatial$valid_all_turnover),
      sum(spatial$valid_current),
      sum(spatial$valid_all_turnover),
      sum(!spatial$switched & spatial$ward_had_turnover & spatial$valid_all_turnover & !spatial$valid_current),
      sum(spatial$valid_all_turnover & spatial$majority_centroid_mismatch, na.rm = TRUE),
      sum(spatial$valid_all_turnover & !spatial$centroid_inside_own_block, na.rm = TRUE),
      sum(spatial$valid_all_turnover & spatial$ward_origin_n_wards > 1, na.rm = TRUE),
      sum(spatial$valid_all_turnover & spatial$ward_dest_n_wards > 1, na.rm = TRUE),
      sum(spatial$valid_current & spatial$event_dist_m <= 152.4, na.rm = TRUE),
      sum(spatial$valid_current & !spatial$majority_centroid_mismatch & spatial$event_dist_m <= 152.4, na.rm = TRUE),
      sum(spatial$valid_all_turnover & !spatial$majority_centroid_mismatch & spatial$event_dist_m <= 152.4, na.rm = TRUE),
      sum(spatial$valid_current & spatial$switched & !spatial$majority_centroid_mismatch & spatial$event_dist_m <= 152.4, na.rm = TRUE),
      sum(spatial$valid_current & !spatial$switched & !spatial$majority_centroid_mismatch & spatial$event_dist_m <= 152.4, na.rm = TRUE),
      sum(spatial$surface_500ft_sample, na.rm = TRUE),
      sum(spatial$centroid_500ft_sample != spatial$surface_500ft_sample, na.rm = TRUE),
      sum(spatial$surface_500ft_sample & spatial$event_dist_m > 800, na.rm = TRUE)
    )
  ),
  tibble(
    statistic = c(
      "minimum_origin_majority_share_complete",
      "p01_origin_majority_share_complete",
      "median_origin_majority_share_complete",
      "minimum_destination_majority_share_complete",
      "maximum_centroid_vs_surface_distance_change_m",
      "blocks_switching_500ft_inclusion_centroid_vs_surface_control"
    ),
    value = c(
      min(spatial$ward_origin_share[spatial$valid_all_turnover], na.rm = TRUE),
      quantile(spatial$ward_origin_share[spatial$valid_all_turnover], 0.01, na.rm = TRUE),
      median(spatial$ward_origin_share[spatial$valid_all_turnover], na.rm = TRUE),
      min(spatial$ward_dest_share[spatial$valid_all_turnover], na.rm = TRUE),
      max(abs(spatial$control_dist_m - spatial$surface_control_dist_m), na.rm = TRUE),
      sum((spatial$control_dist_m <= 152.4) != (spatial$surface_control_dist_m <= 152.4), na.rm = TRUE)
    )
  )
)
write_csv(summary_rows, "../output/permit_spatial_design_summary.csv")

write_csv(
  spatial %>%
    st_drop_geometry() %>%
    select(
      block_id, switched, valid_current, ward_origin, ward_dest,
      majority_centroid_mismatch, surface_origin_mismatch,
      event_pair_id, surface_event_pair_id,
      event_dist_m, surface_event_dist_m,
      centroid_500ft_sample, surface_500ft_sample
    ),
  "../output/permit_spatial_block_assignments.csv"
)

production <- read_parquet(
  "../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"
) %>%
  filter(relative_year == -1L, dist_m <= 152.4) %>%
  select(block_id, production_pair_id = ward_pair_id, production_dist_m = dist_m)

expected <- spatial %>%
  filter(
    valid_current,
    !majority_centroid_mismatch,
    event_dist_m <= 152.4
  ) %>%
  select(block_id, expected_pair_id = event_pair_id, expected_dist_m = event_dist_m)

panel_comparison <- full_join(production, expected, by = "block_id", relationship = "one-to-one") %>%
  summarise(
    production_blocks = sum(!is.na(production_pair_id)),
    independently_expected_blocks = sum(!is.na(expected_pair_id)),
    common_blocks = sum(!is.na(production_pair_id) & !is.na(expected_pair_id)),
    production_only_blocks = sum(!is.na(production_pair_id) & is.na(expected_pair_id)),
    expected_only_blocks = sum(is.na(production_pair_id) & !is.na(expected_pair_id)),
    changed_pair_blocks = sum(
      !is.na(production_pair_id) & !is.na(expected_pair_id) &
        normalize_pair_dash(production_pair_id) != normalize_pair_dash(expected_pair_id)
    ),
    maximum_distance_difference_m = max(abs(production_dist_m - expected_dist_m), na.rm = TRUE)
  )
write_csv(panel_comparison, "../output/permit_spatial_panel_comparison.csv")
