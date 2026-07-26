# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

library(sf)

review <- readr::read_csv(
  "../output/multicard_final_review_bundle.csv",
  show_col_types = FALSE
)
matches <- readr::read_csv(
  "../output/multicard_component_successor_matches.csv",
  show_col_types = FALSE
)
model_input <- readr::read_csv(
  "../output/final_density_model_input.csv",
  show_col_types = FALSE
)
boundary_scope <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE
)
overrides <- readr::read_csv(
  "../adjudication/multicard_manual_overrides.csv",
  show_col_types = FALSE
)
pair_decisions <- readr::read_csv(
  "../adjudication/multicard_parent_pair_decisions.csv",
  show_col_types = FALSE
)
manual_episode_decisions <- readr::read_csv(
  "../adjudication/multicard_manual_episode_decisions.csv",
  show_col_types = FALSE
)
year_overrides <- readr::read_csv(
  "../adjudication/multicard_year_overrides.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(
    year_confidence = confidence,
    year_evidence = evidence
  )
cross_project_suppressions <- readr::read_csv(
  "../adjudication/multicard_cross_project_suppressions.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(
    cross_project_confidence = confidence,
    cross_project_evidence = evidence
  )

if (
  anyDuplicated(review$project_id) ||
    anyDuplicated(model_input$project_id) ||
    anyDuplicated(boundary_scope$project_id) ||
    anyDuplicated(overrides$project_id) ||
    anyDuplicated(manual_episode_decisions$project_id) ||
    anyDuplicated(year_overrides$project_id) ||
    anyDuplicated(cross_project_suppressions$project_id)
) {
  stop("Multicard adjudication inputs must be unique by project.", call. = FALSE)
}
if (
  nrow(review) != 273L ||
    !setequal(
      review$project_id,
      model_input$project_id[
        model_input$project_kind == "same_pin_multiple_cards"
      ]
    )
) {
  stop("The multicard review does not cover the model-input universe.", call. = FALSE)
}
if (!all(overrides$project_id %in% review$project_id)) {
  stop("A manual override is outside the multicard review universe.", call. = FALSE)
}
manual_review_projects <- review |>
  dplyr::filter(review_class == "manual_episode_review") |>
  dplyr::select(project_id, component_id)
if (
  !setequal(
    manual_episode_decisions$project_id,
    manual_review_projects$project_id
  ) ||
    any(!manual_episode_decisions$disposition %in% c(
      "retain_card_inventory",
      "retain_manual_override"
    )) ||
    any(!manual_episode_decisions$successor_rule %in% c(
      "suppress_matched_successors",
      "none"
    )) ||
    any(
      manual_episode_decisions |>
        dplyr::inner_join(
          manual_review_projects,
          by = "project_id",
          relationship = "one-to-one",
          suffix = c("_decision", "_review")
        ) |>
        dplyr::filter(component_id_decision != component_id_review) |>
        nrow() > 0
    )
) {
  stop("Manual episode decisions do not cover the review queue.", call. = FALSE)
}
if (
  !all(year_overrides$project_id %in% review$project_id) ||
    !all(cross_project_suppressions$project_id %in% model_input$project_id) ||
    !all(
      cross_project_suppressions$represented_by_project_id %in%
        model_input$project_id
    )
) {
  stop("A year override or cross-project suppression is out of scope.", call. = FALSE)
}

adjudication <- review |>
  dplyr::mutate(
    rule_disposition = "retain",
    rule_units = dplyr::case_when(
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) &
        successor_candidate_units == summed_card_units ~
        successor_candidate_units,
      TRUE ~ summed_card_units
    ),
    rule_building_sqft = dplyr::case_when(
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) &
        successor_candidate_units == summed_card_units &
        is.finite(successor_candidate_building_sqft) &
        successor_candidate_building_sqft > 0 ~
        successor_candidate_building_sqft,
      TRUE ~ summed_card_building_sqft
    ),
    rule_reason = dplyr::case_when(
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) &
        successor_candidate_units == summed_card_units ~
        "completed_successor_episode",
      TRUE ~ "contemporaneous_card_inventory"
    ),
    rule_confidence = dplyr::case_when(
      requires_manual_adjudication ~ "manual_review_complete",
      TRUE ~ "high"
    ),
    rule_evidence = dplyr::case_when(
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) &
        successor_candidate_units == summed_card_units ~ paste0(
        "cards=", target_cards,
        "; card_units=", summed_card_units,
        "; successor_units=", successor_candidate_units,
        "; successor_building_sqft=",
        successor_candidate_building_sqft,
        "; successor_projects=", successor_candidate_project_ids
      ),
      TRUE ~ paste0(
        "cards=", target_cards,
        "; card_units=", summed_card_units,
        "; card_building_sqft=", summed_card_building_sqft,
        "; permit_addresses=",
        dplyr::coalesce(permit_addresses, "none")
      )
    )
  ) |>
  dplyr::left_join(
    overrides |>
      dplyr::rename(
        override_disposition = disposition,
        override_units = final_units,
        override_building_sqft = final_building_sqft,
        override_confidence = confidence,
        override_evidence = evidence
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    final_disposition = dplyr::coalesce(
      override_disposition,
      rule_disposition
    ),
    final_units = dplyr::if_else(
      final_disposition == "retain",
      dplyr::coalesce(override_units, rule_units),
      NA_real_
    ),
    final_building_sqft = dplyr::if_else(
      final_disposition == "retain",
      dplyr::coalesce(override_building_sqft, rule_building_sqft),
      NA_real_
    ),
    adjudication_reason = dplyr::if_else(
      !is.na(override_disposition),
      "manual_override",
      rule_reason
    ),
    adjudication_confidence = dplyr::coalesce(
      override_confidence,
      rule_confidence
    ),
    adjudication_evidence = dplyr::coalesce(
      override_evidence,
      rule_evidence
    )
  ) |>
  dplyr::left_join(
    year_overrides,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    reported_construction_year = construction_year,
    construction_year = dplyr::coalesce(
      final_construction_year,
      construction_year
    ),
    construction_year_source = dplyr::if_else(
      !is.na(final_construction_year),
      "manual_completed_project_evidence",
      "existing_adjudicated_year"
    )
  ) |>
  dplyr::left_join(
    manual_episode_decisions |>
      dplyr::rename(
        episode_disposition = disposition,
        episode_successor_rule = successor_rule,
        episode_confidence = confidence,
        episode_evidence = evidence
      ),
    by = c("project_id", "component_id"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    adjudication_reason = dplyr::if_else(
      !is.na(episode_disposition),
      episode_disposition,
      adjudication_reason
    ),
    adjudication_confidence = dplyr::coalesce(
      episode_confidence,
      adjudication_confidence
    ),
    adjudication_evidence = dplyr::coalesce(
      episode_evidence,
      adjudication_evidence
    )
  ) |>
  dplyr::left_join(
    cross_project_suppressions |>
      dplyr::select(
        project_id,
        cross_represented_by_project_id = represented_by_project_id,
        cross_project_confidence,
        cross_project_evidence
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    final_disposition = dplyr::if_else(
      !is.na(cross_represented_by_project_id),
      "suppress_cross_project_duplicate",
      final_disposition
    ),
    final_units = dplyr::if_else(
      final_disposition == "retain",
      final_units,
      NA_real_
    ),
    final_building_sqft = dplyr::if_else(
      final_disposition == "retain",
      final_building_sqft,
      NA_real_
    ),
    adjudication_reason = dplyr::if_else(
      !is.na(cross_represented_by_project_id),
      "cross_project_duplicate_episode",
      adjudication_reason
    ),
    adjudication_confidence = dplyr::coalesce(
      cross_project_confidence,
      adjudication_confidence
    ),
    adjudication_evidence = dplyr::coalesce(
      cross_project_evidence,
      adjudication_evidence
    )
  )

if (
  any(
    adjudication$final_disposition == "retain" &
      (
        !is.finite(adjudication$final_units) |
          adjudication$final_units <= 0 |
          !is.finite(adjudication$final_building_sqft) |
          adjudication$final_building_sqft <= 0
      )
  ) ||
    any(!adjudication$final_disposition %in% c(
      "retain",
      "suppress_duplicate_parent",
      "suppress_cross_project_duplicate"
    ))
) {
  stop("A final multicard decision has invalid values.", call. = FALSE)
}

suppression_parents <- adjudication |>
  dplyr::left_join(
    manual_episode_decisions |>
      dplyr::select(project_id, successor_rule),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    final_disposition == "retain",
    review_class %in% c(
      "successor_episode_exact_reproduction",
      "successor_episode_units_reproduced"
    ) |
      successor_rule == "suppress_matched_successors"
  ) |>
  dplyr::select(project_id)

successor_suppressions <- matches |>
  dplyr::filter(!is.na(represented_project_ids)) |>
  dplyr::inner_join(
    suppression_parents,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::rename(represented_by_project_id = project_id) |>
  dplyr::transmute(
    project_id = represented_project_ids,
    suppress_reason = "one_to_one_predecessor_card_match",
    represented_by_project_id,
    component_id,
    card_id,
    successor_id,
    match_quality,
    building_ratio,
    construction_year,
    successor_year,
    successor_address,
    suppression_evidence = paste0(
      "card=", card_id,
      "; successor=", successor_id,
      "; match_quality=", match_quality
    )
  ) |>
  dplyr::distinct(project_id, .keep_all = TRUE)

if (anyDuplicated(successor_suppressions$project_id)) {
  stop("A successor project is matched to more than one card.", call. = FALSE)
}

complete_episode_suppressions <- adjudication |>
  dplyr::filter(
    review_class %in% c(
      "successor_episode_exact_reproduction",
      "successor_episode_units_reproduced"
    ),
    successor_candidate_units == summed_card_units,
    !is.na(successor_candidate_project_ids)
  ) |>
  dplyr::select(
    represented_by_project_id = project_id,
    component_id,
    construction_year,
    successor_candidate_project_ids
  ) |>
  tidyr::separate_longer_delim(
    successor_candidate_project_ids,
    delim = "/"
  ) |>
  dplyr::transmute(
    project_id = successor_candidate_project_ids,
    suppress_reason = "complete_successor_episode",
    represented_by_project_id,
    component_id,
    card_id = NA_character_,
    successor_id = NA_character_,
    match_quality = "aggregate_unit_reproduction",
    building_ratio = NA_real_,
    construction_year,
    successor_year = NA_real_,
    successor_address = NA_character_,
    suppression_evidence = paste0(
      "The completed successor episode reproduces ",
      "the parent card inventory."
    )
  ) |>
  dplyr::anti_join(
    successor_suppressions,
    by = "project_id"
  )

manual_parent_suppressions <- adjudication |>
  dplyr::filter(final_disposition == "suppress_duplicate_parent") |>
  dplyr::transmute(
    project_id,
    suppress_reason = "manual_duplicate_parent_decision",
    represented_by_project_id = NA_character_,
    component_id,
    card_id = NA_character_,
    successor_id = NA_character_,
    match_quality = NA_character_,
    building_ratio = NA_real_,
    construction_year,
    successor_year = NA_real_,
    successor_address = NA_character_,
    suppression_evidence = adjudication_evidence
  )

cross_project_suppressions <- cross_project_suppressions |>
  dplyr::left_join(
    model_input |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::transmute(
    project_id,
    suppress_reason = "cross_project_duplicate_episode",
    represented_by_project_id,
    component_id = NA_character_,
    card_id = NA_character_,
    successor_id = NA_character_,
    match_quality = cross_project_confidence,
    building_ratio = NA_real_,
    construction_year,
    successor_year = NA_real_,
    successor_address = NA_character_,
    suppression_evidence = cross_project_evidence
  )

successor_suppressions <- dplyr::bind_rows(
  successor_suppressions,
  complete_episode_suppressions,
  cross_project_suppressions,
  manual_parent_suppressions
) |>
  dplyr::filter(project_id %in% model_input$project_id) |>
  dplyr::arrange(
    project_id,
    factor(
      suppress_reason,
      levels = c(
        "one_to_one_predecessor_card_match",
        "complete_successor_episode",
        "cross_project_duplicate_episode",
        "manual_duplicate_parent_decision"
      )
    )
  ) |>
  dplyr::distinct(project_id, .keep_all = TRUE) |>
  dplyr::arrange(project_id)

if (anyDuplicated(successor_suppressions$project_id)) {
  stop("A final suppression project appears more than once.", call. = FALSE)
}

matched_assignments <- successor_suppressions |>
  dplyr::filter(!is.na(represented_by_project_id)) |>
  dplyr::left_join(
    model_input |>
      dplyr::select(
        project_id,
        child_ward = ward,
        child_ward_pair = ward_pair,
        child_segment_id = segment_id,
        child_distance_to_boundary_ft = distance_to_boundary_ft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    model_input |>
      dplyr::select(
        represented_by_project_id = project_id,
        parent_ward = ward,
        parent_ward_pair = ward_pair,
        parent_segment_id = segment_id,
        parent_distance_to_boundary_ft = distance_to_boundary_ft
      ),
    by = "represented_by_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    boundary_scope |>
      dplyr::select(
        project_id,
        child_x_3435 = x_3435,
        child_y_3435 = y_3435
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    boundary_scope |>
      dplyr::select(
        represented_by_project_id = project_id,
        parent_x_3435 = x_3435,
        parent_y_3435 = y_3435
      ),
    by = "represented_by_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    physical_distance_ft = sqrt(
      (child_x_3435 - parent_x_3435)^2 +
        (child_y_3435 - parent_y_3435)^2
    ),
    same_ward_assignment = child_ward == parent_ward,
    same_ward_pair_assignment =
      child_ward == parent_ward &
        child_ward_pair == parent_ward_pair,
    same_boundary_assignment =
      same_ward_pair_assignment &
        child_segment_id == parent_segment_id &
        abs(
          child_distance_to_boundary_ft -
            parent_distance_to_boundary_ft
        ) < 1e-8
  )

if (
  any(!is.finite(matched_assignments$physical_distance_ft)) ||
    any(matched_assignments$physical_distance_ft > 500)
) {
  failed_assignments <- matched_assignments |>
      dplyr::filter(
        !is.finite(physical_distance_ft) |
          physical_distance_ft > 500
      ) |>
      dplyr::select(
        project_id,
        represented_by_project_id,
        suppress_reason,
        child_ward,
        parent_ward,
        child_ward_pair,
        parent_ward_pair,
        child_segment_id,
        parent_segment_id,
        child_distance_to_boundary_ft,
        parent_distance_to_boundary_ft,
        physical_distance_ft,
        same_ward_assignment,
        same_ward_pair_assignment,
        same_boundary_assignment
      )
  print(
    failed_assignments |>
      dplyr::count(
        suppress_reason,
        same_ward_pair_assignment,
        same_boundary_assignment
      )
  )
  print(as.data.frame(utils::head(failed_assignments, 10)))
  stop(
    "A suppressed project is outside its parent site or ward.",
    call. = FALSE
  )
}

final_model_input <- model_input |>
  dplyr::left_join(
    adjudication |>
      dplyr::select(
        project_id,
        adjudicated_units = final_units,
        adjudicated_building_sqft = final_building_sqft,
        final_disposition,
        adjudication_reason,
        adjudication_confidence
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(!project_id %in% successor_suppressions$project_id) |>
  dplyr::mutate(
    dwelling_units = dplyr::coalesce(
      adjudicated_units,
      dwelling_units
    ),
    building_sqft = dplyr::coalesce(
      adjudicated_building_sqft,
      building_sqft
    ),
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
  ) |>
  dplyr::select(
    -adjudicated_units,
    -adjudicated_building_sqft
  )

override_rows <- final_model_input |>
  dplyr::inner_join(
    year_overrides,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    boundary_scope |>
      dplyr::select(project_id, x_3435, y_3435),
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  nrow(override_rows) != nrow(year_overrides) ||
    any(override_rows$zone_group != override_rows$expected_zone_group) ||
    any(!is.finite(override_rows$x_3435)) ||
    any(!is.finite(override_rows$y_3435))
) {
  stop("A construction-year override lacks stable zoning or geometry.", call. = FALSE)
}

override_points <- override_rows |>
  dplyr::transmute(
    project_id,
    construction_year = final_construction_year,
    construction_date = as.Date(
      paste0(final_construction_year, "-06-15")
    ),
    boundary_year = canonical_boundary_year_from_date(
      construction_date
    ),
    era = canonical_era_from_boundary_year(boundary_year),
    x_3435,
    y_3435
  ) |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) |>
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers(
  "../input/ward_pair_boundaries.gpkg"
)
override_boundary_assignment <- assign_points_to_boundaries(
  points_sf = override_points,
  era_values = override_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 100L
)

override_assignment <- dplyr::bind_cols(
  sf::st_drop_geometry(override_points),
  override_boundary_assignment
) |>
  dplyr::transmute(
    project_id,
    construction_year,
    construction_date,
    era,
    ward,
    neighbor_ward,
    ward_pair = ward_pair_id,
    distance_to_boundary_ft = dist_ft,
    within_500ft = is.finite(dist_ft) & dist_ft <= 500,
    within_1500ft = is.finite(dist_ft) & dist_ft <= 1500,
    x_3435,
    y_3435
  )

override_assignment_points <- sf::st_as_sf(
  override_assignment,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(override_assignment$era))
)
override_assignment$segment_id <- assign_points_to_nearest_segments(
  override_assignment_points,
  override_assignment$era,
  override_assignment$ward_pair,
  segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 100L
)

aldermen <- readr::read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(
    ward = as.integer(ward),
    yearmon_key = as.character(
      zoo::as.yearmon(month, format = "%b %Y")
    ),
    alderman
  )
scores <- readr::read_csv(
  "../input/alderman_uncertainty_index.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, score = uncertainty_index)
controls <- readr::read_csv(
  "../input/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)

if (
  anyDuplicated(aldermen[c("ward", "yearmon_key")]) ||
    anyDuplicated(scores$alderman) ||
    anyDuplicated(controls[c("ward", "year")])
) {
  stop("A year-reassignment lookup is not unique.", call. = FALSE)
}

override_assignment <- override_assignment |>
  dplyr::mutate(
    yearmon_key = as.character(zoo::as.yearmon(construction_date)),
    dist_to_boundary_m = distance_to_boundary_ft * 0.3048
  ) |>
  dplyr::left_join(
    aldermen,
    by = c("ward", "yearmon_key"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(alderman_own = alderman) |>
  dplyr::left_join(
    aldermen |>
      dplyr::rename(alderman_neighbor = alderman),
    by = c("neighbor_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(strictness_own = score) |>
  dplyr::left_join(
    scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(strictness_neighbor = score) |>
  dplyr::left_join(
    controls,
    by = c("ward", "construction_year" = "year"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    signed_distance_m = dist_to_boundary_m * dplyr::case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    lenient_dist = abs(signed_distance_m) *
      as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) *
      as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference =
      (strictness_own - strictness_neighbor) / 2,
    pair_average_score =
      (strictness_own + strictness_neighbor) / 2
  ) |>
  dplyr::select(
    project_id,
    construction_year,
    construction_date,
    ward,
    neighbor_ward,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    within_1500ft,
    segment_id,
    alderman_own,
    alderman_neighbor,
    strictness_own,
    strictness_neighbor,
    signed_distance_m,
    lenient_dist,
    strict_dist,
    side,
    continuous_score_difference,
    pair_average_score,
    share_white_own = share_white,
    share_black_own = share_black,
    median_hh_income_own = median_hh_income,
    share_bach_plus_own = share_bach_plus,
    homeownership_rate_own = homeownership_rate
  )

reassigned_fields <- setdiff(
  names(override_assignment),
  "project_id"
)
override_rows <- override_rows |>
  dplyr::select(
    -dplyr::all_of(reassigned_fields),
    -final_construction_year,
    -expected_zone_group,
    -year_confidence,
    -year_evidence,
    -x_3435,
    -y_3435
  ) |>
  dplyr::left_join(
    override_assignment,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::select(dplyr::all_of(names(final_model_input)))

final_model_input <- dplyr::bind_rows(
  final_model_input |>
    dplyr::anti_join(year_overrides, by = "project_id"),
  override_rows |>
    dplyr::filter(within_1500ft)
) |>
  dplyr::arrange(project_id)

if (
  anyDuplicated(final_model_input$project_id) ||
    any(final_model_input$project_id %in% successor_suppressions$project_id) ||
    any(
      final_model_input$project_kind == "same_pin_multiple_cards" &
        is.na(final_model_input$adjudication_reason)
    )
) {
  stop("The adjudicated density input failed duplicate checks.", call. = FALSE)
}

multicard_pair_projects <- adjudication |>
  dplyr::left_join(
    boundary_scope |>
      dplyr::select(project_id, x_3435, y_3435),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::select(
    project_id,
    construction_year,
    summed_card_units,
    summed_card_building_sqft,
    land_sqft,
    x_3435,
    y_3435
  )

multicard_pairs <- multicard_pair_projects |>
  dplyr::rename_with(
    ~ paste0(.x, "_1"),
    -project_id
  ) |>
  dplyr::rename(project_id_1 = project_id) |>
  dplyr::inner_join(
    multicard_pair_projects |>
      dplyr::rename_with(
        ~ paste0(.x, "_2"),
        -project_id
      ) |>
      dplyr::rename(project_id_2 = project_id),
    by = dplyr::join_by(project_id_1 < project_id_2)
  ) |>
  dplyr::mutate(
    distance_ft = sqrt(
      (x_3435_1 - x_3435_2)^2 +
        (y_3435_1 - y_3435_2)^2
    ),
    year_gap = abs(construction_year_1 - construction_year_2),
    building_ratio = pmin(
      summed_card_building_sqft_1,
      summed_card_building_sqft_2
    ) / pmax(
      summed_card_building_sqft_1,
      summed_card_building_sqft_2
    ),
    land_ratio = pmin(land_sqft_1, land_sqft_2) /
      pmax(land_sqft_1, land_sqft_2)
  ) |>
  dplyr::filter(
    year_gap <= 4,
    distance_ft <= 200,
    (
      summed_card_units_1 == summed_card_units_2 &
        building_ratio >= 0.90
    ) |
      (
        building_ratio >= 0.90 &
          land_ratio >= 0.90
      )
  ) |>
  dplyr::select(
    project_id_1,
    project_id_2,
    construction_year_1,
    construction_year_2,
    summed_card_units_1,
    summed_card_units_2,
    summed_card_building_sqft_1,
    summed_card_building_sqft_2,
    land_sqft_1,
    land_sqft_2,
    distance_ft,
    year_gap,
    building_ratio,
    land_ratio
  ) |>
  dplyr::left_join(
    pair_decisions,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  )

if (any(is.na(multicard_pairs$disposition))) {
  print(as.data.frame(
    multicard_pairs |>
      dplyr::filter(is.na(disposition))
  ))
  stop("A nearby identical multicard pair lacks adjudication.", call. = FALSE)
}
unused_pair_decisions <- pair_decisions |>
  dplyr::anti_join(
    multicard_pairs,
    by = c("project_id_1", "project_id_2")
  )
if (nrow(unused_pair_decisions) > 0) {
  print(unused_pair_decisions)
  stop("A multicard pair decision is no longer in the review set.", call. = FALSE)
}

card_inventory_rows <- adjudication |>
  dplyr::filter(
    final_disposition == "retain",
    is.na(override_disposition),
    adjudication_reason %in% c(
      "contemporaneous_card_inventory",
      "retain_card_inventory"
    )
  )
successor_inventory_rows <- adjudication |>
  dplyr::filter(
    final_disposition == "retain",
    is.na(override_disposition),
    adjudication_reason == "completed_successor_episode"
  )
year_override_check <- final_model_input |>
  dplyr::inner_join(
    year_overrides |>
      dplyr::select(project_id, final_construction_year),
    by = "project_id",
    relationship = "one-to-one"
  )

validation <- dplyr::bind_rows(
  tibble::tibble(
    check = "multicard_projects_reviewed",
    value = nrow(adjudication),
    expected = 273,
    passed = nrow(adjudication) == 273
  ),
  tibble::tibble(
    check = "multicard_projects_retained",
    value = sum(adjudication$final_disposition == "retain"),
    expected = 271,
    passed = sum(adjudication$final_disposition == "retain") == 271
  ),
  tibble::tibble(
    check = "matched_successor_rows_suppressed",
    value = sum(
      successor_suppressions$suppress_reason ==
        "one_to_one_predecessor_card_match"
    ),
    expected = 520,
    passed = sum(
      successor_suppressions$suppress_reason ==
        "one_to_one_predecessor_card_match"
    ) == 520
  ),
  tibble::tibble(
    check = "aggregate_successor_rows_suppressed",
    value = sum(
      successor_suppressions$suppress_reason ==
        "complete_successor_episode"
    ),
    expected = 12,
    passed = sum(
      successor_suppressions$suppress_reason ==
        "complete_successor_episode"
    ) == 12
  ),
  tibble::tibble(
    check = "cross_project_rows_suppressed",
    value = sum(
      successor_suppressions$suppress_reason ==
        "cross_project_duplicate_episode"
    ),
    expected = 2,
    passed = sum(
      successor_suppressions$suppress_reason ==
        "cross_project_duplicate_episode"
    ) == 2
  ),
  tibble::tibble(
    check = "suppressed_projects_sharing_original_ward",
    value = sum(matched_assignments$same_ward_assignment),
    expected = NA_real_,
    passed = TRUE
  ),
  tibble::tibble(
    check = "suppressed_projects_within_parent_site",
    value = max(matched_assignments$physical_distance_ft),
    expected = 500,
    passed = all(matched_assignments$physical_distance_ft <= 500)
  ),
  tibble::tibble(
    check = "card_inventory_values_follow_cards",
    value = sum(
      card_inventory_rows$final_units ==
        card_inventory_rows$summed_card_units &
        card_inventory_rows$final_building_sqft ==
          card_inventory_rows$summed_card_building_sqft
    ),
    expected = nrow(card_inventory_rows),
    passed = all(
      card_inventory_rows$final_units ==
        card_inventory_rows$summed_card_units &
        card_inventory_rows$final_building_sqft ==
          card_inventory_rows$summed_card_building_sqft
    )
  ),
  tibble::tibble(
    check = "successor_inventory_values_follow_completed_episode",
    value = sum(
      successor_inventory_rows$final_units ==
        successor_inventory_rows$summed_card_units &
        successor_inventory_rows$final_building_sqft ==
          successor_inventory_rows$successor_candidate_building_sqft
    ),
    expected = nrow(successor_inventory_rows),
    passed = all(
      successor_inventory_rows$final_units ==
        successor_inventory_rows$summed_card_units &
        successor_inventory_rows$final_building_sqft ==
          successor_inventory_rows$successor_candidate_building_sqft
    )
  ),
  tibble::tibble(
    check = "construction_year_overrides_reassigned",
    value = sum(
      year_override_check$construction_year ==
        year_override_check$final_construction_year
    ),
    expected = nrow(year_overrides),
    passed =
      nrow(year_override_check) == nrow(year_overrides) &&
        all(
          year_override_check$construction_year ==
            year_override_check$final_construction_year
        )
  ),
  tibble::tibble(
    check = "nearby_identical_pairs_adjudicated",
    value = sum(!is.na(multicard_pairs$disposition)),
    expected = nrow(multicard_pairs),
    passed = all(!is.na(multicard_pairs$disposition))
  ),
  tibble::tibble(
    check = "final_project_ids_unique",
    value = anyDuplicated(final_model_input$project_id),
    expected = 0,
    passed = !anyDuplicated(final_model_input$project_id)
  )
)
if (!all(validation$passed)) {
  stop("The final multicard validation failed.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = c(
      "input_projects",
      "final_projects",
      "multicard_projects_reviewed",
      "manual_value_overrides",
      "construction_year_overrides",
      "duplicate_parent_suppressions",
      "matched_successor_suppressions",
      "aggregate_successor_suppressions",
      "cross_project_suppressions",
      "input_projects_within_500ft",
      "final_projects_within_500ft",
      "input_multifamily_within_500ft",
      "final_multifamily_within_500ft"
    ),
    value = c(
      nrow(model_input),
      nrow(final_model_input),
      nrow(adjudication),
      sum(
        !is.na(adjudication$override_disposition) &
          adjudication$override_disposition == "retain"
      ),
      nrow(year_overrides),
      sum(
        successor_suppressions$suppress_reason ==
          "manual_duplicate_parent_decision"
      ),
      sum(
        successor_suppressions$suppress_reason ==
          "one_to_one_predecessor_card_match"
      ),
      sum(
        successor_suppressions$suppress_reason ==
          "complete_successor_episode"
      ),
      sum(
        successor_suppressions$suppress_reason ==
          "cross_project_duplicate_episode"
      ),
      sum(model_input$within_500ft),
      sum(final_model_input$within_500ft),
      sum(
        model_input$within_500ft & model_input$dwelling_units > 1,
        na.rm = TRUE
      ),
      sum(
        final_model_input$within_500ft &
          final_model_input$dwelling_units > 1,
        na.rm = TRUE
      )
    )
  )
)

readr::write_csv(
  adjudication,
  "../output/multicard_final_adjudication.csv",
  na = ""
)
readr::write_csv(
  successor_suppressions,
  "../output/multicard_successor_suppressions.csv",
  na = ""
)
readr::write_csv(
  multicard_pairs,
  "../output/multicard_parent_pair_adjudication.csv",
  na = ""
)
readr::write_csv(
  final_model_input,
  "../output/multicard_adjudicated_density_model_input.csv",
  na = ""
)
readr::write_csv(
  validation,
  "../output/multicard_final_adjudication_validation.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/multicard_final_adjudication_summary.csv",
  na = ""
)
