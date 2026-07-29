# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

parents <- readr::read_csv(
  "../output/multicard_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

component_nodes <- readr::read_csv(
  "../output/multicard_episode_component_nodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(is_parent) |>
  dplyr::select(project_id, component_id)

component_summary <- readr::read_csv(
  "../output/multicard_episode_component_summary.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    component_id,
    component_status,
    automatic_resolution_eligible,
    root_parent_project_ids,
    terminal_child_project_ids,
    root_card_units,
    root_card_building_sqft,
    terminal_child_units,
    terminal_child_building_sqft,
    terminal_child_land_sqft,
    unit_ratio,
    building_sqft_ratio
  )

successors <- readr::read_csv(
  "../output/multicard_successor_building_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    successor_pin = readr::col_character(),
    represented_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(automatic_candidate) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    successor_candidates = dplyr::n(),
    successor_candidate_units = sum(successor_units, na.rm = TRUE),
    successor_candidate_building_sqft = sum(
      successor_building_sqft,
      na.rm = TRUE
    ),
    successor_candidate_land_sqft = sum(
      successor_land_sqft,
      na.rm = TRUE
    ),
    successor_candidate_project_ids = paste(
      sort(unique(
        represented_project_ids[
          !is.na(represented_project_ids)
        ]
      )),
      collapse = "/"
    ),
    successor_candidate_evidence = paste(
      paste0(
        successor_id,
        " [",
        successor_address,
        "] year=",
        successor_year,
        " units=",
        successor_units,
        " sqft=",
        successor_building_sqft,
        " land=",
        successor_land_sqft,
        " project=",
        represented_project_ids
      ),
      collapse = " || "
    ),
    .groups = "drop"
  )

old_manual <- readr::read_csv(
  "../../density_multicard_manual_review/output/manual_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::transmute(
    pin,
    prior_manual_address = address,
    prior_manual_units = verified_units,
    prior_manual_sqft = verified_sqft,
    prior_manual_classification = classification,
    prior_manual_confidence = confidence,
    prior_manual_evidence = evidence
  )

review <- parents |>
  dplyr::left_join(
    component_nodes,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    component_summary,
    by = "component_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    successors,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    old_manual,
    by = "pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    review_class = dplyr::case_when(
      !is.na(prior_manual_classification) ~
        "prior_manual_decision",
      is.na(component_id) & target_cards == 1L ~
        "single_target_card_no_successor_overlap",
      is.na(component_id) &
        preliminary_disposition ==
          "candidate_selected_card_supported" ~
        "selected_card_supported_no_successor_overlap",
      is.na(component_id) &
        preliminary_disposition ==
          "candidate_sum_supported" ~
        "aggregate_cards_supported_no_successor_overlap",
      is.na(component_id) ~
        "manual_no_successor_review",
      component_status == "high_confidence_reproduction" ~
        "successor_episode_exact_reproduction",
      component_status == "unit_reproduction_sqft_changed" ~
        "successor_episode_units_reproduced",
      TRUE ~ "manual_episode_review"
    ),
    preliminary_project_units = dplyr::case_when(
      !is.na(prior_manual_units) ~ prior_manual_units,
      review_class == "single_target_card_no_successor_overlap" ~
        dwelling_units,
      review_class ==
        "selected_card_supported_no_successor_overlap" ~
        dwelling_units,
      review_class ==
        "aggregate_cards_supported_no_successor_overlap" ~
        summed_card_units,
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) ~ terminal_child_units,
      TRUE ~ NA_real_
    ),
    preliminary_project_building_sqft = dplyr::case_when(
      !is.na(prior_manual_sqft) ~ prior_manual_sqft,
      review_class == "single_target_card_no_successor_overlap" ~
        building_sqft,
      review_class ==
        "selected_card_supported_no_successor_overlap" ~
        building_sqft,
      review_class ==
        "aggregate_cards_supported_no_successor_overlap" ~
        summed_card_building_sqft,
      review_class %in% c(
        "successor_episode_exact_reproduction",
        "successor_episode_units_reproduced"
      ) ~ terminal_child_building_sqft,
      TRUE ~ NA_real_
    ),
    requires_manual_adjudication =
      review_class %in% c(
        "manual_episode_review",
        "manual_no_successor_review"
      ) |
        (
          review_class == "prior_manual_decision" &
            is.na(prior_manual_sqft)
        )
  ) |>
  dplyr::arrange(
    dplyr::desc(requires_manual_adjudication),
    dplyr::desc(within_500ft),
    review_class,
    project_id
  )

summary <- review |>
  dplyr::count(
    within_500ft,
    review_class,
    requires_manual_adjudication,
    name = "projects"
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    dplyr::desc(requires_manual_adjudication),
    review_class
  )

readr::write_csv(
  review,
  "../output/multicard_final_review_bundle.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_final_review_summary.csv"
)
