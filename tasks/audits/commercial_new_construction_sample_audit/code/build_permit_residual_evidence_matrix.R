# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    representative_permit_id = readr::col_character(),
    representative_permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)
assessor_review <- readr::read_csv(
  "../output/permit_residential_assessor_chain_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    plausible_candidate_pins = readr::col_character(),
    represented_candidate_pins = readr::col_character(),
    unrepresented_candidate_pins = readr::col_character(),
    exact_permit_pin_candidates = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    assessor_review_status,
    plausible_candidate_pins,
    represented_candidate_pins,
    unrepresented_candidate_pins,
    exact_permit_pin_candidates,
    nearest_candidate_distance_ft,
    nearest_unrepresented_distance_ft
  )
address_candidate_review <- readr::read_csv(
  "../output/permit_residential_assessor_address_chain_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    plausible_address_candidate_pins = readr::col_character(),
    represented_address_candidate_pins = readr::col_character(),
    unrepresented_address_candidate_pins = readr::col_character(),
    exact_permit_pin_address_candidates = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    address_review_status,
    plausible_address_candidate_pins,
    represented_address_candidate_pins,
    unrepresented_address_candidate_pins,
    exact_permit_pin_address_candidates
  )
current_address_review <- readr::read_csv(
  "../output/permit_residual_current_address_assessor_chain_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    current_exact_address_pin_values = readr::col_character(),
    exact_address_candidate_pins = readr::col_character(),
    unrepresented_exact_address_candidate_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    current_address_assessor_status,
    current_exact_address_pins,
    current_exact_address_pin_values,
    plausible_exact_address_pins,
    plausible_exact_address_pin_cards,
    represented_exact_address_pins,
    unrepresented_exact_address_pins,
    unrepresented_exact_address_pin_cards,
    exact_address_candidate_pins,
    unrepresented_exact_address_candidate_pins
  )
footprint_review <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_chain_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    footprint_review_status,
    plausible_footprints,
    strong_footprint_matches,
    strong_represented_footprints,
    strong_unrepresented_footprints,
    nearest_footprint_distance_ft,
    nearest_unrepresented_footprint_distance_ft
  )

for (review_table in list(
  assessor_review,
  address_candidate_review,
  current_address_review,
  footprint_review
)) {
  if (anyDuplicated(review_table$permit_chain_id)) {
    stop("Permit residual review inputs must be unique by chain.", call. = FALSE)
  }
}

evidence_matrix <- chains |>
  dplyr::left_join(
    assessor_review,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    address_candidate_review,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    current_address_review,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    footprint_review,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        plausible_candidate_pins,
        represented_candidate_pins,
        unrepresented_candidate_pins,
        exact_permit_pin_candidates,
        plausible_address_candidate_pins,
        represented_address_candidate_pins,
        unrepresented_address_candidate_pins,
        exact_permit_pin_address_candidates,
        current_exact_address_pins,
        plausible_exact_address_pins,
        plausible_exact_address_pin_cards,
        represented_exact_address_pins,
        unrepresented_exact_address_pins,
        unrepresented_exact_address_pin_cards,
        plausible_footprints,
        strong_footprint_matches,
        strong_represented_footprints,
        strong_unrepresented_footprints
      ),
      ~ dplyr::coalesce(as.integer(.x), 0L)
    ),
    strong_unrepresented_assessor_episode =
      current_address_assessor_status %in% c(
        "one_unrepresented_exact_address_assessor_episode",
        "one_unrepresented_pin_multiple_card_episodes"
      ) |
      assessor_review_status %in% c(
        "one_unrepresented_exact_pin10_completion_candidate",
        "one_unrepresented_completion_candidate_within_50ft"
      ) |
      address_review_status ==
        "one_unrepresented_exact_address_completion_candidate",
    strong_unrepresented_footprint = strong_unrepresented_footprints > 0L,
    represented_completion_evidence =
      represented_exact_address_pins > 0L |
      strong_represented_footprints > 0L,
    nearby_represented_evidence =
      represented_candidate_pins > 0L |
      represented_address_candidate_pins > 0L,
    exact_current_address_without_assessor_episode =
      current_address_assessor_status ==
        "exact_current_address_without_plausible_assessor_episode",
    completion_evidence_class = dplyr::case_when(
      (strong_unrepresented_assessor_episode |
        strong_unrepresented_footprint) &
        represented_completion_evidence ~
        "conflicting_represented_and_unrepresented_evidence",
      strong_unrepresented_assessor_episode &
        strong_unrepresented_footprint ~
        "unrepresented_assessor_and_city_footprint",
      strong_unrepresented_assessor_episode ~
        "unrepresented_assessor_episode",
      strong_unrepresented_footprint ~
        "unrepresented_city_footprint",
      represented_completion_evidence ~
        "completion_evidence_already_represented",
      exact_current_address_without_assessor_episode ~
        "current_address_but_no_assessor_completion_episode",
      nearby_represented_evidence ~
        "nearby_assessor_evidence_already_represented",
      current_exact_address_pins > 0L ~
        "current_address_without_residential_assessor_history",
      plausible_footprints > 0L ~
        "nearby_city_footprint_without_strong_match",
      TRUE ~ "no_completion_source_match"
    ),
    evidence_review_priority = dplyr::case_when(
      completion_evidence_class %in% c(
        "unrepresented_assessor_and_city_footprint",
        "unrepresented_assessor_episode",
        "unrepresented_city_footprint"
      ) & any_full_residential_signal ~ "high_completion_candidate",
      completion_evidence_class %in% c(
        "unrepresented_assessor_and_city_footprint",
        "unrepresented_assessor_episode",
        "unrepresented_city_footprint"
      ) ~ "medium_completion_candidate",
      completion_evidence_class ==
        "conflicting_represented_and_unrepresented_evidence" ~
        "conflicting_completion_evidence",
      completion_evidence_class ==
        "completion_evidence_already_represented" ~
        "likely_existing_project_link",
      any_full_residential_signal ~ "unresolved_full_residential_permit",
      TRUE ~ "unresolved_other_residential_permit"
    )
  ) |>
  dplyr::arrange(
    factor(
      evidence_review_priority,
      levels = c(
        "high_completion_candidate",
        "medium_completion_candidate",
        "conflicting_completion_evidence",
        "likely_existing_project_link",
        "unresolved_full_residential_permit",
        "unresolved_other_residential_permit"
      )
    ),
    application_boundary_distance_ft,
    representative_application_date,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "residual_permit_chains", value = nrow(evidence_matrix)),
  evidence_matrix |>
    dplyr::count(completion_evidence_class, name = "value") |>
    dplyr::transmute(metric = paste0("completion_class:", completion_evidence_class), value),
  evidence_matrix |>
    dplyr::count(evidence_review_priority, name = "value") |>
    dplyr::transmute(metric = paste0("review_priority:", evidence_review_priority), value),
  evidence_matrix |>
    dplyr::mutate(application_year = lubridate::year(representative_application_date)) |>
    dplyr::filter(evidence_review_priority == "high_completion_candidate") |>
    dplyr::count(application_year, name = "value") |>
    dplyr::transmute(metric = paste0("high_completion_year:", application_year), value)
)

readr::write_csv(
  evidence_matrix,
  "../output/permit_residual_evidence_matrix.csv",
  na = ""
)
readr::write_csv(
  evidence_matrix |>
    dplyr::filter(evidence_review_priority == "high_completion_candidate"),
  "../output/permit_residual_high_completion_queue.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_residual_evidence_matrix_summary.csv",
  na = ""
)
