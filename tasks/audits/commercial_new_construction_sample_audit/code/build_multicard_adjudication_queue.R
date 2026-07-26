# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

base <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

footprints <- readr::read_csv(
  "../output/multicard_footprint_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::select(-construction_year)

historical_addresses <- readr::read_csv(
  "../input/density_parcel_address_selected_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::semi_join(base, by = "pin") |>
  dplyr::arrange(pin, selected_address_year_gap, selected_address_year) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    historical_addresses = paste(
      unique(selected_address[!is.na(selected_address) & selected_address != ""]),
      collapse = " / "
    ),
    historical_address_count = dplyr::n_distinct(
      selected_address[!is.na(selected_address) & selected_address != ""]
    ),
    .groups = "drop"
  )

evidence <- base |>
  dplyr::left_join(footprints, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(historical_addresses, by = "pin", relationship = "one-to-one") |>
  dplyr::mutate(
    review_address = dplyr::coalesce(
      dplyr::na_if(current_addresses, ""),
      dplyr::na_if(historical_addresses, ""),
      dplyr::na_if(permit_addresses, "")
    ),
    city_units_match_selected =
      is.finite(current_city_units_sum) &
      current_city_units_sum == dwelling_units,
    city_units_match_sum =
      is.finite(current_city_units_sum) &
      current_city_units_sum == summed_card_units,
    city_sqft_match_selected =
      is.finite(current_city_building_sqft_sum) &
      abs(current_city_building_sqft_sum - building_sqft) /
        building_sqft <= 0.1,
    city_sqft_match_sum =
      is.finite(current_city_building_sqft_sum) &
      abs(current_city_building_sqft_sum - summed_card_building_sqft) /
        summed_card_building_sqft <= 0.1,
    current_count_matches_cards =
      target_cards > 1 &
      current_footprints_near_construction == target_cards,
    historical_count_matches_cards =
      target_cards > 1 &
      historical_2008_footprints_near_construction == target_cards,
    permit_chain_count_matches_cards =
      target_cards > 1 &
      exact_pin_permit_chains == target_cards &
      exact_pin_distinct_permit_addresses == target_cards,
    selected_evidence =
      (city_units_match_selected & !city_units_match_sum) |
      (city_sqft_match_selected & !city_sqft_match_sum &
        current_footprints_near_construction == 1),
    sum_evidence =
      (exact_pin_permit_units_match_card_sum &
        !exact_pin_permit_units_match_selected) |
      (city_units_match_sum & !city_units_match_selected) |
      (city_sqft_match_sum & !city_sqft_match_selected) |
      current_count_matches_cards |
      historical_count_matches_cards |
      permit_chain_count_matches_cards,
    evidence_conflict = selected_evidence & sum_evidence,
    preliminary_disposition = dplyr::case_when(
      !is.na(manual_classification) ~ "resolved_previous_manual_review",
      target_cards == 1 & !selected_and_sum_differ ~
        "resolved_single_target_year_card",
      !evidence_conflict & sum_evidence ~ "candidate_sum_supported",
      !evidence_conflict & selected_evidence ~ "candidate_selected_card_supported",
      TRUE ~ "manual_review_required"
    ),
    preliminary_units = dplyr::case_when(
      preliminary_disposition == "resolved_previous_manual_review" ~
        manual_verified_units,
      preliminary_disposition == "resolved_single_target_year_card" ~
        dwelling_units,
      preliminary_disposition == "candidate_sum_supported" ~
        summed_card_units,
      preliminary_disposition == "candidate_selected_card_supported" ~
        dwelling_units,
      TRUE ~ NA_real_
    ),
    preliminary_building_sqft = dplyr::case_when(
      preliminary_disposition == "resolved_previous_manual_review" ~
        manual_verified_sqft,
      preliminary_disposition == "resolved_single_target_year_card" ~
        building_sqft,
      preliminary_disposition == "candidate_sum_supported" ~
        summed_card_building_sqft,
      preliminary_disposition == "candidate_selected_card_supported" ~
        building_sqft,
      TRUE ~ NA_real_
    ),
    adjudication_priority = dplyr::case_when(
      preliminary_disposition == "resolved_previous_manual_review" ~
        "0_previous_manual_review",
      preliminary_disposition == "resolved_single_target_year_card" ~
        "1_single_target_confirmation",
      within_500ft & evidence_conflict ~ "2_main_sample_conflicting_evidence",
      within_500ft & summed_cards_change_multifamily ~
        "3_main_sample_multifamily_threshold",
      within_500ft & selected_and_sum_differ ~ "4_main_sample_outcome",
      evidence_conflict ~ "5_placebo_scope_conflicting_evidence",
      summed_cards_change_multifamily ~ "6_placebo_scope_multifamily_threshold",
      selected_and_sum_differ ~ "7_placebo_scope_outcome",
      TRUE ~ "8_confirmation"
    )
  ) |>
  dplyr::arrange(adjudication_priority, distance_to_boundary_ft, project_id)

if (nrow(evidence) != 273L || anyDuplicated(evidence$project_id)) {
  stop("Multicard adjudication evidence is not 273 unique projects.", call. = FALSE)
}

queue <- evidence |>
  dplyr::filter(
    !preliminary_disposition %in%
      c("resolved_previous_manual_review", "resolved_single_target_year_card")
  )

summary <- dplyr::bind_rows(
  evidence |>
    dplyr::count(preliminary_disposition, name = "value") |>
    dplyr::transmute(
      metric = paste0("preliminary_disposition:", preliminary_disposition),
      value
    ),
  evidence |>
    dplyr::count(adjudication_priority, name = "value") |>
    dplyr::transmute(metric = paste0("adjudication_priority:", adjudication_priority), value),
  tibble::tibble(
    metric = c(
      "projects",
      "manual_queue",
      "main_sample_manual_queue",
      "evidence_conflicts"
    ),
    value = c(
      nrow(evidence),
      nrow(queue),
      sum(queue$within_500ft),
      sum(evidence$evidence_conflict)
    )
  )
)

readr::write_csv(evidence, "../output/multicard_adjudication_evidence.csv")
readr::write_csv(queue, "../output/multicard_adjudication_queue.csv")
readr::write_csv(summary, "../output/multicard_adjudication_queue_summary.csv")
