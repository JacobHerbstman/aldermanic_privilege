# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

evidence <- readr::read_csv(
  "../output/permit_rule_coverage.csv",
  show_col_types = FALSE
)

analysis_projects <- readr::read_csv(
  "../input/multicard_external_reviewed_model_input.csv",
  show_col_types = FALSE,
  col_select = project_id,
  col_types = readr::cols(project_id = readr::col_character())
) |>
  dplyr::select(project_id)

commercial_completion <- readr::read_csv(
  "../input/commercial_completion_evidence.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    completion_evidence_status,
    issued_new_construction_permits,
    completed_new_construction_permits,
    new_construction_permit_numbers,
    new_construction_evidence,
    city_year_built_values
  )

predecessor_evidence <- readr::read_csv(
  "../output/predecessor_assessor_evidence.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    predecessor_evidence_status,
    predecessor_same_structure_signal,
    predecessor_replacement_signal,
    predecessor_latest_tax_year,
    predecessor_reported_years,
    predecessor_building_sqft,
    predecessor_dwelling_units
  )

for (
  x in list(
    evidence,
    analysis_projects,
    commercial_completion,
    predecessor_evidence
  )
) {
  if (anyDuplicated(x$project_id)) {
    stop("An eligibility input is not uniquely keyed by project_id.")
  }
}

validation <- analysis_projects |>
  dplyr::left_join(
    evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    commercial_completion,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    predecessor_evidence,
    by = "project_id",
    relationship = "one-to-one"
  )

if (any(is.na(validation$source_family))) {
  stop("Not every analysis project appears in the evidence inventory.")
}

validation <- validation |>
  dplyr::mutate(
    positive_permit_evidence =
      positive_new_building_permit |
      direct_expanded_new_building |
      chain_expanded_new_building,
    positive_commercial_completion =
      source_family == "commercial" &
      dplyr::coalesce(
        completion_evidence_status %in% c(
          "issued_new_permit_and_later_assessor",
          "city_building_year_support"
        ),
        FALSE
      ),
    positive_assessor_replacement =
      source_family == "residential" &
      (
        assessor_physical_change |
          dplyr::coalesce(predecessor_replacement_signal, FALSE)
      ),
    unchanged_preexisting_structure =
      source_family == "residential" &
      (
        assessor_year_only_recode |
          dplyr::coalesce(predecessor_same_structure_signal, FALSE)
      ) &
      !positive_permit_evidence,
    existing_work_without_new_building =
      direct_existing_building_work &
      !positive_permit_evidence &
      !positive_commercial_completion,
    eligibility_rule = dplyr::case_when(
      unchanged_preexisting_structure ~
        "exclude_unchanged_structure_predates_reported_year",
      positive_permit_evidence ~
        "retain_new_building_permit",
      positive_commercial_completion ~
        "retain_commercial_completion_evidence",
      existing_work_without_new_building ~
        "manual_review_existing_building_scope",
      positive_assessor_replacement ~
        "retain_assessor_physical_replacement",
      TRUE ~
        "retain_assessor_report_without_contradictory_evidence"
    ),
    proposed_action = dplyr::case_when(
      stringr::str_starts(eligibility_rule, "exclude_") ~ "exclude",
      stringr::str_starts(eligibility_rule, "manual_review_") ~ "review",
      TRUE ~ "retain"
    ),
    rule_evidence = dplyr::case_when(
      unchanged_preexisting_structure ~ paste0(
        "The current or predecessor parcel contains the same physical ",
        "structure before the reported construction year. Current years: ",
        history_year_values,
        "; current building area: ",
        history_building_area_values,
        "; current units: ",
        history_unit_count_values,
        "; predecessor years: ",
        predecessor_reported_years,
        "; predecessor building area: ",
        predecessor_building_sqft,
        "; predecessor units: ",
        predecessor_dwelling_units,
        "."
      ),
      positive_permit_evidence ~ dplyr::coalesce(
        exact_pin_positive_descriptions,
        direct_permit_descriptions,
        chain_permit_descriptions
      ),
      positive_commercial_completion ~ dplyr::coalesce(
        new_construction_evidence,
        paste0("City building year: ", city_year_built_values)
      ),
      existing_work_without_new_building ~ direct_permit_descriptions,
      positive_assessor_replacement ~ paste0(
        "Current or predecessor assessor history changes building area from ",
        history_building_area_values,
        " / ",
        predecessor_building_sqft,
        " or unit count from ",
        history_unit_count_values,
        " / ",
        predecessor_dwelling_units,
        "."
      ),
      TRUE ~ decision_reason
    )
  )

summary <- validation |>
  dplyr::group_by(
    eligibility_rule,
    proposed_action,
    source_family,
    current_multifamily
  ) |>
  dplyr::summarise(
    projects_1500ft = dplyr::n(),
    projects_500ft = sum(within_500ft),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    factor(proposed_action, levels = c("exclude", "review", "retain")),
    eligibility_rule,
    source_family,
    dplyr::desc(current_multifamily)
  )

review_queue <- validation |>
  dplyr::filter(proposed_action == "review") |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    dplyr::desc(current_multifamily),
    source_family,
    project_id
  )

readr::write_csv(
  validation,
  "../output/eligibility_rule_validation.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/eligibility_rule_summary.csv",
  na = ""
)
readr::write_csv(
  review_queue,
  "../output/eligibility_manual_review_queue.csv",
  na = ""
)
