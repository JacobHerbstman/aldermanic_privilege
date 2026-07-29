# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

adjudication <- readr::read_csv(
  "../input/multicard_final_adjudication.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

matches <- readr::read_csv(
  "../input/multicard_component_successor_matches.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    represented_project_ids = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

existing_suppressions <- readr::read_csv(
  "../input/multicard_successor_suppressions.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    represented_by_project_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

manual_episodes <- readr::read_csv(
  "../input/multicard_manual_episode_decisions.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
)

model_project_ids <- readr::read_csv(
  "../input/final_density_model_input.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_skip()
  ),
  show_col_types = FALSE
) |>
  dplyr::pull(project_id)

if (
  anyDuplicated(adjudication$project_id) ||
    anyDuplicated(existing_suppressions$project_id) ||
    anyDuplicated(manual_episodes$project_id)
) {
  stop("The multicard validation inputs are not unique.", call. = FALSE)
}

direct_match_suppressions <- matches |>
  dplyr::filter(
    !is.na(represented_project_ids),
    represented_project_ids != "",
    represented_project_ids != project_id
  ) |>
  dplyr::transmute(
    represented_by_project_id = project_id,
    project_id = represented_project_ids,
    proposed_rule = "nonself_one_to_one_card_successor_match"
  ) |>
  dplyr::distinct(project_id, .keep_all = TRUE)

complete_episode_suppressions <- adjudication |>
  dplyr::filter(
    review_class %in% c(
      "successor_episode_exact_reproduction",
      "successor_episode_units_reproduced"
    ),
    successor_candidate_units == summed_card_units,
    !is.na(successor_candidate_project_ids),
    successor_candidate_project_ids != ""
  ) |>
  dplyr::select(
    represented_by_project_id = project_id,
    successor_candidate_project_ids
  ) |>
  tidyr::separate_longer_delim(
    successor_candidate_project_ids,
    delim = "/"
  ) |>
  dplyr::transmute(
    project_id = successor_candidate_project_ids,
    represented_by_project_id,
    proposed_rule = "complete_successor_episode_reproduces_parent"
  ) |>
  dplyr::anti_join(
    direct_match_suppressions,
    by = "project_id"
  )

manual_duplicate_exceptions <- existing_suppressions |>
  dplyr::filter(
    suppress_reason %in% c(
      "cross_project_duplicate_episode",
      "manual_duplicate_parent_decision"
    )
  ) |>
  dplyr::transmute(
    project_id,
    represented_by_project_id,
    proposed_rule = "manual_duplicate_exception"
  )

proposed_suppressions <- dplyr::bind_rows(
  direct_match_suppressions,
  complete_episode_suppressions,
  manual_duplicate_exceptions
) |>
  dplyr::filter(project_id %in% model_project_ids) |>
  dplyr::arrange(project_id) |>
  dplyr::distinct(project_id, .keep_all = TRUE)

suppression_comparison <- dplyr::full_join(
  proposed_suppressions |>
    dplyr::mutate(proposed_suppress = TRUE),
  existing_suppressions |>
    dplyr::transmute(
      project_id,
      existing_suppress = TRUE,
      existing_rule = suppress_reason
    ),
  by = "project_id",
  relationship = "one-to-one"
) |>
  dplyr::mutate(
    proposed_suppress = dplyr::coalesce(proposed_suppress, FALSE),
    existing_suppress = dplyr::coalesce(existing_suppress, FALSE),
    rules_agree = proposed_suppress == existing_suppress
  )

parent_comparison <- adjudication |>
  dplyr::transmute(
    project_id,
    within_500ft,
    requires_manual_adjudication,
    rule_disposition,
    rule_units,
    rule_building_sqft,
    reported_construction_year,
    final_disposition,
    final_units,
    final_building_sqft,
    final_construction_year,
    field_exception =
      !is.na(override_disposition) |
      !is.na(override_units) |
      !is.na(override_building_sqft) |
      final_disposition != rule_disposition,
    year_exception = !is.na(final_construction_year),
    programmatic_values_agree =
      final_disposition == rule_disposition &
      dplyr::near(final_units, rule_units) &
      dplyr::near(final_building_sqft, rule_building_sqft)
  )

manual_exception_ledger <- dplyr::bind_rows(
  parent_comparison |>
    dplyr::filter(field_exception) |>
    dplyr::transmute(
      project_id,
      exception_type = "parent_field_or_disposition",
      existing_value = paste(
        final_disposition,
        final_units,
        final_building_sqft,
        sep = " | "
      )
    ),
  parent_comparison |>
    dplyr::filter(year_exception) |>
    dplyr::transmute(
      project_id,
      exception_type = "construction_year",
      existing_value = as.character(final_construction_year)
    ),
  manual_duplicate_exceptions |>
    dplyr::transmute(
      project_id,
      exception_type = "cross_project_duplicate",
      existing_value = dplyr::coalesce(
        represented_by_project_id,
        "duplicate parent suppressed"
      )
    )
) |>
  dplyr::arrange(project_id, exception_type)

validation <- tibble::tibble(
  metric = c(
    "multicard_parent_projects",
    "parents_previously_labeled_manual",
    "parents_resolved_by_general_value_rule",
    "parent_field_or_disposition_exceptions",
    "construction_year_exceptions",
    "manual_episode_rows_no_longer_needed_for_values",
    "existing_successor_suppressions",
    "proposed_successor_suppressions",
    "successor_suppression_disagreements",
    "unique_projects_in_manual_exception_ledger"
  ),
  value = c(
    nrow(parent_comparison),
    sum(parent_comparison$requires_manual_adjudication),
    sum(parent_comparison$programmatic_values_agree),
    sum(parent_comparison$field_exception),
    sum(parent_comparison$year_exception),
    nrow(manual_episodes),
    nrow(existing_suppressions),
    nrow(proposed_suppressions),
    sum(!suppression_comparison$rules_agree),
    dplyr::n_distinct(manual_exception_ledger$project_id)
  )
)

if (
  any(!suppression_comparison$rules_agree) ||
    any(
      !parent_comparison$programmatic_values_agree &
        !parent_comparison$field_exception
    )
) {
  print(
    suppression_comparison |>
      dplyr::filter(!rules_agree)
  )
  print(
    parent_comparison |>
      dplyr::filter(
        !programmatic_values_agree,
        !field_exception
      )
  )
  stop("The reduced multicard rules do not reproduce the approved file.", call. = FALSE)
}

readr::write_csv(
  validation,
  "../output/parent_successor_cluster_rule_validation.csv",
  na = ""
)
readr::write_csv(
  manual_exception_ledger,
  "../output/parent_successor_cluster_review_queue.csv",
  na = ""
)
readr::write_csv(
  suppression_comparison,
  "../output/parent_successor_project_actions.csv",
  na = ""
)
