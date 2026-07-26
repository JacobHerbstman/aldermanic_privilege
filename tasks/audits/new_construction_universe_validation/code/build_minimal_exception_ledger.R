# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

commercial <- readr::read_csv(
  "../output/commercial_minimal_exception_ledger.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(
    exception_family = "commercial_project_fields",
    exception_id = decision_id,
    project_ids,
    projects_within_500ft,
    decision = paste(decision_source, decision_action, sep = ":"),
    evidence = evidence_ids,
    reason = decision_reason
  )

parent_successor <- readr::read_csv(
  "../output/parent_successor_cluster_review_queue.csv",
  show_col_types = FALSE
) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    decision = paste(
      sort(unique(exception_type)),
      collapse = "/"
    ),
    evidence = paste(
      sort(unique(existing_value)),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    readr::read_csv(
      "../output/project_evidence_inventory.csv",
      show_col_types = FALSE,
      col_select = c(project_id, within_500ft),
      col_types = readr::cols(
        project_id = readr::col_character(),
        within_500ft = readr::col_logical()
      )
    ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::transmute(
    exception_family = "residential_parent_successor",
    exception_id = project_id,
    project_ids = project_id,
    projects_within_500ft = as.integer(
      dplyr::coalesce(within_500ft, FALSE)
    ),
    decision,
    evidence,
    reason = "Field or membership exception to the general successor rule."
  )

classification <- readr::read_csv(
  "../output/multifamily_classification_known_exceptions.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    within_500ft,
    class_values,
    external_structure_class,
    multifamily_disposition,
    reviewer_notes
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    within_500ft = readr::col_logical(),
    class_values = readr::col_character(),
    external_structure_class = readr::col_character(),
    multifamily_disposition = readr::col_character(),
    reviewer_notes = readr::col_character()
  )
) |>
  dplyr::transmute(
    exception_family = "multifamily_classification",
    exception_id = project_id,
    project_ids = project_id,
    projects_within_500ft = as.integer(within_500ft),
    decision = multifamily_disposition,
    evidence = paste(
      class_values,
      external_structure_class,
      sep = ":"
    ),
    reason = reviewer_notes
  )

eligibility <- readr::read_csv(
  "../output/eligibility_manual_exception_ledger.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    within_500ft,
    eligibility_rule,
    rule_evidence,
    proposed_action
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    within_500ft = readr::col_logical(),
    eligibility_rule = readr::col_character(),
    rule_evidence = readr::col_character(),
    proposed_action = readr::col_character()
  )
) |>
  dplyr::transmute(
    exception_family = "new_construction_eligibility",
    exception_id = project_id,
    project_ids = project_id,
    projects_within_500ft = as.integer(within_500ft),
    decision = paste(proposed_action, eligibility_rule, sep = ":"),
    evidence = rule_evidence,
    reason = "Eligibility requires case-specific external evidence."
  )

exceptions <- dplyr::bind_rows(
  commercial,
  parent_successor,
  classification,
  eligibility
) |>
  dplyr::arrange(
    exception_family,
    exception_id
  )

summary <- exceptions |>
  dplyr::group_by(exception_family) |>
  dplyr::summarise(
    exception_decisions = dplyr::n(),
    decisions_within_500ft = sum(
      projects_within_500ft > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      exception_family = "unresolved_duplicates",
      exception_decisions = nrow(readr::read_csv(
        "../output/unresolved_duplicate_candidates.csv",
        show_col_types = FALSE
      )),
      decisions_within_500ft = NA_integer_
    )
  )

readr::write_csv(
  exceptions,
  "../output/minimal_manual_exception_ledger.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/minimal_manual_exception_summary.csv",
  na = ""
)
