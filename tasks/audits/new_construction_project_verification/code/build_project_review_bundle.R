# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

review_queue <- readr::read_csv(
  "../output/project_verification_review_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

assessor_history <- readr::read_csv(
  "../output/assessor_history_review_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

permit_candidates <- readr::read_csv(
  "../output/extended_permit_candidate_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (
  anyDuplicated(review_queue$project_id) ||
    anyDuplicated(assessor_history$project_id) ||
    anyDuplicated(permit_candidates$project_id)
) {
  stop("A project-review input is not uniquely keyed by project.")
}

review_bundle <- review_queue |>
  dplyr::left_join(
    assessor_history,
    by = c("project_id", "construction_year" = "selected_construction_year"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    permit_candidates,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    extended_permit_support = dplyr::coalesce(
      extended_permit_support,
      FALSE
    ),
    review_status = "pending",
    project_existence_decision = NA_character_,
    new_construction_decision = NA_character_,
    construction_year_decision = NA_character_,
    reviewed_construction_year = NA_real_,
    manual_evidence = NA_character_,
    manual_notes = NA_character_
  ) |>
  dplyr::arrange(
    manual_review_priority,
    dplyr::desc(current_multifamily),
    construction_year,
    project_id
  )

if (
  nrow(review_bundle) != 360L ||
    anyDuplicated(review_bundle$project_id)
) {
  stop("The review bundle does not contain the expected 360 projects.")
}

readr::write_csv(
  review_bundle,
  "../output/project_review_bundle.csv",
  na = ""
)
