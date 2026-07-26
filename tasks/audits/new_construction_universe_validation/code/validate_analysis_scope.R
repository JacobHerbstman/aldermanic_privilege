# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

inventory <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE,
  col_select = c(project_id, within_500ft, within_1500ft),
  col_types = readr::cols(
    project_id = readr::col_character(),
    within_500ft = readr::col_logical(),
    within_1500ft = readr::col_logical()
  )
)

analysis <- readr::read_csv(
  "../input/multicard_external_reviewed_model_input.csv",
  show_col_types = FALSE,
  col_select = "project_id",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_skip()
  )
)

successors <- readr::read_csv(
  "../output/parent_successor_project_actions.csv",
  show_col_types = FALSE,
  col_select = c(represented_by_project_id, project_id),
  col_types = readr::cols(
    represented_by_project_id = readr::col_character(),
    project_id = readr::col_character()
  )
) |>
  dplyr::distinct(project_id)

external_reviews <- readr::read_csv(
  "../input/multicard_external_review_queue.csv",
  show_col_types = FALSE,
  col_select = c(project_id, review_status, multifamily_disposition),
  col_types = readr::cols(
    project_id = readr::col_character(),
    review_status = readr::col_character(),
    multifamily_disposition = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::filter(multifamily_disposition %in% c("suppress", "pending")) |>
  dplyr::distinct(project_id)

for (x in list(inventory, analysis, successors, external_reviews)) {
  if (anyDuplicated(x$project_id)) {
    stop("An analysis-scope input is not uniquely keyed by project_id.")
  }
}

omitted <- inventory |>
  dplyr::filter(within_1500ft) |>
  dplyr::anti_join(
    analysis,
    by = "project_id"
  ) |>
  dplyr::mutate(
    successor_suppression = project_id %in% successors$project_id,
    external_suppression = project_id %in% external_reviews$project_id,
    omission_explained = successor_suppression | external_suppression
  )

unexpected_analysis_projects <- analysis |>
  dplyr::anti_join(
    inventory |>
      dplyr::filter(within_1500ft),
    by = "project_id"
  )

if (
  any(!omitted$omission_explained) ||
    nrow(unexpected_analysis_projects) > 0
) {
  stop("The reviewed analysis scope has unexplained project differences.")
}

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = c(
      "inventory_projects_within_1500ft",
      "reviewed_analysis_projects",
      "omitted_inventory_projects",
      "successor_suppressions",
      "external_suppressions_or_pending",
      "overlapping_suppression_reasons",
      "unexplained_omissions",
      "unexpected_analysis_projects"
    ),
    projects = c(
      sum(inventory$within_1500ft),
      nrow(analysis),
      nrow(omitted),
      sum(omitted$successor_suppression),
      sum(omitted$external_suppression),
      sum(
        omitted$successor_suppression &
          omitted$external_suppression
      ),
      sum(!omitted$omission_explained),
      nrow(unexpected_analysis_projects)
    )
  ),
  omitted |>
    dplyr::count(
      metric = dplyr::case_when(
        successor_suppression & external_suppression ~
          "omission_reason:successor_and_external",
        successor_suppression ~
          "omission_reason:successor",
        external_suppression ~
          "omission_reason:external",
        TRUE ~ "omission_reason:unexplained"
      ),
      name = "projects"
    )
)

readr::write_csv(
  omitted,
  "../output/analysis_scope_omissions.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/analysis_scope_reconciliation.csv",
  na = ""
)
