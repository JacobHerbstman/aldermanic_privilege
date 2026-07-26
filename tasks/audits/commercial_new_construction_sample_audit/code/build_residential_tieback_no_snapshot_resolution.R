# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review <- readr::read_csv(
  "../output/residential_tieback_no_snapshot_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
)

decisions <- readr::read_csv(
  "../adjudication/residential_tieback_no_snapshot_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    decision_action = readr::col_character(),
    final_project_id = readr::col_character(),
    construction_year = readr::col_double(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    .default = readr::col_character()
  )
)

if (nrow(readr::problems(decisions)) > 0) {
  stop("The no-snapshot tieback decision ledger contains malformed values.", call. = FALSE)
}
if (anyDuplicated(review$source_project_id) > 0 ||
    anyDuplicated(decisions$source_project_id) > 0) {
  stop("No-snapshot tieback inputs violate their declared keys.", call. = FALSE)
}
if (!setequal(review$source_project_id, decisions$source_project_id)) {
  stop("The no-snapshot decision ledger does not cover the exact review universe.", call. = FALSE)
}

retained <- decisions$decision_action == "retain_override"
excluded <- decisions$decision_action %in% c(
  "exclude_unbuilt",
  "exclude_not_ground_up",
  "exclude_duplicate_successor"
)

if (any(!retained & !excluded) ||
    any(retained & (
      is.na(decisions$final_project_id) |
        !between(decisions$construction_year, 2006L, 2022L) |
        !is.finite(decisions$dwelling_units) | decisions$dwelling_units <= 0 |
        !is.finite(decisions$land_sqft) | decisions$land_sqft <= 0
    )) ||
    any(retained & decisions$allow_far & (
      !is.finite(decisions$building_sqft) | decisions$building_sqft <= 0
    )) ||
    any(excluded & (decisions$allow_far | decisions$allow_dupac))) {
  stop("A no-snapshot tieback decision has inconsistent fields or eligibility.", call. = FALSE)
}

resolution <- review %>%
  select(source_project_id, component_pins, candidate_year, distance_to_boundary_ft) %>%
  left_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  arrange(source_project_id)

summary <- bind_rows(
  resolution %>%
    count(decision_action, name = "value") %>%
    transmute(section = "decision_action", metric = decision_action, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "review_projects",
      "decision_rows",
      "retained_projects",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "duplicate_decisions",
      "unresolved_projects"
    ),
    value = c(
      nrow(review),
      nrow(decisions),
      sum(retained),
      sum(decisions$allow_far),
      sum(decisions$allow_dupac),
      anyDuplicated(decisions$source_project_id),
      0
    )
  )
)

readr::write_csv(
  resolution,
  "../output/residential_tieback_no_snapshot_resolution.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_no_snapshot_resolution_summary.csv"
)
