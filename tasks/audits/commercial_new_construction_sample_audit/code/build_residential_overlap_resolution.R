# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(!is.na(project_overlap_evidence)) %>%
  transmute(
    source_project_id = project_id,
    project_kind,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    distance_to_boundary_ft,
    commercial_overlap_evidence,
    project_overlap_evidence
  )

decisions <- readr::read_csv(
  "../adjudication/residential_overlap_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_projects.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(commercial_project_id = project_id)

residential_projects <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(candidate_status == "retain_mechanical") %>%
  select(residential_project_id = project_id)

if (nrow(readr::problems(decisions)) > 0) {
  stop("The residential overlap ledger contains malformed values.", call. = FALSE)
}
if (anyDuplicated(review$source_project_id) > 0 ||
    anyDuplicated(decisions$source_project_id) > 0 ||
    anyDuplicated(commercial_projects$commercial_project_id) > 0 ||
    anyDuplicated(residential_projects$residential_project_id) > 0) {
  stop("Residential overlap inputs violate their declared keys.", call. = FALSE)
}
if (!setequal(review$source_project_id, decisions$source_project_id)) {
  stop("The residential overlap ledger does not cover the exact review universe.", call. = FALSE)
}

valid_actions <- c(
  "replace_by_commercial",
  "replace_by_residential_successor",
  "retain_residential_resolution",
  "exclude_not_new_construction"
)
if (any(!decisions$overlap_action %in% valid_actions)) {
  stop("The residential overlap ledger contains an unsupported action.", call. = FALSE)
}
if (any(
  decisions$overlap_action %in% c("replace_by_commercial", "replace_by_residential_successor") &
    is.na(decisions$replacement_project_id)
) || any(
  !decisions$overlap_action %in% c("replace_by_commercial", "replace_by_residential_successor") &
    !is.na(decisions$replacement_project_id)
)) {
  stop("Residential overlap replacement IDs do not match their actions.", call. = FALSE)
}
if (any(
  decisions$overlap_action == "replace_by_commercial" &
    !decisions$replacement_project_id %in% commercial_projects$commercial_project_id
)) {
  stop("A commercial replacement is absent from the retained commercial ledger.", call. = FALSE)
}
if (any(
  decisions$overlap_action == "replace_by_residential_successor" &
    !decisions$replacement_project_id %in% residential_projects$residential_project_id
)) {
  stop("A residential replacement is absent from the retained residential candidates.", call. = FALSE)
}

resolution <- review %>%
  left_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  arrange(source_project_id)

summary <- bind_rows(
  resolution %>%
    count(overlap_action, name = "value") %>%
    transmute(section = "overlap_action", metric = overlap_action, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "review_projects",
      "decision_rows",
      "duplicate_review_projects",
      "duplicate_decision_projects",
      "missing_decisions",
      "invalid_replacements"
    ),
    value = c(
      nrow(review),
      nrow(decisions),
      anyDuplicated(review$source_project_id),
      anyDuplicated(decisions$source_project_id),
      sum(!review$source_project_id %in% decisions$source_project_id),
      0
    )
  )
)

readr::write_csv(resolution, "../output/residential_overlap_resolution.csv")
readr::write_csv(summary, "../output/residential_overlap_resolution_summary.csv")
