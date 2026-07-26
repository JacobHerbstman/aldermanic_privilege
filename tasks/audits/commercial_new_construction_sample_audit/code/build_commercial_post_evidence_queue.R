# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "commercial") %>%
  select(project_id, review_scope, distance_to_boundary_ft)

units <- readr::read_csv(
  "../output/commercial_unit_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    source_count_pattern,
    recommended_units,
    recommended_units_source,
    unit_review_required,
    unit_review_reason
  )

completion <- readr::read_csv(
  "../output/commercial_completion_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    completion_evidence_status,
    issued_new_construction_permits,
    later_assessor_report_after_permit
  )

land <- readr::read_csv(
  "../output/commercial_land_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    project_land_area_sqft,
    geography_status,
    land_review_required,
    land_review_reason
  )

manual <- readr::read_csv(
  "../adjudication/commercial_manual_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  tidyr::separate_rows(source_project_ids, sep = ";") %>%
  transmute(
    project_id = str_trim(source_project_ids),
    manual_action = action,
    manual_final_project_id = final_project_id,
    manual_decision_reason = decision_reason,
    manual_status = status
  )

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Commercial candidates are not unique by project.", call. = FALSE)
}
if (anyDuplicated(scope$project_id) > 0) {
  stop("Commercial scope is not unique by project.", call. = FALSE)
}
if (anyDuplicated(units$project_id) > 0) {
  stop("Commercial unit evidence is not unique by project.", call. = FALSE)
}
if (anyDuplicated(completion$project_id) > 0) {
  stop("Commercial completion evidence is not unique by project.", call. = FALSE)
}
if (anyDuplicated(land$project_id) > 0) {
  stop("Commercial land evidence is not unique by project.", call. = FALSE)
}
if (anyDuplicated(manual$project_id) > 0) {
  stop("A commercial source project appears in multiple manual decisions.", call. = FALSE)
}

resolution <- candidates %>%
  left_join(scope, by = "project_id", relationship = "one-to-one") %>%
  left_join(units, by = "project_id", relationship = "one-to-one") %>%
  left_join(completion, by = "project_id", relationship = "one-to-one") %>%
  left_join(land, by = "project_id", relationship = "one-to-one") %>%
  left_join(manual, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    stable_assessor_after_permit =
      is.na(manual_action) &
      decision_reason == "permit_unit_counts_do_not_include_assessor_count" &
      stable_component_membership &
      source_count_pattern == "stable_2021_2024" &
      !unit_review_required &
      completion_evidence_status == "issued_new_permit_and_later_assessor" &
      later_assessor_report_after_permit,
    exact_land_recovery =
      is.na(manual_action) &
      decision_reason == "missing_or_nonpositive_land_area" &
      geography_status == "complete_construction_year_geometry" &
      is.finite(project_land_area_sqft) & project_land_area_sqft > 0 &
      !land_review_required,
    resolution_status = case_when(
      !is.na(manual_action) ~ "manual_decision_complete",
      candidate_status == "exclude_outside_period" ~ "outside_study_period",
      candidate_status == "retain_mechanical" ~ "mechanical_candidate",
      stable_assessor_after_permit ~ "evidence_rule_complete",
      exact_land_recovery ~ "evidence_rule_complete",
      review_scope == "review_within_1500ft" ~ "manual_review_required",
      review_scope == "review_geography_unresolved" ~ "geography_review_required",
      TRUE ~ "outside_manual_review_scope"
    ),
    resolution_reason = case_when(
      !is.na(manual_action) ~ paste0("manual_", manual_action),
      candidate_status == "exclude_outside_period" ~ decision_reason,
      candidate_status == "retain_mechanical" ~ decision_reason,
      stable_assessor_after_permit ~
        "stable_two_vintage_assessor_count_after_issued_new_construction_permit",
      exact_land_recovery ~ "complete_construction_year_parcel_union_recovers_land",
      TRUE ~ decision_reason
    )
  )

manual_queue <- resolution %>%
  filter(resolution_status == "manual_review_required") %>%
  select(
    project_id,
    construction_year,
    selected_source_addresses,
    component_pins,
    source_row_ids,
    dwelling_units,
    building_sqft,
    land_sqft,
    project_land_area_sqft,
    source_count_pattern,
    recommended_units,
    recommended_units_source,
    completion_evidence_status,
    issued_new_construction_permits,
    distance_to_boundary_ft,
    decision_reason,
    resolution_reason
  ) %>%
  arrange(decision_reason, project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(manual_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Post-evidence review queue contains a prohibited analysis field.", call. = FALSE)
}

if (any(is.na(resolution$resolution_status))) {
  stop("At least one commercial candidate has no resolution status.", call. = FALSE)
}
if (any(manual_queue$distance_to_boundary_ft > 1500, na.rm = TRUE)) {
  stop("The manual review queue contains a project outside 1,500 feet.", call. = FALSE)
}
if (any(
  resolution$stable_assessor_after_permit &
    resolution$resolution_status != "evidence_rule_complete"
)) {
  stop("Stable post-permit assessor cases were not resolved consistently.", call. = FALSE)
}

summary <- bind_rows(
  resolution %>%
    count(resolution_status, resolution_reason, name = "value") %>%
    transmute(
      section = "all_candidates",
      metric = paste(resolution_status, resolution_reason, sep = ":"),
      value
    ),
  resolution %>%
    filter(between(construction_year, 2006L, 2022L)) %>%
    count(resolution_status, resolution_reason, name = "value") %>%
    transmute(
      section = "study_period",
      metric = paste(resolution_status, resolution_reason, sep = ":"),
      value
    ),
  tibble::tibble(
    section = "validation",
    metric = c(
      "candidate_projects",
      "study_period_projects",
      "stable_assessor_after_permit_resolutions",
      "exact_land_resolutions",
      "manual_review_required_within_1500ft",
      "duplicate_project_ids",
      "duplicate_manual_source_projects"
    ),
    value = c(
      nrow(resolution),
      sum(between(resolution$construction_year, 2006L, 2022L)),
      sum(resolution$stable_assessor_after_permit),
      sum(resolution$exact_land_recovery),
      nrow(manual_queue),
      anyDuplicated(resolution$project_id),
      anyDuplicated(manual$project_id)
    )
  )
)

readr::write_csv(
  resolution,
  "../output/commercial_post_evidence_resolution.csv"
)
readr::write_csv(
  manual_queue,
  "../output/commercial_post_evidence_review_queue.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_post_evidence_resolution_summary.csv"
)
