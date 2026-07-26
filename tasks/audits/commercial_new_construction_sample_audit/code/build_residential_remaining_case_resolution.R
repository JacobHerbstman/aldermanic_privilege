# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

evidence <- readr::read_csv(
  "../output/residential_remaining_case_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

decisions <- readr::read_csv(
  "../adjudication/residential_remaining_case_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

if (nrow(readr::problems(decisions)) > 0 ||
    anyDuplicated(evidence$source_project_id) > 0 ||
    anyDuplicated(decisions$source_project_id) > 0 ||
    !setequal(evidence$source_project_id, decisions$source_project_id)) {
  stop("Remaining-case decisions are malformed, duplicated, or incomplete.", call. = FALSE)
}

resolution <- evidence %>%
  inner_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  mutate(
    final_project_id = if_else(
      decision_action == "retain_override",
      str_replace(source_project_id, "residential_multicard_", "residential_"),
      NA_character_
    ),
    resolved_year = case_when(
      field_rule %in% c("source_cards_geometry_land", "successor_condo_building", "current_successor_site", "later_same_pin_report") ~ construction_year,
      TRUE ~ NA_real_
    ),
    resolved_units = case_when(
      field_rule == "source_cards_geometry_land" ~ dwelling_units,
      field_rule == "successor_condo_building" ~ successor_condo_units,
      field_rule %in% c("current_successor_site", "later_same_pin_report") ~ current_assessor_units,
      TRUE ~ NA_real_
    ),
    resolved_building_sqft = case_when(
      field_rule == "source_cards_geometry_land" ~ building_sqft,
      field_rule == "successor_condo_building" ~ successor_condo_building_sqft,
      field_rule %in% c("current_successor_site", "later_same_pin_report") ~ current_assessor_building_sqft,
      TRUE ~ NA_real_
    ),
    resolved_land_sqft = case_when(
      field_rule == "source_cards_geometry_land" ~ project_land_area_sqft,
      field_rule == "successor_condo_building" ~ land_sqft,
      field_rule %in% c("current_successor_site", "later_same_pin_report") ~ current_assessor_land_sqft,
      TRUE ~ NA_real_
    ),
    allow_far = decision_action == "retain_override",
    allow_dupac = decision_action == "retain_override",
    membership_source = case_when(
      field_rule == "current_successor_site" ~ "construction_year_predecessor_and_current_successor_pins",
      field_rule == "successor_condo_building" ~ "construction_year_predecessor_and_successor_condo_cohort",
      field_rule == "source_cards_geometry_land" ~ "construction_year_predecessor_and_distinct_assessor_cards",
      field_rule == "later_same_pin_report" ~ "same_pin_later_complete_assessor_report",
      TRUE ~ NA_character_
    ),
    year_source = if_else(decision_action == "retain_override", "review_candidate_and_permit_evidence", NA_character_),
    units_source = case_when(
      field_rule == "source_cards_geometry_land" ~ source_row_ids,
      field_rule == "successor_condo_building" ~ successor_condo_rows,
      field_rule %in% c("current_successor_site", "later_same_pin_report") ~ current_assessor_rows,
      TRUE ~ NA_character_
    ),
    building_source = units_source,
    land_source = case_when(
      field_rule == "source_cards_geometry_land" ~ "construction_year_project_polygon",
      field_rule == "successor_condo_building" ~ source_row_ids,
      field_rule %in% c("current_successor_site", "later_same_pin_report") ~ current_assessor_rows,
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    source_project_id,
    final_project_id,
    decision_action,
    component_pins,
    construction_year = resolved_year,
    dwelling_units = resolved_units,
    building_sqft = resolved_building_sqft,
    land_sqft = resolved_land_sqft,
    allow_far,
    allow_dupac,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    decision_reason,
    evidence_ids,
    confidence,
    field_rule,
    distance_to_boundary_ft
  ) %>%
  arrange(source_project_id)

retained <- resolution %>% filter(decision_action == "retain_override")

if (nrow(retained) != 4 ||
    anyDuplicated(retained$final_project_id) > 0 ||
    any(!between(retained$construction_year, 2006, 2022)) ||
    any(!is.finite(retained$dwelling_units) | retained$dwelling_units <= 0) ||
    any(!is.finite(retained$building_sqft) | retained$building_sqft <= 0) ||
    any(!is.finite(retained$land_sqft) | retained$land_sqft <= 0) ||
    any(resolution$decision_action == "exclude_not_new_construction" &
          (!is.na(resolution$final_project_id) | resolution$allow_far | resolution$allow_dupac))) {
  stop("A remaining-case resolution violates the field or exclusion rules.", call. = FALSE)
}

summary <- bind_rows(
  resolution %>% count(decision_action, name = "value") %>% transmute(section = "decision", metric = decision_action, value),
  resolution %>% count(field_rule, name = "value") %>% transmute(section = "field_rule", metric = field_rule, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "source_projects",
      "retained_projects",
      "excluded_projects",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "duplicate_final_project_ids",
      "unresolved_projects"
    ),
    value = c(
      nrow(resolution),
      nrow(retained),
      sum(resolution$decision_action == "exclude_not_new_construction"),
      sum(resolution$allow_far),
      sum(resolution$allow_dupac),
      anyDuplicated(retained$final_project_id),
      0
    )
  )
)

readr::write_csv(resolution, "../output/residential_remaining_case_resolution.csv")
readr::write_csv(summary, "../output/residential_remaining_case_resolution_summary.csv")
