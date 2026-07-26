# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

single_number <- function(x) {
  text <- if (length(x) == 0 || is.na(x)) "" else as.character(x)
  values <- suppressWarnings(as.numeric(str_split_1(text, fixed("/"))))
  values <- unique(values[is.finite(values)])
  if (length(values) == 1) values else NA_real_
}

unique_mode <- function(x) {
  values <- x[is.finite(x) & x > 0]
  if (length(values) == 0) return(NA_real_)
  counts <- table(values)
  modes <- as.numeric(names(counts)[counts == max(counts)])
  if (length(modes) == 1) modes else NA_real_
}

projects <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    project_kind == "class_297" |
      str_detect(coalesce(candidate_review_categories, ""), fixed("class_297"))
  ) %>%
  transmute(
    source_project_id = project_id,
    project_kind,
    component_pins,
    candidate_year = construction_year,
    candidate_units = dwelling_units,
    candidate_building_sqft = building_sqft,
    candidate_land_sqft = land_sqft,
    distance_to_boundary_ft,
    permit_chain_evidence,
    permit_unit_evidence
  )

cohorts <- readr::read_csv(
  "../output/residential_class297_condo_cohort_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    source_project_id = project_id,
    condo_base = pin10,
    condo_cohort_year = year,
    condo_residential_units = residential_pin_records,
    condo_parking_pins = parking_pin_records,
    condo_year_built = map_dbl(year_built_values, single_number),
    condo_building_sqft = map_dbl(building_sqft_values, single_number),
    condo_land_values = land_sqft_values,
    condo_evidence_status
  )

raw_condos <- readr::read_csv(
  "../output/residential_successor_condo_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

cohort_land <- cohorts %>%
  select(condo_base, condo_cohort_year) %>%
  distinct() %>%
  inner_join(
    raw_condos %>%
      transmute(
        condo_base = pin10,
        condo_cohort_year = year,
        condo_land_sqft = as.numeric(char_land_sf)
      ),
    by = c("condo_base", "condo_cohort_year"),
    relationship = "one-to-many"
  ) %>%
  group_by(condo_base, condo_cohort_year) %>%
  summarise(
    condo_modal_land_sqft = unique_mode(condo_land_sqft),
    condo_land_distinct_values = n_distinct(
      condo_land_sqft[is.finite(condo_land_sqft) & condo_land_sqft > 0]
    ),
    .groups = "drop"
  )

exceptions <- readr::read_csv(
  "../adjudication/residential_class297_exceptions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    override_action = readr::col_character(),
    override_final_project_id = readr::col_character(),
    override_condo_base = readr::col_character(),
    override_year = readr::col_double(),
    override_units = readr::col_double(),
    override_building_sqft = readr::col_double(),
    override_land_sqft = readr::col_double(),
    override_allow_far = readr::col_logical(),
    override_allow_dupac = readr::col_logical(),
    decision_reason = readr::col_character(),
    evidence_ids = readr::col_character(),
    confidence = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (nrow(readr::problems(exceptions)) > 0) {
  stop("The class-297 exception ledger contains malformed values.", call. = FALSE)
}

if (anyDuplicated(projects$source_project_id) > 0 ||
    anyDuplicated(exceptions$source_project_id) > 0 ||
    anyDuplicated(cohorts[c("source_project_id", "condo_base")]) > 0 ||
    anyDuplicated(cohort_land[c("condo_base", "condo_cohort_year")]) > 0) {
  stop("Class-297 resolution inputs violate their declared keys.", call. = FALSE)
}
if (any(!exceptions$source_project_id %in% projects$source_project_id)) {
  stop("A class-297 exception names a project outside the review scope.", call. = FALSE)
}

decisions <- projects %>%
  left_join(cohorts, by = "source_project_id", relationship = "one-to-many") %>%
  left_join(
    cohort_land,
    by = c("condo_base", "condo_cohort_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(exceptions, by = "source_project_id", relationship = "many-to-one") %>%
  mutate(
    condo_base = coalesce(override_condo_base, condo_base),
    final_project_id = coalesce(
      override_final_project_id,
      if_else(!is.na(condo_base), paste0("residential_condo_", condo_base), NA_character_)
    ),
    construction_year = coalesce(override_year, condo_year_built, candidate_year),
    dwelling_units = coalesce(override_units, condo_residential_units, candidate_units),
    building_sqft = coalesce(
      override_building_sqft,
      condo_building_sqft,
      na_if(candidate_building_sqft, 0)
    ),
    land_sqft = coalesce(
      override_land_sqft,
      na_if(candidate_land_sqft, 0),
      condo_modal_land_sqft
    ),
    allow_far_default = is.finite(building_sqft) & building_sqft > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    allow_dupac_default = is.finite(dwelling_units) & dwelling_units > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    decision_action = coalesce(
      override_action,
      case_when(
        is.na(condo_base) ~ "unresolved",
        !is.finite(construction_year) ~ "unresolved",
        !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
        !allow_far_default & !allow_dupac_default ~ "unresolved",
        TRUE ~ "retain_successor_evidence"
      )
    ),
    allow_far = if_else(
      !is.na(override_allow_far),
      override_allow_far,
      decision_action %in% c("retain_successor_evidence", "retain_override") & allow_far_default
    ),
    allow_dupac = if_else(
      !is.na(override_allow_dupac),
      override_allow_dupac,
      decision_action %in% c("retain_successor_evidence", "retain_override") & allow_dupac_default
    ),
    membership_source = paste0("original_predecessor_pins:", component_pins),
    year_source = case_when(
      !is.na(override_year) ~ paste0("exception_ledger:", source_project_id),
      is.finite(condo_year_built) ~ paste0("successor_condo_cohort:", condo_base),
      TRUE ~ paste0("assessor_candidate:", source_project_id)
    ),
    units_source = case_when(
      !is.na(override_units) ~ paste0("exception_ledger:", source_project_id),
      is.finite(condo_residential_units) ~ paste0("successor_condo_cohort:", condo_base),
      TRUE ~ paste0("assessor_candidate:", source_project_id)
    ),
    building_source = case_when(
      !is.na(override_building_sqft) ~ paste0("exception_ledger:", source_project_id),
      is.finite(condo_building_sqft) ~ paste0("successor_condo_cohort:", condo_base),
      is.finite(candidate_building_sqft) & candidate_building_sqft > 0 ~
        paste0("assessor_candidate:", source_project_id),
      TRUE ~ NA_character_
    ),
    land_source = case_when(
      !is.na(override_land_sqft) ~ paste0("exception_ledger:", source_project_id),
      is.finite(candidate_land_sqft) & candidate_land_sqft > 0 ~
        paste0("construction_year_predecessor:", source_project_id),
      is.finite(condo_modal_land_sqft) ~ paste0("successor_condo_modal_value:", condo_base),
      TRUE ~ NA_character_
    ),
    decision_reason = coalesce(
      decision_reason,
      case_when(
        decision_action == "exclude_outside_period" ~
          "Successor condominium records place construction outside 2006-2022.",
        decision_action == "retain_successor_evidence" ~
          "The predecessor project and successor condominium cohort identify one completed ground-up project.",
        TRUE ~ "Class-297 evidence remains unresolved."
      )
    ),
    evidence_ids = coalesce(
      evidence_ids,
      paste0("condo_base_", condo_base, "/", source_project_id)
    ),
    confidence = coalesce(confidence, "high")
  ) %>%
  select(
    source_project_id,
    final_project_id,
    decision_action,
    component_pins,
    condo_base,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
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
    distance_to_boundary_ft,
    permit_chain_evidence,
    permit_unit_evidence,
    condo_cohort_year,
    condo_parking_pins,
    condo_land_distinct_values
  ) %>%
  arrange(source_project_id, final_project_id)

source_disposition <- decisions %>%
  group_by(source_project_id) %>%
  summarise(
    decision_rows = n(),
    actions = paste(sort(unique(decision_action)), collapse = "/"),
    retained_projects = n_distinct(
      final_project_id[decision_action %in% c("retain_successor_evidence", "retain_override")]
    ),
    final_project_ids = paste(
      sort(unique(final_project_id[decision_action %in% c(
        "retain_successor_evidence", "retain_override"
      )])),
      collapse = "/"
    ),
    .groups = "drop"
  )

retained_conflicts <- decisions %>%
  filter(decision_action %in% c("retain_successor_evidence", "retain_override")) %>%
  group_by(final_project_id) %>%
  summarise(
    year_values = n_distinct(construction_year),
    unit_values = n_distinct(dwelling_units),
    building_values = n_distinct(building_sqft, na.rm = TRUE),
    land_values = n_distinct(land_sqft),
    .groups = "drop"
  ) %>%
  filter(year_values != 1 | unit_values != 1 | building_values > 1 | land_values != 1)

unresolved <- decisions %>% filter(decision_action == "unresolved")

if (nrow(source_disposition) != nrow(projects) ||
    anyDuplicated(source_disposition$source_project_id) > 0) {
  stop("Not every class-297 source project has one disposition.", call. = FALSE)
}
if (nrow(retained_conflicts) > 0) {
  stop("Merged class-297 source projects disagree on final fields.", call. = FALSE)
}
if (any(decisions$allow_far & decisions$decision_action %in% c(
  "exclude_outside_period", "exclude_not_ground_up", "exclude_unbuilt", "replace_by_commercial"
)) || any(decisions$allow_dupac & decisions$decision_action %in% c(
  "exclude_outside_period", "exclude_not_ground_up", "exclude_unbuilt", "replace_by_commercial"
))) {
  stop("An excluded class-297 decision remains analysis eligible.", call. = FALSE)
}

summary <- bind_rows(
  decisions %>%
    count(decision_action, name = "value") %>%
    transmute(section = "decision_rows", metric = decision_action, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "source_projects",
      "decision_rows",
      "retained_final_projects",
      "far_eligible_final_projects",
      "dupac_eligible_final_projects",
      "unresolved_rows",
      "retained_field_conflicts",
      "duplicate_source_dispositions"
    ),
    value = c(
      nrow(projects),
      nrow(decisions),
      n_distinct(decisions$final_project_id[decisions$decision_action %in% c(
        "retain_successor_evidence", "retain_override"
      )]),
      n_distinct(decisions$final_project_id[decisions$allow_far]),
      n_distinct(decisions$final_project_id[decisions$allow_dupac]),
      nrow(unresolved),
      nrow(retained_conflicts),
      anyDuplicated(source_disposition$source_project_id)
    )
  )
)

readr::write_csv(
  decisions,
  "../output/residential_class297_resolution.csv"
)
readr::write_csv(
  source_disposition,
  "../output/residential_class297_source_disposition.csv"
)
readr::write_csv(
  unresolved,
  "../output/residential_class297_unresolved.csv"
)
readr::write_csv(
  summary,
  "../output/residential_class297_resolution_summary.csv"
)
