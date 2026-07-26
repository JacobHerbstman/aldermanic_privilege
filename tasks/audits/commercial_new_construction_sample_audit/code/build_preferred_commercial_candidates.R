# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

family_review <- readr::read_csv(
  "../output/commercial_project_family_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    source_keypins = readr::col_character(),
    component_pin_list = readr::col_character(),
    production_keypins = readr::col_character(),
    .default = readr::col_guess()
  )
)

family_vintages <- readr::read_csv(
  "../output/commercial_family_vintage_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    source_keypins = readr::col_character(),
    component_pin_list = readr::col_character(),
    source_yearbuilt = readr::col_character(),
    source_units = readr::col_character(),
    source_building_areas = readr::col_character(),
    source_land_areas = readr::col_character(),
    .default = readr::col_guess()
  )
)

entity_versions <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

address_candidates <- readr::read_csv(
  "../output/commercial_address_family_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_ids = readr::col_character(),
    keypins = readr::col_character(),
    .default = readr::col_guess()
  )
)

verified_cases <- readr::read_csv(
  "../output/commercial_verified_case_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
)

permit_summary <- readr::read_csv(
  "../output/project_permit_chain_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    unit_counts = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial") %>%
  select(-source_family)

permit_chains <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial") %>%
  group_by(project_id) %>%
  summarise(
    permit_chain_ids = paste(sort(unique(permit_chain_id)), collapse = "/"),
    direct_permit_ids = paste(sort(unique(permit_id[directly_matched])), collapse = "/"),
    .groups = "drop"
  )

permit_summary <- permit_summary %>%
  left_join(permit_chains, by = "project_id", relationship = "one-to-one")

if (anyDuplicated(family_review$project_family_id) > 0) {
  stop("Commercial family review is not unique by project family.", call. = FALSE)
}
if (anyDuplicated(family_vintages[c("project_family_id", "valuation_year")]) > 0) {
  stop("Commercial vintage summary is not unique by family-vintage.", call. = FALSE)
}
if (anyDuplicated(entity_versions$raw_row) > 0) {
  stop("Commercial entity versions are not unique by raw source row.", call. = FALSE)
}

vintage_source_rows <- entity_versions %>%
  group_by(project_family_id, valuation_year) %>%
  summarise(
    source_row_ids = paste(sort(unique(raw_row)), collapse = "/"),
    source_addresses = paste(sort(unique(address)), collapse = " / "),
    unit_measure_sources = paste(
      sort(unique(case_when(
        apartment_unit_sum > 0 ~ "apartment_unit_sum",
        source_tot_units > 0 ~ "total_units",
        TRUE ~ "missing"
      ))),
      collapse = "/"
    ),
    property_type_uses = paste(sort(unique(na.omit(property_type_use))), collapse = "/"),
    property_descriptions = paste(
      sort(unique(na.omit(property_name_description))),
      collapse = "/"
    ),
    .groups = "drop"
  )

family_vintages <- family_vintages %>%
  left_join(
    vintage_source_rows,
    by = c("project_family_id", "valuation_year"),
    relationship = "one-to-one"
  )

vintage_2021 <- family_vintages %>%
  filter(valuation_year == 2021) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2021"), -project_family_id)

vintage_2024 <- family_vintages %>%
  filter(valuation_year == 2024) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2024"), -project_family_id)

address_review_families <- address_candidates %>%
  select(project_family_ids) %>%
  tidyr::separate_longer_delim(project_family_ids, delim = "/") %>%
  transmute(
    project_family_id = project_family_ids,
    same_address_cross_family = TRUE
  ) %>%
  distinct(project_family_id, .keep_all = TRUE)

verified_case_families <- verified_cases %>%
  transmute(
    pin,
    audit_action,
    evidence,
    confidence
  ) %>%
  inner_join(
    entity_versions %>% distinct(project_family_id, keypin),
    by = c("pin" = "keypin"),
    relationship = "many-to-one"
  ) %>%
  group_by(project_family_id) %>%
  summarise(
    documented_case_pins = paste(sort(unique(pin)), collapse = "/"),
    documented_case_actions = paste(sort(unique(audit_action)), collapse = "/"),
    documented_case_evidence = paste(sort(unique(evidence)), collapse = " / "),
    documented_case_confidence = paste(sort(unique(confidence)), collapse = "/"),
    documented_manual_case = TRUE,
    .groups = "drop"
  )

commercial_candidates <- family_review %>%
  left_join(vintage_2021, by = "project_family_id", relationship = "one-to-one") %>%
  left_join(vintage_2024, by = "project_family_id", relationship = "one-to-one") %>%
  left_join(
    address_review_families,
    by = "project_family_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    verified_case_families,
    by = "project_family_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    permit_summary,
    by = c("project_family_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    observed_2021 = !is.na(source_rows_2021),
    observed_2024 = !is.na(source_rows_2024),
    selected_vintage = if_else(observed_2024, 2024L, 2021L),
    selected_source_rows = if_else(
      observed_2024,
      source_row_ids_2024,
      source_row_ids_2021
    ),
    selected_source_addresses = if_else(
      observed_2024,
      source_addresses_2024,
      source_addresses_2021
    ),
    selected_unit_measure = if_else(
      observed_2024,
      unit_measure_sources_2024,
      unit_measure_sources_2021
    ),
    selected_property_type_use = if_else(
      observed_2024,
      property_type_uses_2024,
      property_type_uses_2021
    ),
    selected_property_description = if_else(
      observed_2024,
      property_descriptions_2024,
      property_descriptions_2021
    ),
    construction_year = if_else(observed_2024, yearbuilt_2024, yearbuilt_2021),
    dwelling_units = if_else(observed_2024, reported_units_2024, reported_units_2021),
    selected_building_sqft = if_else(observed_2024, bldgsf_2024, bldgsf_2021),
    selected_land_sqft = if_else(observed_2024, landsf_2024, landsf_2021),
    stable_component_membership =
      !observed_2021 | !observed_2024 |
      component_pin_list_2021 == component_pin_list_2024,
    building_fallback_2021 =
      observed_2021 & observed_2024 & stable_component_membership &
      (!is.finite(selected_building_sqft) | selected_building_sqft <= 0) &
      is.finite(bldgsf_2021) & bldgsf_2021 > 0,
    land_fallback_2021 =
      observed_2021 & observed_2024 & stable_component_membership &
      (!is.finite(selected_land_sqft) | selected_land_sqft <= 0) &
      is.finite(landsf_2021) & landsf_2021 > 0,
    building_sqft = if_else(building_fallback_2021, bldgsf_2021, selected_building_sqft),
    land_sqft = if_else(land_fallback_2021, landsf_2021, selected_land_sqft),
    component_pins = if_else(
      observed_2024,
      component_pin_list_2024,
      component_pin_list_2021
    ),
    component_count = if_else(
      observed_2024,
      component_pins_2024,
      component_pins_2021
    ),
    selected_vintage_conflict = if_else(
      observed_2024,
      within_vintage_conflict_2024,
      within_vintage_conflict_2021
    ),
    selected_parser_repair = if_else(
      observed_2024,
      parser_repair_applied_2024,
      parser_repair_applied_2021
    ),
    selected_parser_residue = if_else(
      observed_2024,
      parser_numeric_residue_2024,
      parser_numeric_residue_2021
    ),
    substantive_unit_change =
      observed_2021 & observed_2024 &
      is.finite(reported_units_2021) & is.finite(reported_units_2024) &
      reported_units_2021 != reported_units_2024,
    substantive_building_change =
      observed_2021 & observed_2024 &
      is.finite(bldgsf_2021) & bldgsf_2021 > 0 &
      is.finite(bldgsf_2024) & bldgsf_2024 > 0 &
      bldgsf_2021 != bldgsf_2024,
    substantive_land_change =
      observed_2021 & observed_2024 &
      is.finite(landsf_2021) & landsf_2021 > 0 &
      is.finite(landsf_2024) & landsf_2024 > 0 &
      landsf_2021 != landsf_2024,
    same_address_cross_family = coalesce(same_address_cross_family, FALSE),
    documented_manual_case = coalesce(documented_manual_case, FALSE),
    student_housing = str_detect(
      coalesce(selected_property_type_use, ""),
      regex("student housing", ignore_case = TRUE)
    ),
    permit_reports_selected_units = map2_lgl(
      dwelling_units,
      unit_counts,
      ~ {
        counts <- suppressWarnings(as.numeric(str_split(coalesce(.y, ""), "/")[[1]]))
        is.finite(.x) && any(counts == .x, na.rm = TRUE)
      }
    ),
    permit_unit_mismatch =
      !is.na(unit_counts) & unit_counts != "" & !permit_reports_selected_units,
    project_id = project_family_id,
    source_family = "commercial",
    project_kind = "commercial_entity_family",
    class_values = if_else(observed_2024, "commercial_2024", "commercial_2021"),
    source_row_ids = selected_source_rows,
    year_source = paste0("commercial_", selected_vintage, "_rows:", selected_source_rows),
    units_source = paste0(
      "commercial_", selected_vintage, "_", selected_unit_measure,
      "_rows:", selected_source_rows
    ),
    building_source = if_else(
      building_fallback_2021,
      paste0("commercial_2021_field_fallback_rows:", source_row_ids_2021),
      paste0("commercial_", selected_vintage, "_rows:", selected_source_rows)
    ),
    land_source = if_else(
      land_fallback_2021,
      paste0("commercial_2021_field_fallback_rows:", source_row_ids_2021),
      paste0("commercial_", selected_vintage, "_rows:", selected_source_rows)
    ),
    current_distance_m = minimum_boundary_distance_m,
    current_within_1500ft = any_within_1500ft,
    candidate_status = case_when(
      documented_manual_case ~ "review_required",
      selected_vintage_conflict ~ "review_required",
      !stable_component_membership ~ "review_required",
      selected_parser_repair | selected_parser_residue ~ "review_required",
      !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
      student_housing | permit_unit_mismatch ~ "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      documented_manual_case ~ "documented_manual_case",
      selected_vintage_conflict ~ "selected_vintage_contains_multiple_entity_values",
      !stable_component_membership ~ "component_membership_changed_across_vintages",
      selected_parser_residue ~ "unparsed_component_pin_text",
      selected_parser_repair ~ "component_pin_text_required_repair",
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      student_housing ~ "student_housing_unit_definition_requires_review",
      permit_unit_mismatch ~ "permit_unit_counts_do_not_include_assessor_count",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "missing_or_nonpositive_units",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      building_fallback_2021 | land_fallback_2021 ~ "stable_membership_2021_field_fallback",
      substantive_unit_change | substantive_building_change | substantive_land_change ~
        "coherent_2024_revision_same_entity",
      observed_2024 & yearbuilt_changed ~ "2024_primary_year_recode",
      observed_2024 ~ "coherent_2024_primary_entity",
      TRUE ~ "coherent_2021_only_entity"
    )
  ) %>%
  select(
    project_id,
    source_family,
    project_kind,
    component_pins,
    component_count,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    class_values,
    source_row_ids,
    permit_chain_ids,
    permit_numbers,
    direct_permit_ids,
    year_source,
    units_source,
    building_source,
    land_source,
    current_distance_m,
    current_within_1500ft,
    selected_vintage,
    selected_source_addresses,
    selected_property_type_use,
    selected_property_description,
    observed_2021,
    observed_2024,
    selected_vintage_conflict,
    stable_component_membership,
    substantive_unit_change,
    substantive_building_change,
    substantive_land_change,
    selected_parser_repair,
    selected_parser_residue,
    same_address_cross_family,
    documented_manual_case,
    student_housing,
    permit_unit_mismatch,
    permit_unit_evidence = unit_counts,
    documented_case_pins,
    documented_case_actions,
    documented_case_evidence,
    documented_case_confidence,
    candidate_status,
    decision_reason
  ) %>%
  arrange(project_id)

if (anyDuplicated(commercial_candidates$project_id) > 0) {
  stop("Preferred commercial candidate IDs are not unique.", call. = FALSE)
}

component_rows <- commercial_candidates %>%
  select(project_id, source_family, project_kind, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>%
  distinct(project_id, component_pin, .keep_all = TRUE) %>%
  arrange(project_id, component_pin)

component_conflicts <- component_rows %>%
  group_by(component_pin) %>%
  summarise(projects = n_distinct(project_id), .groups = "drop") %>%
  filter(projects > 1)

if (nrow(component_conflicts) > 0) {
  stop(
    paste0(
      "Commercial component PINs belong to multiple project families: ",
      paste(head(component_conflicts$component_pin, 10), collapse = ", ")
    ),
    call. = FALSE
  )
}

adjudication_queue <- commercial_candidates %>%
  filter(
    candidate_status == "review_required",
    is.na(construction_year) | between(construction_year, 2006L, 2022L)
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(adjudication_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial adjudication queue contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  commercial_candidates %>%
    count(candidate_status, decision_reason, name = "value") %>%
    transmute(
      section = "candidate_decisions",
      metric = paste(candidate_status, decision_reason, sep = ":"),
      value
    ),
  tibble::tibble(
    section = "validation",
    metric = c(
      "candidate_projects",
      "candidate_components",
      "retained_mechanical_projects",
      "study_period_projects_requiring_review",
      "duplicate_project_ids",
      "component_pins_in_multiple_projects"
    ),
    value = c(
      nrow(commercial_candidates),
      nrow(component_rows),
      sum(commercial_candidates$candidate_status == "retain_mechanical"),
      nrow(adjudication_queue),
      anyDuplicated(commercial_candidates$project_id),
      nrow(component_conflicts)
    )
  )
)

readr::write_csv(
  commercial_candidates,
  "../output/preferred_commercial_project_candidates.csv"
)
readr::write_csv(
  component_rows,
  "../output/preferred_commercial_project_components.csv"
)
readr::write_csv(
  adjudication_queue,
  "../output/commercial_adjudication_queue.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_commercial_candidate_summary.csv"
)
