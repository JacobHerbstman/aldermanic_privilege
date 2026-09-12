# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    class_values = readr::col_character(), source_row_ids = readr::col_character(), .default = readr::col_guess()))
condos <- readr::read_csv("../output/residential_class297_resolution.csv",
  col_types = readr::cols(component_pins = readr::col_character(), condo_base = readr::col_character(),
    .default = readr::col_guess()))
overlap <- readr::read_csv("../output/residential_overlap_resolution.csv",
  col_types = readr::cols(component_pins = readr::col_character(), .default = readr::col_guess()))
houses <- readr::read_csv("../output/residential_tieback_episode_resolution.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(overlap$source_project_id),
  !any(houses$source_project_id[houses$decision_action != "unresolved"] %in% condos$source_project_id))
# Matched condominium buildings use the completed-building review above.
houses <- houses %>% filter(!source_project_id %in% condos$source_project_id)

# Only supported decisions produce a building. Unfinished reviews remain explicit
# dispositions, rather than silently becoming exclusions or incomplete buildings.
retained <- condos %>% filter(decision_action == "retain_successor_evidence") %>%
  transmute(project_id = final_project_id, source_project_id, component_pins,
    construction_year, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    membership_source, year_source, units_source, building_source, land_source,
    evidence_ids, decision_reason, confidence, decision_source = "completed_condominium_assessment") %>%
  bind_rows(overlap %>% filter(overlap_action == "retain_residential_resolution") %>%
    transmute(project_id = source_project_id, source_project_id, component_pins,
      construction_year, dwelling_units, building_sqft, land_sqft, allow_far = TRUE, allow_dupac = TRUE,
      membership_source = "recorded_residential_overlap_decision",
      year_source = "selected_residential_assessment", units_source = year_source,
      building_source = year_source, land_source = year_source,
      evidence_ids, decision_reason, confidence, decision_source = "recorded_overlap_decision")) %>%
  left_join(candidates %>% select(source_project_id = project_id, project_kind, class_values),
    by = "source_project_id", relationship = "one-to-one")
stopifnot(!anyDuplicated(retained$project_id), !anyDuplicated(retained$source_project_id),
  all(retained$construction_year >= 2006 & retained$construction_year <= 2022),
  all(retained$dwelling_units > 0 & retained$land_sqft > 1),
  all(!retained$allow_far | (is.finite(retained$building_sqft) & retained$building_sqft > 1)))
review <- retained %>% rename(source_project_ids = source_project_id) %>%
  mutate(geometry_source_project_ids = source_project_ids) %>% arrange(project_id)
components <- review %>% select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>% arrange(project_id, component_pin)
stopifnot(!anyDuplicated(components$component_pin))

condo_dispositions <- condos %>% group_by(source_project_id) %>%
  summarise(resolution_action = if (all(decision_action == "retain_successor_evidence")) "retain_successor_evidence" else if (all(decision_action == "exclude_outside_study_period")) "exclude_outside_study_period" else "unresolved",
    decision_reason = paste(sort(unique(decision_reason)), collapse = " | "),
    evidence_ids = paste(sort(unique(evidence_ids[!is.na(evidence_ids)])), collapse = " | "),
    .groups = "drop")
decisions <- bind_rows(condo_dispositions,
  overlap %>% transmute(source_project_id, resolution_action = overlap_action, decision_reason, evidence_ids),
  houses %>% transmute(source_project_id, resolution_action = decision_action, decision_reason, evidence_ids))
stopifnot(!anyDuplicated(decisions$source_project_id))
dispositions <- candidates %>% filter(candidate_status %in% c("review_required", "defer_to_commercial_reconciliation")) %>%
  transmute(source_project_id = project_id, prior_reason = decision_reason) %>%
  left_join(decisions, by = "source_project_id", relationship = "one-to-one") %>%
  left_join(retained %>% select(source_project_id, final_project_ids = project_id),
    by = "source_project_id", relationship = "one-to-one") %>%
  left_join(overlap %>% select(source_project_id, replacement_project_id),
    by = "source_project_id", relationship = "one-to-one") %>%
  left_join(houses %>% select(source_project_id, house_replacements = replacement_project_ids),
    by = "source_project_id", relationship = "one-to-one") %>%
  mutate(resolution_action = coalesce(resolution_action, "unresolved"),
    decision_reason = coalesce(decision_reason, prior_reason),
    final_project_ids = coalesce(final_project_ids,
      if_else(resolution_action == "replace_by_commercial", replacement_project_id, NA_character_),
      if_else(resolution_action == "replace_by_residential_successors", house_replacements, NA_character_)),
    final_disposition = case_when(
      resolution_action == "exclude_outside_study_period" ~ "excluded_outside_study_period",
      source_project_id %in% retained$source_project_id ~ "retained_as_resolved_project",
      resolution_action == "replace_by_commercial" ~ "replaced_by_commercial_project",
      resolution_action == "replace_by_residential_successors" ~ "replaced_by_existing_residential_project",
      TRUE ~ "review_required")) %>%
  select(source_project_id, final_disposition, final_project_ids, resolution_action, decision_reason, evidence_ids) %>%
  arrange(source_project_id)
stopifnot(!anyDuplicated(dispositions$source_project_id),
  all(!is.na(dispositions$final_project_ids[!dispositions$final_disposition %in% c("review_required", "excluded_outside_study_period")])))
SaveData(dispositions, c("source_project_id"), "../output/residential_review_source_dispositions.csv")

stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(review$project_id),
  !anyDuplicated(dispositions$source_project_id),
  setequal(dispositions$source_project_id,
    candidates$project_id[candidates$candidate_status %in% c("review_required", "defer_to_commercial_reconciliation")]))

# Validate commercial replacements against the completed commercial dataset;
# this check does not copy or reapply its measurement corrections.
commercial <- readr::read_csv("../output/preferred_commercial_projects.csv",
  col_types = readr::cols(project_id = "c", construction_year = "d", dwelling_units = "d", land_sqft = "d",
    .default = readr::col_skip()))
replacements <- candidates %>% filter(replacement_check == "recorded_complete_commercial_project") %>%
  select(project_id, replacement_project_ids) %>% left_join(commercial,
    by = c("replacement_project_ids" = "project_id"), relationship = "many-to-one")
stopifnot(!anyDuplicated(commercial$project_id),
  all(between(replacements$construction_year, 2006, 2022)),
  all(is.finite(replacements$dwelling_units) & replacements$dwelling_units > 0),
  all(is.finite(replacements$land_sqft) & replacements$land_sqft > 1))

# This is the selected residential dataset. Unresolved sources remain in the
# complete source-disposition table and do not silently become analysis rows.
mechanical <- candidates %>% filter(candidate_status == "retain_mechanical") %>%
  transmute(project_id, source_project_ids = project_id, geometry_source_project_ids = project_id,
    component_pins, project_kind, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, class_values, membership_source = "preferred_residential_candidate",
    year_source, units_source, building_source, land_source, evidence_ids = source_row_ids,
    decision_reason, confidence = "source_rule", decision_source = "residential_candidate")
# Recorded cross-source replacements remove the residential copy of a building.
cross_source_decisions <- readr::read_csv(
  "../input/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE) %>% filter(decision == "replace_by_commercial")
stopifnot(!anyDuplicated(cross_source_decisions$candidate_project_id),
  !anyNA(cross_source_decisions$replacement_project_ids))
projects <- bind_rows(mechanical, review) %>%
  filter(!project_id %in% cross_source_decisions$candidate_project_id) %>% arrange(project_id)
stopifnot(!anyDuplicated(projects$project_id), !anyNA(projects$construction_year),
  all(between(projects$construction_year, 2006, 2022)),
  all(!projects$allow_dupac | (is.finite(projects$dwelling_units) & projects$dwelling_units > 0 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)),
  all(!projects$allow_far | (is.finite(projects$building_sqft) & projects$building_sqft > 1 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)))
components <- projects %>% select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>% rename(component_pin = component_pins)
stopifnot(!anyDuplicated(components$component_pin))

SaveData(projects, c("project_id"), "../output/residential_selected_buildings.csv")
