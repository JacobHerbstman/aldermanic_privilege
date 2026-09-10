# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

candidates <- readr::read_csv("../output/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = readr::col_character(), component_pins = readr::col_character(),
    source_row_ids = readr::col_character(), class_values = readr::col_character(), .default = readr::col_guess()))
review <- readr::read_csv("../output/residential_review_resolution_projects.csv",
  col_types = readr::cols(component_pins = readr::col_character(), class_values = readr::col_character(),
    .default = readr::col_guess()))
dispositions <- readr::read_csv("../output/residential_review_source_dispositions.csv",
  col_types = readr::cols(.default = readr::col_character()))
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
  "../adjudication/residential_additional_candidate_decisions.csv",
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

readr::write_csv(projects, "../output/residential_selected_buildings.csv")
