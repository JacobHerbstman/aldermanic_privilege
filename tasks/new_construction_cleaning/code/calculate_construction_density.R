# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

# Each branch has finished choosing buildings, assessments and recorded corrections.
residential_projects <- readr::read_csv("../output/residential_selected_buildings.csv",
  col_types = readr::cols(component_pins = "c", .default = readr::col_guess()))
commercial_projects <- readr::read_csv("../output/preferred_commercial_projects.csv",
  col_types = readr::cols(component_pins = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(residential_projects$project_id), !anyDuplicated(commercial_projects$project_id))

projects <- bind_rows(
  residential_projects %>%
    transmute(
      project_id,
      source_family = "residential",
      source_project_ids,
      source_addresses = NA_character_,
      component_pins,
      project_kind,
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
      evidence_ids,
      decision_reason,
      confidence,
      decision_source,
      decision_action = decision_source
    ),
  commercial_projects %>%
    transmute(
      project_id,
      source_family = "commercial",
      source_project_ids,
      source_addresses = selected_source_addresses,
      component_pins,
      project_kind = "commercial_assessor_project",
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
      evidence_ids,
      decision_reason,
      confidence,
      decision_source,
      decision_action
    )
) %>%
  arrange(source_family, project_id)

stopifnot(!anyDuplicated(projects$project_id), !anyNA(projects$construction_year),
  all(between(projects$construction_year, 2006, 2022)),
  all(!projects$allow_far | (is.finite(projects$building_sqft) & projects$building_sqft > 1 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)),
  all(!projects$allow_dupac | (is.finite(projects$dwelling_units) & projects$dwelling_units > 0 &
    is.finite(projects$land_sqft) & projects$land_sqft > 1)))
# Source measurements stay in square feet. One acre is exactly 43,560 square feet.
projects <- projects %>% mutate(
  far = if_else(allow_far, building_sqft / land_sqft, NA_real_),
  dupac = if_else(allow_dupac, dwelling_units * 43560 / land_sqft, NA_real_))
readr::write_csv(projects, "../output/new_construction_measurements.csv")
