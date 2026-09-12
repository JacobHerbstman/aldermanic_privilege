# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/construction_density/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")

# Each branch has finished choosing buildings, assessments and recorded corrections.
residential_projects <- readr::read_csv("../input/residential_selected_buildings.csv",
  col_types = readr::cols(component_pins = "c", class_values = "c", .default = readr::col_guess()))
commercial_projects <- readr::read_csv("../input/preferred_commercial_projects.csv",
  col_types = readr::cols(component_pins = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(residential_projects$project_id), !anyDuplicated(commercial_projects$project_id))

projects <- bind_rows(
  residential_projects %>%
    transmute(
      project_id,
      source_family = "residential",
      class_values,
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
      class_values = NA_character_,
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
# Preserve the established class-first distinction: a group of townhouses is
# not an apartment building simply because the project has several homes.
projects <- projects |> mutate(
  single_family_class = stringr::str_detect(coalesce(class_values, ""),
    paste0("(^|/)(", paste(single_family_assessor_classes, collapse = "|"), ")($|/)")),
  multifamily_class = stringr::str_detect(coalesce(class_values, ""), "(^|/)(211|212|297)($|/)"),
  external_multifamily = case_when(
    dwelling_units <= 1 ~ FALSE,
    source_family == "commercial" ~ coalesce(dwelling_units > 1, FALSE),
    multifamily_class ~ TRUE,
    single_family_class ~ FALSE,
    class_values %in% c("EX", "OA2") ~ dwelling_units > 1,
    TRUE ~ NA),
  multifamily_source = "selected_assessor_class_and_finished_home_count")

# Recorded building-type decisions contain only the approved classification.
# A finished building with one dwelling remains single-family.
type_reviews <- readr::read_csv("../input/construction_building_types.csv", show_col_types = FALSE) |>
  select(project_id, reviewed_multifamily = multifamily, classification_source)
stopifnot(!anyDuplicated(type_reviews$project_id), !anyNA(type_reviews$reviewed_multifamily))
projects <- projects |>
  left_join(type_reviews, by = "project_id", relationship = "one-to-one") |>
  mutate(external_multifamily = case_when(
    dwelling_units <= 1 ~ FALSE,
    !is.na(reviewed_multifamily) ~ reviewed_multifamily,
    TRUE ~ external_multifamily),
    multifamily_source = case_when(
      dwelling_units <= 1 ~ "finished_building_has_one_home",
      !is.na(reviewed_multifamily) ~ classification_source,
      TRUE ~ multifamily_source)) |>
  select(-single_family_class, -multifamily_class, -reviewed_multifamily, -classification_source)
# Source measurements stay in square feet. One acre is exactly 43,560 square feet.
projects <- projects %>% mutate(
  far = if_else(allow_far, building_sqft / land_sqft, NA_real_),
  dupac = if_else(allow_dupac, dwelling_units * 43560 / land_sqft, NA_real_))
SaveData(projects, c("project_id"), "../output/new_construction_measurements.csv")
