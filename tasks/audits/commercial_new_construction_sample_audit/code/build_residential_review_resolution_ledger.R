# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "residential", review_scope == "review_within_1500ft") %>%
  select(project_id)

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    class_values = readr::col_character(),
    .default = readr::col_guess()
  )
)

class297 <- readr::read_csv(
  "../output/residential_class297_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

overlap <- readr::read_csv(
  "../output/residential_overlap_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    replacement_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_episodes <- readr::read_csv(
  "../output/residential_tieback_episode_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_no_snapshot <- readr::read_csv(
  "../output/residential_tieback_no_snapshot_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

remaining <- readr::read_csv(
  "../output/residential_remaining_case_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    final_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

component_overrides <- readr::read_csv(
  "../adjudication/residential_class297_component_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    final_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

resolution_coverage <- bind_rows(
  class297 %>% distinct(project_id = source_project_id) %>% mutate(resolver = "class297"),
  overlap %>% distinct(project_id = source_project_id) %>% mutate(resolver = "overlap"),
  tieback_episodes %>% distinct(project_id = source_project_id) %>% mutate(resolver = "tieback_episode"),
  tieback_no_snapshot %>%
    distinct(project_id = source_project_id) %>%
    mutate(resolver = "tieback_no_snapshot"),
  remaining %>% distinct(project_id = source_project_id) %>% mutate(resolver = "remaining")
)

if (nrow(scope) != 184 ||
    anyDuplicated(scope$project_id) > 0 ||
    !setequal(scope$project_id, resolution_coverage$project_id)) {
  stop("The within-1,500-foot residential review scope is not covered exactly.", call. = FALSE)
}

overlap_precedence <- overlap %>%
  select(source_project_id, overlap_action)

class_projects_raw <- class297 %>%
  filter(str_starts(decision_action, "retain_")) %>%
  anti_join(
    overlap_precedence %>%
      filter(overlap_action != "retain_residential_resolution"),
    by = "source_project_id"
  ) %>%
  transmute(
    project_id = final_project_id,
    source_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "class297_resolution"
  )

tieback_episode_projects_raw <- tieback_episodes %>%
  anti_join(
    overlap_precedence %>%
      filter(overlap_action != "retain_residential_resolution"),
    by = "source_project_id"
  ) %>%
  transmute(
    project_id = final_project_id,
    source_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "tieback_episode_resolution"
  )

tieback_no_snapshot_projects_raw <- tieback_no_snapshot %>%
  filter(decision_action == "retain_override") %>%
  transmute(
    project_id = final_project_id,
    source_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    membership_source = "adjudicated_tieback_membership",
    year_source,
    units_source,
    building_source,
    land_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "tieback_no_snapshot_resolution"
  )

remaining_projects_raw <- remaining %>%
  filter(decision_action == "retain_override") %>%
  transmute(
    project_id = final_project_id,
    source_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "remaining_case_resolution"
  )

direct_overlap_projects_raw <- overlap %>%
  filter(overlap_action == "retain_residential_resolution") %>%
  anti_join(
    bind_rows(
      class_projects_raw %>% distinct(source_project_id),
      tieback_episode_projects_raw %>% distinct(source_project_id)
    ),
    by = "source_project_id"
  ) %>%
  transmute(
    project_id = source_project_id,
    source_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    membership_source = "residential_overlap_resolution",
    year_source = "residential_overlap_resolution",
    units_source = "residential_overlap_resolution",
    building_source = "residential_overlap_resolution",
    land_source = "residential_overlap_resolution",
    evidence_ids,
    decision_reason,
    confidence,
    decision_source = "overlap_resolution"
  )

retained_rows <- bind_rows(
  class_projects_raw,
  tieback_episode_projects_raw,
  tieback_no_snapshot_projects_raw,
  remaining_projects_raw,
  direct_overlap_projects_raw
) %>%
  left_join(
    component_overrides %>%
      transmute(
        project_id = final_project_id,
        override_component_pins = component_pins,
        component_override_reason = decision_reason,
        component_override_evidence = evidence_ids
      ),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    component_pins = coalesce(override_component_pins, component_pins),
    decision_reason = if_else(
      !is.na(component_override_reason),
      paste(decision_reason, component_override_reason, sep = " || "),
      decision_reason
    ),
    evidence_ids = if_else(
      !is.na(component_override_evidence),
      paste(evidence_ids, component_override_evidence, sep = "/"),
      evidence_ids
    )
  ) %>%
  left_join(
    candidates %>%
      select(
        source_project_id = project_id,
        project_kind,
        class_values
      ),
    by = "source_project_id",
    relationship = "many-to-one"
  )

field_conflicts <- retained_rows %>%
  group_by(project_id) %>%
  summarise(
    construction_year_values = n_distinct(construction_year, na.rm = TRUE),
    dwelling_unit_values = n_distinct(dwelling_units, na.rm = TRUE),
    building_sqft_values = n_distinct(building_sqft, na.rm = TRUE),
    land_sqft_values = n_distinct(land_sqft, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    construction_year_values > 1 |
      dwelling_unit_values > 1 |
      building_sqft_values > 1 |
      land_sqft_values > 1
  )

if (nrow(field_conflicts) > 0 ||
    any(is.na(retained_rows$project_id)) ||
    any(!retained_rows$source_project_id %in% scope$project_id) ||
    anyDuplicated(component_overrides$final_project_id) > 0 ||
    !setequal(
      component_overrides$final_project_id,
      retained_rows$project_id[!is.na(retained_rows$override_component_pins)]
    )) {
  stop("Retained residential review rows conflict or fall outside the review scope.", call. = FALSE)
}

project_components <- retained_rows %>%
  select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(component_pin = component_pins) %>%
  distinct(project_id, component_pin)

projects <- retained_rows %>%
  group_by(project_id) %>%
  summarise(
    source_project_ids = paste(sort(unique(source_project_id)), collapse = "/"),
    geometry_source_project_ids = source_project_ids,
    project_kind = paste(sort(unique(project_kind)), collapse = "/"),
    construction_year = first(construction_year),
    dwelling_units = first(dwelling_units),
    building_sqft = first(building_sqft),
    land_sqft = first(land_sqft),
    class_values = paste(sort(unique(class_values)), collapse = "/"),
    membership_source = paste(sort(unique(membership_source)), collapse = " || "),
    year_source = paste(sort(unique(year_source)), collapse = " || "),
    units_source = paste(sort(unique(units_source)), collapse = " || "),
    building_source = paste(sort(unique(building_source)), collapse = " || "),
    land_source = paste(sort(unique(land_source)), collapse = " || "),
    evidence_ids = paste(sort(unique(evidence_ids)), collapse = "/"),
    decision_reason = paste(sort(unique(decision_reason)), collapse = " || "),
    confidence = if_else(all(confidence == "high"), "high", "medium"),
    decision_source = paste(sort(unique(decision_source)), collapse = "/"),
    .groups = "drop"
  ) %>%
  left_join(
    project_components %>%
      group_by(project_id) %>%
      summarise(
        component_pins = paste(sort(component_pin), collapse = "/"),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    building_source = if_else(
      !is.finite(building_sqft) &
        (is.na(building_source) | building_source == ""),
      "unavailable_after_adjudication",
      building_source
    )
  ) %>%
  relocate(component_pins, .after = geometry_source_project_ids) %>%
  arrange(project_id)

source_to_final <- retained_rows %>%
  distinct(source_project_id, project_id) %>%
  group_by(source_project_id) %>%
  summarise(
    final_project_ids = paste(sort(project_id), collapse = "/"),
    .groups = "drop"
  )

base_dispositions <- bind_rows(
  class297 %>%
    group_by(source_project_id) %>%
    summarise(
      base_action = paste(sort(unique(decision_action)), collapse = "/"),
      base_reason = paste(sort(unique(decision_reason)), collapse = " || "),
      base_evidence = paste(sort(unique(evidence_ids)), collapse = "/"),
      .groups = "drop"
    ),
  tieback_episodes %>%
    transmute(
      source_project_id,
      base_action = "retain_resolution",
      base_reason = decision_reason,
      base_evidence = evidence_ids
    ),
  tieback_no_snapshot %>%
    transmute(
      source_project_id,
      base_action = decision_action,
      base_reason = decision_reason,
      base_evidence = evidence_ids
    ),
  remaining %>%
    transmute(
      source_project_id,
      base_action = decision_action,
      base_reason = decision_reason,
      base_evidence = evidence_ids
    )
) %>%
  anti_join(overlap %>% select(source_project_id), by = "source_project_id")

overlap_dispositions <- overlap %>%
  transmute(
    source_project_id,
    base_action = overlap_action,
    base_reason = decision_reason,
    base_evidence = evidence_ids,
    overlap_replacement = replacement_project_id
  )

source_dispositions <- bind_rows(base_dispositions, overlap_dispositions) %>%
  left_join(source_to_final, by = "source_project_id", relationship = "one-to-one") %>%
  mutate(
    final_project_ids = case_when(
      !is.na(final_project_ids) ~ final_project_ids,
      base_action == "replace_by_residential_successor" ~ overlap_replacement,
      base_action == "replace_by_commercial" ~ overlap_replacement,
      TRUE ~ NA_character_
    ),
    final_disposition = case_when(
      !is.na(source_project_id) & source_project_id %in% retained_rows$source_project_id ~
        "retained_as_resolved_project",
      base_action == "replace_by_residential_successor" ~
        "replaced_by_existing_residential_project",
      base_action == "replace_by_commercial" ~
        "replaced_by_commercial_project",
      TRUE ~ "excluded_after_review"
    )
  ) %>%
  select(
    source_project_id,
    final_disposition,
    final_project_ids,
    resolution_action = base_action,
    decision_reason = base_reason,
    evidence_ids = base_evidence
  ) %>%
  arrange(source_project_id)

duplicate_components <- project_components %>%
  add_count(component_pin, name = "projects_using_component") %>%
  filter(projects_using_component > 1) %>%
  arrange(component_pin, project_id)

if (nrow(source_dispositions) != nrow(scope) ||
    anyDuplicated(source_dispositions$source_project_id) > 0 ||
    !setequal(source_dispositions$source_project_id, scope$project_id) ||
    nrow(projects) != 139 ||
    anyDuplicated(projects$project_id) > 0 ||
    anyDuplicated(project_components$component_pin) > 0) {
  stop(
    paste0(
      "The normalized residential review ledger does not reconcile: dispositions=",
      nrow(source_dispositions),
      ", scope=", nrow(scope),
      ", disposition duplicates=", anyDuplicated(source_dispositions$source_project_id),
      ", final projects=", nrow(projects),
      ", project duplicates=", anyDuplicated(projects$project_id),
      ", component duplicates=", anyDuplicated(project_components$component_pin),
      "; first duplicate components=",
      paste(head(unique(duplicate_components$component_pin), 10), collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}

summary <- bind_rows(
  source_dispositions %>%
    count(final_disposition, name = "value") %>%
    transmute(section = "source_disposition", metric = final_disposition, value),
  tibble::tibble(
    section = "final_projects",
    metric = c(
      "retained_projects",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "component_pins"
    ),
    value = c(
      nrow(projects),
      sum(is.finite(projects$building_sqft) & projects$building_sqft > 0),
      sum(is.finite(projects$dwelling_units) & projects$dwelling_units > 0),
      nrow(project_components)
    )
  )
)

readr::write_csv(
  projects,
  "../output/residential_review_resolution_projects.csv"
)
readr::write_csv(
  project_components,
  "../output/residential_review_resolution_components.csv"
)
readr::write_csv(
  source_dispositions,
  "../output/residential_review_source_dispositions.csv"
)
readr::write_csv(
  summary,
  "../output/residential_review_resolution_summary.csv"
)
