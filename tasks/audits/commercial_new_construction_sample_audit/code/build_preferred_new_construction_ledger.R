# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

residential_projects <- readr::read_csv(
  "../output/preferred_residential_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_components <- readr::read_csv(
  "../output/preferred_residential_project_components_final.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_components <- readr::read_csv(
  "../output/preferred_commercial_project_component_locations.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_boundary <- readr::read_csv(
  "../output/preferred_residential_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

commercial_boundary <- readr::read_csv(
  "../output/preferred_commercial_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

additional_decisions <- readr::read_csv(
  "../adjudication/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

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
      decision_action = decision_source,
      geometry_source,
      geometry_evidence,
      x_3435,
      y_3435
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
      decision_action,
      geometry_source = project_geometry_source,
      geometry_evidence = project_geometry_evidence,
      x_3435,
      y_3435
    )
) %>%
  arrange(source_family, project_id)

components <- bind_rows(
  residential_components %>%
    transmute(
      project_id,
      source_family = "residential",
      project_kind,
      component_pin
    ),
  commercial_components %>%
    distinct(project_id, component_pin) %>%
    transmute(
      project_id,
      source_family = "commercial",
      project_kind = "commercial_assessor_project",
      component_pin
    )
) %>%
  arrange(source_family, project_id, component_pin)

boundary <- bind_rows(
  residential_boundary %>% mutate(source_family = "residential"),
  commercial_boundary %>% mutate(source_family = "commercial")
) %>%
  select(source_family, everything()) %>%
  arrange(source_family, project_id)

if (anyDuplicated(projects$project_id) > 0 ||
    anyDuplicated(components$component_pin) > 0 ||
    anyDuplicated(components[c("project_id", "component_pin")]) > 0 ||
    anyDuplicated(boundary$project_id) > 0 ||
    !setequal(projects$project_id, components$project_id) ||
    !setequal(projects$project_id, boundary$project_id)) {
  stop("The combined project, component, and boundary ledgers do not reconcile.", call. = FALSE)
}

component_membership <- components %>%
  group_by(project_id) %>%
  summarise(
    reconstructed_component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    .groups = "drop"
  )

project_membership <- projects %>%
  transmute(
    project_id,
    recorded_component_pins = vapply(
      strsplit(component_pins, "/", fixed = TRUE),
      function(x) paste(sort(unique(x)), collapse = "/"),
      character(1)
    )
  ) %>%
  left_join(component_membership, by = "project_id", relationship = "one-to-one")

if (any(project_membership$recorded_component_pins !=
    project_membership$reconstructed_component_pins)) {
  stop("A combined project component list does not match the component ledger.", call. = FALSE)
}

boundary_contract <- boundary %>%
  select(
    project_id,
    boundary_allow_far = allow_far,
    boundary_allow_dupac = allow_dupac
  ) %>%
  inner_join(
    projects %>% select(project_id, allow_far, allow_dupac),
    by = "project_id",
    relationship = "one-to-one"
  )

if (any(boundary_contract$boundary_allow_far != boundary_contract$allow_far) ||
    any(boundary_contract$boundary_allow_dupac != boundary_contract$allow_dupac) ||
    any(!is.finite(boundary$distance_to_boundary_ft)) ||
    any(is.na(boundary$ward)) ||
    any(is.na(boundary$ward_pair))) {
  stop("The combined boundary file violates the project ledger contract.", call. = FALSE)
}

cross_family_replacements <- additional_decisions %>%
  filter(decision == "replace_by_commercial")

if (nrow(cross_family_replacements) != 1 ||
    any(cross_family_replacements$candidate_project_id %in% projects$project_id) ||
    any(!cross_family_replacements$replacement_project_ids %in% projects$project_id) ||
    any(projects$source_family[
      match(cross_family_replacements$replacement_project_ids, projects$project_id)
    ] != "commercial")) {
  stop("The documented residential-to-commercial replacement is not enforced.", call. = FALSE)
}

required_text_columns <- c(
  "project_id",
  "source_family",
  "source_project_ids",
  "component_pins",
  "project_kind",
  "membership_source",
  "year_source",
  "units_source",
  "land_source",
  "decision_reason",
  "confidence",
  "decision_source",
  "decision_action",
  "geometry_source",
  "geometry_evidence"
)

field_failures <- c(
  construction_year = sum(!between(projects$construction_year, 2006L, 2022L)),
  land_area = sum(!is.finite(projects$land_sqft) | projects$land_sqft <= 0),
  coordinates = sum(!is.finite(projects$x_3435) | !is.finite(projects$y_3435)),
  far_flag = sum(
    projects$allow_far &
      (!is.finite(projects$building_sqft) |
        projects$building_sqft <= 0 |
        projects$land_sqft <= 0)
  ),
  dupac_flag = sum(
    projects$allow_dupac &
      (!is.finite(projects$dwelling_units) |
        projects$dwelling_units <= 0 |
        projects$land_sqft <= 0)
  ),
  missing_provenance = sum(is.na(projects[required_text_columns])),
  blank_provenance = sum(projects[required_text_columns] == "")
)

if (any(field_failures > 0)) {
  stop(
    "The combined preferred project ledger violates its field contract: ",
    paste(
      names(field_failures)[field_failures > 0],
      field_failures[field_failures > 0],
      sep = "=",
      collapse = ", "
    ),
    call. = FALSE
  )
}

residential_centroids <- sf::st_read(
  "../output/preferred_residential_project_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  mutate(source_family = "residential")
names(residential_centroids)[
  names(residential_centroids) == attr(residential_centroids, "sf_column")
] <- "geometry"
sf::st_geometry(residential_centroids) <- "geometry"

commercial_centroids <- sf::st_read(
  "../output/preferred_commercial_project_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  mutate(source_family = "commercial")
names(commercial_centroids)[
  names(commercial_centroids) == attr(commercial_centroids, "sf_column")
] <- "geometry"
sf::st_geometry(commercial_centroids) <- "geometry"

centroids <- rbind(
  residential_centroids %>% select(project_id, source_family, geometry),
  commercial_centroids %>% select(project_id, source_family, geometry)
) %>%
  arrange(source_family, project_id)

if (sf::st_crs(centroids)$epsg != 3435 ||
    nrow(centroids) != nrow(projects) ||
    anyDuplicated(centroids$project_id) > 0 ||
    !setequal(centroids$project_id, projects$project_id) ||
    any(sf::st_is_empty(centroids)) ||
    any(!sf::st_is_valid(centroids))) {
  stop("The combined centroid file is incomplete or invalid.", call. = FALSE)
}

boundary_counts <- boundary %>%
  group_by(source_family) %>%
  summarise(
    n_within_1500ft = sum(within_1500ft),
    n_within_500ft = sum(within_500ft),
    n_far_within_500ft = sum(within_500ft & allow_far),
    n_dupac_within_500ft = sum(within_500ft & allow_dupac),
    .groups = "drop"
  )

if (any(boundary_counts$n_far_within_500ft > boundary_counts$n_within_500ft) ||
    any(boundary_counts$n_dupac_within_500ft > boundary_counts$n_within_500ft) ||
    any(boundary_counts$n_within_500ft > boundary_counts$n_within_1500ft)) {
  stop("Combined boundary eligibility counts exceed their geographic samples.", call. = FALSE)
}

summary <- bind_rows(
  projects %>%
    group_by(source_family) %>%
    summarise(
      projects = n(),
      far_eligible = sum(allow_far),
      dupac_eligible = sum(allow_dupac),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      -source_family,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(section = "projects") %>%
    select(section, source_family, metric, value),
  components %>%
    count(source_family, name = "value") %>%
    transmute(
      section = "components",
      source_family,
      metric = "component_pins",
      value
    ),
  boundary_counts %>%
    tidyr::pivot_longer(
      -source_family,
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(metric = str_remove(metric, "^n_")) %>%
    mutate(section = "boundary_scope") %>%
    select(section, source_family, metric, value),
  tibble::tibble(
    section = "validation",
    source_family = "combined",
    metric = c(
      "projects",
      "component_pins",
      "duplicate_project_ids",
      "duplicate_component_pins",
      "missing_project_locations",
      "cross_family_replacements"
    ),
    value = c(
      nrow(projects),
      nrow(components),
      anyDuplicated(projects$project_id),
      anyDuplicated(components$component_pin),
      sum(!is.finite(projects$x_3435) | !is.finite(projects$y_3435)),
      nrow(cross_family_replacements)
    )
  )
) %>%
  arrange(section, source_family, metric)

readr::write_csv(
  projects,
  "../output/preferred_new_construction_project_ledger.csv"
)
readr::write_csv(
  components,
  "../output/preferred_new_construction_project_components.csv"
)
readr::write_csv(
  boundary,
  "../output/preferred_new_construction_boundary_scope.csv"
)
sf::st_write(
  centroids,
  "../output/preferred_new_construction_project_centroids.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  summary,
  "../output/preferred_new_construction_validation_summary.csv"
)
