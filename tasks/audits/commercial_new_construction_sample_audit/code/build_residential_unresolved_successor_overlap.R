# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

successor_links <- readr::read_csv(
  "../output/residential_unresolved_episode_successor_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    episode_id = readr::col_character(),
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(inside_episode_polygon | distance_to_episode_polygon_ft <= 25) %>%
  distinct(episode_id, pin, .keep_all = TRUE)

if (anyDuplicated(successor_links[c("episode_id", "pin")]) > 0) {
  stop("Episode-successor links are not unique.", call. = FALSE)
}

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

components <- readr::read_csv(
  "../output/preferred_residential_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Preferred residential candidate IDs are not unique.", call. = FALSE)
}
if (anyDuplicated(components$component_pin) > 0) {
  stop("A residential component PIN belongs to more than one preferred candidate.", call. = FALSE)
}

candidate_lookup <- components %>%
  left_join(
    candidates %>%
      select(
        candidate_project_id = project_id,
        project_kind,
        candidate_component_pins = component_pins,
        construction_year,
        dwelling_units,
        building_sqft,
        land_sqft,
        year_source,
        units_source,
        building_source,
        land_source,
        candidate_status,
        decision_reason
      ),
    by = c("project_id" = "candidate_project_id"),
    relationship = "many-to-one"
  ) %>%
  rename(candidate_project_id = project_id)

if (any(is.na(candidate_lookup$candidate_status))) {
  stop("A residential candidate component lacks its project record.", call. = FALSE)
}

overlap <- successor_links %>%
  rename(source_project_id = project_id, successor_pin = pin) %>%
  left_join(
    candidate_lookup,
    by = c("successor_pin" = "component_pin"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    overlaps_candidate = !is.na(candidate_project_id),
    overlaps_retained_candidate = candidate_status == "retain_mechanical"
  ) %>%
  arrange(
    source_project_id,
    target_year,
    desc(inside_episode_polygon),
    distance_to_episode_polygon_ft,
    successor_pin
  )

project_summary <- overlap %>%
  group_by(source_project_id) %>%
  summarise(
    episode_count = n_distinct(episode_id),
    successor_pins = n_distinct(successor_pin),
    successor_pins_in_inventory = n_distinct(successor_pin[overlaps_candidate]),
    overlapping_candidate_projects = n_distinct(candidate_project_id[overlaps_candidate]),
    retained_candidate_projects = n_distinct(candidate_project_id[overlaps_retained_candidate]),
    retained_candidate_ids = paste(
      sort(unique(candidate_project_id[overlaps_retained_candidate])),
      collapse = "/"
    ),
    .groups = "drop"
  ) %>%
  arrange(source_project_id)

summary <- tibble::tibble(
  metric = c(
    "episode_successor_links",
    "distinct_successor_pins",
    "links_matching_candidate_inventory",
    "links_matching_retained_candidates",
    "distinct_overlapping_candidate_projects",
    "distinct_overlapping_retained_candidates",
    "source_projects_with_retained_candidate_overlap"
  ),
  value = c(
    nrow(overlap),
    n_distinct(overlap$successor_pin),
    sum(overlap$overlaps_candidate),
    sum(overlap$overlaps_retained_candidate, na.rm = TRUE),
    n_distinct(overlap$candidate_project_id[overlap$overlaps_candidate]),
    n_distinct(overlap$candidate_project_id[overlap$overlaps_retained_candidate]),
    sum(project_summary$retained_candidate_projects > 0)
  )
)

readr::write_csv(
  overlap,
  "../output/residential_unresolved_successor_candidate_overlap.csv"
)
readr::write_csv(
  project_summary,
  "../output/residential_unresolved_successor_overlap_projects.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_successor_overlap_summary.csv"
)
