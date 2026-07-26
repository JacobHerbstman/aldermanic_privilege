# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

unresolved <- readr::read_csv(
  "../output/residential_unresolved_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    study_period_year_values = readr::col_character(),
    review_category = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(review_category != "outside_period_candidate")

episodes <- readr::read_csv(
  "../output/residential_unresolved_construction_episodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    episode_component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(unresolved %>% select(project_id), by = "project_id") %>%
  filter(between(construction_year, 2006L, 2022L)) %>%
  transmute(
    project_id,
    target_year = as.integer(construction_year),
    component_pins = episode_component_pins,
    request_source = "assessor_construction_episode"
  ) %>%
  distinct()

projects_without_episodes <- unresolved %>%
  anti_join(episodes %>% distinct(project_id), by = "project_id") %>%
  tidyr::separate_longer_delim(study_period_year_values, delim = "/") %>%
  transmute(
    project_id,
    target_year = as.integer(study_period_year_values),
    component_pins,
    request_source = "reported_study_period_year"
  )

episode_inventory <- bind_rows(episodes, projects_without_episodes) %>%
  filter(between(target_year, 2006L, 2022L)) %>%
  distinct(project_id, target_year, .keep_all = TRUE) %>%
  arrange(project_id, target_year) %>%
  mutate(episode_id = paste(project_id, target_year, sep = "|")) %>%
  select(episode_id, everything())

if (n_distinct(episode_inventory$project_id) != nrow(unresolved)) {
  stop("Every study-period unresolved project must have an episode request.", call. = FALSE)
}
if (anyDuplicated(episode_inventory$episode_id) > 0) {
  stop("Residential unresolved episode IDs are not unique.", call. = FALSE)
}

requests <- episode_inventory %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  transmute(
    episode_id,
    project_id,
    target_year,
    request_source,
    component_pin = component_pins,
    pin10 = str_sub(component_pins, 1, 10)
  ) %>%
  distinct(episode_id, component_pin, .keep_all = TRUE) %>%
  arrange(target_year, pin10, project_id, component_pin)

if (any(is.na(requests$component_pin) | str_length(requests$component_pin) != 14)) {
  stop("Residential unresolved episode request contains an invalid PIN.", call. = FALSE)
}
if (anyDuplicated(requests[c("episode_id", "component_pin")]) > 0) {
  stop("Residential unresolved episode-component requests are not unique.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "study_period_projects",
      "project_year_episodes",
      "component_year_requests",
      "distinct_year_pin10_requests"
    ),
    value = c(
      nrow(unresolved),
      nrow(episode_inventory),
      nrow(requests),
      nrow(distinct(requests, target_year, pin10))
    )
  ),
  episode_inventory %>%
    count(request_source, name = "value") %>%
    transmute(metric = paste0("episodes_", request_source), value)
)

readr::write_csv(
  episode_inventory,
  "../output/residential_unresolved_episode_inventory.csv"
)
readr::write_csv(
  requests,
  "../output/residential_unresolved_episode_requests.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_episode_request_summary.csv"
)
