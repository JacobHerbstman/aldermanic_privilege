# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/residential_tieback_episode_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    proposed_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

exceptions <- readr::read_csv(
  "../adjudication/residential_tieback_episode_exceptions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    override_year = readr::col_double(),
    decision_reason = readr::col_character(),
    evidence_ids = readr::col_character(),
    confidence = readr::col_character(),
    .default = readr::col_character()
  )
)

if (nrow(readr::problems(exceptions)) > 0) {
  stop("The tieback episode exception ledger contains malformed values.", call. = FALSE)
}
if (anyDuplicated(candidates$proposed_project_id) > 0 ||
    anyDuplicated(exceptions$source_project_id) > 0) {
  stop("Tieback episode inputs violate their declared keys.", call. = FALSE)
}
if (any(!exceptions$source_project_id %in% candidates$source_project_id)) {
  stop("A tieback episode exception names a project outside the review scope.", call. = FALSE)
}

resolution <- candidates %>%
  group_by(source_project_id) %>%
  summarise(
    component_pins = first(component_pins),
    construction_episodes = n(),
    first_episode_year = min(proposed_year),
    last_episode_year = max(proposed_year),
    dwelling_units = sum(proposed_units),
    building_sqft = sum(proposed_building_sqft),
    land_values = n_distinct(proposed_land_sqft),
    land_sqft = first(proposed_land_sqft),
    physical_cards = sum(physical_cards),
    evidence_status = paste(sort(unique(evidence_status)), collapse = "/"),
    candidate_resolution_status = paste(
      sort(unique(candidate_resolution_status)),
      collapse = "/"
    ),
    distance_to_boundary_ft = first(distance_to_boundary_ft),
    episode_source_rows = paste(episode_source_rows, collapse = "/"),
    episode_card_evidence = paste(episode_card_evidence, collapse = " || "),
    permit_chain_evidence = first(permit_chain_evidence),
    .groups = "drop"
  ) %>%
  left_join(exceptions, by = "source_project_id", relationship = "one-to-one") %>%
  mutate(
    final_project_id = source_project_id,
    construction_year = coalesce(override_year, last_episode_year),
    allow_far = TRUE,
    allow_dupac = TRUE,
    membership_source = paste0("tied_site:", component_pins),
    year_source = case_when(
      !is.na(override_year) ~ paste0("exception_ledger:", source_project_id),
      construction_episodes > 1 ~ paste0("final_adjacent_card_episode:", last_episode_year),
      TRUE ~ paste0("selected_card_snapshot:", source_project_id)
    ),
    units_source = paste0("distinct_card_episode_sum:", source_project_id),
    building_source = paste0("distinct_card_episode_sum:", source_project_id),
    land_source = paste0("tied_site_land_counted_once:", source_project_id),
    decision_reason = coalesce(
      decision_reason,
      case_when(
        construction_episodes > 1 ~
          "Adjacent-year building cards form one tied-site project; buildings are summed, land is counted once, and the project is dated by its final construction year.",
        candidate_resolution_status == "manual_distinct_building_review" ~
          "Distinct complete building-card signatures form one tied-site construction episode; buildings are summed and land is counted once.",
        evidence_status == "commercial_overlap_review" ~
          "The overlapping commercial record was excluded in favor of the complete tied residential building cards.",
        TRUE ~
          "Complete active building-card evidence identifies one tied-site construction episode."
      )
    ),
    evidence_ids = coalesce(evidence_ids, episode_source_rows),
    confidence = coalesce(confidence, "high")
  ) %>%
  select(
    source_project_id,
    final_project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    allow_far,
    allow_dupac,
    construction_episodes,
    land_values,
    first_episode_year,
    last_episode_year,
    physical_cards,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    decision_reason,
    evidence_ids,
    confidence,
    evidence_status,
    candidate_resolution_status,
    distance_to_boundary_ft,
    episode_card_evidence,
    permit_chain_evidence
  ) %>%
  arrange(source_project_id)

if (any(resolution$land_values != 1) ||
    any(resolution$last_episode_year - resolution$first_episode_year > 1) ||
    any(!between(resolution$construction_year, 2006L, 2022L)) ||
    any(!is.finite(resolution$dwelling_units) | resolution$dwelling_units <= 0) ||
    any(!is.finite(resolution$building_sqft) | resolution$building_sqft <= 0) ||
    any(!is.finite(resolution$land_sqft) | resolution$land_sqft <= 0) ||
    anyDuplicated(resolution$final_project_id) > 0) {
  stop("A resolved tieback episode violates project-level field rules.", call. = FALSE)
}

summary <- bind_rows(
  resolution %>%
    count(candidate_resolution_status, name = "value") %>%
    transmute(section = "source_status", metric = candidate_resolution_status, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "source_projects",
      "resolved_projects",
      "multi_episode_projects",
      "exception_rows",
      "far_eligible_projects",
      "dupac_eligible_projects",
      "duplicate_final_project_ids",
      "unresolved_projects"
    ),
    value = c(
      n_distinct(candidates$source_project_id),
      nrow(resolution),
      sum(resolution$construction_episodes > 1),
      nrow(exceptions),
      sum(resolution$allow_far),
      sum(resolution$allow_dupac),
      anyDuplicated(resolution$final_project_id),
      0
    )
  )
)

readr::write_csv(
  resolution,
  "../output/residential_tieback_episode_resolution.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_episode_resolution_summary.csv"
)
