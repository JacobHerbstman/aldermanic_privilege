# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_kind == "tieback_building") %>%
  transmute(
    source_project_id = project_id,
    component_pins,
    candidate_year = construction_year,
    candidate_units = dwelling_units,
    candidate_building_sqft = building_sqft,
    candidate_land_sqft = land_sqft,
    distance_to_boundary_ft,
    permit_chain_evidence,
    permit_unit_evidence,
    city_footprint_evidence
  )

card_projects <- readr::read_csv(
  "../output/residential_tieback_card_project_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    source_project_id = project_id,
    selected_tax_year,
    evidence_status,
    selected_snapshot_reason,
    selected_card_evidence,
    selected_episode_evidence
  )

episodes <- readr::read_csv(
  "../output/residential_tieback_construction_episode_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    source_project_id = project_id,
    selected_tax_year = tax_year,
    episode_year = construction_year,
    physical_cards,
    repeated_across_pins,
    card_number_conflicts,
    episode_building_sqft,
    episode_dwelling_units,
    episode_component_pins,
    episode_source_rows,
    episode_card_evidence,
    site_pins,
    site_land_sqft,
    site_land_complete,
    episode_fields_complete
  )

class297 <- readr::read_csv(
  "../output/residential_class297_source_disposition.csv",
  show_col_types = FALSE,
  col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(source_project_id)

overlap <- readr::read_csv(
  "../output/residential_overlap_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(overlap_action != "retain_residential_resolution") %>%
  select(source_project_id)

if (anyDuplicated(projects$source_project_id) > 0 ||
    anyDuplicated(card_projects$source_project_id) > 0 ||
    anyDuplicated(episodes[c("source_project_id", "selected_tax_year", "episode_year")]) > 0 ||
    anyDuplicated(class297$source_project_id) > 0 ||
    anyDuplicated(overlap$source_project_id) > 0) {
  stop("Tieback resolution inputs violate their declared keys.", call. = FALSE)
}
if (!setequal(projects$source_project_id, card_projects$source_project_id)) {
  stop("Tieback card evidence does not cover the full tieback review universe.", call. = FALSE)
}

active_projects <- projects %>%
  anti_join(class297, by = "source_project_id") %>%
  anti_join(overlap, by = "source_project_id") %>%
  left_join(card_projects, by = "source_project_id", relationship = "one-to-one")

episode_candidates <- active_projects %>%
  filter(evidence_status != "no_usable_card_snapshot") %>%
  left_join(
    episodes,
    by = c("source_project_id", "selected_tax_year"),
    relationship = "one-to-many"
  ) %>%
  group_by(source_project_id) %>%
  mutate(
    episodes_in_project = n(),
    proposed_project_id = if_else(
      episodes_in_project == 1,
      source_project_id,
      paste0(source_project_id, "_year", episode_year)
    )
  ) %>%
  ungroup() %>%
  mutate(
    proposed_year = episode_year,
    proposed_units = episode_dwelling_units,
    proposed_building_sqft = episode_building_sqft,
    proposed_land_sqft = site_land_sqft,
    year_changed = proposed_year != candidate_year,
    units_changed = proposed_units != candidate_units,
    building_sqft_changed = proposed_building_sqft != candidate_building_sqft,
    land_sqft_changed = proposed_land_sqft != candidate_land_sqft,
    candidate_resolution_status = case_when(
      evidence_status == "one_episode_card_evidence" &
        episodes_in_project == 1 &
        episode_fields_complete &
        site_land_complete ~ "mechanically_resolvable_one_episode",
      evidence_status == "distinct_buildings_share_card_number_review" ~
        "manual_distinct_building_review",
      evidence_status == "multiple_construction_episodes_review" ~
        "manual_multiple_episode_review",
      TRUE ~ "manual_other_episode_review"
    )
  ) %>%
  select(
    source_project_id,
    proposed_project_id,
    candidate_resolution_status,
    evidence_status,
    component_pins,
    selected_tax_year,
    episodes_in_project,
    proposed_year,
    proposed_units,
    proposed_building_sqft,
    proposed_land_sqft,
    physical_cards,
    repeated_across_pins,
    card_number_conflicts,
    episode_component_pins,
    episode_source_rows,
    candidate_year,
    candidate_units,
    candidate_building_sqft,
    candidate_land_sqft,
    year_changed,
    units_changed,
    building_sqft_changed,
    land_sqft_changed,
    distance_to_boundary_ft,
    selected_snapshot_reason,
    episode_card_evidence,
    permit_chain_evidence,
    permit_unit_evidence,
    city_footprint_evidence
  ) %>%
  arrange(source_project_id, proposed_year)

no_snapshot <- active_projects %>%
  filter(evidence_status == "no_usable_card_snapshot") %>%
  select(
    source_project_id,
    component_pins,
    candidate_year,
    candidate_units,
    candidate_building_sqft,
    candidate_land_sqft,
    distance_to_boundary_ft,
    evidence_status,
    permit_chain_evidence,
    permit_unit_evidence,
    city_footprint_evidence
  ) %>%
  arrange(source_project_id)

if (n_distinct(episode_candidates$source_project_id) + nrow(no_snapshot) != nrow(active_projects) ||
    any(is.na(episode_candidates$proposed_year)) ||
    any(episode_candidates$episodes_in_project < 1)) {
  stop("Tieback resolution candidates do not partition the active review universe.", call. = FALSE)
}

summary <- bind_rows(
  active_projects %>%
    count(evidence_status, name = "value") %>%
    transmute(section = "active_project_status", metric = evidence_status, value),
  episode_candidates %>%
    count(candidate_resolution_status, name = "value") %>%
    transmute(section = "episode_candidate_rows", metric = candidate_resolution_status, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "tieback_review_projects",
      "handled_by_class297",
      "handled_by_overlap",
      "active_tieback_projects",
      "episode_candidate_projects",
      "episode_candidate_rows",
      "no_snapshot_projects",
      "duplicate_episode_candidate_ids"
    ),
    value = c(
      nrow(projects),
      sum(projects$source_project_id %in% class297$source_project_id),
      sum(projects$source_project_id %in% overlap$source_project_id),
      nrow(active_projects),
      n_distinct(episode_candidates$source_project_id),
      nrow(episode_candidates),
      nrow(no_snapshot),
      anyDuplicated(episode_candidates$proposed_project_id)
    )
  )
)

readr::write_csv(
  episode_candidates,
  "../output/residential_tieback_episode_candidates.csv"
)
readr::write_csv(
  no_snapshot,
  "../output/residential_tieback_no_snapshot_review.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_resolution_candidate_summary.csv"
)
