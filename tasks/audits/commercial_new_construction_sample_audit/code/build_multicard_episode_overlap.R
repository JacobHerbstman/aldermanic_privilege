# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

successor_links <- readr::read_csv(
  "../output/multicard_current_successor_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    current_pin = readr::col_character(),
    component_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(!is.na(component_project_ids)) |>
  tidyr::separate_longer_delim(component_project_ids, delim = "/") |>
  dplyr::rename(child_project_id = component_project_ids) |>
  dplyr::filter(child_project_id != project_id) |>
  dplyr::distinct(
    project_id,
    child_project_id,
    current_pin,
    search_geometry_source
  )

parents <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    parent_pin = pin,
    parent_year = construction_year,
    within_500ft,
    within_1500ft,
    ward_pair,
    target_cards,
    target_classes,
    target_card_numbers,
    target_card_signatures,
    summed_card_units,
    summed_card_building_sqft,
    selected_units = dwelling_units,
    selected_building_sqft = building_sqft
  )

projects <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    child_project_id = project_id,
    child_source_family = source_family,
    child_project_kind = project_kind,
    child_component_pins = component_pins,
    child_year = construction_year,
    child_units = dwelling_units,
    child_building_sqft = building_sqft,
    child_land_sqft = land_sqft
  )

if (anyDuplicated(parents$project_id) ||
    anyDuplicated(projects$child_project_id)) {
  stop("Multicard episode project keys are not unique.", call. = FALSE)
}

edges <- successor_links |>
  dplyr::inner_join(
    parents,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    projects,
    by = "child_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    year_gap = child_year - parent_year,
    same_construction_episode_window = abs(year_gap) <= 1,
    exact_parcel_search =
      search_geometry_source != "centroid_150ft_candidate_search"
  ) |>
  dplyr::distinct(project_id, child_project_id, .keep_all = TRUE) |>
  dplyr::arrange(project_id, child_year, child_project_id)

episode_edges <- edges |>
  dplyr::filter(
    same_construction_episode_window,
    exact_parcel_search,
    child_source_family == "residential"
  )

parent_summary <- parents |>
  dplyr::left_join(
    episode_edges |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        child_projects = dplyr::n_distinct(child_project_id),
        child_project_ids = paste(
          sort(unique(child_project_id)),
          collapse = "/"
        ),
        child_component_pins = paste(
          sort(unique(current_pin)),
          collapse = "/"
        ),
        child_year_values = paste(
          sort(unique(child_year)),
          collapse = "/"
        ),
        child_units_sum = sum(child_units, na.rm = TRUE),
        child_building_sqft_sum =
          sum(child_building_sqft, na.rm = TRUE),
        child_land_sqft_sum = sum(child_land_sqft, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    child_projects = dplyr::coalesce(child_projects, 0L),
    child_units_sum = dplyr::coalesce(child_units_sum, 0),
    child_building_sqft_sum =
      dplyr::coalesce(child_building_sqft_sum, 0),
    child_land_sqft_sum = dplyr::coalesce(child_land_sqft_sum, 0),
    child_count_matches_cards = child_projects == target_cards,
    child_units_match_card_sum =
      child_units_sum == summed_card_units,
    child_building_ratio = dplyr::if_else(
      summed_card_building_sqft > 0,
      child_building_sqft_sum / summed_card_building_sqft,
      NA_real_
    ),
    child_building_matches_card_sum =
      is.finite(child_building_ratio) &
        abs(child_building_ratio - 1) <= 0.05,
    episode_overlap_status = dplyr::case_when(
      child_projects == 0L ~ "no_same_episode_successor_project",
      child_count_matches_cards &
        child_units_match_card_sum &
        child_building_matches_card_sum ~
        "complete_child_reproduction",
      child_count_matches_cards &
        child_units_match_card_sum ~
        "complete_unit_reproduction_sqft_changed",
      child_projects < target_cards &
        child_units_sum < summed_card_units ~
        "partial_child_reproduction",
      child_projects > target_cards &
        child_units_sum > summed_card_units ~
        "child_projects_exceed_parent_cards",
      TRUE ~ "same_episode_overlap_requires_review"
    ),
    parent_disposition_candidate = dplyr::case_when(
      episode_overlap_status %in% c(
        "complete_child_reproduction",
        "complete_unit_reproduction_sqft_changed"
      ) ~ "suppress_parent_aggregate",
      episode_overlap_status ==
        "no_same_episode_successor_project" ~
        "retain_parent_pending_card_adjudication",
      TRUE ~ "manual_episode_adjudication"
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    parent_disposition_candidate,
    project_id
  )

shared_children <- episode_edges |>
  dplyr::group_by(child_project_id) |>
  dplyr::summarise(
    parent_projects = dplyr::n_distinct(project_id),
    parent_project_ids = paste(
      sort(unique(project_id)),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(parent_projects > 1L) |>
  dplyr::arrange(dplyr::desc(parent_projects), child_project_id)

summary <- dplyr::bind_rows(
  parent_summary |>
    dplyr::count(
      within_500ft,
      episode_overlap_status,
      parent_disposition_candidate,
      name = "value"
    ) |>
    dplyr::transmute(
      section = dplyr::if_else(
        within_500ft,
        "within_500ft",
        "outside_500ft_within_1500ft"
      ),
      metric = paste(
        episode_overlap_status,
        parent_disposition_candidate,
        sep = ":"
      ),
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "multicard_projects",
      "same_episode_parent_child_edges",
      "parents_with_same_episode_children",
      "children_linked_to_multiple_parents"
    ),
    value = c(
      nrow(parents),
      nrow(episode_edges),
      sum(parent_summary$child_projects > 0),
      nrow(shared_children)
    )
  )
)

readr::write_csv(
  edges,
  "../output/multicard_parent_child_edges.csv"
)
readr::write_csv(
  episode_edges,
  "../output/multicard_same_episode_edges.csv"
)
readr::write_csv(
  parent_summary,
  "../output/multicard_episode_overlap_projects.csv"
)
readr::write_csv(
  shared_children,
  "../output/multicard_shared_successor_projects.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_episode_overlap_summary.csv"
)
