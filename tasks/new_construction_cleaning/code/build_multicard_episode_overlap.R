# setwd("tasks/new_construction_cleaning/code")
# episode_year_window <- 2

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(episode_year_window)
if (length(args) != 1L) stop("Expected the construction-episode year window.")
episode_year_window <- suppressWarnings(as.integer(args[1]))
if (!is.finite(episode_year_window) || episode_year_window < 0L ||
    episode_year_window != suppressWarnings(as.numeric(args[1]))) {
  stop("The construction-episode year window must be a nonnegative integer.")
}


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
    same_construction_episode_window = abs(year_gap) <= episode_year_window,
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

readr::write_csv(
  episode_edges,
  "../output/multicard_same_episode_edges.csv"
)
