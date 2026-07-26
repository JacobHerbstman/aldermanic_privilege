# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

edges <- readr::read_csv(
  "../output/multicard_same_episode_edges.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    child_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(project_id, child_project_id)

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
    construction_year,
    within_500ft,
    distance_to_boundary_ft,
    ward_pair,
    target_cards,
    summed_card_units,
    summed_card_building_sqft,
    selected_units = dwelling_units,
    selected_building_sqft = building_sqft,
    current_addresses,
    manual_address
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
    project_id,
    source_family,
    project_kind,
    source_addresses,
    component_pins,
    project_year = construction_year,
    project_units = dwelling_units,
    project_building_sqft = building_sqft,
    project_land_sqft = land_sqft,
    x_3435,
    y_3435
  )

if (anyDuplicated(parents$project_id) ||
    anyDuplicated(projects$project_id)) {
  stop("Episode component project keys are not unique.", call. = FALSE)
}

graph <- igraph::graph_from_data_frame(
  edges |>
    dplyr::rename(from = project_id, to = child_project_id),
  directed = TRUE
)

membership <- igraph::components(
  igraph::as_undirected(graph, mode = "collapse")
)$membership

component_keys <- tibble::tibble(
  project_id = names(membership),
  raw_component = as.integer(membership)
) |>
  dplyr::group_by(raw_component) |>
  dplyr::mutate(component_key = min(project_id)) |>
  dplyr::ungroup() |>
  dplyr::arrange(component_key, project_id) |>
  dplyr::mutate(
    component_id = sprintf(
      "episode_%03d",
      dplyr::dense_rank(component_key)
    )
  )

parent_ids <- unique(edges$project_id)
child_ids <- unique(edges$child_project_id)

nodes <- component_keys |>
  dplyr::mutate(
    is_parent = project_id %in% parent_ids,
    is_child = project_id %in% child_ids,
    is_root_parent = is_parent & !is_child,
    is_terminal_child = is_child & !is_parent
  ) |>
  dplyr::left_join(
    parents,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(component_id, dplyr::desc(is_root_parent), project_id)

component_summary <- nodes |>
  dplyr::group_by(component_id, component_key) |>
  dplyr::summarise(
    graph_nodes = dplyr::n(),
    parent_projects = sum(is_parent),
    root_parent_projects = sum(is_root_parent),
    intermediate_projects = sum(is_parent & is_child),
    terminal_child_projects = sum(is_terminal_child),
    parent_project_ids = paste(
      sort(project_id[is_parent]),
      collapse = "/"
    ),
    root_parent_project_ids = paste(
      sort(project_id[is_root_parent]),
      collapse = "/"
    ),
    terminal_child_project_ids = paste(
      sort(project_id[is_terminal_child]),
      collapse = "/"
    ),
    root_parent_pins = paste(
      sort(parent_pin[is_root_parent & !is.na(parent_pin)]),
      collapse = "/"
    ),
    root_parent_years = paste(
      sort(unique(construction_year[
        is_root_parent & !is.na(construction_year)
      ])),
      collapse = "/"
    ),
    within_500ft = any(
      within_500ft[is_root_parent] %in% TRUE,
      na.rm = TRUE
    ),
    minimum_boundary_distance_ft = suppressWarnings(
      min(distance_to_boundary_ft[is_root_parent], na.rm = TRUE)
    ),
    ward_pairs = paste(
      sort(unique(ward_pair[is_root_parent & !is.na(ward_pair)])),
      collapse = "/"
    ),
    root_target_cards = sum(
      target_cards[is_root_parent],
      na.rm = TRUE
    ),
    root_card_units = sum(
      summed_card_units[is_root_parent],
      na.rm = TRUE
    ),
    root_card_building_sqft = sum(
      summed_card_building_sqft[is_root_parent],
      na.rm = TRUE
    ),
    terminal_child_units = sum(
      project_units[is_terminal_child],
      na.rm = TRUE
    ),
    terminal_child_building_sqft = sum(
      project_building_sqft[is_terminal_child],
      na.rm = TRUE
    ),
    terminal_child_land_sqft = sum(
      project_land_sqft[is_terminal_child],
      na.rm = TRUE
    ),
    root_addresses = paste(
      sort(unique(dplyr::coalesce(
        manual_address[is_root_parent],
        current_addresses[is_root_parent]
      ))),
      collapse = " / "
    ),
    terminal_child_addresses = paste(
      sort(unique(source_addresses[
        is_terminal_child & !is.na(source_addresses)
      ])),
      collapse = " / "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    minimum_boundary_distance_ft = dplyr::if_else(
      is.infinite(minimum_boundary_distance_ft),
      NA_real_,
      minimum_boundary_distance_ft
    ),
    unit_ratio = dplyr::if_else(
      root_card_units > 0,
      terminal_child_units / root_card_units,
      NA_real_
    ),
    building_sqft_ratio = dplyr::if_else(
      root_card_building_sqft > 0,
      terminal_child_building_sqft / root_card_building_sqft,
      NA_real_
    ),
    exact_units =
      terminal_child_units == root_card_units,
    close_building_sqft =
      is.finite(building_sqft_ratio) &
        abs(building_sqft_ratio - 1) <= 0.05,
    component_status = dplyr::case_when(
      root_parent_projects != 1L ~
        "manual_shared_parent_component",
      intermediate_projects > 0L ~
        "manual_multigeneration_component",
      exact_units & close_building_sqft ~
        "high_confidence_reproduction",
      exact_units ~
        "unit_reproduction_sqft_changed",
      unit_ratio > 1 ~
        "successors_exceed_parent_inventory",
      unit_ratio < 1 ~
        "partial_successor_reproduction",
      TRUE ~ "manual_component_review"
    ),
    automatic_resolution_eligible =
      component_status == "high_confidence_reproduction"
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    automatic_resolution_eligible,
    component_status,
    component_id
  )

edge_components <- edges |>
  dplyr::left_join(
    component_keys |>
      dplyr::select(project_id, component_id),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    parents |>
      dplyr::select(
        project_id,
        parent_pin,
        construction_year,
        target_cards,
        summed_card_units,
        summed_card_building_sqft
      ),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        child_project_id = project_id,
        child_addresses = source_addresses,
        child_pins = component_pins,
        child_year = project_year,
        child_units = project_units,
        child_building_sqft = project_building_sqft,
        child_land_sqft = project_land_sqft
      ),
    by = "child_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(component_id, project_id, child_project_id)

summary <- dplyr::bind_rows(
  component_summary |>
    dplyr::count(within_500ft, component_status, name = "value") |>
    dplyr::transmute(
      section = dplyr::if_else(
        within_500ft,
        "within_500ft",
        "outside_500ft_within_1500ft"
      ),
      metric = component_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "connected_components",
      "component_nodes",
      "component_edges",
      "automatic_resolution_eligible_components"
    ),
    value = c(
      nrow(component_summary),
      nrow(nodes),
      nrow(edge_components),
      sum(component_summary$automatic_resolution_eligible)
    )
  )
)

readr::write_csv(
  component_summary,
  "../output/multicard_episode_component_summary.csv"
)
readr::write_csv(
  nodes,
  "../output/multicard_episode_component_nodes.csv"
)
readr::write_csv(
  edge_components,
  "../output/multicard_episode_component_edges.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_episode_component_counts.csv"
)
