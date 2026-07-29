# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

component_parents <- readr::read_csv(
  "../output/multicard_episode_component_nodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(is_root_parent) |>
  dplyr::select(component_id, project_id)

cards <- readr::read_csv(
  "../output/multicard_card_snapshot.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    card_num = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    target_card,
    card_units > 0,
    building_sqft > 0
  ) |>
  dplyr::transmute(
    project_id = paste0("residential_multicard_", pin),
    card_id = paste0(pin, "_card_", card_num),
    card_num,
    card_units,
    card_building_sqft = building_sqft
  ) |>
  dplyr::inner_join(
    component_parents,
    by = "project_id",
    relationship = "many-to-one"
  )

successors <- readr::read_csv(
  "../output/multicard_successor_building_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    successor_id = readr::col_character(),
    successor_pin = readr::col_character(),
    represented_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    automatic_candidate,
    successor_units > 0,
    successor_building_sqft > 0
  ) |>
  dplyr::inner_join(
    component_parents,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::distinct(
    component_id,
    project_id,
    successor_id,
    .keep_all = TRUE
  )

component_ids <- intersect(
  unique(cards$component_id),
  unique(successors$component_id)
)

matches <- vector("list", length(component_ids))

for (i in seq_along(component_ids)) {
  component_cards <- cards |>
    dplyr::filter(component_id == component_ids[[i]])
  component_successors <- successors |>
    dplyr::filter(component_id == component_ids[[i]])

  parent_ids <- intersect(
    unique(component_cards$project_id),
    unique(component_successors$project_id)
  )
  candidate_edges <- dplyr::bind_rows(lapply(
    parent_ids,
    function(parent_id) {
      tidyr::crossing(
        card_id = component_cards$card_id[
          component_cards$project_id == parent_id
        ],
        successor_id = component_successors$successor_id[
          component_successors$project_id == parent_id
        ]
      ) |>
        dplyr::mutate(
          component_id = component_ids[[i]],
          project_id = parent_id
        )
    }
  )) |>
    dplyr::left_join(
      component_cards,
      by = c("component_id", "project_id", "card_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      component_successors,
      by = c("component_id", "project_id", "successor_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      building_ratio =
        successor_building_sqft / card_building_sqft,
      compatible =
        successor_units == card_units &
          dplyr::between(building_ratio, 0.75, 1.35)
    ) |>
    dplyr::filter(compatible)

  if (nrow(candidate_edges) == 0L) {
    next
  }

  graph_edges <- candidate_edges |>
    dplyr::transmute(
      from = paste0("card:", card_id),
      to = paste0("successor:", successor_id),
      weight = 100 - abs(log(building_ratio))
    ) |>
    dplyr::distinct(from, to, .keep_all = TRUE)

  graph <- igraph::graph_from_data_frame(
    graph_edges,
    directed = FALSE
  )
  vertex_types <- stringr::str_starts(
    igraph::V(graph)$name,
    "successor:"
  )
  matching <- igraph::max_bipartite_match(
    graph,
    types = vertex_types,
    weights = igraph::E(graph)$weight
  )$matching

  matched_cards <- names(matching)[
    stringr::str_starts(names(matching), "card:") &
      !is.na(matching)
  ]

  matches[[i]] <- tibble::tibble(
    component_id = component_ids[[i]],
    card_id = stringr::str_remove(matched_cards, "^card:"),
    successor_id = stringr::str_remove(
      unname(matching[matched_cards]),
      "^successor:"
    )
  )
}

matched_pairs <- dplyr::bind_rows(matches) |>
  dplyr::left_join(
    cards,
    by = c("component_id", "card_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    successors |>
      dplyr::select(-project_id) |>
      dplyr::distinct(component_id, successor_id, .keep_all = TRUE),
    by = c("component_id", "successor_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    building_ratio =
      successor_building_sqft / card_building_sqft,
    match_quality = dplyr::case_when(
      abs(building_ratio - 1) <= 0.02 ~ "within_2pct",
      abs(building_ratio - 1) <= 0.05 ~ "within_5pct",
      abs(building_ratio - 1) <= 0.10 ~ "within_10pct",
      TRUE ~ "within_35pct"
    )
  ) |>
  dplyr::arrange(component_id, project_id, card_num)

component_summary <- cards |>
  dplyr::group_by(component_id) |>
  dplyr::summarise(
    root_parent_projects = dplyr::n_distinct(project_id),
    root_cards = dplyr::n(),
    root_card_units = sum(card_units),
    root_card_building_sqft = sum(card_building_sqft),
    .groups = "drop"
  ) |>
  dplyr::inner_join(
    successors |>
      dplyr::group_by(component_id) |>
      dplyr::summarise(
        successor_candidates = dplyr::n_distinct(successor_id),
        successor_units = sum(
          successor_units[!duplicated(successor_id)]
        ),
        successor_building_sqft = sum(
          successor_building_sqft[!duplicated(successor_id)]
        ),
        .groups = "drop"
      ),
    by = "component_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    matched_pairs |>
      dplyr::group_by(component_id) |>
      dplyr::summarise(
        matched_cards = dplyr::n(),
        matched_card_units = sum(card_units),
        matched_card_building_sqft = sum(card_building_sqft),
        matched_successor_units = sum(successor_units),
        matched_successor_building_sqft =
          sum(successor_building_sqft),
        matched_successor_ids = paste(
          sort(unique(successor_id)),
          collapse = "/"
        ),
        matched_successor_project_ids = paste(
          sort(unique(
            unlist(stringr::str_split(
              stats::na.omit(represented_project_ids),
              "/"
            ))
          )),
          collapse = "/"
        ),
        all_matches_within_5pct =
          all(abs(building_ratio - 1) <= 0.05),
        .groups = "drop"
      ),
    by = "component_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    matched_cards = dplyr::coalesce(matched_cards, 0L),
    complete_card_matching = matched_cards == root_cards,
    exact_unit_reproduction =
      matched_successor_units == root_card_units
  ) |>
  dplyr::arrange(component_id)

summary <- tibble::tibble(
  metric = c(
    "components_with_cards_and_successors",
    "matched_card_successor_pairs",
    "successors_matched_more_than_once",
    "represented_projects_matched_more_than_once"
  ),
  value = c(
    nrow(component_summary),
    nrow(matched_pairs),
    sum(duplicated(matched_pairs[c("component_id", "successor_id")])),
    matched_pairs |>
      dplyr::filter(!is.na(represented_project_ids)) |>
      tidyr::separate_longer_delim(represented_project_ids, delim = "/") |>
      dplyr::count(component_id, represented_project_ids) |>
      dplyr::filter(n > 1) |>
      nrow()
  )
)

readr::write_csv(
  matched_pairs,
  "../output/multicard_component_successor_matches.csv"
)
readr::write_csv(
  component_summary,
  "../output/multicard_component_successor_match_components.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_component_successor_match_summary.csv"
)
