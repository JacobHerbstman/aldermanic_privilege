# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

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
  dplyr::distinct(project_id, successor_id, .keep_all = TRUE)

project_ids <- intersect(
  unique(cards$project_id),
  unique(successors$project_id)
)

matches <- vector("list", length(project_ids))

for (i in seq_along(project_ids)) {
  project_cards <- cards |>
    dplyr::filter(project_id == project_ids[[i]])
  project_successors <- successors |>
    dplyr::filter(project_id == project_ids[[i]])

  candidate_edges <- tidyr::crossing(
    card_id = project_cards$card_id,
    successor_id = project_successors$successor_id
  ) |>
    dplyr::left_join(
      project_cards,
      by = "card_id",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      project_successors,
      by = c("project_id", "successor_id"),
      relationship = "many-to-one",
      suffix = c("_card", "_successor")
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
    )

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
    project_id = project_ids[[i]],
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
    by = c("project_id", "card_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    successors,
    by = c("project_id", "successor_id"),
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
  dplyr::arrange(project_id, card_num, successor_id)

project_summary <- cards |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    cards = dplyr::n(),
    card_units = sum(card_units),
    card_building_sqft = sum(card_building_sqft),
    .groups = "drop"
  ) |>
  dplyr::inner_join(
    successors |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        successor_candidates = dplyr::n(),
        successor_units = sum(successor_units),
        successor_building_sqft = sum(
          successor_building_sqft
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    matched_pairs |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        matched_cards = dplyr::n(),
        matched_card_units = sum(card_units),
        matched_card_building_sqft = sum(
          card_building_sqft
        ),
        matched_successor_units = sum(successor_units),
        matched_successor_building_sqft = sum(
          successor_building_sqft
        ),
        matched_successor_ids = paste(
          sort(unique(successor_id)),
          collapse = "/"
        ),
        matched_successor_project_ids = paste(
          sort(unique(
            represented_project_ids[
              !is.na(represented_project_ids)
            ]
          )),
          collapse = "/"
        ),
        all_matches_within_5pct =
          all(abs(building_ratio - 1) <= 0.05),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    matched_cards = dplyr::coalesce(matched_cards, 0L),
    complete_card_matching = matched_cards == cards,
    exact_unit_reproduction =
      matched_successor_units == card_units
  ) |>
  dplyr::arrange(
    dplyr::desc(complete_card_matching),
    project_id
  )

summary <- dplyr::bind_rows(
  project_summary |>
    dplyr::mutate(
      all_matches_within_5pct = dplyr::coalesce(
        all_matches_within_5pct,
        FALSE
      )
    ) |>
    dplyr::count(
      complete_card_matching,
      all_matches_within_5pct,
      name = "value"
    ) |>
    dplyr::transmute(
      section = "project_matching",
      metric = paste(
        "complete",
        complete_card_matching,
        "all_within_5pct",
        all_matches_within_5pct,
        sep = ":"
      ),
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "projects_with_cards_and_successors",
      "matched_card_successor_pairs"
    ),
    value = c(nrow(project_summary), nrow(matched_pairs))
  )
)

readr::write_csv(
  matched_pairs,
  "../output/multicard_card_successor_matches.csv"
)
readr::write_csv(
  project_summary,
  "../output/multicard_card_successor_match_projects.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_card_successor_match_summary.csv"
)
