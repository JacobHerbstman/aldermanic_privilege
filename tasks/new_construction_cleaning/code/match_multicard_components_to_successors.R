# setwd("tasks/new_construction_cleaning/code")
# automatic_building_gap <- 0.02

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(automatic_building_gap)
if (length(args) != 1L) stop("Expected the automatic card-match building-area tolerance.")
automatic_building_gap <- suppressWarnings(as.numeric(args[1]))
if (!is.finite(automatic_building_gap) || automatic_building_gap < 0 || automatic_building_gap > 0.35) {
  stop("The automatic card-match tolerance must be between zero and 0.35.")
}

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
  )

if (anyDuplicated(cards$card_id) ||
    anyDuplicated(successors[c("component_id", "project_id", "successor_id")])) {
  stop("Card IDs and parent-specific successor candidates must be unique.")
}

component_ids <- intersect(
  unique(cards$component_id),
  unique(successors$component_id)
)

matches <- vector("list", length(component_ids))
close_candidates <- vector("list", length(component_ids))

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

  close_candidates[[i]] <- candidate_edges |>
    dplyr::filter(abs(building_ratio - 1) <= automatic_building_gap) |>
    dplyr::distinct(project_id, successor_id)

  graph_edges <- candidate_edges |>
    dplyr::transmute(
      from = paste0("card:", card_id),
      to = paste0("successor:", successor_id),
      weight = 100 - abs(log(building_ratio))
    )
  if (anyDuplicated(graph_edges[c("from", "to")])) {
    stop("A card-successor edge appears more than once.")
  }

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

matched_pairs <- dplyr::bind_rows(
  tibble::tibble(component_id = character(), card_id = character(), successor_id = character()),
  matches
) |>
  dplyr::left_join(
    cards,
    by = c("component_id", "card_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    # Candidate dates and gaps belong to the matched parent, even when the
    # same successor was considered for several parents in the component.
    successors,
    by = c("component_id", "project_id", "successor_id"),
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

if (anyDuplicated(matched_pairs$card_id) ||
    anyDuplicated(matched_pairs[c("component_id", "successor_id")]) ||
    anyNA(matched_pairs$successor_year)) {
  stop("Matched cards must have unique successors within each episode and valid candidate metadata.")
}

readr::write_csv(
  matched_pairs,
  "../output/multicard_component_successor_matches.csv"
)

# Accept a complete inventory automatically only when no extra close candidate
# competes for any card. Permutations among identical cards do not add successors.
match_summary <- component_parents |>
  dplyr::left_join(
    cards |> dplyr::count(project_id, name = "matchable_cards"),
    by = "project_id", relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    dplyr::bind_rows(
      tibble::tibble(project_id = character(), successor_id = character()),
      close_candidates
    ) |>
      dplyr::count(project_id, name = "close_candidate_successors"),
    by = "project_id", relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    matched_pairs |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        matched_cards = dplyr::n(),
        all_matches_close = all(abs(building_ratio - 1) <= automatic_building_gap),
        .groups = "drop"
      ),
    by = "project_id", relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    matched_cards = dplyr::coalesce(matched_cards, 0L),
    close_candidate_successors = dplyr::coalesce(close_candidate_successors, 0L),
    complete_unambiguous_successor_inventory = dplyr::coalesce(
      matchable_cards > 0 & matched_cards == matchable_cards &
        all_matches_close & close_candidate_successors == matchable_cards,
      FALSE
    )
  ) |>
  dplyr::arrange(project_id)
readr::write_csv(match_summary, "../output/multicard_successor_match_summary.csv")
