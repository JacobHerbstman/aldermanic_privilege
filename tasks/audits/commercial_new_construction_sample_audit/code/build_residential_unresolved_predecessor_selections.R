# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- sf::st_read(
  "../output/residential_unresolved_predecessor_candidates.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

selections <- readr::read_csv(
  "../adjudication/residential_unresolved_predecessor_selections.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    episode_id = readr::col_character(),
    object_id = readr::col_integer(),
    .default = readr::col_guess()
  )
)

candidate_keys <- candidates %>%
  sf::st_drop_geometry() %>%
  select(episode_id, object_id) %>%
  arrange(episode_id, object_id)
selection_keys <- selections %>%
  select(episode_id, object_id) %>%
  arrange(episode_id, object_id)

if (nrow(candidate_keys) != nrow(selection_keys) ||
    nrow(anti_join(candidate_keys, selection_keys, by = c("episode_id", "object_id"))) > 0 ||
    nrow(anti_join(selection_keys, candidate_keys, by = c("episode_id", "object_id"))) > 0) {
  stop("Predecessor selections must disposition every candidate exactly once.", call. = FALSE)
}
if (anyDuplicated(selections[c("episode_id", "object_id")]) > 0) {
  stop("Predecessor selection keys are not unique.", call. = FALSE)
}
if (any(is.na(selections$selection_reason) | selections$selection_reason == "")) {
  stop("Every predecessor decision requires a reason.", call. = FALSE)
}

review <- candidates %>%
  left_join(
    selections,
    by = c("episode_id", "object_id"),
    relationship = "one-to-one"
  )
review$parcel_area_sqft <- as.numeric(sf::st_area(review))
review <- review %>%
  arrange(project_id, target_year, desc(accept_predecessor), predecessor_pin14)

accepted <- review %>%
  filter(accept_predecessor)

if (anyDuplicated(accepted[c("episode_id", "object_id")]) > 0 ||
    any(!sf::st_is_valid(accepted)) ||
    any(sf::st_is_empty(accepted))) {
  stop("Accepted predecessor polygons violate their geometry contract.", call. = FALSE)
}

episode_summary <- review %>%
  sf::st_drop_geometry() %>%
  group_by(episode_id, project_id, target_year) %>%
  summarise(
    candidate_parcels = n(),
    accepted_parcels = sum(accept_predecessor),
    accepted_pin14s = paste(sort(predecessor_pin14[accept_predecessor]), collapse = "/"),
    fallback_geometry = paste(sort(unique(fallback_geometry[!accept_predecessor])), collapse = "/"),
    .groups = "drop"
  ) %>%
  arrange(project_id, target_year)

summary <- tibble::tibble(
  metric = c(
    "candidate_parcels",
    "accepted_parcels",
    "rejected_parcels",
    "episodes_with_accepted_predecessor",
    "episodes_using_other_geometry"
  ),
  value = c(
    nrow(review),
    nrow(accepted),
    sum(!review$accept_predecessor),
    n_distinct(accepted$episode_id),
    sum(episode_summary$accepted_parcels == 0)
  )
)

sf::st_write(
  accepted,
  "../output/residential_unresolved_predecessor_selected.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  sf::st_drop_geometry(review),
  "../output/residential_unresolved_predecessor_selection_review.csv"
)
readr::write_csv(
  episode_summary,
  "../output/residential_unresolved_predecessor_selection_episodes.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_predecessor_selection_summary.csv"
)
