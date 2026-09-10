# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

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

stopifnot(!anyNA(selections$accept_predecessor), is.logical(selections$accept_predecessor))

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

sf::st_write(accepted, "../output/residential_unresolved_predecessor_selected.gpkg", delete_dsn = TRUE, quiet = TRUE)
