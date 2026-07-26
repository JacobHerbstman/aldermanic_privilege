# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

episode_inventory <- readr::read_csv(
  "../output/residential_unresolved_episode_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    episode_id = readr::col_character(),
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

direct_components <- sf::st_read(
  "../output/residential_unresolved_episode_component_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(direct_components)[names(direct_components) == attr(direct_components, "sf_column")] <- "geometry"
sf::st_geometry(direct_components) <- "geometry"
direct_components <- direct_components %>%
  transmute(
    episode_id,
    project_id,
    target_year,
    geometry_piece = paste0("direct_component:", component_pin),
    geometry
  )

selected_predecessors <- sf::st_read(
  "../output/residential_unresolved_predecessor_selected.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(selected_predecessors)[names(selected_predecessors) == attr(selected_predecessors, "sf_column")] <- "geometry"
sf::st_geometry(selected_predecessors) <- "geometry"
selected_predecessors <- selected_predecessors %>%
  transmute(
    episode_id,
    project_id,
    target_year,
    geometry_piece = paste0("selected_predecessor:", predecessor_pin14),
    geometry
  )

geometry_pieces <- rbind(direct_components, selected_predecessors) %>%
  arrange(episode_id, geometry_piece)

if (anyDuplicated(geometry_pieces[c("episode_id", "geometry_piece")]) > 0) {
  stop("Accepted episode geometry pieces are not unique.", call. = FALSE)
}

accepted_geometry <- geometry_pieces %>%
  group_by(episode_id, project_id, target_year) %>%
  summarise(
    geometry_pieces = paste(sort(unique(geometry_piece)), collapse = "/"),
    geometry_piece_count = n(),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  )
accepted_geometry$geometry_area_sqft <- as.numeric(sf::st_area(accepted_geometry))
accepted_geometry$geometry_valid <- sf::st_is_valid(accepted_geometry)

if (any(!accepted_geometry$geometry_valid) || any(sf::st_is_empty(accepted_geometry))) {
  stop("Accepted episode geometries must be valid and nonempty.", call. = FALSE)
}

coverage <- episode_inventory %>%
  select(episode_id, project_id, target_year, request_source) %>%
  left_join(
    accepted_geometry %>%
      sf::st_drop_geometry() %>%
      select(episode_id, geometry_piece_count, geometry_pieces, geometry_area_sqft),
    by = "episode_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    geometry_piece_count = coalesce(geometry_piece_count, 0L),
    accepted_geometry_available = geometry_piece_count > 0
  ) %>%
  arrange(project_id, target_year)

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "requested_episodes",
      "episodes_with_accepted_geometry",
      "episodes_without_accepted_geometry",
      "direct_component_pieces",
      "selected_predecessor_pieces"
    ),
    value = c(
      nrow(episode_inventory),
      nrow(accepted_geometry),
      sum(!coverage$accepted_geometry_available),
      nrow(direct_components),
      nrow(selected_predecessors)
    )
  ),
  coverage %>%
    filter(!accepted_geometry_available) %>%
    count(project_id, name = "value") %>%
    transmute(metric = paste0("unresolved_project:", project_id), value)
)

sf::st_write(
  accepted_geometry,
  "../output/residential_unresolved_accepted_episode_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  coverage,
  "../output/residential_unresolved_accepted_episode_geometry_coverage.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_accepted_episode_geometry_summary.csv"
)
