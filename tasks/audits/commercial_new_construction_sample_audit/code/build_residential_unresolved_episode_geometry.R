# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

requests <- readr::read_csv(
  "../output/residential_unresolved_episode_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

coverage <- readr::read_csv(
  "../output/residential_unresolved_historical_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (!identical(
  requests %>% select(episode_id, component_pin) %>% arrange(episode_id, component_pin),
  coverage %>% select(episode_id, component_pin) %>% arrange(episode_id, component_pin)
)) {
  stop("Episode request and parcel coverage keys do not agree.", call. = FALSE)
}

parcels <- sf::st_read(
  "../output/residential_unresolved_historical_parcels.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(parcels)[names(parcels) == attr(parcels, "sf_column")] <- "geometry"
sf::st_geometry(parcels) <- "geometry"

parcels_by_pin <- parcels %>%
  group_by(target_year, pin10, pin14) %>%
  summarise(
    object_ids = paste(sort(unique(object_id)), collapse = "/"),
    .groups = "drop"
  )

exact_components <- parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "exact_pin14"),
    by = c("target_year", "pin10", "pin14" = "component_pin"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    episode_id,
    project_id,
    target_year,
    request_source,
    component_pin = pin14,
    match_method = "exact_construction_year_pin14",
    parcel_pin14 = pin14,
    parcel_pin10 = pin10,
    object_ids,
    geometry
  )

unique_pin10_components <- parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "unique_pin10_only"),
    by = c("target_year", "pin10"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    episode_id,
    project_id,
    target_year,
    request_source,
    component_pin,
    match_method = "unique_construction_year_pin10",
    parcel_pin14 = pin14,
    parcel_pin10 = pin10,
    object_ids,
    geometry
  )

component_geometry <- rbind(exact_components, unique_pin10_components) %>%
  arrange(project_id, target_year, component_pin)

if (anyDuplicated(component_geometry[c("episode_id", "component_pin")]) > 0) {
  stop("A residential episode component resolves to multiple accepted polygons.", call. = FALSE)
}

episode_geometry <- component_geometry %>%
  group_by(episode_id, project_id, target_year, request_source) %>%
  summarise(
    resolved_components = n_distinct(component_pin),
    resolved_component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    source_parcel_pins = paste(sort(unique(parcel_pin14)), collapse = "/"),
    match_methods = paste(sort(unique(match_method)), collapse = "/"),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(
    episode_polygon_valid = sf::st_is_valid(geometry),
    episode_land_area_sqft = as.numeric(sf::st_area(geometry))
  )

if (any(!episode_geometry$episode_polygon_valid) || any(sf::st_is_empty(episode_geometry))) {
  stop("Residential unresolved episode geometries must be valid and nonempty.", call. = FALSE)
}

episode_coverage <- coverage %>%
  group_by(episode_id, project_id, target_year, request_source) %>%
  summarise(
    requested_components = n_distinct(component_pin),
    exact_components = sum(coverage_status == "exact_pin14"),
    unique_pin10_components = sum(coverage_status == "unique_pin10_only"),
    ambiguous_components = sum(coverage_status == "ambiguous_pin10"),
    missing_components = sum(coverage_status == "missing_pin10"),
    .groups = "drop"
  ) %>%
  left_join(
    episode_geometry %>%
      sf::st_drop_geometry() %>%
      select(episode_id, resolved_components, episode_land_area_sqft),
    by = "episode_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    resolved_components = coalesce(resolved_components, 0L),
    episode_polygon_available = resolved_components > 0
  ) %>%
  arrange(project_id, target_year)

if (nrow(episode_coverage) != n_distinct(requests$episode_id)) {
  stop("Episode geometry coverage lost a requested project-year episode.", call. = FALSE)
}

episode_boxes <- episode_geometry %>%
  mutate(bounds = purrr::map(geometry, sf::st_bbox)) %>%
  sf::st_drop_geometry() %>%
  transmute(
    episode_id,
    xmin = purrr::map_dbl(bounds, "xmin"),
    ymin = purrr::map_dbl(bounds, "ymin"),
    xmax = purrr::map_dbl(bounds, "xmax"),
    ymax = purrr::map_dbl(bounds, "ymax")
  )

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
duckdb::duckdb_register(connection, "episode_boxes", episode_boxes)

current_candidates <- DBI::dbGetQuery(
  connection,
  paste0(
    "WITH current_parcels AS (",
    "SELECT trim(pin) AS pin, trim(pin10) AS pin10, trim(class) AS class, ",
    "try_cast(centroid_x_crs_3435 AS DOUBLE) AS x_3435, ",
    "try_cast(centroid_y_crs_3435 AS DOUBLE) AS y_3435 ",
    "FROM read_csv('../input/parcel_universe_2025_city.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) ",
    "WHERE try_cast(centroid_x_crs_3435 AS DOUBLE) IS NOT NULL ",
    "AND try_cast(centroid_y_crs_3435 AS DOUBLE) IS NOT NULL",
    ") ",
    "SELECT b.episode_id, p.pin, p.pin10, p.class, p.x_3435, p.y_3435 ",
    "FROM episode_boxes b JOIN current_parcels p ",
    "ON p.x_3435 BETWEEN b.xmin - 25 AND b.xmax + 25 ",
    "AND p.y_3435 BETWEEN b.ymin - 25 AND b.ymax + 25"
  )
) %>%
  as_tibble()

current_points <- sf::st_as_sf(
  current_candidates,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

episode_geometry_for_join <- episode_geometry %>%
  select(episode_id, project_id, target_year, geometry)

candidate_pairs <- current_points %>%
  left_join(
    episode_geometry_for_join %>% sf::st_drop_geometry(),
    by = "episode_id",
    relationship = "many-to-one"
  )

episode_rows <- match(candidate_pairs$episode_id, episode_geometry_for_join$episode_id)
candidate_pairs$inside_episode_polygon <- vapply(
  seq_len(nrow(candidate_pairs)),
  function(i) {
    lengths(
      sf::st_intersects(
        candidate_pairs[i, ],
        episode_geometry_for_join[episode_rows[i], ],
        sparse = TRUE
      )
    ) > 0
  },
  logical(1)
)
candidate_pairs$distance_to_episode_polygon_ft <- as.numeric(
  sf::st_distance(
    candidate_pairs,
    episode_geometry_for_join[episode_rows, ],
    by_element = TRUE
  )
)

successor_evidence <- readr::read_csv(
  "../output/residential_unresolved_successor_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  select(
    project_id,
    pin,
    exact_address_match,
    accepted_point_match,
    minimum_point_distance_ft,
    current_parcel_addresses,
    candidate_methods
  )

candidate_pairs <- candidate_pairs %>%
  sf::st_drop_geometry() %>%
  left_join(
    successor_evidence,
    by = c("project_id", "pin"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    exact_address_match = coalesce(exact_address_match, FALSE),
    accepted_point_match = coalesce(accepted_point_match, FALSE)
  ) %>%
  filter(inside_episode_polygon | distance_to_episode_polygon_ft <= 25) %>%
  arrange(project_id, target_year, desc(inside_episode_polygon), distance_to_episode_polygon_ft, pin)

if (anyDuplicated(candidate_pairs[c("episode_id", "pin")]) > 0) {
  stop("Episode-successor candidate links are not unique.", call. = FALSE)
}

candidate_point_geometry <- sf::st_as_sf(
  candidate_pairs,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

candidate_rows_by_episode <- candidate_point_geometry %>%
  sf::st_drop_geometry() %>%
  select(episode_id, pin, pin10, class, x_3435, y_3435) %>%
  group_by(episode_id) %>%
  summarise(candidate_rows = list(pick(everything())), .groups = "drop")

component_candidate_pairs <- component_geometry %>%
  select(episode_id, project_id, target_year, component_pin, geometry) %>%
  left_join(
    candidate_rows_by_episode,
    by = "episode_id",
    relationship = "many-to-one"
  ) %>%
  tidyr::unnest(candidate_rows)

component_points <- sf::st_as_sf(
  component_candidate_pairs %>% sf::st_drop_geometry(),
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

component_candidate_pairs$inside_component_polygon <- vapply(
  seq_len(nrow(component_candidate_pairs)),
  function(i) {
    lengths(
      sf::st_intersects(
        component_points[i, ],
        component_candidate_pairs[i, ],
        sparse = TRUE
      )
    ) > 0
  },
  logical(1)
)
component_candidate_pairs$distance_to_component_polygon_ft <- as.numeric(
  sf::st_distance(
    component_points,
    component_candidate_pairs,
    by_element = TRUE
  )
)

component_successor_links <- component_candidate_pairs %>%
  sf::st_drop_geometry() %>%
  filter(inside_component_polygon | distance_to_component_polygon_ft <= 25) %>%
  select(
    episode_id,
    project_id,
    target_year,
    component_pin,
    pin,
    pin10,
    class,
    x_3435,
    y_3435,
    inside_component_polygon,
    distance_to_component_polygon_ft
  ) %>%
  arrange(
    project_id,
    target_year,
    component_pin,
    desc(inside_component_polygon),
    distance_to_component_polygon_ft,
    pin
  )

if (anyDuplicated(component_successor_links[c("episode_id", "component_pin", "pin")]) > 0) {
  stop("Component-successor links are not unique.", call. = FALSE)
}

successor_summary <- episode_coverage %>%
  left_join(
    candidate_pairs %>%
      group_by(episode_id) %>%
      summarise(
        current_pins_inside = n_distinct(pin[inside_episode_polygon]),
        current_pins_within_25ft = n_distinct(pin),
        exact_address_pins_inside = n_distinct(pin[inside_episode_polygon & exact_address_match]),
        current_pin10_inside = n_distinct(pin10[inside_episode_polygon]),
        current_classes_inside = paste(sort(unique(class[inside_episode_polygon])), collapse = "/"),
        .groups = "drop"
      ),
    by = "episode_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(current_pins_inside, current_pins_within_25ft, exact_address_pins_inside, current_pin10_inside),
      ~ coalesce(.x, 0L)
    )
  ) %>%
  arrange(project_id, target_year)

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "requested_episodes",
      "episodes_with_polygon",
      "episodes_with_current_pin_inside",
      "episode_current_pin_links_inside",
      "episode_current_pin_links_within_25ft",
      "component_current_pin_links_inside"
    ),
    value = c(
      nrow(episode_coverage),
      sum(episode_coverage$episode_polygon_available),
      sum(successor_summary$current_pins_inside > 0),
      sum(candidate_pairs$inside_episode_polygon),
      nrow(candidate_pairs),
      sum(component_successor_links$inside_component_polygon)
    )
  ),
  episode_coverage %>%
    count(episode_polygon_available, name = "value") %>%
    transmute(
      metric = if_else(
        episode_polygon_available,
        "episode_polygon_available",
        "episode_polygon_unresolved"
      ),
      value
    )
)

sf::st_write(
  component_geometry,
  "../output/residential_unresolved_episode_component_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  episode_geometry,
  "../output/residential_unresolved_episode_geometry.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  episode_coverage,
  "../output/residential_unresolved_episode_geometry_coverage.csv"
)
readr::write_csv(
  candidate_pairs,
  "../output/residential_unresolved_episode_successor_links.csv"
)
readr::write_csv(
  successor_summary,
  "../output/residential_unresolved_episode_successor_summary.csv"
)
readr::write_csv(
  component_successor_links,
  "../output/residential_unresolved_component_successor_links.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_episode_geometry_summary.csv"
)
