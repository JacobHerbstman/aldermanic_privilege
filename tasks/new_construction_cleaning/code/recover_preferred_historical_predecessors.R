# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
reference_points <- readr::read_csv(
  "../output/preferred_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    permit_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(reference_points$request_id) > 0) {
  stop("Preferred predecessor request IDs are not unique.", call. = FALSE)
}
if (!all(reference_points$target_year %in% 2006:2022)) {
  stop("One or more predecessor requests use an unsupported parcel year.", call. = FALSE)
}

available_points <- reference_points %>%
  filter(
    reference_status == "reference_point_available",
    is.finite(reference_x_3435),
    is.finite(reference_y_3435)
  ) %>%
  sf::st_as_sf(
    coords = c("reference_x_3435", "reference_y_3435"),
    crs = 3435,
    remove = FALSE
  )

source_queries <- readr::read_csv(
  "../input/preferred_predecessor_source_queries.csv",
  col_types = readr::cols(target_year = readr::col_integer(),
    reference_x_3435 = readr::col_double(), reference_y_3435 = readr::col_double())
)
if (anyDuplicated(source_queries) ||
    nrow(anti_join(sf::st_drop_geometry(available_points), source_queries,
      by = c("target_year", "reference_x_3435", "reference_y_3435"))) > 0) {
  stop("Predecessor reference points exceed the pinned source query coverage; acquire and pin the missing source queries.")
}
source_parcels <- sf::st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE, promote_to_multi = FALSE) %>%
  sf::st_transform(3435)
names(source_parcels)[names(source_parcels) == attr(source_parcels, "sf_column")] <- "geometry"
sf::st_geometry(source_parcels) <- "geometry"
if (anyDuplicated(sf::st_drop_geometry(source_parcels)[c("target_year", "object_id")]) ||
    any(!sf::st_is_valid(source_parcels)) || any(sf::st_is_empty(source_parcels))) {
  stop("Pinned predecessor source identifiers or geometries are invalid.")
}

predecessor_parcels <- list()
predecessor_matches <- list()
for (year_value in sort(unique(available_points$target_year))) {
  year_points <- available_points %>% filter(target_year == year_value)
  year_parcels <- source_parcels %>% filter(target_year == year_value)
  containing_polygons <- sf::st_within(year_points, year_parcels)
  year_matches <- purrr::map2_dfr(
    seq_len(nrow(year_points)),
    containing_polygons,
    function(point_row, parcel_rows) {
      request <- sf::st_drop_geometry(year_points[point_row, ])
      if (length(parcel_rows) == 0) {
        return(request %>% transmute(request_id, predecessor_polygon_count = 0L))
      }
      bind_cols(
        request[rep(1, length(parcel_rows)), ] %>% select(request_id),
        sf::st_drop_geometry(year_parcels[parcel_rows, ]) %>%
          select(object_id, predecessor_pin14, predecessor_pin10),
        tibble::tibble(predecessor_polygon_count = length(parcel_rows))
      )
    }
  )

  predecessor_parcels[[as.character(year_value)]] <- year_parcels
  predecessor_matches[[as.character(year_value)]] <- year_matches
}

predecessor_parcels <- do.call(rbind, predecessor_parcels) %>%
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
predecessor_matches <- bind_rows(predecessor_matches)

predecessor_resolution <- reference_points %>%
  left_join(
    predecessor_matches,
    by = "request_id",
    relationship = "one-to-many"
  ) %>%
  mutate(
    predecessor_polygon_count = coalesce(predecessor_polygon_count, 0L),
    predecessor_status = case_when(
      reference_status == "reference_point_unresolved" ~ "no_reference_point",
      predecessor_polygon_count == 1 ~ "unique_predecessor_polygon",
      predecessor_polygon_count > 1 ~ "multiple_predecessor_polygons",
      TRUE ~ "no_predecessor_polygon"
    )
  ) %>%
  arrange(target_year, source_family, project_id, component_pin, object_id)

if (nrow(predecessor_resolution %>% distinct(request_id)) != nrow(reference_points)) {
  stop("One or more predecessor requests disappeared during spatial matching.", call. = FALSE)
}

# When the first location fails, try the same PIN's coordinate one year later.
# The parcel geometry still comes from the construction-year map.
predecessor_resolution <- predecessor_resolution %>% mutate(
  first_attempt_status = predecessor_status,
  first_reference_source = reference_source,
  first_reference_x_3435 = reference_x_3435,
  first_reference_y_3435 = reference_y_3435,
  history_coordinate_year = NA_integer_, history_coordinate_row_id = NA_character_)
history <- readr::read_csv("../input/geocoding_parcel_history.csv",
  col_types = readr::cols(pin = readr::col_character(), row_id = readr::col_character(),
    .default = readr::col_guess())) %>% filter(is.finite(lon), is.finite(lat))
earlier_history <- readr::read_csv("../input/predecessor_parcel_history.csv",
  col_types = readr::cols(pin = readr::col_character(), row_id = readr::col_character(),
    .default = readr::col_guess())) %>% filter(is.finite(lon), is.finite(lat))
overlap_history <- inner_join(history %>% select(pin, year, lon, lat, row_id),
  earlier_history %>% select(pin, year, lon, lat, row_id),
  by = c("pin", "year"), relationship = "one-to-one")
stopifnot(all(overlap_history$lon.x == overlap_history$lon.y),
  all(overlap_history$lat.x == overlap_history$lat.y),
  all(overlap_history$row_id.x == overlap_history$row_id.y))
history <- bind_rows(history, anti_join(earlier_history, history, by = c("pin", "year")))
stopifnot(!anyDuplicated(history[c("pin", "year")]))
history_points <- sf::st_as_sf(history, coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(3435)
fallback <- predecessor_resolution %>%
  filter(predecessor_status %in% c("no_reference_point", "no_predecessor_polygon")) %>%
  select(request_id, component_pin, target_year) %>%
  inner_join(history %>% transmute(component_pin = pin, target_year = year - 1L,
    coordinate_year = year, coordinate_row_id = row_id,
    x = sf::st_coordinates(history_points)[, 1], y = sf::st_coordinates(history_points)[, 2]),
    by = c("component_pin", "target_year"), relationship = "many-to-one")
fallback_queries <- readr::read_csv("../input/history_reference_queries.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(fallback$request_id), !anyDuplicated(fallback_queries))
fallback_parcels <- sf::st_read("../input/history_reference_parcels.gpkg", quiet = TRUE)
names(fallback_parcels)[names(fallback_parcels) == attr(fallback_parcels, "sf_column")] <- "geometry"
sf::st_geometry(fallback_parcels) <- "geometry"
fallback_parcels <- fallback_parcels %>% select(all_of(names(predecessor_parcels)))
stopifnot(sf::st_crs(fallback_parcels)$epsg == 3435,
  !anyDuplicated(sf::st_drop_geometry(fallback_parcels)[c("target_year", "object_id")]),
  all(sf::st_is_valid(fallback_parcels)), !any(sf::st_is_empty(fallback_parcels)))
fallback_rows <- list()
for (i in seq_len(nrow(fallback))) {
  # CSV round trips can change the last binary digit; this is not a spatial buffer.
  stopifnot(any(fallback_queries$target_year == fallback$target_year[i] &
    abs(fallback_queries$reference_x_3435 - fallback$x[i]) < 1e-6 &
    abs(fallback_queries$reference_y_3435 - fallback$y[i]) < 1e-6))
  point <- sf::st_as_sf(fallback[i, ], coords = c("x", "y"), crs = 3435)
  parcels <- fallback_parcels %>% filter(target_year == fallback$target_year[i])
  hits <- sf::st_within(point, parcels)[[1]]
  row <- predecessor_resolution %>% filter(request_id == fallback$request_id[i])
  stopifnot(nrow(row) == 1L)
  row <- row[rep(1L, max(1L, length(hits))), ]
  row$reference_source <- "exact_pin_coordinate_one_year_after_construction"
  row$reference_status <- "reference_point_available"
  row$reference_x_3435 <- fallback$x[i]
  row$reference_y_3435 <- fallback$y[i]
  row$history_coordinate_year <- fallback$coordinate_year[i]
  row$history_coordinate_row_id <- fallback$coordinate_row_id[i]
  row$object_id <- if (length(hits)) parcels$object_id[hits] else NA_integer_
  row$predecessor_pin14 <- if (length(hits)) parcels$predecessor_pin14[hits] else NA_character_
  row$predecessor_pin10 <- if (length(hits)) parcels$predecessor_pin10[hits] else NA_character_
  row$predecessor_polygon_count <- length(hits)
  row$predecessor_status <- if (length(hits) == 1L) "unique_predecessor_polygon" else
    if (length(hits) > 1L) "multiple_predecessor_polygons" else "no_predecessor_polygon"
  fallback_rows[[i]] <- row
  index <- match(fallback$request_id[i], reference_points$request_id)
  reference_points$reference_source[index] <- row$reference_source[1]
}
predecessor_resolution <- bind_rows(
  anti_join(predecessor_resolution, fallback, by = "request_id"), bind_rows(fallback_rows))

# Preserve the pinned original geometry when both downloads contain an object.
overlap <- match(paste(fallback_parcels$target_year, fallback_parcels$object_id),
  paste(predecessor_parcels$target_year, predecessor_parcels$object_id))
for (i in which(!is.na(overlap))) {
  stopifnot(length(sf::st_equals(fallback_parcels[i, ], predecessor_parcels[overlap[i], ])[[1]]) == 1L)
}
predecessor_parcels <- rbind(predecessor_parcels, fallback_parcels[is.na(overlap), ])

candidate_geometry <- predecessor_parcels %>%
  inner_join(
    predecessor_resolution %>%
      filter(predecessor_polygon_count > 0) %>%
      select(request_id, target_year, object_id),
    by = c("target_year", "object_id"),
    relationship = "one-to-many"
  )

geometry_equivalence <- bind_rows(lapply(
  split(seq_len(nrow(candidate_geometry)), candidate_geometry$request_id),
  function(rows) {
    group_geometry <- candidate_geometry[rows, ]
    tibble::tibble(
      request_id = group_geometry$request_id[1],
      all_predecessor_geometries_equivalent = all(
        lengths(sf::st_equals(group_geometry)) == nrow(group_geometry)
      )
    )
  }
))

predecessor_resolution <- predecessor_resolution %>%
  left_join(
    geometry_equivalence,
    by = "request_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    predecessor_status = case_when(
      predecessor_status == "multiple_predecessor_polygons" &
        all_predecessor_geometries_equivalent ~ "equivalent_predecessor_geometry",
      TRUE ~ predecessor_status
    )
  )

accepted_requests <- predecessor_resolution %>%
  distinct(request_id, predecessor_status) %>%
  filter(predecessor_status %in% c(
    "unique_predecessor_polygon",
    "equivalent_predecessor_geometry"
  ))

selected_predecessors <- candidate_geometry %>%
  inner_join(
    accepted_requests,
    by = "request_id",
    relationship = "many-to-one"
  ) %>%
  group_by(request_id, target_year, predecessor_status) %>%
  summarise(
    predecessor_object_ids = paste(sort(unique(object_id)), collapse = "/"),
    predecessor_pin14s = paste(sort(unique(predecessor_pin14)), collapse = "/"),
    predecessor_pin10s = paste(sort(unique(predecessor_pin10)), collapse = "/"),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  left_join(
    reference_points %>%
      select(
        request_id,
        source_family,
        project_id,
        project_kind,
        candidate_status,
        component_pin,
        pin10,
        target_year,
        reference_source
      ),
    by = c("request_id", "target_year"),
    relationship = "one-to-one"
  ) %>%
  select(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin,
    pin10,
    target_year,
    reference_source,
    predecessor_status,
    predecessor_object_ids,
    predecessor_pin14s,
    predecessor_pin10s,
    geometry
  )

if (anyDuplicated(selected_predecessors$request_id) > 0) {
  stop("Selected predecessor geometries are not unique by request ID.", call. = FALSE)
}
if (any(!sf::st_is_valid(selected_predecessors)) || any(sf::st_is_empty(selected_predecessors))) {
  stop("Selected predecessor geometries must be valid and nonempty.", call. = FALSE)
}

SaveData(selected_predecessors, c("request_id"), "../output/preferred_historical_predecessor_selected.gpkg", delete_dsn = TRUE, quiet = TRUE)
SaveData(arrange(predecessor_resolution, target_year, source_family, project_id, component_pin, object_id), character(), "../output/preferred_historical_predecessor_resolution.csv")
