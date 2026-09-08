# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

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

sf::st_write(
  selected_predecessors,
  "../output/preferred_historical_predecessor_selected.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  predecessor_resolution,
  "../output/preferred_historical_predecessor_resolution.csv"
)
