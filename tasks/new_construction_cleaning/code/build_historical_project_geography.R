# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
# List the parcel-years needed for candidate projects.
components <- read_csv("../output/new_construction_project_components.csv",
  col_types = cols(source_family = col_character(), project_id = col_character(),
    component_pin = col_character(), pin10 = col_character(),
    candidate_year_min = col_double(), candidate_year_max = col_double(),
    within_1500ft = col_logical(), .default = col_skip())) |>
  filter(within_1500ft, is.finite(candidate_year_min), is.finite(candidate_year_max)) |>
  mutate(first_year = pmax(2006L, as.integer(candidate_year_min)),
         last_year = pmin(2022L, as.integer(candidate_year_max))) |>
  filter(first_year <= last_year)

# A candidate can still have several possible construction years at this stage.
requests <- components |>
  mutate(target_year = Map(seq.int, first_year, last_year)) |>
  unnest_longer(target_year) |>
  distinct(source_family, project_id, component_pin, pin10, target_year) |>
  arrange(target_year, pin10, source_family, project_id, component_pin)
stopifnot(nrow(requests) > 0L, !anyNA(requests),
          all(grepl("^[0-9]{14}$", requests$component_pin)),
          all(requests$pin10 == substr(requests$component_pin, 1, 10)),
          !anyDuplicated(requests[c("source_family", "project_id", "component_pin", "target_year")]))
SaveData(requests, c("source_family", "project_id", "component_pin", "target_year"), "../output/historical_project_parcel_requests.csv")

# Match the recorded parcel polygons.
requests <- read_csv("../output/historical_project_parcel_requests.csv",
  col_types = cols(target_year = col_integer(), .default = col_character()))
queries <- read_csv("../input/historical_project_parcel_source_queries.csv",
  col_types = cols(target_year = col_integer(), pin10 = col_character()))
stopifnot(!anyDuplicated(requests[c("source_family", "project_id", "component_pin", "target_year")]),
          !anyDuplicated(queries), !anyNA(queries))
unqueried <- requests |>
  distinct(target_year, pin10) |>
  anti_join(queries, by = c("target_year", "pin10"))
if (nrow(unqueried) > 0L) {
  stop(nrow(unqueried), " parcel-year queries are outside the pinned source; acquire and pin them before classifying coverage.")
}

parcels <- st_read("../input/historical_project_parcel_source.gpkg", quiet = TRUE)
stopifnot(st_crs(parcels)$epsg == 3435,
          !anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
parcels <- st_drop_geometry(parcels)
stopifnot(nrow(anti_join(parcels, queries, by = c("target_year", "pin10"))) == 0L)

counts <- count(parcels, target_year, pin10, name = "polygon_count_pin10")
exact <- parcels |>
  distinct(target_year, pin10, pin14) |>
  mutate(exact_pin14_available = TRUE)
coverage <- requests |>
  left_join(counts, by = c("target_year", "pin10"), relationship = "many-to-one") |>
  left_join(exact, by = c("target_year", "pin10", "component_pin" = "pin14"), relationship = "many-to-one") |>
  mutate(polygon_count_pin10 = coalesce(polygon_count_pin10, 0L),
         exact_pin14_available = coalesce(exact_pin14_available, FALSE),
         coverage_status = case_when(
           exact_pin14_available ~ "exact_pin14",
           polygon_count_pin10 == 1L ~ "unique_pin10_only",
           polygon_count_pin10 > 1L ~ "ambiguous_pin10",
           TRUE ~ "missing")) |>
  arrange(target_year, source_family, project_id, component_pin)
stopifnot(nrow(coverage) == nrow(requests))
SaveData(coverage, c("source_family", "project_id", "component_pin", "target_year"), "../output/historical_project_parcel_coverage.csv")

# Find recorded coordinates for missing parcels.
missing <- read_csv("../output/historical_project_parcel_coverage.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(), .default = col_guess())) |>
  filter(coverage_status == "missing") |>
  mutate(request_id = row_number(), .before = 1)
queries <- read_csv("../input/predecessor_history_queries.csv", col_types = cols(pin = col_character()))
history <- read_csv("../input/predecessor_parcel_history.csv",
  col_types = cols(pin = col_character(), pin10 = col_character(), row_id = col_character(), .default = col_double()))
stopifnot(!anyDuplicated(missing[c("source_family", "project_id", "component_pin", "target_year")]),
          !anyDuplicated(queries), !anyNA(queries),
          nrow(anti_join(missing, queries, by = c("component_pin" = "pin"))) == 0L,
          nrow(anti_join(history, queries, by = "pin")) == 0L,
          !anyNA(history[c("pin", "year", "row_id")]),
          !anyDuplicated(history[c("pin", "year")]), !anyDuplicated(history$row_id))
history <- filter(history, is.finite(x_3435), is.finite(y_3435))
history_rows <- split(seq_len(nrow(history)), history$pin)
selected_rows <- rep(NA_integer_, nrow(missing))
for (i in seq_len(nrow(missing))) {
  rows <- history_rows[[missing$component_pin[i]]]
  if (length(rows) == 0L) next
  years <- history$year[rows]
  # Closest assessment year; prefer the earlier year when equally close.
  selected_rows[i] <- rows[order(abs(years - missing$target_year[i]),
                                years > missing$target_year[i], -years)[1]]
}
selected <- history[selected_rows, ]
reference_points <- missing |>
  mutate(history_reference_year = selected$year,
    history_reference_year_gap = selected$year - target_year,
    history_row_id = selected$row_id,
    reference_source = if_else(is.na(selected_rows), "unresolved", "nearest_exact_pin_history"),
    reference_x_3435 = selected$x_3435, reference_y_3435 = selected$y_3435)
# Missing history remains explicit; no address or permit proxy is substituted.
SaveData(reference_points, c("source_family", "project_id", "component_pin", "target_year"), "../output/missing_project_reference_points.csv")

# Match those coordinates to the historical predecessor polygons.
references <- read_csv("../output/missing_project_reference_points.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(),
    history_row_id = col_character(), .default = col_guess())) |>
  select(request_id, source_family, project_id, component_pin, pin10, target_year,
    reference_source, reference_x_3435, reference_y_3435, history_reference_year,
    history_reference_year_gap, history_row_id)
queries <- read_csv("../input/historical_predecessor_source_queries.csv", show_col_types = FALSE)
available <- references |> filter(is.finite(reference_x_3435), is.finite(reference_y_3435))
stopifnot(!anyDuplicated(references$request_id), !anyDuplicated(queries), !anyNA(queries))
if (nrow(anti_join(available, queries, by = c("target_year", "reference_x_3435", "reference_y_3435"))) > 0L) {
  stop("Predecessor points exceed the pinned spatial query scope; acquire and pin the additional queries.")
}
points <- st_as_sf(available, coords = c("reference_x_3435", "reference_y_3435"), crs = 3435)
parcels <- st_read("../input/historical_predecessor_parcel_source.gpkg", quiet = TRUE)
stopifnot(st_crs(parcels)$epsg == 3435,
          !anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))

matches <- list()
for (year in sort(unique(points$target_year))) {
  year_points <- filter(points, target_year == year)
  year_parcels <- filter(parcels, target_year == year)
  # Preserve boundary intersections as candidates; never choose among multiple hits.
  hits <- st_intersects(year_points, year_parcels)
  matches[[as.character(year)]] <- tibble(request_id = year_points$request_id,
    predecessor_polygon_count = lengths(hits), parcel_index = as.list(hits)) |>
    unnest_longer(parcel_index, keep_empty = TRUE) |>
    mutate(object_id = year_parcels$object_id[parcel_index],
      predecessor_pin14 = year_parcels$predecessor_pin14[parcel_index],
      predecessor_pin10 = year_parcels$predecessor_pin10[parcel_index]) |>
    select(-parcel_index)
}
matches <- bind_rows(tibble(request_id = integer(), predecessor_polygon_count = integer(),
  object_id = integer(), predecessor_pin14 = character(), predecessor_pin10 = character()), bind_rows(matches))
resolution <- references |>
  left_join(matches, by = "request_id", relationship = "one-to-many") |>
  mutate(predecessor_polygon_count = coalesce(predecessor_polygon_count, 0L),
    predecessor_status = case_when(
      reference_source == "unresolved" ~ "no_reference_point",
      predecessor_polygon_count == 1L ~ "unique_predecessor_polygon",
      predecessor_polygon_count > 1L ~ "multiple_predecessor_polygons",
      TRUE ~ "no_predecessor_polygon")) |>
  select(all_of(names(references)), object_id, predecessor_pin14, predecessor_pin10,
    predecessor_polygon_count, predecessor_status) |>
  arrange(target_year, source_family, project_id, component_pin, object_id)
stopifnot(n_distinct(resolution$request_id) == nrow(references),
          !anyDuplicated(resolution[c("request_id", "object_id")]))
SaveData(resolution, character(), "../output/historical_project_predecessor_resolution.csv")

# Assemble the project geometry.
coverage <- readr::read_csv(
  "../output/historical_project_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

direct_parcels <- sf::st_read("../input/historical_project_parcel_source.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
names(direct_parcels)[names(direct_parcels) == attr(direct_parcels, "sf_column")] <- "geometry"
sf::st_geometry(direct_parcels) <- "geometry"

if (anyDuplicated(direct_parcels[c("target_year", "object_id")]) > 0) {
  stop("Direct historical parcels are not unique by layer year and object ID.", call. = FALSE)
}
if (nrow(
  direct_parcels %>%
    sf::st_drop_geometry() %>%
    count(target_year, pin14, pin10) %>%
    count(target_year, pin14) %>%
    filter(n > 1)
) > 0) {
  stop("One annual PIN14 maps to multiple PIN10 values.", call. = FALSE)
}

direct_parcels_by_pin <- direct_parcels %>%
  group_by(target_year, layer_id, pin14, pin10) %>%
  summarise(
    parcel_object_ids = paste(sort(unique(object_id)), collapse = "/"),
    object_id = min(object_id),
    geometry_valid = all(geometry_valid),
    .groups = "drop"
  )

direct_exact <- direct_parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "exact_pin14"),
    by = c("target_year", "pin14" = "component_pin"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    source_family,
    project_id,
    component_pin = pin14,
    requested_pin10 = pin10.y,
    target_year,
    match_method = "exact_construction_year_pin14",
    layer_id,
    object_id,
    parcel_object_ids,
    parcel_pin14 = pin14,
    parcel_pin10 = pin10.x,
    geometry_valid,
    geometry
  )

direct_pin10 <- direct_parcels_by_pin %>%
  inner_join(
    coverage %>% filter(coverage_status == "unique_pin10_only"),
    by = c("target_year", "pin10"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    source_family,
    project_id,
    component_pin,
    requested_pin10 = pin10,
    target_year,
    match_method = "unique_construction_year_pin10",
    layer_id,
    object_id,
    parcel_object_ids,
    parcel_pin14 = pin14,
    parcel_pin10 = pin10,
    geometry_valid,
    geometry
  )

predecessor_resolution <- readr::read_csv(
  "../output/historical_project_predecessor_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    predecessor_pin14 = readr::col_character(),
    predecessor_pin10 = readr::col_character(),
    history_row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(predecessor_status == "unique_predecessor_polygon")

if (anyDuplicated(predecessor_resolution$request_id) > 0) {
  stop("Unique predecessor resolutions are not unique by request ID.", call. = FALSE)
}

predecessor_parcels <- sf::st_read(
  "../input/historical_predecessor_parcel_source.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)
names(predecessor_parcels)[
  names(predecessor_parcels) == attr(predecessor_parcels, "sf_column")
] <- "geometry"
sf::st_geometry(predecessor_parcels) <- "geometry"

if (anyDuplicated(predecessor_parcels[c("target_year", "object_id")]) > 0) {
  stop("Predecessor parcels are not unique by layer year and object ID.", call. = FALSE)
}

predecessor <- predecessor_parcels %>%
  inner_join(
    predecessor_resolution,
    by = c("target_year", "object_id"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    source_family,
    project_id,
    component_pin,
    requested_pin10 = pin10,
    target_year,
    match_method = "point_in_construction_year_predecessor",
    layer_id,
    object_id,
    parcel_object_ids = as.character(object_id),
    parcel_pin14 = predecessor_pin14.x,
    parcel_pin10 = predecessor_pin10.x,
    geometry_valid,
    geometry
  )

component_geometry <- rbind(direct_exact, direct_pin10, predecessor) %>%
  arrange(target_year, source_family, project_id, component_pin)

if (anyDuplicated(component_geometry[c(
  "source_family", "project_id", "component_pin", "target_year"
)]) > 0) {
  stop("A project component-year resolves to more than one accepted parcel polygon.", call. = FALSE)
}

requested_counts <- coverage %>%
  group_by(source_family, project_id, target_year) %>%
  summarise(requested_components = n_distinct(component_pin), .groups = "drop")

resolved_counts <- component_geometry %>%
  sf::st_drop_geometry() %>%
  group_by(source_family, project_id, target_year) %>%
  summarise(
    resolved_components = n_distinct(component_pin),
    distinct_parcel_polygons = n_distinct(paste(layer_id, object_id)),
    component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    parcel_pins = paste(sort(unique(parcel_pin14)), collapse = "/"),
    match_methods = paste(sort(unique(match_method)), collapse = "/"),
    .groups = "drop"
  )

project_year_coverage <- requested_counts %>%
  left_join(
    resolved_counts,
    by = c("source_family", "project_id", "target_year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    resolved_components = coalesce(resolved_components, 0L),
    distinct_parcel_polygons = coalesce(distinct_parcel_polygons, 0L),
    complete_project_geometry = requested_components == resolved_components,
    unresolved_components = requested_components - resolved_components
  )

complete_projects <- project_year_coverage %>%
  filter(complete_project_geometry) %>%
  select(source_family, project_id, target_year)

project_geometry <- component_geometry %>%
  inner_join(
    complete_projects,
    by = c("source_family", "project_id", "target_year"),
    relationship = "many-to-one"
  ) %>%
  group_by(source_family, project_id, target_year) %>%
  summarise(
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(
    project_polygon_valid = sf::st_is_valid(geometry),
    project_land_area_sqft = as.numeric(sf::st_area(geometry))
  )

SaveData(project_geometry, c("source_family", "project_id", "target_year"), "../output/historical_project_year_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)
