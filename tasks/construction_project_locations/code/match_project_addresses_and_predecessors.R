# setwd("tasks/construction_project_locations/code")

source("../../shared/code/save_data.R")
source("../../setup_environment/code/packages.R")
source("../../shared/code/normalize_chicago_address.R")


# Build preferred address geocode requests

library(dplyr)
library(readr)
library(stringr)

coverage <- read_csv("../input/preferred_historical_parcel_coverage.csv",
  col_types = cols(component_pin = col_character(), pin10 = col_character(), .default = col_guess())) |>
  filter(coverage_status %in% c("missing_pin10", "ambiguous_pin10")) |>
  mutate(request_id = paste(project_id, component_pin, target_year, sep = "|"))
historical <- read_csv("../input/density_historical_coordinates.csv",
  col_types = cols(pin = col_character(), construction_year = col_double(),
    longitude = col_double(), latitude = col_double(), .default = col_skip())) |>
  filter(is.finite(longitude), is.finite(latitude)) |>
  select(component_pin = pin, target_year = construction_year)
current <- data.table::fread("../input/parcel_universe_2025_city.csv",
  select = c("pin", "centroid_x_crs_3435", "centroid_y_crs_3435"), colClasses = c(pin = "character")) |>
  as_tibble() |>
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) |>
  select(component_pin = pin)
addresses <- read_csv("../input/density_parcel_address_selected_history.csv",
  col_types = cols(pin = col_character(), .default = col_guess())) |>
  filter(address_selection_status == "selected_nearest_year_address") |>
  select(component_pin = pin, target_year = construction_year, selected_address,
    selected_address_normalized, selected_address_year, selected_address_year_gap,
    nearest_address_count, address_selection_status)
stopifnot(!anyDuplicated(coverage$request_id), !anyDuplicated(historical),
          !anyDuplicated(current), !anyDuplicated(addresses[c("component_pin", "target_year")]))

# Address geocoding is needed only when both exact-parcel coordinate sources fail.
requests <- coverage |>
  anti_join(historical, by = c("component_pin", "target_year")) |>
  anti_join(current, by = "component_pin") |>
  left_join(addresses, by = c("component_pin", "target_year"), relationship = "many-to-one") |>
  mutate(address_query = if_else(!is.na(selected_address), paste(selected_address, "Chicago, IL", sep = ", "), NA_character_),
    query_house_number = str_extract(selected_address, "^[0-9]+")) |>
  arrange(target_year, source_family, project_id, component_pin)
stopifnot(!anyDuplicated(requests$request_id))
SaveData(requests, c("request_id"), "../output/preferred_address_geocode_requests.csv")

# Read address geocodes

# Check the recorded Census matches.
requests <- read_csv("../output/preferred_address_geocode_requests.csv",
  col_types = cols(request_id = col_character(), component_pin = col_character(), pin10 = col_character(),
    selected_address = col_character(), address_query = col_character(), query_house_number = col_character(), .default = col_guess()))
responses <- read_csv("../input/address_geocodes_census.csv", col_types = cols(.default = col_character()))
stopifnot(!anyNA(requests$request_id), !anyDuplicated(requests$request_id),
          !anyNA(responses), !anyDuplicated(responses$selected_address))
missing_queries <- requests |>
  filter(!is.na(selected_address)) |>
  anti_join(responses, by = c("selected_address", "address_query"))
if (nrow(missing_queries)) stop("Census requests exceed the pinned response coverage; acquire and pin the missing queries.")
geocodes <- requests |>
  mutate(query_street_address = geocode_street_address(selected_address),
    matched_street_address = NA_character_, census_match_count = 0L, census_status = "no_selected_historical_address",
    matched_address = NA_character_, matched_house_number = NA_character_,
    longitude = NA_real_, latitude = NA_real_, tiger_line_id = NA_character_,
    tiger_line_side = NA_character_, response_error = NA_character_,
    census_x_3435 = NA_real_, census_y_3435 = NA_real_)

# A unique candidate must match the full street address, allowing an omitted unit label.
for (i in which(!is.na(geocodes$selected_address))) {
  response <- jsonlite::fromJSON(responses$response_json[match(geocodes$selected_address[i], responses$selected_address)],
    simplifyVector = FALSE)
  stopifnot(is.null(response$error), is.null(response$errors), "addressMatches" %in% names(response$result))
  matches <- response$result$addressMatches
  geocodes$census_match_count[i] <- length(matches)
  if (length(matches) != 1L) {
    geocodes$census_status[i] <- if (length(matches) == 0L) "no_match" else "multiple_matches"
    next
  }
  candidate <- matches[[1]]
  stopifnot(length(candidate$matchedAddress) == 1L, length(candidate$coordinates$x) == 1L,
            length(candidate$coordinates$y) == 1L, length(candidate$tigerLine$tigerLineId) == 1L,
            length(candidate$tigerLine$side) == 1L)
  geocodes$matched_address[i] <- candidate$matchedAddress
  geocodes$matched_street_address[i] <- geocode_street_address(candidate$matchedAddress)
  geocodes$matched_house_number[i] <- str_extract(candidate$matchedAddress, "^[0-9]+")
  geocodes$longitude[i] <- as.numeric(candidate$coordinates$x)
  geocodes$latitude[i] <- as.numeric(candidate$coordinates$y)
  geocodes$tiger_line_id[i] <- as.character(candidate$tigerLine$tigerLineId)
  geocodes$tiger_line_side[i] <- candidate$tigerLine$side
  geocodes$census_status[i] <- case_when(
    is.na(geocodes$query_house_number[i]) | is.na(geocodes$matched_house_number[i]) |
      geocodes$query_house_number[i] != geocodes$matched_house_number[i] ~ "house_number_mismatch",
    is.na(geocodes$query_street_address[i]) | is.na(geocodes$matched_street_address[i]) ~ "street_address_unresolved",
    geocodes$query_street_address[i] != geocodes$matched_street_address[i] ~ "street_address_mismatch",
    !is.finite(geocodes$longitude[i]) | !is.finite(geocodes$latitude[i]) |
      !between(geocodes$longitude[i], -88, -87.5) | !between(geocodes$latitude[i], 41.6, 42.1) ~ "coordinate_outside_chicago_bounds",
    TRUE ~ "accepted_reference_point")
}
accepted <- which(geocodes$census_status == "accepted_reference_point")
if (length(accepted)) {
  points <- geocodes[accepted, ] |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(3435)
  coordinates <- sf::st_coordinates(points)
  geocodes$census_x_3435[accepted] <- coordinates[, "X"]
  geocodes$census_y_3435[accepted] <- coordinates[, "Y"]
}
geocodes <- arrange(geocodes, target_year, source_family, project_id, component_pin)
stopifnot(nrow(geocodes) == nrow(requests), !anyDuplicated(geocodes$request_id))
SaveData(geocodes, c("request_id"), "../output/preferred_historical_address_geocodes.csv")

# Check the recorded Chicago matches.
requests <- read_csv("../output/preferred_address_geocode_requests.csv",
  col_types = cols(request_id = col_character(), component_pin = col_character(), pin10 = col_character(),
    selected_address = col_character(), address_query = col_character(), query_house_number = col_character(), .default = col_guess()))
responses <- read_csv("../input/address_geocodes_chicago.csv", col_types = cols(.default = col_character()))
stopifnot(!anyNA(requests$request_id), !anyDuplicated(requests$request_id),
          !anyNA(responses), !anyDuplicated(responses$selected_address))
missing_queries <- requests |>
  filter(!is.na(selected_address)) |>
  anti_join(responses, by = c("selected_address", "address_query"))
if (nrow(missing_queries)) stop("Chicago requests exceed the pinned response coverage; acquire and pin the missing queries.")
geocodes <- requests |>
  select(request_id, source_family, project_id, project_kind, candidate_status,
    component_pin, pin10, target_year, selected_address, selected_address_normalized,
    selected_address_year, selected_address_year_gap) |>
  mutate(query_street_address = geocode_street_address(selected_address),
    matched_street_address = NA_character_, chicago_candidate_count = 0L, chicago_exact_point_count = 0L,
    chicago_status = "no_selected_historical_address", chicago_matched_address = NA_character_,
    chicago_house_number = NA_character_, chicago_score = NA_real_,
    chicago_x_3435 = NA_real_, chicago_y_3435 = NA_real_, chicago_locator = NA_character_,
    chicago_address_type = NA_character_, response_error = NA_character_)

# Require a unique score-100 point candidate with the same full street address.
for (i in which(!is.na(geocodes$selected_address))) {
  response <- jsonlite::fromJSON(responses$response_json[match(geocodes$selected_address[i], responses$selected_address)],
    simplifyVector = FALSE)
  stopifnot(is.null(response$error), is.null(response$errors), "candidates" %in% names(response),
            !isTRUE(response$exceededTransferLimit))
  candidates <- response$candidates
  geocodes$chicago_candidate_count[i] <- length(candidates)
  if (!length(candidates)) {
    geocodes$chicago_status[i] <- "no_match"
    next
  }
  stopifnot(length(response$spatialReference$wkid) == 1L,
            response$spatialReference$wkid %in% c(3435, 102671))
  candidate_rows <- vector("list", length(candidates))
  for (j in seq_along(candidates)) {
    candidate <- candidates[[j]]
    values <- list(chicago_matched_address = as.character(candidate$address),
      chicago_house_number = as.character(candidate$attributes$AddNum),
      chicago_score = as.numeric(candidate$score), chicago_x_3435 = as.numeric(candidate$location$x),
      chicago_y_3435 = as.numeric(candidate$location$y), chicago_locator = as.character(candidate$attributes$Loc_name),
      chicago_address_type = as.character(candidate$attributes$Addr_type))
    stopifnot(all(lengths(values) == 1L))
    candidate_rows[[j]] <- as_tibble(values)
  }
  exact <- bind_rows(candidate_rows) |>
    filter(chicago_locator == "CHI_singleaddr", chicago_address_type == "PointAddress",
      chicago_score == 100, chicago_house_number == requests$query_house_number[i],
      is.finite(chicago_x_3435), is.finite(chicago_y_3435)) |>
    distinct()
  geocodes$chicago_exact_point_count[i] <- nrow(exact)
  geocodes$chicago_status[i] <- case_when(nrow(exact) == 0L ~ "no_exact_point_address",
    nrow(exact) > 1L ~ "multiple_exact_point_addresses", TRUE ~ "accepted_reference_point")
  if (nrow(exact) == 1L) {
    geocodes$matched_street_address[i] <- geocode_street_address(exact$chicago_matched_address)
    if (is.na(geocodes$query_street_address[i]) || is.na(geocodes$matched_street_address[i])) {
      geocodes$chicago_status[i] <- "street_address_unresolved"
    } else if (geocodes$query_street_address[i] != geocodes$matched_street_address[i]) {
      geocodes$chicago_status[i] <- "street_address_mismatch"
    } else {
      geocodes[i, names(exact)] <- exact
    }
  }
}
geocodes <- arrange(geocodes, target_year, source_family, project_id, component_pin)
stopifnot(nrow(geocodes) == nrow(requests), !anyDuplicated(geocodes$request_id))
SaveData(geocodes, c("request_id"), "../output/preferred_chicago_address_geocodes.csv")

# Build preferred predecessor reference points

coverage <- readr::read_csv(
  "../input/preferred_historical_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(coverage_status %in% c("missing_pin10", "ambiguous_pin10")) %>%
  mutate(request_id = paste(project_id, component_pin, target_year, sep = "|"))

historical_coordinates <- readr::read_csv(
  "../input/density_historical_coordinates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_transform(3435)

historical_matrix <- sf::st_coordinates(historical_coordinates)
historical_coordinates <- historical_coordinates %>%
  sf::st_drop_geometry() %>%
  transmute(
    component_pin = pin,
    target_year = construction_year,
    reference_x_3435 = historical_matrix[, "X"],
    reference_y_3435 = historical_matrix[, "Y"],
    reference_source = coordinate_source
  )

if (anyDuplicated(historical_coordinates[c("component_pin", "target_year")]) > 0) {
  stop("Historical coordinate reference is not unique by PIN-year.", call. = FALSE)
}

current_coordinates <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  filter(
    is.finite(centroid_x_crs_3435),
    is.finite(centroid_y_crs_3435)
  ) %>%
  transmute(
    component_pin = pin,
    current_x_3435 = centroid_x_crs_3435,
    current_y_3435 = centroid_y_crs_3435
  )

if (anyDuplicated(current_coordinates$component_pin) > 0) {
  stop("Current parcel coordinate reference is not unique by PIN.", call. = FALSE)
}

historical_address_coordinates <- readr::read_csv(
  "../output/preferred_historical_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    tiger_line_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(census_status == "accepted_reference_point") %>%
  select(
    request_id,
    address_x_3435 = census_x_3435,
    address_y_3435 = census_y_3435,
    selected_address,
    selected_address_year,
    matched_address,
    tiger_line_id
  )

if (anyDuplicated(historical_address_coordinates$request_id) > 0) {
  stop("Accepted historical address coordinates are not unique by request.", call. = FALSE)
}

chicago_address_coordinates <- readr::read_csv(
  "../output/preferred_chicago_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(chicago_status == "accepted_reference_point") %>%
  select(
    request_id,
    chicago_x_3435,
    chicago_y_3435,
    chicago_matched_address,
    chicago_score,
    chicago_locator
  )

if (anyDuplicated(chicago_address_coordinates$request_id) > 0) {
  stop("Accepted Chicago address coordinates are not unique by request.", call. = FALSE)
}

permit_coordinates <- readr::read_csv(
  "../input/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    plausible_application_window | plausible_issue_window,
    is.finite(permit_x_3435),
    is.finite(permit_y_3435)
  ) %>%
  group_by(component_pin) %>%
  summarise(
    permit_points = n_distinct(paste(permit_x_3435, permit_y_3435)),
    permit_x_min = min(permit_x_3435),
    permit_x_max = max(permit_x_3435),
    permit_y_min = min(permit_y_3435),
    permit_y_max = max(permit_y_3435),
    permit_x_3435 = median(permit_x_3435),
    permit_y_3435 = median(permit_y_3435),
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    .groups = "drop"
  ) %>%
  mutate(
    permit_point_spread_ft = sqrt(
      (permit_x_max - permit_x_min)^2 +
        (permit_y_max - permit_y_min)^2
    )
  ) %>%
  filter(permit_point_spread_ft <= 100)

reference_points <- coverage %>%
  left_join(
    historical_coordinates,
    by = c("component_pin", "target_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    current_coordinates,
    by = "component_pin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    historical_address_coordinates,
    by = "request_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    chicago_address_coordinates,
    by = "request_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    permit_coordinates,
    by = "component_pin",
    relationship = "many-to-one"
  ) %>%
  mutate(
    reference_source = case_when(
      is.finite(reference_x_3435) & is.finite(reference_y_3435) ~ reference_source,
      is.finite(current_x_3435) & is.finite(current_y_3435) ~ "parcel_universe_2025_exact_pin",
      is.finite(chicago_x_3435) & is.finite(chicago_y_3435) ~
        "chicago_point_geocode_of_cook_county_historical_address",
      is.finite(address_x_3435) & is.finite(address_y_3435) ~
        "census_geocode_of_cook_county_historical_address",
      is.finite(permit_x_3435) & is.finite(permit_y_3435) ~ "issued_permit_exact_component_pin10",
      TRUE ~ NA_character_
    ),
    reference_x_3435 = coalesce(
      reference_x_3435,
      current_x_3435,
      chicago_x_3435,
      address_x_3435,
      permit_x_3435
    ),
    reference_y_3435 = coalesce(
      reference_y_3435,
      current_y_3435,
      chicago_y_3435,
      address_y_3435,
      permit_y_3435
    ),
    reference_status = if_else(
      is.finite(reference_x_3435) & is.finite(reference_y_3435),
      "reference_point_available",
      "reference_point_unresolved"
    )
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
    coverage_status,
    reference_status,
    reference_source,
    reference_x_3435,
    reference_y_3435,
    selected_address,
    selected_address_year,
    matched_address,
    tiger_line_id,
    chicago_matched_address,
    chicago_score,
    chicago_locator,
    permit_ids,
    permit_point_spread_ft
  ) %>%
  arrange(target_year, project_id, component_pin)

if (anyDuplicated(reference_points$request_id) > 0) {
  stop("Preferred predecessor reference requests are not unique.", call. = FALSE)
}

SaveData(reference_points, c("request_id"), "../output/preferred_predecessor_reference_points.csv")

# Recover preferred historical predecessors

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
