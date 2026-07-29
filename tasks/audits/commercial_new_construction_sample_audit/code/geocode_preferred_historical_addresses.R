# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

coverage <- readr::read_csv(
  "../output/preferred_historical_parcel_coverage.csv",
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
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  transmute(
    component_pin = pin,
    target_year = construction_year,
    historical_coordinate_available = TRUE
  )

if (anyDuplicated(historical_coordinates[c("component_pin", "target_year")]) > 0) {
  stop("Historical coordinate availability is not unique by PIN-year.", call. = FALSE)
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
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) %>%
  transmute(
    component_pin = pin,
    current_coordinate_available = TRUE
  )

if (anyDuplicated(current_coordinates$component_pin) > 0) {
  stop("Current coordinate availability is not unique by PIN.", call. = FALSE)
}

unresolved <- coverage %>%
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
  filter(
    is.na(historical_coordinate_available),
    is.na(current_coordinate_available)
  ) %>%
  select(-historical_coordinate_available, -current_coordinate_available)

historical_addresses <- readr::read_csv(
  "../input/density_parcel_address_selected_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    project_keys = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(address_selection_status == "selected_nearest_year_address") %>%
  select(
    component_pin = pin,
    target_year = construction_year,
    selected_address,
    selected_address_normalized,
    selected_address_year,
    selected_address_year_gap,
    nearest_address_count,
    address_selection_status
  )

if (anyDuplicated(historical_addresses[c("component_pin", "target_year")]) > 0) {
  stop("Selected historical addresses are not unique by PIN-year.", call. = FALSE)
}

address_requests <- unresolved %>%
  left_join(
    historical_addresses,
    by = c("component_pin", "target_year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    address_query = if_else(
      !is.na(selected_address),
      paste(selected_address, "Chicago, IL", sep = ", "),
      NA_character_
    ),
    query_house_number = str_extract(selected_address, "^[0-9]+")
  )

geocode_rows <- vector("list", nrow(address_requests))
for (row_number in seq_len(nrow(address_requests))) {
  request_row <- address_requests[row_number, ]
  if (is.na(request_row$address_query)) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      census_match_count = 0L,
      census_status = "no_selected_historical_address",
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      longitude = NA_real_,
      latitude = NA_real_,
      tiger_line_id = NA_character_,
      tiger_line_side = NA_character_,
      response_error = NA_character_
    )
    next
  }

  response <- tryCatch(
    httr2::request(
      "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
    ) %>%
      httr2::req_url_query(
        address = request_row$address_query,
        benchmark = "Public_AR_Current",
        format = "json"
      ) %>%
      httr2::req_retry(max_tries = 5) %>%
      httr2::req_timeout(seconds = 60) %>%
      httr2::req_perform(),
    error = function(error) error
  )

  if (inherits(response, "error")) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      census_match_count = NA_integer_,
      census_status = "request_failed",
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      longitude = NA_real_,
      latitude = NA_real_,
      tiger_line_id = NA_character_,
      tiger_line_side = NA_character_,
      response_error = conditionMessage(response)
    )
    next
  }

  response_body <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  matches <- response_body$result$addressMatches
  match_count <- length(matches)

  if (match_count != 1) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      census_match_count = match_count,
      census_status = if_else(match_count == 0, "no_match", "multiple_matches"),
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      longitude = NA_real_,
      latitude = NA_real_,
      tiger_line_id = NA_character_,
      tiger_line_side = NA_character_,
      response_error = NA_character_
    )
    next
  }

  match <- matches[[1]]
  matched_address <- as.character(match$matchedAddress)
  matched_house_number <- str_extract(matched_address, "^[0-9]+")
  longitude <- suppressWarnings(as.numeric(match$coordinates$x))
  latitude <- suppressWarnings(as.numeric(match$coordinates$y))
  house_number_matches <- !is.na(request_row$query_house_number) &&
    request_row$query_house_number == matched_house_number
  chicago_coordinate <- is.finite(longitude) && is.finite(latitude) &&
    longitude >= -88.0 && longitude <= -87.5 &&
    latitude >= 41.6 && latitude <= 42.1

  geocode_rows[[row_number]] <- tibble::tibble(
    request_id = request_row$request_id,
    census_match_count = match_count,
    census_status = case_when(
      !house_number_matches ~ "house_number_mismatch",
      !chicago_coordinate ~ "coordinate_outside_chicago_bounds",
      TRUE ~ "accepted_reference_point"
    ),
    matched_address,
    matched_house_number,
    longitude,
    latitude,
    tiger_line_id = as.character(match$tigerLine$tigerLineId),
    tiger_line_side = as.character(match$tigerLine$side),
    response_error = NA_character_
  )
}

geocodes <- address_requests %>%
  left_join(
    bind_rows(geocode_rows),
    by = "request_id",
    relationship = "one-to-one"
  )

accepted <- geocodes %>%
  filter(census_status == "accepted_reference_point") %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(3435)
accepted_coordinates <- sf::st_coordinates(accepted)
accepted <- accepted %>%
  sf::st_drop_geometry() %>%
  mutate(
    census_x_3435 = accepted_coordinates[, "X"],
    census_y_3435 = accepted_coordinates[, "Y"]
  ) %>%
  select(request_id, census_x_3435, census_y_3435)

geocodes <- geocodes %>%
  left_join(accepted, by = "request_id", relationship = "one-to-one") %>%
  arrange(target_year, source_family, project_id, component_pin)

if (anyDuplicated(geocodes$request_id) > 0 || nrow(geocodes) != nrow(unresolved)) {
  stop("Historical address geocoding changed or duplicated the request set.", call. = FALSE)
}

summary <- bind_rows(
  geocodes %>%
    count(census_status, name = "value") %>%
    transmute(metric = paste0("census_geocode_", census_status), value),
  tibble::tibble(metric = "historical_address_requests", value = nrow(geocodes))
)

readr::write_csv(
  geocodes,
  "../output/preferred_historical_address_geocodes.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_historical_address_geocode_summary.csv"
)
