# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
# Check the recorded Census matches.
source("../../shared/code/normalize_chicago_address.R")

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
source("../../shared/code/normalize_chicago_address.R")

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
