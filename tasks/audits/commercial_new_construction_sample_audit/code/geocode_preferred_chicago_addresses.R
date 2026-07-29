# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

address_requests <- readr::read_csv(
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
  mutate(query_house_number = str_extract(selected_address, "^[0-9]+"))

geocode_rows <- vector("list", nrow(address_requests))
for (row_number in seq_len(nrow(address_requests))) {
  request_row <- address_requests[row_number, ]
  if (is.na(request_row$selected_address)) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      chicago_candidate_count = 0L,
      chicago_exact_point_count = 0L,
      chicago_status = "no_selected_historical_address",
      chicago_matched_address = NA_character_,
      chicago_house_number = NA_character_,
      chicago_score = NA_real_,
      chicago_x_3435 = NA_real_,
      chicago_y_3435 = NA_real_,
      chicago_locator = NA_character_,
      chicago_address_type = NA_character_,
      response_error = NA_character_
    )
    next
  }

  response <- tryCatch(
    httr2::request(paste0(
      "https://gisapps.cityofchicago.org/arcgis/rest/services/",
      "Chicago_Addresses/GeocodeServer/findAddressCandidates"
    )) %>%
      httr2::req_url_query(
        SingleLine = request_row$selected_address,
        outFields = "*",
        outSR = 3435,
        f = "json"
      ) %>%
      httr2::req_retry(max_tries = 5) %>%
      httr2::req_timeout(seconds = 60) %>%
      httr2::req_perform(),
    error = function(error) error
  )

  if (inherits(response, "error")) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      chicago_candidate_count = NA_integer_,
      chicago_exact_point_count = NA_integer_,
      chicago_status = "request_failed",
      chicago_matched_address = NA_character_,
      chicago_house_number = NA_character_,
      chicago_score = NA_real_,
      chicago_x_3435 = NA_real_,
      chicago_y_3435 = NA_real_,
      chicago_locator = NA_character_,
      chicago_address_type = NA_character_,
      response_error = conditionMessage(response)
    )
    next
  }

  response_body <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  candidates <- response_body$candidates
  candidate_count <- length(candidates)

  if (candidate_count == 0) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      chicago_candidate_count = 0L,
      chicago_exact_point_count = 0L,
      chicago_status = "no_match",
      chicago_matched_address = NA_character_,
      chicago_house_number = NA_character_,
      chicago_score = NA_real_,
      chicago_x_3435 = NA_real_,
      chicago_y_3435 = NA_real_,
      chicago_locator = NA_character_,
      chicago_address_type = NA_character_,
      response_error = NA_character_
    )
    next
  }

  candidate_table <- bind_rows(lapply(candidates, function(candidate) {
    tibble::tibble(
      chicago_matched_address = as.character(candidate$address),
      chicago_house_number = as.character(candidate$attributes$AddNum),
      chicago_score = suppressWarnings(as.numeric(candidate$score)),
      chicago_x_3435 = suppressWarnings(as.numeric(candidate$location$x)),
      chicago_y_3435 = suppressWarnings(as.numeric(candidate$location$y)),
      chicago_locator = as.character(candidate$attributes$Loc_name),
      chicago_address_type = as.character(candidate$attributes$Addr_type)
    )
  })) %>%
    filter(
      chicago_locator == "CHI_singleaddr",
      chicago_address_type == "PointAddress",
      chicago_score == 100,
      chicago_house_number == request_row$query_house_number,
      is.finite(chicago_x_3435),
      is.finite(chicago_y_3435)
    ) %>%
    distinct(
      chicago_matched_address,
      chicago_house_number,
      chicago_score,
      chicago_x_3435,
      chicago_y_3435,
      chicago_locator,
      chicago_address_type
    )

  exact_point_count <- nrow(candidate_table)
  if (exact_point_count != 1) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      chicago_candidate_count = candidate_count,
      chicago_exact_point_count = exact_point_count,
      chicago_status = if_else(
        exact_point_count == 0,
        "no_exact_point_address",
        "multiple_exact_point_addresses"
      ),
      chicago_matched_address = NA_character_,
      chicago_house_number = NA_character_,
      chicago_score = NA_real_,
      chicago_x_3435 = NA_real_,
      chicago_y_3435 = NA_real_,
      chicago_locator = NA_character_,
      chicago_address_type = NA_character_,
      response_error = NA_character_
    )
    next
  }

  geocode_rows[[row_number]] <- bind_cols(
    tibble::tibble(
      request_id = request_row$request_id,
      chicago_candidate_count = candidate_count,
      chicago_exact_point_count = exact_point_count,
      chicago_status = "accepted_reference_point"
    ),
    candidate_table,
    tibble::tibble(response_error = NA_character_)
  )
}

geocodes <- address_requests %>%
  select(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin,
    pin10,
    target_year,
    selected_address,
    selected_address_normalized,
    selected_address_year,
    selected_address_year_gap
  ) %>%
  left_join(
    bind_rows(geocode_rows),
    by = "request_id",
    relationship = "one-to-one"
  ) %>%
  arrange(target_year, source_family, project_id, component_pin)

if (anyDuplicated(geocodes$request_id) > 0 || nrow(geocodes) != nrow(address_requests)) {
  stop("Chicago address geocoding changed or duplicated the request set.", call. = FALSE)
}

summary <- bind_rows(
  geocodes %>%
    count(chicago_status, name = "value") %>%
    transmute(metric = paste0("chicago_geocode_", chicago_status), value),
  tibble::tibble(metric = "historical_address_requests", value = nrow(geocodes))
)

readr::write_csv(
  geocodes,
  "../output/preferred_chicago_address_geocodes.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_chicago_address_geocode_summary.csv"
)
