# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

existing_addresses <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    address_normalized = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  distinct(project_id, address_normalized)

address_requests <- readr::read_csv(
  "../output/residential_unresolved_address_permit_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_address = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    project_id,
    address = str_squish(str_to_upper(permit_address)),
    address_normalized = str_squish(str_replace_all(address, "[^A-Z0-9 ]", " ")),
    query_house_number = str_extract(address_normalized, "^[0-9]+")
  ) %>%
  filter(!is.na(address), address != "", !is.na(query_house_number)) %>%
  distinct(project_id, address_normalized, .keep_all = TRUE) %>%
  anti_join(existing_addresses, by = c("project_id", "address_normalized")) %>%
  arrange(project_id, address_normalized) %>%
  mutate(
    request_id = paste0("unresolved_permit_address_", row_number()),
    address_sources = "exact_project_permit_address"
  ) %>%
  select(request_id, project_id, address, address_normalized, address_sources, query_house_number)

if (anyDuplicated(address_requests$request_id) > 0 ||
    anyDuplicated(address_requests[c("project_id", "address_normalized")]) > 0) {
  stop("Unresolved permit-address requests violate their declared keys.", call. = FALSE)
}

geocode_rows <- vector("list", nrow(address_requests))
for (row_number in seq_len(nrow(address_requests))) {
  request_row <- address_requests[row_number, ]
  response <- tryCatch(
    httr2::request(paste0(
      "https://gisapps.cityofchicago.org/arcgis/rest/services/",
      "Chicago_Addresses/GeocodeServer/findAddressCandidates"
    )) %>%
      httr2::req_url_query(
        SingleLine = request_row$address,
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
      candidate_count = NA_integer_,
      exact_point_count = NA_integer_,
      geocode_status = "request_failed",
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      match_score = NA_real_,
      x_3435 = NA_real_,
      y_3435 = NA_real_,
      locator = NA_character_,
      address_type = NA_character_,
      response_error = conditionMessage(response)
    )
    next
  }

  response_body <- jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)
  candidates <- response_body$candidates
  candidate_count <- length(candidates)

  if (candidate_count == 0) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      candidate_count = 0L,
      exact_point_count = 0L,
      geocode_status = "no_match",
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      match_score = NA_real_,
      x_3435 = NA_real_,
      y_3435 = NA_real_,
      locator = NA_character_,
      address_type = NA_character_,
      response_error = NA_character_
    )
    next
  }

  candidate_table <- bind_rows(lapply(candidates, function(candidate) {
    tibble::tibble(
      matched_address = as.character(candidate$address),
      matched_house_number = as.character(candidate$attributes$AddNum),
      match_score = suppressWarnings(as.numeric(candidate$score)),
      x_3435 = suppressWarnings(as.numeric(candidate$location$x)),
      y_3435 = suppressWarnings(as.numeric(candidate$location$y)),
      locator = as.character(candidate$attributes$Loc_name),
      address_type = as.character(candidate$attributes$Addr_type)
    )
  })) %>%
    filter(
      locator == "CHI_singleaddr",
      address_type == "PointAddress",
      match_score == 100,
      matched_house_number == request_row$query_house_number,
      is.finite(x_3435),
      is.finite(y_3435)
    ) %>%
    distinct()

  exact_point_count <- nrow(candidate_table)
  if (exact_point_count != 1) {
    geocode_rows[[row_number]] <- tibble::tibble(
      request_id = request_row$request_id,
      candidate_count,
      exact_point_count,
      geocode_status = if_else(
        exact_point_count == 0,
        "no_exact_point_address",
        "multiple_exact_point_addresses"
      ),
      matched_address = NA_character_,
      matched_house_number = NA_character_,
      match_score = NA_real_,
      x_3435 = NA_real_,
      y_3435 = NA_real_,
      locator = NA_character_,
      address_type = NA_character_,
      response_error = NA_character_
    )
    next
  }

  geocode_rows[[row_number]] <- bind_cols(
    tibble::tibble(
      request_id = request_row$request_id,
      candidate_count,
      exact_point_count,
      geocode_status = "accepted_reference_point"
    ),
    candidate_table,
    tibble::tibble(response_error = NA_character_)
  )
}

geocodes <- address_requests %>%
  left_join(bind_rows(geocode_rows), by = "request_id", relationship = "one-to-one") %>%
  arrange(project_id, address_normalized)

if (nrow(geocodes) != nrow(address_requests) || anyDuplicated(geocodes$request_id) > 0) {
  stop("Permit-address geocoding changed or duplicated the request set.", call. = FALSE)
}

summary <- bind_rows(
  geocodes %>%
    count(geocode_status, name = "value") %>%
    transmute(metric = paste0("geocode_", geocode_status), value),
  tibble::tibble(
    metric = c("permit_address_requests", "projects_with_permit_address", "projects_with_accepted_permit_point"),
    value = c(
      nrow(geocodes),
      n_distinct(geocodes$project_id),
      n_distinct(geocodes$project_id[geocodes$geocode_status == "accepted_reference_point"])
    )
  )
)

readr::write_csv(
  geocodes,
  "../output/residential_unresolved_permit_address_geocodes.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_permit_address_geocode_summary.csv"
)
