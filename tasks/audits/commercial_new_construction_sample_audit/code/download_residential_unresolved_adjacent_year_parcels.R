# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

layer_ids <- c(
  `2000` = 0, `2001` = 1, `2002` = 2, `2003` = 3,
  `2004` = 4, `2005` = 5, `2006` = 6, `2007` = 7,
  `2008` = 8, `2009` = 9, `2010` = 10, `2011` = 11,
  `2012` = 12, `2013` = 14, `2014` = 15, `2015` = 16,
  `2016` = 17, `2017` = 18, `2018` = 20, `2019` = 21,
  `2020` = 22, `2021` = 23, `2022` = 2022
)
service_url <- "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer"

request_arcgis <- function(url, query) {
  request <- httr2::request(url)
  request <- do.call(httr2::req_url_query, c(list(request), query))
  request %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_perform()
}

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

suppressions <- readr::read_csv(
  "../adjudication/residential_candidate_suppressions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(candidate_project_id = readr::col_character(), .default = readr::col_guess())
)

preferred_centroids <- sf::st_read(
  "../output/preferred_project_year_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_drop_geometry() %>%
  filter(source_family == "residential") %>%
  select(project_id, target_year)

reference_points <- readr::read_csv(
  "../output/preferred_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(
    source_family == "residential",
    reference_status == "reference_point_available",
    is.finite(reference_x_3435),
    is.finite(reference_y_3435)
  ) %>%
  distinct(project_id, target_year)

requests <- candidates %>%
  filter(candidate_status == "retain_mechanical") %>%
  anti_join(suppressions, by = c("project_id" = "candidate_project_id")) %>%
  anti_join(
    preferred_centroids,
    by = c("project_id", "construction_year" = "target_year")
  ) %>%
  anti_join(
    reference_points,
    by = c("project_id", "construction_year" = "target_year")
  ) %>%
  transmute(
    project_id,
    target_year = as.integer(construction_year),
    component_pin = component_pins
  ) %>%
  tidyr::separate_longer_delim(component_pin, delim = "/") %>%
  mutate(pin10 = substr(component_pin, 1, 10))

if (anyDuplicated(requests[c("project_id", "component_pin")]) > 0) {
  stop("Unresolved adjacent-year parcel requests are not unique.", call. = FALSE)
}

downloaded <- list()
manifest <- list()

for (query_year in as.integer(names(layer_ids))) {
  layer_id <- unname(layer_ids[as.character(query_year)])
  layer_url <- paste0(service_url, "/", layer_id)
  pin10_values <- sort(unique(requests$pin10))
  where <- paste0("PIN10 IN ('", paste(pin10_values, collapse = "','"), "')")

  id_response <- request_arcgis(
    paste0(layer_url, "/query"),
    list(
      where = where,
      returnIdsOnly = "true",
      returnGeometry = "false",
      f = "json"
    )
  )
  id_result <- jsonlite::fromJSON(httr2::resp_body_string(id_response))
  if (!is.null(id_result$error)) {
    stop("Object-ID query failed for ", query_year, ": ", id_result$error$message, call. = FALSE)
  }
  object_ids <- sort(unique(as.integer(id_result$objectIds)))
  object_ids <- object_ids[is.finite(object_ids)]

  year_parcels <- list()
  for (id_chunk in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
    if (length(id_chunk) == 0) {
      next
    }
    geometry_response <- request_arcgis(
      paste0(layer_url, "/query"),
      list(
        objectIds = paste(id_chunk, collapse = ","),
        outFields = "*",
        returnGeometry = "true",
        outSR = "3435",
        f = "geojson"
      )
    )
    body <- httr2::resp_body_string(geometry_response)
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    year_parcels[[length(year_parcels) + 1]] <- sf::st_read(
      temporary_geojson,
      quiet = TRUE
    )
    unlink(temporary_geojson)
  }

  if (length(year_parcels) > 0) {
    year_parcels <- do.call(rbind, year_parcels) %>%
      sf::st_transform(3435)
    pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
    pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
    object_id_field <- names(year_parcels)[
      vapply(year_parcels, inherits, logical(1), what = "integer64")
    ][1]
    if (is.na(object_id_field)) {
      object_id_field <- intersect(c("OBJECTID", "OBJECTID_1", "FID"), names(year_parcels))[1]
    }
    if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
      stop("Expected parcel fields are missing for ", query_year, call. = FALSE)
    }
    downloaded[[as.character(query_year)]] <- year_parcels %>%
      transmute(
        query_year,
        layer_id,
        object_id = as.integer(.data[[object_id_field]]),
        pin14 = str_replace_all(as.character(.data[[pin14_field]]), "[^0-9]", ""),
        pin10 = str_replace_all(as.character(.data[[pin10_field]]), "[^0-9]", ""),
        geometry_valid = sf::st_is_valid(geometry)
      )
  }

  manifest[[as.character(query_year)]] <- tibble::tibble(
    query_year,
    layer_id,
    returned_object_ids = length(object_ids),
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

parcels <- do.call(rbind, downloaded) %>%
  filter(geometry_valid, !sf::st_is_empty(geometry))

parcel_counts <- parcels %>%
  sf::st_drop_geometry() %>%
  count(query_year, pin10, name = "pin10_parcel_count")

requests_by_pin10 <- requests %>%
  group_by(pin10) %>%
  summarise(
    request_rows = list(pick(project_id, target_year, component_pin)),
    .groups = "drop"
  )

matches <- parcels %>%
  inner_join(requests_by_pin10, by = "pin10", relationship = "many-to-one") %>%
  tidyr::unnest(request_rows) %>%
  left_join(parcel_counts, by = c("query_year", "pin10"), relationship = "many-to-one") %>%
  mutate(
    match_status = case_when(
      pin14 == component_pin ~ "exact_pin14",
      pin10_parcel_count == 1 ~ "unique_pin10",
      TRUE ~ "ambiguous_pin10"
    ),
    year_gap = query_year - target_year,
    absolute_year_gap = abs(year_gap),
    future_priority = if_else(year_gap >= 0, 0L, 1L)
  )

accepted <- matches %>%
  filter(match_status != "ambiguous_pin10") %>%
  group_by(project_id, component_pin) %>%
  arrange(
    desc(match_status == "exact_pin14"),
    absolute_year_gap,
    future_priority,
    query_year,
    object_id,
    .by_group = TRUE
  ) %>%
  slice_head(n = 1) %>%
  ungroup()

coverage <- requests %>%
  left_join(
    accepted %>%
      sf::st_drop_geometry() %>%
      select(
        project_id,
        component_pin,
        selected_parcel_year = query_year,
        selected_pin14 = pin14,
        match_status,
        year_gap,
        object_id
      ),
    by = c("project_id", "component_pin"),
    relationship = "one-to-one"
  ) %>%
  mutate(location_status = if_else(is.na(object_id), "unresolved", "accepted_adjacent_year_parcel"))

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "requested_projects",
      "requested_components",
      "projects_with_accepted_adjacent_year_parcel",
      "projects_unresolved"
    ),
    value = c(
      n_distinct(requests$project_id),
      nrow(requests),
      n_distinct(accepted$project_id),
      n_distinct(requests$project_id) - n_distinct(accepted$project_id)
    )
  ),
  coverage %>%
    count(location_status, name = "value") %>%
    transmute(metric = paste0("component_", location_status), value)
)

sf::st_write(
  accepted,
  "../output/residential_unresolved_adjacent_year_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  coverage,
  "../output/residential_unresolved_adjacent_year_parcel_coverage.csv"
)
readr::write_csv(
  dplyr::bind_rows(manifest),
  "../output/residential_unresolved_adjacent_year_parcel_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_adjacent_year_parcel_summary.csv"
)
