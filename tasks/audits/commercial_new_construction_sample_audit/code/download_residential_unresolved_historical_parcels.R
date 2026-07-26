# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

layer_ids <- c(
  `2006` = 6, `2007` = 7, `2008` = 8, `2009` = 9,
  `2010` = 10, `2011` = 11, `2012` = 12, `2013` = 14,
  `2014` = 15, `2015` = 16, `2016` = 17, `2017` = 18,
  `2018` = 20, `2019` = 21, `2020` = 22, `2021` = 23,
  `2022` = 2022
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

if (anyDuplicated(requests[c("episode_id", "component_pin")]) > 0) {
  stop("Residential unresolved episode requests are not unique.", call. = FALSE)
}

requested_year_pin10 <- requests %>%
  distinct(target_year, pin10)

cached_parcels <- sf::st_read(
  "../output/historical_project_parcels.gpkg",
  quiet = TRUE
) %>%
  semi_join(
    requested_year_pin10,
    by = c("target_year", "pin10")
  ) %>%
  select(target_year, layer_id, object_id, pin14, pin10, geometry_valid)
sf::st_geometry(cached_parcels) <- "geometry"

cached_year_pin10 <- cached_parcels %>%
  sf::st_drop_geometry() %>%
  distinct(target_year, pin10)

missing_year_pin10 <- requested_year_pin10 %>%
  anti_join(cached_year_pin10, by = c("target_year", "pin10"))

downloaded_parcels <- list()
layer_manifest <- list()

for (target_year in as.integer(names(layer_ids))) {
  year_pin10 <- missing_year_pin10 %>%
    filter(target_year == .env$target_year) %>%
    pull(pin10)

  if (length(year_pin10) == 0) {
    next
  }

  layer_id <- unname(layer_ids[as.character(target_year)])
  layer_url <- paste0(service_url, "/", layer_id)
  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  if (!is.null(metadata$error)) {
    stop("Layer metadata error for ", target_year, ": ", metadata$error$message, call. = FALSE)
  }
  object_id_field <- metadata$fields$name[metadata$fields$type == "esriFieldTypeOID"][1]

  object_ids <- integer()
  for (pin_chunk in split(year_pin10, ceiling(seq_along(year_pin10) / 75))) {
    where <- paste0("PIN10 IN ('", paste(pin_chunk, collapse = "','"), "')")
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
      stop("Object-ID query error for ", target_year, ": ", id_result$error$message, call. = FALSE)
    }
    object_ids <- c(object_ids, as.integer(id_result$objectIds))
  }
  object_ids <- sort(unique(object_ids[is.finite(object_ids)]))

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
    if (str_detect(body, '"error"\\s*:')) {
      error_body <- jsonlite::fromJSON(body)
      stop("Geometry query error for ", target_year, ": ", error_body$error$message, call. = FALSE)
    }
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    year_parcels[[length(year_parcels) + 1]] <- sf::st_read(temporary_geojson, quiet = TRUE)
    unlink(temporary_geojson)
  }

  if (length(year_parcels) > 0) {
    year_parcels <- do.call(rbind, year_parcels) %>%
      sf::st_transform(3435)
    pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
    pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
    if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
      stop("Expected parcel fields are missing for layer year ", target_year, call. = FALSE)
    }
    year_parcels <- year_parcels %>%
      transmute(
        target_year,
        layer_id,
        object_id = as.integer(.data[[object_id_field]]),
        pin14 = str_replace_all(as.character(.data[[pin14_field]]), "[^0-9]", ""),
        pin10 = str_replace_all(as.character(.data[[pin10_field]]), "[^0-9]", ""),
        geometry_valid = sf::st_is_valid(geometry)
      )
    sf::st_geometry(year_parcels) <- "geometry"
    downloaded_parcels[[as.character(target_year)]] <- year_parcels
  }

  layer_manifest[[as.character(target_year)]] <- tibble::tibble(
    target_year,
    layer_id,
    layer_name = metadata$name,
    object_id_field,
    requested_pin10 = length(year_pin10),
    returned_object_ids = length(object_ids),
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

downloaded_parcels <- if (length(downloaded_parcels) > 0) {
  do.call(rbind, downloaded_parcels)
} else {
  cached_parcels[0, ]
}

historical_parcels <- rbind(cached_parcels, downloaded_parcels) %>%
  group_by(target_year, object_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(target_year, pin10, pin14, object_id)

if (anyDuplicated(historical_parcels[c("target_year", "object_id")]) > 0) {
  stop("Residential unresolved annual parcel records are not unique.", call. = FALSE)
}
if (any(!historical_parcels$geometry_valid) || any(sf::st_is_empty(historical_parcels))) {
  stop("Residential unresolved annual parcel geometries must be valid and nonempty.", call. = FALSE)
}

parcel_counts <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  count(target_year, pin10, name = "polygon_count_pin10")

exact_pin14 <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  distinct(target_year, pin10, pin14) %>%
  mutate(exact_pin14_available = TRUE)

coverage <- requests %>%
  left_join(
    parcel_counts,
    by = c("target_year", "pin10"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    exact_pin14,
    by = c("target_year", "pin10", "component_pin" = "pin14"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    polygon_count_pin10 = coalesce(polygon_count_pin10, 0L),
    exact_pin14_available = coalesce(exact_pin14_available, FALSE),
    coverage_status = case_when(
      exact_pin14_available ~ "exact_pin14",
      polygon_count_pin10 == 1 ~ "unique_pin10_only",
      polygon_count_pin10 > 1 ~ "ambiguous_pin10",
      TRUE ~ "missing_pin10"
    )
  ) %>%
  arrange(target_year, project_id, component_pin)

summary <- bind_rows(
  coverage %>%
    count(coverage_status, name = "value") %>%
    transmute(metric = paste0("component_year_", coverage_status), value),
  tibble::tibble(
    metric = c(
      "requested_component_years",
      "cached_year_pin10_requests",
      "downloaded_year_pin10_requests",
      "historical_parcel_polygons"
    ),
    value = c(
      nrow(coverage),
      nrow(cached_year_pin10),
      nrow(missing_year_pin10),
      nrow(historical_parcels)
    )
  )
)

sf::st_write(
  historical_parcels,
  "../output/residential_unresolved_historical_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  coverage,
  "../output/residential_unresolved_historical_parcel_coverage.csv"
)
readr::write_csv(
  bind_rows(layer_manifest),
  "../output/residential_unresolved_historical_parcel_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_historical_parcel_summary.csv"
)
