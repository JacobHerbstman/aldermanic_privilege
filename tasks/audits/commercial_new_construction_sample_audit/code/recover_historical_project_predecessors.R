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
  response <- request %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_perform()
  if (httr2::resp_status(response) != 200) {
    stop("Cook County parcel request failed: ", httr2::resp_status(response), call. = FALSE)
  }
  response
}

reference_points <- readr::read_csv(
  "../output/missing_project_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    history_row_id = readr::col_character(),
    permit_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(is.finite(reference_x_3435), is.finite(reference_y_3435))

if (anyDuplicated(reference_points$request_id) > 0) {
  stop("Historical predecessor request IDs are not unique.", call. = FALSE)
}

reference_sf <- sf::st_as_sf(
  reference_points,
  coords = c("reference_x_3435", "reference_y_3435"),
  crs = 3435,
  remove = FALSE
)

predecessor_parcels <- list()
predecessor_matches <- list()

for (target_year in as.integer(names(layer_ids))) {
  year_points <- reference_sf %>%
    filter(target_year == .env$target_year)
  if (nrow(year_points) == 0) {
    next
  }

  layer_id <- unname(layer_ids[as.character(target_year)])
  layer_url <- paste0(service_url, "/", layer_id)
  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  if (!is.null(metadata$error)) {
    stop("Layer metadata error for ", target_year, ": ", metadata$error$message, call. = FALSE)
  }
  object_id_field <- metadata$fields$name[
    metadata$fields$type == "esriFieldTypeOID"
  ][1]

  object_ids <- integer()
  point_chunks <- split(seq_len(nrow(year_points)), ceiling(seq_len(nrow(year_points)) / 75))
  for (point_rows in point_chunks) {
    coordinates <- sf::st_coordinates(year_points[point_rows, ])
    geometry <- jsonlite::toJSON(
      list(
        points = unname(split(coordinates, row(coordinates))),
        spatialReference = list(wkid = 3435)
      ),
      auto_unbox = TRUE
    )
    id_response <- request_arcgis(
      paste0(layer_url, "/query"),
      list(
        geometry = geometry,
        geometryType = "esriGeometryMultipoint",
        spatialRel = "esriSpatialRelIntersects",
        inSR = "3435",
        returnIdsOnly = "true",
        returnGeometry = "false",
        f = "json"
      )
    )
    id_result <- jsonlite::fromJSON(httr2::resp_body_string(id_response))
    if (!is.null(id_result$error)) {
      stop("Spatial object-ID query error for ", target_year, ": ", id_result$error$message, call. = FALSE)
    }
    object_ids <- c(object_ids, as.integer(id_result$objectIds))
  }
  object_ids <- sort(unique(object_ids[is.finite(object_ids)]))

  year_parcels <- list()
  if (length(object_ids) > 0) {
    for (id_chunk in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
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
      chunk_parcels <- sf::st_read(temporary_geojson, quiet = TRUE)
      unlink(temporary_geojson)
      year_parcels[[length(year_parcels) + 1]] <- chunk_parcels
    }
  }

  if (length(year_parcels) == 0) {
    predecessor_matches[[as.character(target_year)]] <- sf::st_drop_geometry(year_points) %>%
      transmute(request_id, predecessor_polygon_count = 0L)
    next
  }

  year_parcels <- do.call(rbind, year_parcels) %>%
    sf::st_transform(3435)
  pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
  pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
  if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
    stop("Expected parcel fields are missing for layer year ", target_year, call. = FALSE)
  }
  year_parcels <- year_parcels %>%
    transmute(
      target_year = target_year,
      layer_id = layer_id,
      object_id = as.integer(.data[[object_id_field]]),
      predecessor_pin14 = str_replace_all(as.character(.data[[pin14_field]]), "[^0-9]", ""),
      predecessor_pin10 = str_replace_all(as.character(.data[[pin10_field]]), "[^0-9]", ""),
      geometry_valid = sf::st_is_valid(geometry),
      geometry = geometry
    )
  if (anyDuplicated(year_parcels$object_id) > 0) {
    stop("Duplicate predecessor object IDs returned for year ", target_year, call. = FALSE)
  }

  candidate_index <- sf::st_intersects(year_points, year_parcels)
  year_matches <- purrr::map2_dfr(
    seq_len(nrow(year_points)),
    candidate_index,
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

  predecessor_parcels[[as.character(target_year)]] <- year_parcels
  predecessor_matches[[as.character(target_year)]] <- year_matches
}

predecessor_parcels <- do.call(rbind, predecessor_parcels) %>%
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
predecessor_matches <- bind_rows(predecessor_matches)

predecessor_resolution <- reference_points %>%
  select(
    request_id,
    source_family,
    project_id,
    component_pin,
    pin10,
    target_year,
    reference_source,
    reference_x_3435,
    reference_y_3435,
    history_reference_year,
    history_reference_year_gap,
    history_row_id
  ) %>%
  left_join(
    predecessor_matches,
    by = "request_id",
    relationship = "one-to-many"
  ) %>%
  mutate(
    predecessor_polygon_count = coalesce(predecessor_polygon_count, 0L),
    predecessor_status = case_when(
      predecessor_polygon_count == 1 ~ "unique_predecessor_polygon",
      predecessor_polygon_count > 1 ~ "multiple_predecessor_polygons",
      TRUE ~ "no_predecessor_polygon"
    )
  ) %>%
  arrange(target_year, source_family, project_id, component_pin, object_id)

if (nrow(predecessor_resolution %>% distinct(request_id)) != nrow(reference_points)) {
  stop("One or more predecessor requests disappeared during spatial matching.", call. = FALSE)
}

summary <- bind_rows(
  predecessor_resolution %>%
    distinct(request_id, predecessor_status) %>%
    count(predecessor_status, name = "value") %>%
    transmute(metric = paste0("missing_component_year_", predecessor_status), value),
  tibble::tibble(metric = "predecessor_candidate_polygons", value = nrow(predecessor_parcels)),
  tibble::tibble(metric = "invalid_predecessor_geometries", value = sum(!predecessor_parcels$geometry_valid))
)

sf::st_write(
  predecessor_parcels,
  "../output/historical_project_predecessor_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(predecessor_resolution, "../output/historical_project_predecessor_resolution.csv")
readr::write_csv(summary, "../output/historical_project_predecessor_summary.csv")
