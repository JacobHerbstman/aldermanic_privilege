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

reference_points <- readr::read_csv(
  "../output/residential_unresolved_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    episode_id = readr::col_character(),
    project_id = readr::col_character(),
    point_id = readr::col_character(),
    point_request_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(reference_points$point_request_id) > 0) {
  stop("Residential unresolved predecessor request IDs are not unique.", call. = FALSE)
}

available_points <- reference_points %>%
  filter(
    reference_status == "reference_point_available",
    is.finite(x_3435),
    is.finite(y_3435)
  ) %>%
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )

candidate_parcels <- list()
point_matches <- list()
layer_manifest <- list()

for (year_value in sort(unique(available_points$target_year))) {
  year_points <- available_points %>%
    filter(target_year == year_value)
  layer_id <- unname(layer_ids[as.character(year_value)])
  layer_url <- paste0(service_url, "/", layer_id)

  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  if (!is.null(metadata$error)) {
    stop("Layer metadata error for ", year_value, ": ", metadata$error$message, call. = FALSE)
  }
  object_id_field <- metadata$fields$name[metadata$fields$type == "esriFieldTypeOID"][1]

  coordinates <- sf::st_coordinates(year_points)
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
    stop("Spatial object-ID query error for ", year_value, ": ", id_result$error$message, call. = FALSE)
  }
  object_ids <- sort(unique(as.integer(id_result$objectIds)))
  object_ids <- object_ids[is.finite(object_ids)]

  if (length(object_ids) == 0) {
    point_matches[[as.character(year_value)]] <- sf::st_drop_geometry(year_points) %>%
      transmute(point_request_id, candidate_polygon_count = 0L)
    next
  }

  year_parcels <- list()
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
      stop("Geometry query error for ", year_value, ": ", error_body$error$message, call. = FALSE)
    }
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    year_parcels[[length(year_parcels) + 1]] <- sf::st_read(temporary_geojson, quiet = TRUE)
    unlink(temporary_geojson)
  }

  year_parcels <- do.call(rbind, year_parcels) %>%
    sf::st_transform(3435)
  pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
  pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
  if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
    stop("Expected parcel fields are missing for layer year ", year_value, call. = FALSE)
  }

  year_parcels <- year_parcels %>%
    transmute(
      target_year = year_value,
      layer_id,
      object_id = as.integer(.data[[object_id_field]]),
      predecessor_pin14 = str_replace_all(as.character(.data[[pin14_field]]), "[^0-9]", ""),
      predecessor_pin10 = str_replace_all(as.character(.data[[pin10_field]]), "[^0-9]", ""),
      geometry = geometry
    )

  if (anyDuplicated(year_parcels$object_id) > 0 ||
      any(!sf::st_is_valid(year_parcels)) ||
      any(sf::st_is_empty(year_parcels))) {
    stop("Returned predecessor polygons violate their geometry contract.", call. = FALSE)
  }

  containing_polygons <- sf::st_intersects(year_points, year_parcels)
  year_matches <- purrr::map2_dfr(
    seq_len(nrow(year_points)),
    containing_polygons,
    function(point_row, parcel_rows) {
      request <- sf::st_drop_geometry(year_points[point_row, ])
      if (length(parcel_rows) == 0) {
        return(request %>% transmute(point_request_id, candidate_polygon_count = 0L))
      }
      bind_cols(
        request[rep(1, length(parcel_rows)), ] %>% select(point_request_id),
        sf::st_drop_geometry(year_parcels[parcel_rows, ]) %>%
          select(object_id, predecessor_pin14, predecessor_pin10),
        tibble::tibble(candidate_polygon_count = length(parcel_rows))
      )
    }
  )

  candidate_parcels[[as.character(year_value)]] <- year_parcels
  point_matches[[as.character(year_value)]] <- year_matches
  layer_manifest[[as.character(year_value)]] <- tibble::tibble(
    target_year = year_value,
    layer_id,
    layer_name = metadata$name,
    reference_points = nrow(year_points),
    returned_object_ids = length(object_ids),
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

candidate_parcels <- do.call(rbind, candidate_parcels) %>%
  arrange(target_year, predecessor_pin10, predecessor_pin14, object_id)
point_matches <- bind_rows(point_matches)

point_resolution <- reference_points %>%
  left_join(point_matches, by = "point_request_id", relationship = "one-to-many") %>%
  mutate(
    candidate_polygon_count = coalesce(candidate_polygon_count, 0L),
    point_resolution_status = case_when(
      reference_status == "reference_point_unresolved" ~ "no_reference_point",
      candidate_polygon_count == 1 ~ "unique_containing_polygon",
      candidate_polygon_count > 1 ~ "multiple_containing_polygons",
      TRUE ~ "no_containing_polygon"
    )
  ) %>%
  arrange(project_id, target_year, project_point_number, object_id)

matched_candidates <- candidate_parcels %>%
  inner_join(
    point_resolution %>%
      filter(candidate_polygon_count > 0) %>%
      select(
        point_request_id,
        episode_id,
        project_id,
        target_year,
        point_id,
        reference_sources,
        object_id
      ),
    by = c("target_year", "object_id"),
    relationship = "one-to-many"
  ) %>%
  group_by(
    episode_id,
    project_id,
    target_year,
    object_id,
    predecessor_pin14,
    predecessor_pin10
  ) %>%
  summarise(
    matching_point_ids = paste(sort(unique(point_id)), collapse = "/"),
    matching_reference_sources = paste(sort(unique(reference_sources)), collapse = " || "),
    matching_reference_points = n_distinct(point_id),
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  arrange(project_id, target_year, predecessor_pin14, object_id)

if (anyDuplicated(matched_candidates[c("episode_id", "object_id")]) > 0) {
  stop("Episode predecessor candidates are not unique by parcel object.", call. = FALSE)
}

episode_summary <- reference_points %>%
  distinct(episode_id, project_id, target_year) %>%
  left_join(
    matched_candidates %>%
      sf::st_drop_geometry() %>%
      group_by(episode_id) %>%
      summarise(
        candidate_parcels = n_distinct(object_id),
        candidate_pin14s = paste(sort(unique(predecessor_pin14)), collapse = "/"),
        reference_points_with_match = n_distinct(matching_point_ids),
        .groups = "drop"
      ),
    by = "episode_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    candidate_parcels = coalesce(candidate_parcels, 0L),
    reference_points_with_match = coalesce(reference_points_with_match, 0L)
  ) %>%
  arrange(project_id, target_year)

summary <- bind_rows(
  point_resolution %>%
    distinct(point_request_id, point_resolution_status) %>%
    count(point_resolution_status, name = "value") %>%
    transmute(metric = paste0("point_", point_resolution_status), value),
  tibble::tibble(
    metric = c(
      "episodes_requested",
      "episodes_with_candidate_polygon",
      "candidate_episode_parcel_links"
    ),
    value = c(
      nrow(episode_summary),
      sum(episode_summary$candidate_parcels > 0),
      nrow(matched_candidates)
    )
  )
)

sf::st_write(
  matched_candidates,
  "../output/residential_unresolved_predecessor_candidates.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  point_resolution,
  "../output/residential_unresolved_predecessor_point_resolution.csv"
)
readr::write_csv(
  episode_summary,
  "../output/residential_unresolved_predecessor_episode_summary.csv"
)
readr::write_csv(
  bind_rows(layer_manifest),
  "../output/residential_unresolved_predecessor_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_predecessor_candidate_summary.csv"
)
