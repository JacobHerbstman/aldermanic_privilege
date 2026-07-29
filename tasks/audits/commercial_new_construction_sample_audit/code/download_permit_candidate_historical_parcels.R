# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

layer_ids <- c(
  `2006` = 6, `2007` = 7, `2008` = 8, `2009` = 9,
  `2010` = 10, `2011` = 11, `2012` = 12, `2013` = 14,
  `2014` = 15, `2015` = 16, `2016` = 17, `2017` = 18,
  `2018` = 20, `2019` = 21, `2020` = 22, `2021` = 23,
  `2022` = 2022
)
service_url <- paste0(
  "https://gis.cookcountyil.gov/traditional/rest/services/",
  "parcelHistorical/MapServer"
)

request_arcgis <- function(url, query) {
  request <- httr2::request(url)
  request <- do.call(httr2::req_url_query, c(list(request), query))
  request |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_perform()
}

footprint_members <- readr::read_csv(
  "../output/permit_candidate_site_footprint_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_site_id = readr::col_character(),
    footprint_id = readr::col_character()
  )
)
site_ledger <- readr::read_csv(
  "../output/permit_candidate_site_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_site_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
site_reference_points <- sf::st_read(
  "../output/permit_candidate_site_reference_points.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)
footprints <- sf::st_read(
  "../output/permit_residual_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)

if (anyDuplicated(footprint_members$footprint_id) ||
    anyDuplicated(site_ledger$candidate_site_id) ||
    anyDuplicated(site_reference_points$candidate_site_id) ||
    anyDuplicated(footprints$footprint_id)) {
  stop("Candidate sites and footprint requests must be unique.", call. = FALSE)
}

footprint_requests <- footprints |>
  dplyr::inner_join(
    footprint_members,
    by = "footprint_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(dplyr::between(city_year_built, 2006L, 2022L)) |>
  suppressWarnings(sf::st_point_on_surface()) |>
  dplyr::transmute(
    request_id = paste0("footprint:", footprint_id),
    candidate_site_id,
    footprint_id,
    target_year = as.integer(city_year_built),
    reference_source = "city_footprint_point_on_surface"
  )
sf::st_crs(footprint_requests) <- 3435

sites_without_footprints <- site_reference_points |>
  dplyr::inner_join(
    site_ledger |>
      dplyr::filter(footprint_2015_count == 0L) |>
      dplyr::select(candidate_site_id, earliest_application_date),
    by = "candidate_site_id",
    relationship = "one-to-one"
  )
no_footprint_requests <- do.call(rbind, lapply(0:2, function(year_offset) {
  sites_without_footprints |>
    dplyr::mutate(
      target_year = pmin(
        lubridate::year(earliest_application_date) + year_offset,
        2022L
      ),
      request_id = paste0(
        "permit_point:",
        candidate_site_id,
        ":",
        target_year
      ),
      footprint_id = NA_character_,
      reference_source = paste0(
        "permit_point_application_year_plus_",
        year_offset
      )
    ) |>
    dplyr::select(
      request_id,
      candidate_site_id,
      footprint_id,
      target_year,
      reference_source
    )
})) |>
  dplyr::distinct(request_id, .keep_all = TRUE)
sf::st_crs(no_footprint_requests) <- 3435

requests <- rbind(
  footprint_requests,
  no_footprint_requests
) |>
  dplyr::arrange(target_year, candidate_site_id, request_id)
request_rows_before_cast <- nrow(requests)
sf::st_geometry(requests) <- sf::st_cast(
  sf::st_geometry(requests),
  "POINT",
  warn = FALSE
)

if (anyDuplicated(requests$request_id) ||
    nrow(requests) != request_rows_before_cast ||
    any(!dplyr::between(requests$target_year, 2006L, 2022L)) ||
    any(sf::st_is_empty(requests))) {
  stop("Candidate historical-parcel requests are invalid.", call. = FALSE)
}

candidate_parcels <- list()
request_matches <- list()
layer_manifest <- list()

for (year_value in sort(unique(requests$target_year))) {
  year_requests <- requests |>
    dplyr::filter(target_year == year_value)
  layer_id <- unname(layer_ids[as.character(year_value)])
  layer_url <- paste0(service_url, "/", layer_id)

  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  if (!is.null(metadata$error)) {
    stop(
      "Historical parcel metadata failed for ",
      year_value,
      ": ",
      metadata$error$message,
      call. = FALSE
    )
  }
  object_id_field <- metadata$fields$name[
    metadata$fields$type == "esriFieldTypeOID"
  ][1]

  object_ids <- integer()
  request_batches <- split(
    seq_len(nrow(year_requests)),
    ceiling(seq_len(nrow(year_requests)) / 75)
  )
  for (request_rows in request_batches) {
    coordinates <- sf::st_coordinates(year_requests[request_rows, ])
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
      stop(
        "Historical parcel object-ID query failed for ",
        year_value,
        ": ",
        id_result$error$message,
        call. = FALSE
      )
    }
    object_ids <- c(object_ids, as.integer(id_result$objectIds))
  }
  object_ids <- sort(unique(object_ids))
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
    if (stringr::str_detect(body, '"error"\\s*:')) {
      error_body <- jsonlite::fromJSON(body)
      stop(
        "Historical parcel geometry query failed for ",
        year_value,
        ": ",
        error_body$error$message,
        call. = FALSE
      )
    }
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    year_parcels[[length(year_parcels) + 1]] <- sf::st_read(
      temporary_geojson,
      quiet = TRUE
    )
    unlink(temporary_geojson)
  }

  if (length(year_parcels) == 0) {
    request_matches[[as.character(year_value)]] <- year_requests |>
      sf::st_drop_geometry() |>
      dplyr::mutate(candidate_polygon_count = 0L)
    next
  }

  year_parcels <- do.call(rbind, year_parcels) |>
    sf::st_transform(3435)
  pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
  pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
  if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
    stop(
      "Historical parcel fields are missing for ",
      year_value,
      call. = FALSE
    )
  }

  year_parcels <- year_parcels |>
    dplyr::transmute(
      target_year = year_value,
      layer_id,
      object_id = as.integer(.data[[object_id_field]]),
      historical_pin14 = stringr::str_replace_all(
        as.character(.data[[pin14_field]]),
        "[^0-9]",
        ""
      ),
      historical_pin10 = stringr::str_replace_all(
        as.character(.data[[pin10_field]]),
        "[^0-9]",
        ""
      ),
      geometry = geometry
    )
  invalid_geometry_before_repair <- sum(!sf::st_is_valid(year_parcels))
  if (invalid_geometry_before_repair > 0) {
    sf::st_geometry(year_parcels) <- sf::st_make_valid(
      sf::st_geometry(year_parcels)
    )
  }
  if (anyDuplicated(year_parcels$object_id) ||
      any(!sf::st_is_valid(year_parcels)) ||
      any(sf::st_is_empty(year_parcels))) {
    stop(
      "Historical parcel geometries are invalid for ",
      year_value,
      call. = FALSE
    )
  }

  containing_rows <- sf::st_intersects(year_requests, year_parcels)
  year_matches <- purrr::map2_dfr(
    seq_len(nrow(year_requests)),
    containing_rows,
    function(request_row, parcel_rows) {
      request <- sf::st_drop_geometry(year_requests[request_row, ])
      if (length(parcel_rows) == 0) {
        return(
          request |>
            dplyr::mutate(candidate_polygon_count = 0L)
        )
      }
      dplyr::bind_cols(
        request[rep(1, length(parcel_rows)), ],
        sf::st_drop_geometry(year_parcels[parcel_rows, ]) |>
          dplyr::select(
            object_id,
            historical_pin14,
            historical_pin10
          ),
        tibble::tibble(candidate_polygon_count = length(parcel_rows))
      )
    }
  )

  candidate_parcels[[as.character(year_value)]] <- year_parcels
  request_matches[[as.character(year_value)]] <- year_matches
  layer_manifest[[as.character(year_value)]] <- tibble::tibble(
    target_year = year_value,
    layer_id,
    layer_name = metadata$name,
    reference_points = nrow(year_requests),
    returned_object_ids = length(object_ids),
    invalid_geometries_repaired = invalid_geometry_before_repair,
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

candidate_parcels <- do.call(rbind, candidate_parcels) |>
  dplyr::arrange(target_year, historical_pin10, historical_pin14, object_id)
request_matches <- dplyr::bind_rows(request_matches) |>
  dplyr::mutate(
    candidate_polygon_count = dplyr::coalesce(candidate_polygon_count, 0L),
    parcel_match_status = dplyr::case_when(
      candidate_polygon_count == 1L ~ "unique_containing_parcel",
      candidate_polygon_count > 1L ~ "multiple_containing_parcels",
      TRUE ~ "no_containing_parcel"
    )
  ) |>
  dplyr::arrange(target_year, candidate_site_id, request_id, object_id)

if (anyDuplicated(
  candidate_parcels[c("target_year", "object_id")]
) || anyDuplicated(request_matches[c("request_id", "object_id")])) {
  stop("Candidate historical parcel outputs contain duplicate keys.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  request_matches |>
    dplyr::distinct(request_id, parcel_match_status) |>
    dplyr::count(parcel_match_status, name = "value") |>
    dplyr::transmute(
      section = "request_status",
      metric = parcel_match_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "candidate_sites",
      "reference_point_requests",
      "sites_with_historical_pin",
      "unique_historical_pins",
      "downloaded_parcel_polygons"
    ),
    value = c(
      nrow(site_ledger),
      dplyr::n_distinct(request_matches$request_id),
      dplyr::n_distinct(
        request_matches$candidate_site_id[
          stringr::str_length(request_matches$historical_pin14) == 14L
        ]
      ),
      dplyr::n_distinct(request_matches$historical_pin14[
        stringr::str_length(request_matches$historical_pin14) == 14L
      ]),
      nrow(candidate_parcels)
    )
  )
)

sf::st_write(
  candidate_parcels,
  "../output/permit_candidate_historical_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  request_matches,
  "../output/permit_candidate_historical_parcel_matches.csv"
)
readr::write_csv(
  dplyr::bind_rows(layer_manifest),
  "../output/permit_candidate_historical_parcel_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/permit_candidate_historical_parcel_summary.csv"
)
