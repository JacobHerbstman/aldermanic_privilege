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
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(seconds = 90) |>
    httr2::req_perform()
}

query_object_ids <- function(layer_url, points) {
  coordinates <- sf::st_coordinates(points)
  geometry <- jsonlite::toJSON(
    list(
      points = unname(split(coordinates, row(coordinates))),
      spatialReference = list(wkid = 3435)
    ),
    auto_unbox = TRUE
  )
  response <- tryCatch(
    request_arcgis(
      paste0(layer_url, "/query"),
      list(
        geometry = geometry,
        geometryType = "esriGeometryMultipoint",
        spatialRel = "esriSpatialRelIntersects",
        inSR = "3435",
        distance = 100,
        units = "esriSRUnit_Foot",
        returnIdsOnly = "true",
        returnGeometry = "false",
        f = "json"
      )
    ),
    error = identity
  )
  if (inherits(response, "error")) {
    if (nrow(points) == 1L) {
      stop(
        "Transition-parcel query failed for ",
        points$transition_request_id,
        ": ",
        conditionMessage(response),
        call. = FALSE
      )
    }
    split_at <- ceiling(nrow(points) / 2)
    return(c(
      query_object_ids(layer_url, points[seq_len(split_at), ]),
      query_object_ids(
        layer_url,
        points[seq.int(split_at + 1L, nrow(points)), ]
      )
    ))
  }
  result <- jsonlite::fromJSON(httr2::resp_body_string(response))
  if (!is.null(result$error)) {
    stop(
      "Transition-parcel query failed: ",
      result$error$message,
      call. = FALSE
    )
  }
  as.integer(result$objectIds)
}

pairs <- readr::read_csv(
  "../output/preferred_project_duplicate_temporal_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    temporal_lineage_status ==
      "likely_predecessor_successor_duplicate" |
      (
        temporal_lineage_status == "insufficient_temporal_evidence" &
          source_family_1 == "residential" &
          source_family_2 == "residential" &
          !coexisted_in_assessor &
          is.finite(first_assessor_year_1) &
          is.finite(last_assessor_year_1) &
          is.finite(first_assessor_year_2) &
          is.finite(last_assessor_year_2)
      )
  ) |>
  dplyr::mutate(
    transition_candidate_source = dplyr::if_else(
      temporal_lineage_status ==
        "likely_predecessor_successor_duplicate",
      "similar_fields_and_sequential_assessor_history",
      "sequential_assessor_history_only"
    ),
    predecessor_project_id = dplyr::if_else(
      last_assessor_year_1 < first_assessor_year_2,
      project_id_1,
      project_id_2
    ),
    successor_project_id = dplyr::if_else(
      last_assessor_year_1 < first_assessor_year_2,
      project_id_2,
      project_id_1
    ),
    predecessor_last_assessor_year = pmin(
      last_assessor_year_1,
      last_assessor_year_2
    ),
    successor_first_assessor_year = pmax(
      first_assessor_year_1,
      first_assessor_year_2
    ),
    transition_map_year = pmin(
      pmax(predecessor_last_assessor_year, 2006L),
      2022L
    ),
    transition_request_id = paste(
      predecessor_project_id,
      transition_map_year,
      sep = ":"
    )
  )
projects <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
project_points <- sf::st_as_sf(
  projects |>
    dplyr::select(project_id, x_3435, y_3435),
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
requests <- pairs |>
  dplyr::distinct(
    transition_request_id,
    predecessor_project_id,
    transition_map_year
  ) |>
  dplyr::left_join(
    sf::st_drop_geometry(project_points) |>
      dplyr::rename(predecessor_project_id = project_id),
    by = "predecessor_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        predecessor_project_id = project_id,
        predecessor_component_pins = component_pins
      ),
    by = "predecessor_project_id",
    relationship = "many-to-one"
  ) |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = FALSE
  )

if (anyDuplicated(requests$transition_request_id) ||
    any(!is.finite(requests$x_3435)) ||
    any(!is.finite(requests$y_3435))) {
  stop("Transition parcel requests are invalid.", call. = FALSE)
}

selected_parcels <- list()
manifest <- list()

for (year_value in sort(unique(requests$transition_map_year))) {
  year_requests <- requests |>
    dplyr::filter(transition_map_year == year_value)
  layer_id <- unname(layer_ids[as.character(year_value)])
  layer_url <- paste0(service_url, "/", layer_id)
  message(
    "Transition parcels ",
    year_value,
    ": ",
    nrow(year_requests),
    " predecessor points"
  )

  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  object_id_field <- metadata$fields$name[
    metadata$fields$type == "esriFieldTypeOID"
  ][1]
  object_ids <- integer()
  batches <- split(
    seq_len(nrow(year_requests)),
    ceiling(seq_len(nrow(year_requests)) / 30)
  )
  for (rows in batches) {
    object_ids <- c(
      object_ids,
      query_object_ids(layer_url, year_requests[rows, ])
    )
  }
  object_ids <- sort(unique(object_ids[is.finite(object_ids)]))

  parcel_chunks <- list()
  for (id_chunk in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
    if (length(id_chunk) == 0) {
      next
    }
    response <- request_arcgis(
      paste0(layer_url, "/query"),
      list(
        objectIds = paste(id_chunk, collapse = ","),
        outFields = "*",
        returnGeometry = "true",
        outSR = "3435",
        f = "geojson"
      )
    )
    body <- httr2::resp_body_string(response)
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    parcel_chunks[[length(parcel_chunks) + 1]] <- sf::st_read(
      temporary_geojson,
      quiet = TRUE
    )
    unlink(temporary_geojson)
  }
  if (length(parcel_chunks) == 0) {
    next
  }

  year_parcels <- do.call(rbind, parcel_chunks) |>
    sf::st_transform(3435)
  pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
  year_parcels <- year_parcels |>
    dplyr::transmute(
      transition_map_year = year_value,
      object_id = as.integer(.data[[object_id_field]]),
      transition_pin14 = stringr::str_replace_all(
        as.character(.data[[pin14_field]]),
        "[^0-9]",
        ""
      ),
      geometry = geometry
    )
  sf::st_geometry(year_parcels) <- sf::st_make_valid(
    sf::st_geometry(year_parcels)
  )
  distances <- units::drop_units(
    sf::st_distance(year_requests, year_parcels)
  )
  year_selected <- purrr::map_dfr(
    seq_len(nrow(year_requests)),
    function(request_row) {
      minimum_distance <- min(distances[request_row, ])
      if (!is.finite(minimum_distance) || minimum_distance > 100) {
        return(NULL)
      }
      predecessor_pins <- stringr::str_split_1(
        dplyr::coalesce(
          year_requests$predecessor_component_pins[request_row],
          ""
        ),
        "/"
      )
      exact_pin_rows <- which(
        year_parcels$transition_pin14 %in% predecessor_pins
      )
      parcel_rows <- if (length(exact_pin_rows) > 0L) {
        exact_pin_rows
      } else {
        which(distances[request_row, ] <= minimum_distance + 0.5)
      }
      parcels <- year_parcels[parcel_rows, ]
      parcels$transition_request_id <-
        year_requests$transition_request_id[request_row]
      parcels$predecessor_project_id <-
        year_requests$predecessor_project_id[request_row]
      parcels$predecessor_to_parcel_distance_ft <-
        distances[request_row, parcel_rows]
      parcels$predecessor_parcel_tie_count <- length(parcel_rows)
      parcels$predecessor_parcel_match_method <- if (
        length(exact_pin_rows) > 0L
      ) {
        "exact_predecessor_pin"
      } else {
        "nearest_predecessor_point"
      }
      parcels
    }
  )
  selected_parcels[[as.character(year_value)]] <- year_selected
  manifest[[as.character(year_value)]] <- tibble::tibble(
    transition_map_year = year_value,
    layer_id,
    predecessor_points = nrow(year_requests),
    returned_object_ids = length(object_ids),
    selected_parcels = nrow(year_selected),
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

selected_parcels <- do.call(rbind, selected_parcels) |>
  dplyr::arrange(
    transition_request_id,
    predecessor_to_parcel_distance_ft,
    object_id
  )
parcel_rows_by_request <- split(
  seq_len(nrow(selected_parcels)),
  selected_parcels$transition_request_id
)
successor_index <- match(
  pairs$successor_project_id,
  project_points$project_id
)

edge_results <- purrr::map_dfr(
  seq_len(nrow(pairs)),
  function(pair_row) {
    pair <- pairs[pair_row, ]
    parcel_rows <- parcel_rows_by_request[[pair$transition_request_id]]
    if (is.null(parcel_rows)) {
      return(
        pair |>
          dplyr::mutate(
            predecessor_parcel_count = 0L,
            successor_to_predecessor_parcel_ft = NA_real_,
            successor_inside_predecessor_parcel = FALSE,
            transition_parcel_status =
              "predecessor_transition_parcel_unresolved"
          )
      )
    }
    distances <- units::drop_units(
      sf::st_distance(
        project_points[successor_index[pair_row], ],
        selected_parcels[parcel_rows, ]
      )
    )
    minimum_distance <- min(distances)
    pair |>
      dplyr::mutate(
        predecessor_parcel_count = length(parcel_rows),
        successor_to_predecessor_parcel_ft = minimum_distance,
        successor_inside_predecessor_parcel = minimum_distance == 0,
        transition_parcel_status = dplyr::case_when(
          minimum_distance == 0 ~
            "successor_inside_predecessor_transition_parcel",
          minimum_distance <= 5 ~
            "successor_within_5ft_of_predecessor_transition_parcel",
          TRUE ~ "successor_outside_predecessor_transition_parcel"
        )
      )
  }
)

summary <- dplyr::bind_rows(
  edge_results |>
    dplyr::count(
      transition_candidate_source,
      transition_parcel_status,
      name = "value"
    ) |>
    dplyr::transmute(
      section = transition_candidate_source,
      metric = transition_parcel_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "candidate_transition_edges",
      "predecessor_projects",
      "predecessors_with_transition_parcel",
      "transition_lineage_edges"
    ),
    value = c(
      nrow(pairs),
      dplyr::n_distinct(pairs$predecessor_project_id),
      dplyr::n_distinct(selected_parcels$predecessor_project_id),
      sum(
        edge_results$transition_parcel_status %in% c(
          "successor_inside_predecessor_transition_parcel",
          "successor_within_5ft_of_predecessor_transition_parcel"
        )
      )
    )
  )
)

sf::st_write(
  selected_parcels,
  "../output/preferred_project_duplicate_transition_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  edge_results,
  "../output/preferred_project_duplicate_transition_edges.csv"
)
readr::write_csv(
  dplyr::bind_rows(manifest),
  "../output/preferred_project_duplicate_transition_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_duplicate_transition_summary.csv"
)
