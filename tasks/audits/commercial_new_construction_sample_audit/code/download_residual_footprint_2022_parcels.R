# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

layer_url <- paste0(
  "https://gis.cookcountyil.gov/traditional/rest/services/",
  "parcelHistorical/MapServer/2022"
)

request_arcgis <- function(url, query) {
  request <- httr2::request(url)
  request <- do.call(httr2::req_url_query, c(list(request), query))
  request |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(seconds = 90) |>
    httr2::req_perform()
}

footprint_links <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
candidate_chains <- readr::read_csv(
  "../output/residual_permit_historical_chain_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    historical_reconciliation_status ==
      "unrepresented_footprint_candidate"
  ) |>
  dplyr::select(permit_chain_id)
selected_footprint_links <- footprint_links |>
  dplyr::semi_join(
    candidate_chains,
    by = "permit_chain_id"
  ) |>
  dplyr::filter(
    strong_footprint_match,
    !represented_in_preferred_ledger
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::filter(
    dplyr::case_when(
      any(address_range_match) ~ address_range_match,
      any(exact_harris_pin) ~ exact_harris_pin,
      TRUE ~ permit_point_inside_footprint
    )
  ) |>
  dplyr::ungroup()
footprints <- sf::st_read(
  "../output/permit_residual_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::semi_join(
    selected_footprint_links |>
      dplyr::distinct(footprint_id),
    by = "footprint_id"
  )
footprint_points <- footprints |>
  sf::st_point_on_surface() |>
  dplyr::select(footprint_id)

coordinates <- sf::st_coordinates(footprint_points)
geometry <- jsonlite::toJSON(
  list(
    points = unname(split(coordinates, row(coordinates))),
    spatialReference = list(wkid = 3435)
  ),
  auto_unbox = TRUE
)
response <- request_arcgis(
  paste0(layer_url, "/query"),
  list(
    geometry = geometry,
    geometryType = "esriGeometryMultipoint",
    spatialRel = "esriSpatialRelIntersects",
    inSR = "3435",
    distance = 25,
    units = "esriSRUnit_Foot",
    returnIdsOnly = "true",
    returnGeometry = "false",
    f = "json"
  )
)
object_id_result <- jsonlite::fromJSON(
  httr2::resp_body_string(response)
)
object_ids <- sort(unique(as.integer(object_id_result$objectIds)))

parcel_chunks <- list()
for (id_chunk in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
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
parcels <- do.call(rbind, parcel_chunks) |>
  sf::st_transform(3435) |>
  dplyr::transmute(
    object_id = as.integer(OBJECTID),
    pin14_2022 = stringr::str_replace_all(
      as.character(Name),
      "[^0-9]",
      ""
    ),
    pin10_2022 = stringr::str_replace_all(
      as.character(PIN10),
      "[^0-9]",
      ""
    ),
    geometry = geometry
  ) |>
  dplyr::group_by(pin14_2022, pin10_2022) |>
  dplyr::summarise(
    object_id = min(object_id),
    object_id_count = dplyr::n(),
    .groups = "drop"
  )
sf::st_geometry(parcels) <- sf::st_make_valid(sf::st_geometry(parcels))

distance_matrix <- units::drop_units(
  sf::st_distance(footprint_points, parcels)
)
matches <- purrr::map_dfr(
  seq_len(nrow(footprint_points)),
  function(point_row) {
    minimum_distance <- min(distance_matrix[point_row, ])
    parcel_rows <- which(
      distance_matrix[point_row, ] <= minimum_distance + 0.5
    )
    dplyr::bind_cols(
      tibble::tibble(
        footprint_id = rep(
          footprint_points$footprint_id[[point_row]],
          length(parcel_rows)
        )
      ),
      sf::st_drop_geometry(parcels[parcel_rows, ]),
      tibble::tibble(
        footprint_to_2022_parcel_ft =
          distance_matrix[point_row, parcel_rows],
        nearest_parcel_tie_count = length(parcel_rows),
        footprint_parcel_match_status = dplyr::case_when(
          minimum_distance == 0 & length(parcel_rows) == 1L ~
            "unique_containing_2022_parcel",
          minimum_distance == 0 ~
            "multiple_containing_2022_parcels",
          minimum_distance <= 25 & length(parcel_rows) == 1L ~
            "unique_nearest_2022_parcel_within_25ft",
          TRUE ~ "ambiguous_nearest_2022_parcels"
        )
      )
    )
  }
)

summary <- dplyr::bind_rows(
  matches |>
    dplyr::distinct(footprint_id, footprint_parcel_match_status) |>
    dplyr::count(footprint_parcel_match_status, name = "value") |>
    dplyr::transmute(
      section = "match_status",
      metric = footprint_parcel_match_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "candidate_footprints",
      "downloaded_2022_parcels",
      "footprints_with_unique_2022_parcel",
      "unique_2022_pins"
    ),
    value = c(
      nrow(footprint_points),
      nrow(parcels),
      dplyr::n_distinct(matches$footprint_id[
        matches$footprint_parcel_match_status %in% c(
          "unique_containing_2022_parcel",
          "unique_nearest_2022_parcel_within_25ft"
        )
      ]),
      dplyr::n_distinct(matches$pin14_2022[
        matches$footprint_parcel_match_status %in% c(
          "unique_containing_2022_parcel",
          "unique_nearest_2022_parcel_within_25ft"
        )
      ])
    )
  )
)

sf::st_write(
  parcels,
  "../output/residual_permit_footprint_2022_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  matches,
  "../output/residual_permit_footprint_2022_parcel_matches.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_footprint_2022_parcel_summary.csv"
)
