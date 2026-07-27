# setwd("tasks/audits/new_construction_project_verification/code")
# snapshot_year <- 2008

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Expected one argument: snapshot_year.")
}
snapshot_year <- as.integer(args[1])
if (!snapshot_year %in% c(2008L, 2022L)) {
  stop("snapshot_year must be 2008 or 2022.")
}

projects <- readr::read_csv(
  "../input/eligibility_uncorroborated_retained.csv",
  show_col_types = FALSE,
  col_select = c(project_id, within_500ft, x_3435, y_3435),
  col_types = readr::cols(
    project_id = readr::col_character(),
    within_500ft = readr::col_logical(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double()
  )
) |>
  dplyr::filter(within_500ft) |>
  dplyr::select(project_id, x_3435, y_3435)

if (nrow(projects) != 795L || anyDuplicated(projects$project_id)) {
  stop("The verification scope is not the expected 795 unique projects.")
}

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id),
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  anyDuplicated(project_polygons$project_id) ||
    any(!sf::st_is_valid(project_polygons))
) {
  stop("Available project polygons are duplicated or invalid.")
}

project_points <- projects |>
  dplyr::anti_join(
    sf::st_drop_geometry(project_polygons) |>
      dplyr::select(project_id),
    by = "project_id"
  )
if (
  any(!is.finite(project_points$x_3435)) ||
    any(!is.finite(project_points$y_3435))
) {
  stop("Projects without polygons also lack audited coordinates.")
}
project_points <- project_points |>
  sf::st_as_sf(
    coords = c("x_3435", "y_3435"),
    crs = 3435,
    remove = TRUE
  )

project_search_sites <- dplyr::bind_rows(
  project_polygons |>
    dplyr::select(project_id),
  sf::st_buffer(project_points, 100) |>
    dplyr::select(project_id)
)
if (nrow(project_search_sites) != nrow(projects)) {
  stop("Project search sites are incomplete.")
}

project_points <- suppressWarnings(
  sf::st_point_on_surface(project_search_sites)
)
coordinates <- sf::st_coordinates(project_points)
tiles <- sf::st_drop_geometry(project_points) |>
  dplyr::mutate(
    tile_x = floor(coordinates[, 1] / 2640),
    tile_y = floor(coordinates[, 2] / 2640)
  ) |>
  dplyr::group_by(tile_x, tile_y) |>
  dplyr::summarise(
    xmin = dplyr::first(tile_x) * 2640 - 200,
    ymin = dplyr::first(tile_y) * 2640 - 200,
    xmax = (dplyr::first(tile_x) + 1) * 2640 + 200,
    ymax = (dplyr::first(tile_y) + 1) * 2640 + 200,
    .groups = "drop"
  ) |>
  dplyr::arrange(tile_x, tile_y)

if (snapshot_year == 2008L) {
  base_url <- paste0(
    "https://gis.cookcountyil.gov/traditional/rest/services/",
    "buildingFootprint_2008/MapServer/1/query"
  )
  out_fields <- paste(
    c(
      "OBJECTID", "BLDG_ID", "FOOTPRINT_", "BLDG_CREAT",
      "BLDG_ACTIV", "HARRIS_STR", "YEAR_BUILT", "BLDG_SQ_FO",
      "NO_OF_UNIT", "NO_STORIES"
    ),
    collapse = ","
  )
} else {
  base_url <- paste0(
    "https://gis.cookcountyil.gov/traditional/rest/services/",
    "buildingFootprint_2022/MapServer/0/query"
  )
  out_fields <- paste(
    c("OBJECTID", "Area_SQFT", "Year", "Height", "GlobalID"),
    collapse = ","
  )
}

page_size <- 2000L
responses <- list()
request_log <- list()
response_index <- 0L

for (tile_index in seq_len(nrow(tiles))) {
  if (tile_index %% 25L == 0L) {
    message(
      "Downloading ",
      snapshot_year,
      " footprint tile ",
      tile_index,
      " of ",
      nrow(tiles),
      "."
    )
  }
  offset <- 0L
  repeat {
    envelope <- paste(
      tiles$xmin[tile_index],
      tiles$ymin[tile_index],
      tiles$xmax[tile_index],
      tiles$ymax[tile_index],
      sep = ","
    )
    response <- httr2::request(base_url) |>
      httr2::req_url_query(
        where = "1=1",
        geometry = envelope,
        geometryType = "esriGeometryEnvelope",
        inSR = 3435,
        spatialRel = "esriSpatialRelIntersects",
        outFields = out_fields,
        returnGeometry = "true",
        outSR = 3435,
        orderByFields = "OBJECTID",
        resultOffset = offset,
        resultRecordCount = page_size,
        f = "geojson"
      ) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_timeout(seconds = 180) |>
      httr2::req_perform()

    if (httr2::resp_status(response) != 200L) {
      stop("Cook County footprint request failed.")
    }

    body <- httr2::resp_body_string(response)
    if (grepl('"error"', body, fixed = TRUE)) {
      stop("Cook County footprint service returned an error: ", body)
    }

    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    page <- sf::st_read(temporary_geojson, quiet = TRUE)
    unlink(temporary_geojson)

    page_count <- nrow(page)
    request_log[[length(request_log) + 1L]] <- tibble::tibble(
      snapshot_year = snapshot_year,
      tile_index = tile_index,
      result_offset = offset,
      returned_features = page_count
    )

    if (page_count == 0L) {
      break
    }

    response_index <- response_index + 1L
    responses[[response_index]] <- page
    if (page_count < page_size) {
      break
    }
    offset <- offset + page_size
  }
}

if (length(responses) == 0L) {
  stop("The footprint service returned no features.")
}

footprints <- dplyr::bind_rows(responses) |>
  sf::st_as_sf() |>
  sf::st_transform(3435)

object_id <- names(footprints)[tolower(names(footprints)) == "objectid"]
if (length(object_id) != 1L) {
  stop("The footprint response does not contain one OBJECTID field.")
}
footprints <- footprints[!duplicated(footprints[[object_id]]), ]

near_project_sites <- lengths(
  sf::st_intersects(
    footprints,
    sf::st_union(sf::st_buffer(project_search_sites, 100))
  )
) > 0L
footprints <- footprints[near_project_sites, ]
footprints <- sf::st_make_valid(footprints)
footprints <- footprints[!sf::st_is_empty(footprints), ]

if (nrow(footprints) == 0L || any(!sf::st_is_valid(footprints))) {
  stop("Downloaded footprint geometries are empty or invalid.")
}

sf::st_write(
  footprints,
  paste0(
    "../output/official_building_footprints_",
    snapshot_year,
    ".gpkg"
  ),
  layer = paste0("official_building_footprints_", snapshot_year),
  delete_dsn = TRUE,
  quiet = TRUE
)

manifest <- dplyr::bind_rows(request_log) |>
  dplyr::summarise(
    snapshot_year = dplyr::first(.data$snapshot_year),
    source_url = base_url,
    retrieved_on = as.character(Sys.Date()),
    query_tiles = dplyr::n_distinct(tile_index),
    requests = dplyr::n(),
    returned_features_before_deduplication = sum(returned_features),
    retained_unique_features = nrow(footprints),
    project_scope = nrow(projects)
  )

readr::write_csv(
  manifest,
  paste0(
    "../output/official_building_footprints_",
    snapshot_year,
    "_manifest.csv"
  )
)
