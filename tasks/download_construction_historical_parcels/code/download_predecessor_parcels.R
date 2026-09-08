# setwd("tasks/download_construction_historical_parcels/code")
# scope <- "initial"

library(dplyr)
library(readr)
library(sf)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(scope)
stopifnot(length(args) == 1L, args[1] %in% c("initial", "preferred"))
scope <- args[1]
if (scope == "initial") {
  queries <- read_csv("../output/predecessor_spatial_queries_download.csv", show_col_types = FALSE)
} else {
  queries <- read_csv("../output/preferred_predecessor_spatial_queries_download.csv", show_col_types = FALSE)
}
layers <- read_csv("../input/historical_project_parcel_layers.csv", show_col_types = FALSE)
stopifnot(!anyNA(queries), !anyDuplicated(queries), !anyDuplicated(layers$target_year),
          all(queries$target_year %in% layers$target_year))
parcels <- st_sf(target_year = integer(), layer_id = integer(), object_id = integer(),
  predecessor_pin14 = character(), predecessor_pin10 = character(),
  geometry_valid = logical(), geometry = st_sfc(crs = 3435))

request_parcels <- function(query) {
  request <- httr2::request(paste0(
    "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/", layer_id, "/query"))
  body <- do.call(httr2::req_url_query, c(list(request), query)) |>
    httr2::req_retry(max_tries = 5) |> httr2::req_timeout(180) |>
    httr2::req_perform() |> httr2::resp_body_string()
  result <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (!is.null(result$error) || isTRUE(result$exceededTransferLimit)) {
    stop("Historical parcel service returned an error or incomplete response.")
  }
  body
}

for (year in sort(unique(queries$target_year))) {
  layer_id <- layers$layer_id[match(year, layers$target_year)]
  points <- queries |> filter(target_year == year)
  object_ids <- integer()
  for (rows in split(seq_len(nrow(points)), ceiling(seq_len(nrow(points)) / 75))) {
    coordinates <- as.matrix(points[rows, c("reference_x_3435", "reference_y_3435")])
    body <- request_parcels(list(geometry = jsonlite::toJSON(list(
      points = unname(split(coordinates, row(coordinates))), spatialReference = list(wkid = 3435)), auto_unbox = TRUE),
      geometryType = "esriGeometryMultipoint", spatialRel = "esriSpatialRelIntersects",
      inSR = "3435", returnIdsOnly = "true", returnGeometry = "false", f = "json"))
    result <- jsonlite::fromJSON(body)
    stopifnot("objectIds" %in% names(result))
    object_ids <- c(object_ids, as.integer(result$objectIds))
  }
  object_ids <- sort(unique(object_ids))
  stopifnot(!anyNA(object_ids))
  for (ids in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
    body <- request_parcels(list(objectIds = paste(ids, collapse = ","), outFields = "*",
      returnGeometry = "true", outSR = "3435", f = "geojson"))
    writeLines(body, paste0("../temp/", scope, "_predecessor_parcels.geojson"), useBytes = TRUE)
    batch <- st_read(paste0("../temp/", scope, "_predecessor_parcels.geojson"), quiet = TRUE) |> st_transform(3435)
    pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(batch))[1]
    pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(batch))[1]
    stopifnot(!is.na(pin14_field), !is.na(pin10_field), !anyDuplicated(batch$OBJECTID),
              setequal(as.integer(batch$OBJECTID), ids))
    batch <- batch |> transmute(target_year = year, layer_id = .env$layer_id,
      object_id = as.integer(OBJECTID),
      predecessor_pin14 = gsub("[^0-9]", "", as.character(.data[[pin14_field]])),
      predecessor_pin10 = gsub("[^0-9]", "", as.character(.data[[pin10_field]])),
      geometry_valid = st_is_valid(geometry))
    parcels <- rbind(parcels, batch)
  }
}
stopifnot(!anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]),
          all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
parcels <- arrange(parcels, target_year, predecessor_pin10, predecessor_pin14, object_id)
st_write(parcels, paste0("../temp/", scope, "_predecessor_parcels_download.gpkg"),
  layer = "historical_project_predecessor_parcels", delete_dsn = TRUE, quiet = TRUE)
if (scope == "initial") {
  stopifnot(file.rename("../temp/initial_predecessor_parcels_download.gpkg", "../output/predecessor_parcels_download.gpkg"))
} else {
  stopifnot(file.rename("../temp/preferred_predecessor_parcels_download.gpkg", "../output/preferred_predecessor_parcels_download.gpkg"))
}
