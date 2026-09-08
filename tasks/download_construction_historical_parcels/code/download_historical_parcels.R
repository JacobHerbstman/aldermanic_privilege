# setwd("tasks/download_construction_historical_parcels/code")
# target_year <- 2006L
# scope <- "historical"

library(dplyr)
library(readr)
library(sf)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(target_year, scope)
stopifnot(length(args) == 2L, args[2] %in% c("historical", "preferred"))
target_year <- as.integer(args[1])
scope <- args[2]
layers <- read_csv("../input/historical_project_parcel_layers.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(layers$target_year), target_year %in% layers$target_year)
layer_id <- layers$layer_id[match(target_year, layers$target_year)]
requests <- read_csv(paste0("../output/", scope, "_parcel_queries_additional.csv"),
  col_types = cols(target_year = col_integer(), pin10 = col_character(), .default = col_skip())) |>
  filter(target_year == .env$target_year) |>
  distinct(pin10) |>
  arrange(pin10)
stopifnot(!anyNA(requests$pin10), all(grepl("^[0-9]{10}$", requests$pin10)))

# The same endpoint serves both ID queries and complete geometry batches.
request_parcels <- function(query) {
  request <- httr2::request(paste0(
    "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/", layer_id, "/query"))
  request <- do.call(httr2::req_url_query, c(list(request), query))
  body <- request |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(180) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  result <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (!is.null(result$error) || isTRUE(result$exceededTransferLimit)) {
    stop("Historical parcel service returned an error or incomplete response.")
  }
  body
}

object_ids <- integer()
for (rows in split(seq_len(nrow(requests)), ceiling(seq_len(nrow(requests)) / 75))) {
  body <- request_parcels(list(where = paste0("PIN10 IN ('", paste(requests$pin10[rows], collapse = "','"), "')"),
    returnIdsOnly = "true", returnGeometry = "false", f = "json"))
  result <- jsonlite::fromJSON(body)
  stopifnot("objectIds" %in% names(result))
  object_ids <- c(object_ids, as.integer(result$objectIds))
}
object_ids <- sort(unique(object_ids))
stopifnot(!anyNA(object_ids))

parcels <- st_sf(target_year = integer(), layer_id = integer(), object_id = integer(),
  pin14 = character(), pin10 = character(), geometry_valid = logical(), geometry = st_sfc(crs = 3435))
for (ids in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
  body <- request_parcels(list(objectIds = paste(ids, collapse = ","), outFields = "*",
    returnGeometry = "true", outSR = "3435", f = "geojson"))
  writeLines(body, paste0("../temp/", scope, "_parcels_", target_year, ".geojson"), useBytes = TRUE)
  batch <- st_read(paste0("../temp/", scope, "_parcels_", target_year, ".geojson"), quiet = TRUE) |>
    st_transform(3435)
  pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(batch))[1]
  pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(batch))[1]
  stopifnot(!is.na(pin14_field), !is.na(pin10_field), !anyDuplicated(batch$OBJECTID),
            setequal(as.integer(batch$OBJECTID), ids))
  batch <- batch |>
    transmute(target_year = .env$target_year, layer_id = .env$layer_id,
      object_id = as.integer(OBJECTID), pin14 = gsub("[^0-9]", "", as.character(.data[[pin14_field]])),
      pin10 = gsub("[^0-9]", "", as.character(.data[[pin10_field]])), geometry_valid = st_is_valid(geometry))
  parcels <- rbind(parcels, batch)
}
stopifnot(setequal(parcels$object_id, object_ids), !anyDuplicated(parcels$object_id),
          all(parcels$pin10 %in% requests$pin10), all(st_is_valid(parcels)), !any(st_is_empty(parcels)))
parcels <- arrange(parcels, target_year, pin10, pin14, object_id)
st_write(parcels, paste0("../temp/", scope, "_parcels_additional_", target_year, ".gpkg"),
  layer = "historical_parcels", delete_dsn = TRUE, quiet = TRUE)
stopifnot(file.rename(paste0("../temp/", scope, "_parcels_additional_", target_year, ".gpkg"),
                      paste0("../output/", scope, "_parcels_additional_", target_year, ".gpkg")))
