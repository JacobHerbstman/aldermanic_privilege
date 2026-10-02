# setwd("tasks/download_parcel_history/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

tile_ft <- 5280  # side of the square query tiles (one mile)

# Cook County Clerk parcel maps (gis.cookcountyil.gov parcelHistorical/MapServer). Layer 19, "Parcel History
# 2000-2023", holds one polygon per 10-digit parcel in the maps of 2000-2023 with its first map year (InGIS) and
# last taxed year; the yearly layers 2018-2025 add the Chicago parcels the history layer lacks. The service returns
# at most 2,000 features per request.
service <- "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer"
# Responses are written to ../temp and read back; pages are stacked by binding their attributes and their geometries
# separately, since a page can mix polygon types or hold an empty geometry.
query <- function(layer, path, ...) {
  httr2::request(sprintf("%s/%s/query", service, layer)) |> httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |> httr2::req_timeout(300) |> httr2::req_perform(path = path)
  stopifnot(!any(grepl('"error"', readLines(path, n = 5L, warn = FALSE), fixed = TRUE)))
  st_read(path, quiet = TRUE)
}
stack <- function(pages) {
  pages <- Filter(\(p) !is.null(p) && nrow(p) > 0L, pages)
  if (!length(pages)) return(NULL)
  attributes <- bind_rows(lapply(pages, st_drop_geometry))
  geometry <- do.call(c, lapply(pages, \(p) st_set_crs(st_geometry(p), NA)))
  stopifnot(length(geometry) == nrow(attributes))
  st_sf(attributes, geometry = st_set_crs(geometry, 3435))
}
paged <- function(layer, name, ...) {
  pages <- list()
  repeat {
    page <- query(layer, sprintf("../temp/%s_%03d.geojson", name, length(pages)), ..., returnGeometry = "true", outSR = 3435,
      orderByFields = "OBJECTID", resultOffset = 2000L * length(pages), resultRecordCount = 2000L, f = "geojson")
    pages[[length(pages) + 1L]] <- page
    if (nrow(page) < 2000L) break
  }
  stack(pages)
}
centroids <- read_csv("../input/parcel_centroids.csv", col_types = cols(pin10 = "c", .default = "d"))

# The history layer in every one-mile tile that holds a Chicago parcel centroid; a parcel crossing a tile edge is kept once.
tiles <- centroids |> filter(is.finite(x_3435), is.finite(y_3435)) |>
  distinct(tile_x = floor(x_3435 / tile_ft), tile_y = floor(y_3435 / tile_ft)) |> arrange(tile_x, tile_y)
history <- map(seq_len(nrow(tiles)), \(i) {
  paged(19, sprintf("history_%d_%d", tiles$tile_x[i], tiles$tile_y[i]), where = "1=1", geometry = paste(tiles$tile_x[i] * tile_ft, tiles$tile_y[i] * tile_ft, (tiles$tile_x[i] + 1) * tile_ft,
    (tiles$tile_y[i] + 1) * tile_ft, sep = ","), geometryType = "esriGeometryEnvelope", inSR = 3435,
    spatialRel = "esriSpatialRelIntersects", outFields = "OBJECTID,Pin10,PARCELTYPE,InGIS,LastTaxed")
}, .progress = TRUE) |> stack() |> distinct(OBJECTID, .keep_all = TRUE) |>
  transmute(layer = "history", object_id = as.integer(OBJECTID), pin10 = Pin10, parcel_type = as.integer(PARCELTYPE),
    in_gis = as.integer(InGIS), last_taxed = as.integer(LastTaxed))
stopifnot(nrow(history) > 600000L, !anyNA(history$pin10))

# The yearly layers' polygons of the Chicago parcels missing from the history layer.
later <- setdiff(centroids$pin10, history$pin10)
yearly_layers <- c(`2018` = "20", `2019` = "21", `2020` = "22", `2021` = "23", `2022` = "2022", `2023` = "2023", `2024` = "2024", `2025` = "2025")
yearly <- map(names(yearly_layers), \(y) {
  batches <- split(later, ceiling(seq_along(later) / 150L))
  found <- map(seq_along(batches), \(b) {
    paged(yearly_layers[[y]], sprintf("parcels_%s_%03d", y, b), where = sprintf("PIN10 IN ('%s')", paste(batches[[b]], collapse = "','")), outFields = "OBJECTID,PIN10")
  }) |> stack()
  if (is.null(found)) return(NULL)  # a layer may hold none of the missing parcels
  found |> distinct(OBJECTID, .keep_all = TRUE) |>
    transmute(layer = y, object_id = as.integer(OBJECTID), pin10 = PIN10, parcel_type = NA_integer_, in_gis = NA_integer_, last_taxed = NA_integer_)
}, .progress = TRUE) |> stack()

parcels <- stack(list(history, yearly)) |> st_zm() |> arrange(layer, object_id)
message(sprintf("parcel polygons: %d; empty geometries: %d", nrow(parcels), sum(st_is_empty(parcels))))
stopifnot(all(grepl("^[0-9]+$", parcels$pin10)))
message(sprintf("parcel numbers not 10 digits long, kept as received: %d", sum(nchar(parcels$pin10) != 10L)))
message(sprintf("Chicago parcels missing from the history layer: %d; found in a yearly layer: %d", length(later), sum(later %in% yearly$pin10)))
SaveData(parcels, c("layer", "object_id"), "../output/parcel_history.gpkg", delete_dsn = TRUE, quiet = TRUE)
