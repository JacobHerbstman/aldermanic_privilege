# setwd("tasks/download_building_footprints_2022/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

tile_ft <- 5280  # side of the square query tiles (one mile)

# Cook County GIS Building Footprints 2022 (buildingFootprint_2022/MapServer/0): one polygon per building from 2022
# imagery, with footprint area and height (highest roof point above ground, ft). The service returns at most 2,000
# features per request, so Chicago is queried in square tiles, each tile that holds a Chicago parcel centroid, paged
# by object ID. A footprint crossing a tile edge is returned by both tiles and kept once.
tiles <- read_csv("../input/parcel_centroids.csv", col_types = cols(pin10 = "c", .default = "d")) |>
  filter(is.finite(x_3435), is.finite(y_3435)) |>
  distinct(tile_x = floor(x_3435 / tile_ft), tile_y = floor(y_3435 / tile_ft)) |> arrange(tile_x, tile_y)
footprints <- map(seq_len(nrow(tiles)), \(i) {
  envelope <- paste(tiles$tile_x[i] * tile_ft, tiles$tile_y[i] * tile_ft, (tiles$tile_x[i] + 1) * tile_ft,
    (tiles$tile_y[i] + 1) * tile_ft, sep = ",")
  pages <- list()
  repeat {
    body <- httr2::request("https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query") |>
      httr2::req_url_query(where = "1=1", geometry = envelope, geometryType = "esriGeometryEnvelope", inSR = 3435,
        spatialRel = "esriSpatialRelIntersects", outFields = "OBJECTID,Area_SQFT,Year,Ground_Z,Max_Point,Height",
        returnGeometry = "true", outSR = 3435, orderByFields = "OBJECTID", resultOffset = 2000L * length(pages),
        resultRecordCount = 2000L, f = "geojson") |>
      httr2::req_retry(max_tries = 5) |> httr2::req_timeout(300) |> httr2::req_perform() |> httr2::resp_body_string()
    stopifnot(!grepl('"error"', body, fixed = TRUE))
    page <- st_read(body, quiet = TRUE)
    if (nrow(page) == 0L) break
    pages[[length(pages) + 1L]] <- page
    if (nrow(page) < 2000L) break
  }
  bind_rows(pages)
}, .progress = TRUE) |> bind_rows()

footprints <- footprints |> distinct(OBJECTID, .keep_all = TRUE) |> st_zm() |> st_set_crs(3435) |>
  transmute(object_id = as.integer(OBJECTID), footprint_sqft = Area_SQFT, image_year = as.integer(Year),
    ground_z = Ground_Z, max_point = Max_Point, height_ft = Height)
stopifnot(nrow(footprints) > 500000L, !anyNA(footprints$object_id), all(footprints$image_year == 2022L),
  all(abs(footprints$max_point - footprints$ground_z - footprints$height_ft) < 0.01, na.rm = TRUE))
SaveData(footprints, "object_id", "../output/building_footprints_2022_chicago.gpkg", delete_dsn = TRUE, quiet = TRUE)
