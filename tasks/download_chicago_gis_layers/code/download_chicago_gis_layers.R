# setwd("tasks/download_chicago_gis_layers/code")
# Three layers from the City of Chicago's data portal, saved as received, for placing zoning map amendments:
#   - the zoning districts in force (dataset dj47-wfun), as GeoJSON. A district set by an amendment since about 2002
#     carries the amendment's application number (`ordinance`) and passage date (`ordinance_1`). The source is live,
#     so a rerun is a deliberate refresh.
#   - the zoning districts of November 2012 (dataset p8va-airx, "deprecated October 2014"), a zipped shapefile, which
#     keeps districts amended again since.
#   - the street centerlines of 2013 (dataset xy4z-b6aa, "deprecated July 2013"), a zipped shapefile: the latest
#     centerlines the portal still serves as data (its later versions are map views without data).
# Each file is downloaded to ../temp, checked to open as a layer and moved to ../output. The record lists each
# layer's dataset, URL, file, bytes, SHA-256 and retrieval date.
portal <- "https://data.cityofchicago.org"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

layers <- tribble(
  ~layer, ~dataset, ~url, ~file, ~shapefile,
  "zoning_districts_current", "dj47-wfun", paste0(portal, "/api/geospatial/dj47-wfun?method=export&format=GeoJSON"),
  "zoning_districts_current.geojson", NA,
  "zoning_districts_2012", "p8va-airx", paste0(portal, "/download/p8va-airx/application%2Fzip"),
  "zoning_districts_2012.zip", "Zoning_nov2012.shp",
  "street_centerlines_2013", "xy4z-b6aa", paste0(portal, "/download/xy4z-b6aa/application%2Fzip"),
  "street_centerlines_2013.zip", "Transportation.shp")

for (i in seq_len(nrow(layers))) {
  received <- file.path("../temp", layers$file[i])
  httr2::request(layers$url[i]) |> httr2::req_retry(max_tries = 5) |> httr2::req_timeout(1800) |>
    httr2::req_perform(path = received)
  source_path <- if (is.na(layers$shapefile[i])) received else paste0("/vsizip/", received, "/", layers$shapefile[i])
  stopifnot(nrow(st_read(source_path, quiet = TRUE)) > 1000)
  stopifnot(file.rename(received, file.path("../output", layers$file[i])))
}

layers <- layers |>
  mutate(bytes = file.size(file.path("../output", file)),
    sha256 = vapply(file.path("../output", file), function(f) as.character(openssl::sha256(file(f))), character(1),
      USE.NAMES = FALSE),
    retrieved_on = Sys.Date())
SaveData(layers, "layer", "../output/chicago_gis_layers.csv")
