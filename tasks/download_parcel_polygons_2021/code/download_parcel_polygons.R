# setwd("tasks/download_parcel_polygons_2021/code")
# Lot areas of Chicago's tax parcels from the Cook County GIS parcel polygons for tax year 2021 ("ccgisdata - Parcel
# 2021", dataset 77tz-riq7, a fixed historical vintage). One query per 3-digit parcel-number prefix keeps each response
# small; responses are written to ../temp and read back. A parcel drawn as several polygons gets
# their summed area, in square feet in the Illinois State Plane projection (EPSG:3435).
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

prefixes <- sprintf("%03d", 0:339)
for (prefix in prefixes) {
  httr2::request("https://datacatalog.cookcountyil.gov/resource/77tz-riq7.geojson") |>
    httr2::req_url_query(`$select` = "pin10, the_geom",
      `$where` = sprintf("municipality = 'Chicago' AND starts_with(pin10, '%s')", prefix),
      `$order` = "pin10", `$limit` = 500000L) |>
    httr2::req_retry(max_tries = 5) |> httr2::req_timeout(600) |> httr2::req_perform(path = sprintf("../temp/parcels_2021_%s.geojson", prefix))
}

areas <- map(prefixes, function(prefix) {
  polygons <- st_read(sprintf("../temp/parcels_2021_%s.geojson", prefix), quiet = TRUE)
  if (nrow(polygons) == 0) return(NULL)
  polygons |>
    st_transform(3435) |>
    mutate(area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    summarise(lot_sqft = sum(area), polygons = n(), .by = pin10)
}) |> bind_rows()
stopifnot(nrow(areas) > 600000L, !anyDuplicated(areas$pin10), all(grepl("^[0-9]{10}$", areas$pin10)),
  all(areas$lot_sqft > 0))
SaveData(areas, "pin10", "../output/parcel_lot_areas_2021.csv")
