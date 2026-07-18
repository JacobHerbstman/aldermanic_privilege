# setwd("tasks/audits/density_multicard_manual_review/code")

library(dplyr)
library(httr)
library(jsonlite)
library(readr)
library(sf)
library(tibble)

options(timeout = 120)

review_parcels <- tribble(
  ~pin,              ~construction_year, ~assessor_cards,
  "11303200330000", 2008,               9,
  "13141270250000", 2011,               4,
  "13322180280000", 2022,               2,
  "14074070280000", 2006,               2,
  "14074070290000", 2006,               2,
  "14301010390000", 2021,               2,
  "17173330340000", 2008,               4,
  "19083090620000", 2022,               2,
  "20211200220000", 2006,               5,
  "20211200230000", 2006,               5,
  "20211200290000", 2006,               5,
  "20231160220000", 2006,               2,
  "25194060030000", 2008,               2
)

read_arcgis_geojson <- function(url, query) {
  response_body <- NULL
  for (attempt in 1:5) {
    response <- try(GET(url, query = c(query, f = "geojson")), silent = TRUE)
    if (inherits(response, "try-error")) {
      Sys.sleep(attempt)
      next
    }
    response_body <- content(response, as = "raw")
    if (status_code(response) == 200 &&
        grepl("FeatureCollection", rawToChar(response_body), fixed = TRUE)) {
      break
    }
    Sys.sleep(attempt)
  }
  if (inherits(response, "try-error")) {
    stop(response)
  }
  if (status_code(response) != 200 ||
      !grepl("FeatureCollection", rawToChar(response_body), fixed = TRUE)) {
    stop(rawToChar(response_body))
  }
  path <- tempfile(fileext = ".geojson")
  writeBin(response_body, path)
  st_read(path, quiet = TRUE)
}

read_arcgis_spatial_geojson <- function(url, envelope, out_fields) {
  id_result <- NULL
  for (attempt in 1:5) {
    id_response <- try(GET(
      url,
      query = list(
        where = "1=1",
        geometry = envelope,
        geometryType = "esriGeometryEnvelope",
        inSR = "3435",
        spatialRel = "esriSpatialRelIntersects",
        returnIdsOnly = "true",
        f = "json"
      )
    ), silent = TRUE)
    if (inherits(id_response, "try-error")) {
      Sys.sleep(attempt)
      next
    }
    stop_for_status(id_response)
    id_result <- fromJSON(content(id_response, as = "text"))
    if (is.null(id_result$error)) {
      break
    }
    Sys.sleep(attempt)
  }
  if (is.null(id_result)) {
    stop(id_response)
  }
  if (!is.null(id_result$error)) {
    stop(id_result$error$message)
  }
  object_ids <- id_result$objectIds
  if (length(object_ids) == 0) {
    return(st_sf(geometry = st_sfc(crs = 3435)))
  }

  bind_rows(lapply(object_ids, function(object_id) {
    read_arcgis_geojson(
      url,
      list(
        objectIds = object_id,
        outFields = out_fields,
        returnGeometry = "true",
        outSR = "3435"
      )
    )
  }))
}

pin_sql <- paste0("'", review_parcels$pin, "'", collapse = ",")

parcels <- read_arcgis_geojson(
  "https://gis.cookcountyil.gov/traditional/rest/services/CookViewer3Parcels/MapServer/0/query",
  list(
    where = paste0("PIN14 IN (", pin_sql, ")"),
    outFields = paste(c(
      "PIN14", "street_address", "city_state_zip", "LANDSF", "BLDGSQFT",
      "BLDGAGE", "assessor_link", "XMin", "XMax", "YMin", "YMax"
    ), collapse = ","),
    returnGeometry = "true",
    outSR = "3435"
  )
) |>
  rename(pin = PIN14) |>
  left_join(review_parcels, by = "pin") |>
  arrange(pin)

footprints_2008 <- list()
footprints_2022 <- list()

for (i in seq_len(nrow(parcels))) {
  bbox <- st_bbox(parcels[i, ])
  envelope <- paste(bbox[c("xmin", "ymin", "xmax", "ymax")], collapse = ",")

  footprints_2008[[i]] <- read_arcgis_spatial_geojson(
    "https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2008/MapServer/1/query",
    envelope,
    paste(c(
      "OBJECTID", "BLDG_ID", "F_ADD1", "T_ADD1", "PRE_DIR1", "ST_NAME1",
      "ST_TYPE1", "YEAR_BUILT", "BLDG_SQ_FO", "NO_OF_UNIT"
    ), collapse = ",")
  ) |>
    mutate(pin = parcels$pin[i], source_year = 2008L)

  footprints_2022[[i]] <- read_arcgis_spatial_geojson(
    "https://gis.cookcountyil.gov/traditional/rest/services/Planim/CookCountyBuildings/MapServer/0/query",
    envelope,
    paste(c("OBJECTID", "Area_SQFT", "Year", "Height"), collapse = ",")
  ) |>
    mutate(pin = parcels$pin[i], source_year = 2022L)
}

footprints_2008 <- bind_rows(footprints_2008)
footprints_2022 <- bind_rows(footprints_2022)

summarize_overlap <- function(parcel, footprints) {
  candidate <- filter(footprints, pin == parcel$pin)
  if (nrow(candidate) == 0) {
    return(tibble(
      footprint_count = 0L,
      footprint_area_on_parcel = 0,
      footprint_area_total = 0
    ))
  }

  overlap <- suppressWarnings(st_intersection(candidate, st_geometry(parcel))) |>
    mutate(overlap_area = as.numeric(st_area(geometry))) |>
    filter(overlap_area >= 25)

  tibble(
    footprint_count = nrow(overlap),
    footprint_area_on_parcel = sum(overlap$overlap_area),
    footprint_area_total = sum(as.numeric(st_area(candidate)))
  )
}

summary_2008 <- bind_rows(lapply(
  seq_len(nrow(parcels)),
  function(i) summarize_overlap(parcels[i, ], footprints_2008)
)) |>
  rename_with(~ paste0(.x, "_2008"))

summary_2022 <- bind_rows(lapply(
  seq_len(nrow(parcels)),
  function(i) summarize_overlap(parcels[i, ], footprints_2022)
)) |>
  rename_with(~ paste0(.x, "_2022"))

parcel_summary <- bind_cols(st_drop_geometry(parcels), summary_2008, summary_2022) |>
  mutate(
    footprint_far_2008 = footprint_area_on_parcel_2008 / LANDSF,
    footprint_far_2022 = footprint_area_on_parcel_2022 / LANDSF,
    google_maps = paste0(
      "https://www.google.com/maps/search/?api=1&query=",
      URLencode(paste(street_address, city_state_zip, sep = ", "), reserved = TRUE)
    )
  )

write_csv(parcel_summary, "../output/parcel_footprint_crosswalk.csv")

pdf("../output/parcel_footprint_maps.pdf", width = 8.5, height = 8.5)
for (i in seq_len(nrow(parcels))) {
  parcel <- parcels[i, ]
  f2008 <- filter(footprints_2008, pin == parcel$pin)
  f2022 <- filter(footprints_2022, pin == parcel$pin)
  map_bbox <- st_bbox(st_buffer(parcel, 40))

  plot(
    st_geometry(parcel),
    col = "grey95",
    border = "black",
    lwd = 3,
    xlim = map_bbox[c("xmin", "xmax")],
    ylim = map_bbox[c("ymin", "ymax")],
    main = paste(parcel$pin, parcel$street_address, sep = "\n")
  )
  if (nrow(f2008) > 0) {
    plot(st_geometry(f2008), add = TRUE, border = "#2166ac", lwd = 2)
  }
  if (nrow(f2022) > 0) {
    plot(st_geometry(f2022), add = TRUE, border = "#b2182b", lwd = 2, lty = 2)
  }
  legend(
    "bottomleft",
    legend = c("Parcel", "2008 footprint", "2022 footprint"),
    col = c("black", "#2166ac", "#b2182b"),
    lty = c(1, 1, 2),
    lwd = c(3, 2, 2),
    bty = "n"
  )
}
dev.off()

write_csv(
  st_drop_geometry(footprints_2008),
  "../output/footprints_2008_attributes.csv"
)
