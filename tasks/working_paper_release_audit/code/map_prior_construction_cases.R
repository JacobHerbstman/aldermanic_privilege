# setwd("tasks/working_paper_release_audit/code")
library(sf)
library(dplyr)
library(readr)

footprints <- bind_rows(
  st_read("../input/lincoln_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "Lincoln"),
  st_read("../input/campbell_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "Campbell"),
  st_read("../input/seeley_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "Seeley"),
  st_read("../input/street38_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "38th Street"),
  st_read("../input/calumet_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "Calumet"),
  st_read("../input/marquette_footprints_2022.geojson", quiet = TRUE) |> mutate(site = "Marquette")) |> st_transform(3435)
stopifnot(all(st_is_valid(footprints)), !any(st_is_empty(footprints)))
old <- st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) |>
  transmute(pin14 = predecessor_pin14, target_year, geom)
lots <- st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE) |>
  select(pin14, target_year, geom) |> bind_rows(old)
stopifnot(st_crs(lots)$epsg == 3435)
review <- read_csv("../reference/prior_construction_cases.csv", show_col_types = FALSE) |>
  filter(site %in% footprints$site)
candidates <- read_csv("../input/preferred_residential_project_candidates.csv", show_col_types = FALSE,
  col_types = cols(class_values = col_character()))
universe <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(
  pin = col_character(), longitude = col_double(), latitude = col_double(), .default = col_skip()))
stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(universe$pin))
pdf("../output/prior_construction_case_maps.pdf", width = 9, height = 9)
for (i in seq_len(nrow(review))) {
  pins <- strsplit(candidates$component_pins[match(review$project_id[i], candidates$project_id)], "/", fixed = TRUE)[[1]]
  point_rows <- universe |> filter(pin %in% pins, is.finite(longitude), is.finite(latitude))
  shapes <- footprints |> filter(site == review$site[i])
  # Show the latest available mapped boundary of each old PIN as comparison evidence.
  # Different map years are labeled; these plots do not assign production geography.
  parcels <- lots |> filter(pin14 %in% pins) |> group_by(pin14) |>
    filter(target_year == max(target_year)) |> group_by(pin14, target_year) |>
    summarise(geom = st_union(geom), .groups = "drop")
  plot(st_geometry(shapes), col = "gray85", border = "gray40",
    main = paste0(review$site[i], ": 2022 building outlines"), axes = TRUE)
  if (nrow(parcels)) {
    plot(st_geometry(parcels), add = TRUE, border = "blue", lwd = 2)
    labels <- suppressWarnings(st_point_on_surface(parcels))
    text(st_coordinates(labels), labels = paste0(substr(parcels$pin14, 8, 10), " / ", parcels$target_year),
      col = "blue", cex = .8, pos = 3)
  }
  if (nrow(point_rows)) {
    points <- st_as_sf(point_rows, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
    plot(st_geometry(points), add = TRUE, col = "red", pch = 16)
  }
  legend("bottomleft", legend = c("2022 building outlines", "Available historical lots: PIN suffix / map year", "2025 parcel reference points"),
    fill = c("gray85", NA, NA), border = c("gray40", NA, NA), col = c(NA, "blue", "red"),
    lty = c(NA, 1, NA), pch = c(NA, NA, 16), bg = "white", cex = .75)
}
dev.off()
