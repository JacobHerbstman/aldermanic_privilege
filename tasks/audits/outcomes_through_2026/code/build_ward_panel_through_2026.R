# setwd("tasks/audits/outcomes_through_2026/code")
# Exploratory: the paper's annual ward panel (tasks/ward_panel_create; 2003 and 2015 maps, through 2022) with the
# 2023 ward map, in effect from May 15, 2023, added for 2024-2026. The shared geometry helpers read the post-2023
# map from year 2024 (canonical_map_year_for_era).
map_2023_years <- 2024:2026

source("../../../setup_environment/code/packages.R")

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
stopifnot(st_crs(ward_panel)$epsg == 3435, max(ward_panel$year) == 2022)
wards_2023 <- st_read("../input/Wards_2024.geojson", quiet = TRUE) |>
  transmute(ward = as.numeric(ward)) |>
  st_make_valid() |>
  st_transform(3435)
stopifnot(nrow(wards_2023) == 50, !anyDuplicated(wards_2023$ward))
st_geometry(wards_2023) <- "geom"
st_geometry(ward_panel) <- "geom"
ward_panel <- rbind(
  select(ward_panel, year, ward, geom),
  tidyr::crossing(st_drop_geometry(wards_2023), year = map_2023_years) |>
    left_join(select(wards_2023, ward, geom), by = "ward", relationship = "many-to-one") |>
    st_as_sf() |>
    select(year, ward, geom)
)
stopifnot(!anyDuplicated(st_drop_geometry(ward_panel)[c("ward", "year")]))
st_write(ward_panel, "../output/ward_panel_through_2026.gpkg", delete_dsn = TRUE, quiet = TRUE)
