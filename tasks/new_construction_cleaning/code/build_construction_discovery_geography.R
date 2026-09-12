# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

# Discovery includes pre-study and post-study construction histories. Preserve
# the four original maps without expanding each geometry to annual copies.
wards_1998 <- st_read("../input/Chicago_Wards_1998.geojson", quiet = TRUE) |>
  janitor::clean_names() |>
  filter(!is.na(ward), ward != "OUT") |>
  transmute(year = 1998L, ward = as.integer(ward)) |>
  st_transform(3435)
wards_2003 <- st_read("../input/Wards_2014.geojson", quiet = TRUE) |>
  filter(ward != "OUT") |>
  transmute(year = 2003L, ward = as.integer(ward)) |>
  st_transform(3435)
wards_2015 <- st_read("../input/Wards_2015.geojson", quiet = TRUE) |>
  filter(ward != "OUT") |>
  transmute(year = 2015L, ward = as.integer(ward)) |>
  st_transform(3435)
wards_2024 <- st_read("../input/Wards_2024.geojson", quiet = TRUE) |>
  filter(ward != "OUT") |>
  transmute(year = 2024L, ward = as.integer(ward)) |>
  st_transform(3435)

wards <- bind_rows(wards_1998, wards_2003, wards_2015, wards_2024) |>
  arrange(ward, year)
if (nrow(wards) != 200L || anyNA(wards$ward) ||
    anyDuplicated(st_drop_geometry(wards)[c("year", "ward")]) ||
    any(st_is_empty(wards))) {
  stop("Construction discovery requires four complete, unique 50-ward maps.")
}
SaveData(wards, character(), "../output/construction_discovery_ward_maps.gpkg", delete_dsn = TRUE, quiet = TRUE)

boundaries <- build_canonical_boundary_list(wards)
for (era in names(boundaries)) {
  if (is.null(boundaries[[era]]) || nrow(boundaries[[era]]) == 0L ||
      anyDuplicated(boundaries[[era]]$ward_pair_id)) {
    stop("A discovery ward era has missing or duplicate boundaries: ", era)
  }
  SaveData(boundaries[[era]], character(), "../output/construction_discovery_boundaries.gpkg", layer = era, delete_layer = TRUE, quiet = TRUE)
}
