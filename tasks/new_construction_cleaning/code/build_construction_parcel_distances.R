# setwd("tasks/new_construction_cleaning/code")
# minimum_construction_year <- 1999
# maximum_construction_year <- 2026

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")
args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(minimum_construction_year, maximum_construction_year)
if (length(args) != 2L) stop("Expected the first and last discovery construction years.")
minimum_construction_year <- as.integer(args[1])
maximum_construction_year <- as.integer(args[2])
if (anyNA(c(minimum_construction_year, maximum_construction_year)) ||
    minimum_construction_year > maximum_construction_year) stop("Invalid discovery window.")

parcels <- st_read("../output/geocoded_residential_data.gpkg", quiet = TRUE) |>
  st_transform(3435) |>
  filter(between(yearbuilt, minimum_construction_year, maximum_construction_year)) |>
  mutate(construction_date = as.Date(sprintf("%d-06-15", yearbuilt)),
         era = canonical_era_from_date(construction_date))
if (anyNA(parcels$pin) || anyDuplicated(parcels$pin)) stop("Discovery parcel PINs must be unique.")
ward_maps <- load_canonical_ward_maps(
  st_read("../output/construction_discovery_ward_maps.gpkg", quiet = TRUE)
)
boundaries <- load_boundary_layers("../output/construction_discovery_boundaries.gpkg")
assignments <- assign_points_to_boundaries(parcels, parcels$era, ward_maps, boundaries, chunk_n = 2000L)
ward_has_boundary <- rep(FALSE, nrow(parcels))
for (era in names(boundaries)) {
  ward_has_boundary[parcels$era == era] <- assignments$ward[parcels$era == era] %in%
    union(boundaries[[era]]$ward_a, boundaries[[era]]$ward_b)
}
if (nrow(assignments) != nrow(parcels) ||
    any(ward_has_boundary & !is.finite(assignments$dist_ft))) {
  stop("A discovery parcel assigned to a ward lacks its boundary distance.")
}
tibble(pin = parcels$pin, dist_to_boundary_m = assignments$dist_ft * 0.3048,
       boundary_assignment_status = case_when(
         is.na(assignments$ward) ~ "outside_ward_map",
         !ward_has_boundary ~ "ward_has_no_shared_boundary_in_source_map",
         TRUE ~ "assigned")) |>
  write_csv("../output/construction_parcel_boundary_distances.csv")
