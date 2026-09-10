# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

wards <- st_read("../output/construction_discovery_ward_maps.gpkg", quiet = TRUE)
boundaries <- build_canonical_boundary_list(wards)
for (era in names(boundaries)) {
  if (is.null(boundaries[[era]]) || nrow(boundaries[[era]]) == 0L ||
      anyDuplicated(boundaries[[era]]$ward_pair_id)) {
    stop("A discovery ward era has missing or duplicate boundaries: ", era)
  }
  st_write(boundaries[[era]], "../output/construction_discovery_boundaries.gpkg",
           layer = era, delete_layer = TRUE, quiet = TRUE)
}
