# setwd("tasks/new_construction_cleaning/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")

ledger <- readr::read_csv("../output/preferred_residential_project_ledger.csv",
  col_types = readr::cols(component_pins = readr::col_character(), class_values = readr::col_character(),
    .default = readr::col_guess()))
points <- sf::st_read("../output/preferred_residential_project_centroids.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>% inner_join(ledger %>% select(project_id, construction_year),
    by = "project_id", relationship = "one-to-one") %>%
  mutate(construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year))
stopifnot(!anyDuplicated(ledger$project_id), !anyDuplicated(points$project_id),
  setequal(points$project_id, ledger$project_id[ledger$location_resolved]))
ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>% sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
for (era_value in unique(points$era)) stopifnot(all(lengths(sf::st_within(
  points[points$era == era_value, ], ward_maps[[era_value]])) == 1L))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundary_lines, chunk_n = 2000L)
located <- bind_cols(sf::st_drop_geometry(points), assignment) %>%
  transmute(project_id, construction_year, construction_date, boundary_year, era,
    ward, neighbor_ward, ward_pair = ward_pair_id, distance_to_boundary_ft = dist_ft,
    within_1500ft = dist_ft <= 1500, within_500ft = dist_ft <= 500)
stopifnot(all(is.finite(located$distance_to_boundary_ft)), !anyNA(located$ward_pair))
boundary_scope <- ledger %>% select(project_id, construction_year, allow_far, allow_dupac, location_resolved) %>%
  left_join(located, by = c("project_id", "construction_year"), relationship = "one-to-one")
readr::write_csv(boundary_scope, "../output/preferred_residential_boundary_scope.csv")
