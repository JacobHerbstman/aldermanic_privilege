# setwd("tasks/working_paper_release_audit/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/assessor_classification.R")
source("../../shared/code/canonical_geometry_helpers.R")

scope <- readr::read_csv("../input/preferred_project_boundary_scope.csv", col_types = readr::cols(
  project_id = "c", source_family = "c", candidate_status = "c", location_source = "c",
  target_year = "d", boundary_year = "d", era = "c", ward = "d", ward_pair = "c",
  distance_to_boundary_ft = "d", project_land_area_sqft = "d", .default = readr::col_skip()))
projects <- readr::read_csv("../input/preferred_residential_project_candidates.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", construction_year = "d",
    dwelling_units = "d", building_sqft = "d", land_sqft = "d", .default = readr::col_skip()))
review <- scope %>% filter(source_family == "residential", candidate_status == "retain_mechanical",
  location_source == "former_parcel_centroid_unresolved_individual") %>%
  select(project_id, target_year, boundary_year, era, old_ward = ward,
    old_ward_pair = ward_pair, old_distance_ft = distance_to_boundary_ft,
    historical_land_sqft = project_land_area_sqft) %>%
  left_join(projects %>% select(project_id, pin = component_pins, construction_year,
    dwelling_units, building_sqft, land_sqft), by = "project_id", relationship = "one-to-one")
stopifnot(!anyDuplicated(review$project_id), !anyDuplicated(review$pin))
current <- readr::read_csv("../input/parcel_universe_2025_city.csv",
  col_types = readr::cols(pin = readr::col_character(), longitude = readr::col_double(),
    latitude = readr::col_double(), row_id = readr::col_character(), .default = readr::col_skip()))
stopifnot(!anyDuplicated(current$pin))
review <- review %>% left_join(current, by = "pin", relationship = "one-to-one") %>%
  mutate(has_current_point = is.finite(longitude) & is.finite(latitude),
    historical_to_reported_land_ratio = historical_land_sqft / land_sqft)

# Does the coordinate-year assessment still describe the selected building?
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "review_pins", review %>% select(pin))
history <- DBI::dbGetQuery(con, "SELECT h.pin, h.tax_year, h.card_num, h.class,
  h.year_built, h.building_sqft, h.land_sqft, h.num_apartments, h.row_id
  FROM read_parquet('../input/residential_assessor_history.parquet') h
  INNER JOIN review_pins p ON h.pin = p.pin
  WHERE h.tax_year = 2025 AND h.building_sqft > 1")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(history[c("pin", "tax_year", "card_num")]))
counts <- history %>% count(pin, name = "occupied_cards_2025")
one_card <- history %>% semi_join(counts %>% filter(occupied_cards_2025 == 1L), by = "pin") %>%
  transmute(pin, assessment_row_2025 = row_id, year_2025 = year_built,
    building_sqft_2025 = building_sqft, land_sqft_2025 = land_sqft,
    units_2025 = if_else(class %in% single_family_assessor_classes, 1, num_apartments))
review <- review %>% left_join(counts, by = "pin", relationship = "one-to-one") %>%
  left_join(one_card, by = "pin", relationship = "one-to-one") %>% mutate(
    same_building_measurements = coalesce(building_sqft_2025 == building_sqft &
      land_sqft_2025 == land_sqft & units_2025 == dwelling_units, FALSE),
    same_construction_year = coalesce(year_2025 == construction_year, FALSE))
polygons <- sf::st_read("../input/preferred_project_year_geometry.gpkg", quiet = TRUE)
old_points <- sf::st_read("../input/preferred_project_year_centroids.gpkg", quiet = TRUE)
stopifnot(!anyDuplicated(polygons$project_id), !anyDuplicated(old_points$project_id))
points <- review %>% filter(has_current_point) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% sf::st_transform(3435)
site <- polygons[match(points$project_id, polygons$project_id), ]
old <- old_points[match(points$project_id, old_points$project_id), ]
stopifnot(!anyNA(site$project_id), !anyNA(old$project_id), sf::st_crs(site)$epsg == 3435)
# Require containment in this record's own site, not any other reviewed site's polygon.
points$inside_historical_site <- vapply(seq_len(nrow(points)), function(i)
  length(sf::st_within(points[i, ], site[i, ])[[1]]) == 1L, logical(1))
points$outside_site_ft <- as.numeric(sf::st_distance(points, site, by_element = TRUE))
points$point_shift_ft <- as.numeric(sf::st_distance(points, old, by_element = TRUE))
ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>% sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel, eras = unique(points$era))
boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = unique(points$era))
assignment <- assign_points_to_boundaries(points, points$era, ward_maps, boundaries, chunk_n = 2000L)
comparison <- bind_cols(sf::st_drop_geometry(points), assignment) %>%
  transmute(project_id, inside_historical_site, outside_site_ft, point_shift_ft,
    new_ward = ward, new_ward_pair = ward_pair_id, new_distance_ft = dist_ft)
review <- review %>% left_join(comparison, by = "project_id", relationship = "one-to-one") %>%
  mutate(conservative_rule_pass = has_current_point & same_building_measurements &
    same_construction_year & coalesce(inside_historical_site, FALSE) & is.finite(new_distance_ft),
    distance_change_ft = new_distance_ft - old_distance_ft,
    enters_500ft = old_distance_ft > 500 & new_distance_ft <= 500,
    leaves_500ft = old_distance_ft <= 500 & new_distance_ft > 500,
    ward_changes = old_ward != new_ward, pair_changes = old_ward_pair != new_ward_pair,
    rule_result = case_when(
      !has_current_point ~ "no_current_exact_parcel_point",
      !same_building_measurements ~ "assessment_measurements_need_check",
      !same_construction_year ~ "assessment_year_differs",
      !inside_historical_site ~ "point_outside_historical_site",
      !is.finite(new_distance_ft) ~ "no_boundary_assignment",
      TRUE ~ "same_building_and_lot_current_point_inside_historical_site"))
paper <- readr::read_csv("../input/frozen_construction_analysis.csv",
  col_types = readr::cols(project_id = "c", distance_to_boundary_ft = "d",
    within_500ft = "l", .default = readr::col_skip())) %>%
  select(project_id, paper_distance_ft = distance_to_boundary_ft, paper_within_500ft = within_500ft)
stopifnot(!anyDuplicated(paper$project_id))
review <- review %>% left_join(paper, by = "project_id", relationship = "one-to-one") %>%
  mutate(exact_id_in_paper = project_id %in% paper$project_id) %>% arrange(project_id)
readr::write_csv(review, "../output/individual_home_location_comparison.csv")
