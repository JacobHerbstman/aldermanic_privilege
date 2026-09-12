# setwd("tasks/working_paper_release_audit/code")

source("../../setup_environment/code/packages.R")

scope <- readr::read_csv("../output/multicard_land_scope.csv", show_col_types = FALSE)
projects <- readr::read_csv("../input/projects.csv", show_col_types = FALSE)
matches <- readr::read_csv("../input/multicard_component_successor_matches.csv",
  col_types = cols(.default = col_guess(), successor_pin = col_character()))
geometry <- sf::st_read("../reference/multicard_project_query_geometries.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>% filter(project_id %in% scope$project_id)
stopifnot(!anyDuplicated(scope$project_id), !anyDuplicated(projects$project_id),
  !anyDuplicated(geometry$project_id), !anyDuplicated(matches$successor_id))

current <- data.table::fread("../input/parcel_universe_2025_city.csv",
  select = c("pin", "class", "centroid_x_crs_3435", "centroid_y_crs_3435"),
  colClasses = "character") %>% as_tibble() %>%
  mutate(x = as.numeric(centroid_x_crs_3435), y = as.numeric(centroid_y_crs_3435)) %>%
  filter(is.finite(x), is.finite(y))
stopifnot(!anyDuplicated(current$pin))
points <- sf::st_as_sf(current, coords = c("x", "y"), crs = 3435)
# Candidate point containment is evidence of land scope, not an accepted match.
# Buffers are not parcel boundaries and cannot establish land allocation.
coverage <- sf::st_drop_geometry(geometry) %>%
  select(project_id, geometry_project_id, construction_year, query_geometry_source) %>%
  mutate(polygon_area_sqft = as.numeric(sf::st_area(geometry)),
    current_points_inside = NA_integer_, matched_points_inside = NA_integer_,
    matched_points_outside = NA_integer_, other_individual_projects_inside = NA_integer_,
    other_individual_project_ids = NA_character_)
inside_by_project <- sf::st_contains(geometry, points)
for (i in seq_len(nrow(geometry))) {
  if (geometry$query_geometry_source[i] != "construction_year_parcel_polygon") next
  inside <- seq_len(nrow(points)) %in% inside_by_project[[i]]
  matched_pins <- matches$successor_pin[matches$project_id == geometry$project_id[i]]
  other_ids <- paste0("residential_", current$pin[inside & !current$pin %in% matched_pins])
  other_ids <- sort(intersect(other_ids, projects$project_id))
  coverage$current_points_inside[i] <- sum(inside)
  coverage$matched_points_inside[i] <- sum(current$pin[inside] %in% matched_pins)
  coverage$matched_points_outside[i] <- sum(current$pin[!inside] %in% matched_pins)
  coverage$other_individual_projects_inside[i] <- length(other_ids)
  coverage$other_individual_project_ids[i] <- paste(other_ids, collapse = "/")
}
review <- scope %>%
  left_join(coverage, by = c("project_id", "construction_year"), relationship = "one-to-one") %>%
  mutate(
    geometry_review_available = coalesce(query_geometry_source == "construction_year_parcel_polygon", FALSE),
    parent_land_ratio_above_two = coalesce(parent_to_matched_successor_land_ratio > 2, FALSE),
    matched_land_comparison_status = case_when(
      is.na(matched_cards) ~ "no_recorded_successor_match",
      !all_target_cards_matched ~ "incomplete_card_matches",
      !matched_cards_reproduce_final_units | !matched_cards_reproduce_final_building ~ "matched_totals_differ",
      TRUE ~ "complete_card_matches"),
    additional_retained_project_inside = coalesce(other_individual_projects_inside > 0, FALSE)
  ) %>% arrange(project_id)
stopifnot(nrow(review) == nrow(scope), !anyDuplicated(review$project_id))
readr::write_csv(review, "../output/multicard_parent_coverage.csv", na = "")
