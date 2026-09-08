# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)
library(sf)

review <- read_csv("../output/candidate_geography_review_queue.csv", show_col_types = FALSE) |>
  filter(source_family == "residential", is.finite(history_x_3435), is.finite(history_y_3435)) |>
  select(case_id, project_id, target_year, history_year, history_x_3435, history_y_3435)
candidates <- read_csv("../input/preferred_residential_project_candidates.csv", col_types = cols(
  component_pins = col_character(), .default = col_guess()))
universe <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(
  pin = col_character(), longitude = col_double(), latitude = col_double(), .default = col_skip()))
stopifnot(!anyDuplicated(review$case_id), !anyDuplicated(candidates$project_id), !anyDuplicated(universe$pin))
current <- candidates |> filter(component_count == 1, dwelling_units == 1,
    candidate_status == "retain_mechanical") |>
  inner_join(universe, by = c("component_pins" = "pin"), relationship = "many-to-one") |>
  filter(is.finite(longitude), is.finite(latitude))
old_points <- st_as_sf(review, coords = c("history_x_3435", "history_y_3435"), crs = 3435)
current_points <- st_as_sf(current, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
nearest <- st_nearest_feature(old_points, current_points)
review$nearest_current_project_id <- current$project_id[nearest]
review$nearest_current_pin <- current$component_pins[nearest]
review$nearest_current_year <- current$construction_year[nearest]
review$nearest_current_building_sqft <- current$building_sqft[nearest]
review$nearest_current_land_sqft <- current$land_sqft[nearest]
review$point_difference_ft <- as.numeric(st_distance(old_points, current_points[nearest, ], by_element = TRUE))
# Count all candidates within one foot, so a nearest result is not mistaken for unique identity.
review$current_candidates_within_one_foot <- lengths(st_is_within_distance(old_points, current_points, dist = 1))
review <- review |> left_join(candidates |> select(project_id,
    old_building_sqft = building_sqft, old_land_sqft = land_sqft),
  by = "project_id", relationship = "many-to-one") |>
  arrange(project_id)
# These are comparison candidates, never automatic replacements or construction-year decisions.
write_csv(review, "../output/remaining_project_successor_evidence.csv")
