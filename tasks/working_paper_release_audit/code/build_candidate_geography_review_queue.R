# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)

initial <- read_csv("../input/historical_project_predecessor_resolution.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  filter(predecessor_status %in% c("multiple_predecessor_polygons", "no_predecessor_polygon")) |>
  distinct(source_family, project_id, component_pin, target_year,
    initial_predecessor_status = predecessor_status)
current <- read_csv("../input/preferred_historical_predecessor_resolution.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  filter(!predecessor_status %in% c("unique_predecessor_polygon", "equivalent_predecessor_geometry")) |>
  distinct(source_family, project_id, component_pin, target_year,
    current_predecessor_status = predecessor_status)
stopifnot(!anyDuplicated(initial[c("source_family", "project_id", "component_pin", "target_year")]),
  !anyDuplicated(current[c("source_family", "project_id", "component_pin", "target_year")]))
review <- full_join(initial, current,
  by = c("source_family", "project_id", "component_pin", "target_year"), relationship = "one-to-one")
unknown_year <- read_csv("../input/preferred_adjudication_scope.csv", show_col_types = FALSE) |>
  filter(is.na(target_year)) |> select(source_family, project_id, target_year) |>
  mutate(construction_year_unresolved = TRUE)
centroids <- read_csv("../output/candidate_geography_checks.csv", show_col_types = FALSE) |>
  filter(centroid_outside_parcel_ft > 1e-6) |>
  select(source_family, project_id, target_year, centroid_outside_parcel_ft)
review <- bind_rows(review, unknown_year, centroids) |>
  mutate(initial_checkpoint_pending = !is.na(initial_predecessor_status),
    construction_year_unresolved = coalesce(construction_year_unresolved, FALSE),
    centroid_outside_parcel = coalesce(centroid_outside_parcel_ft > 1e-6, FALSE),
    case_id = paste(source_family, project_id, coalesce(component_pin, "project"),
      coalesce(as.character(target_year), "year_unresolved"), sep = "|"))
stopifnot(!anyDuplicated(review$case_id))

scope <- read_csv("../input/preferred_project_boundary_scope.csv", show_col_types = FALSE) |>
  select(source_family, project_id, target_year, project_kind, candidate_status,
    geography_status, requested_components, resolved_components,
    candidate_component_pins = component_pins, candidate_parcel_pins = parcel_pins,
    distance_to_boundary_ft, within_500ft)
history <- read_csv("../output/geocoding_parcel_history_review.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  select(project_id, component_pin, target_year, selected_address, history_year, history_year_gap,
    history_row_id, history_x_3435, history_y_3435, history_status)
stopifnot(!anyDuplicated(scope[c("source_family", "project_id", "target_year")]),
  !anyDuplicated(history[c("project_id", "component_pin", "target_year")]))
review <- review |>
  left_join(scope, by = c("source_family", "project_id", "target_year"), relationship = "many-to-one") |>
  left_join(history, by = c("project_id", "component_pin", "target_year"), relationship = "many-to-one") |>
  mutate(next_year_exact_pin_candidate = coalesce(history_year == target_year + 1L, FALSE)) |>
  arrange(source_family, project_id, target_year, component_pin)
# Coordinate candidates are evidence only; this audit does not assign locations or exclusions.
write_csv(review, "../output/candidate_geography_review_queue.csv")
