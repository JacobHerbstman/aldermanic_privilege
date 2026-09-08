# setwd("tasks/working_paper_release_audit/code")

library(dplyr)
library(readr)
library(sf)

initial <- read_csv("../input/historical_project_predecessor_resolution.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess())) |>
  filter(predecessor_status %in% c("multiple_predecessor_polygons", "no_predecessor_polygon")) |>
  distinct(source_family, project_id, component_pin, target_year,
    initial_predecessor_status = predecessor_status)
current_matches <- read_csv("../input/preferred_historical_predecessor_resolution.csv",
  col_types = cols(component_pin = col_character(), .default = col_guess()))
current <- current_matches |>
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
  select(source_family, project_id, target_year, centroid_outside_parcel_ft,
    polygon_parts, maximum_part_separation_ft)
review <- bind_rows(review, unknown_year, centroids) |>
  mutate(initial_checkpoint_pending = !is.na(initial_predecessor_status),
    construction_year_unresolved = coalesce(construction_year_unresolved, FALSE),
    centroid_outside_parcel = coalesce(centroid_outside_parcel_ft > 1e-6, FALSE),
    case_id = paste(source_family, project_id, coalesce(component_pin, "project"),
      coalesce(as.character(target_year), "year_unresolved"), sep = "|"))
stopifnot(!anyDuplicated(review$case_id))

# Follow an old identifier only when its component-year has one current project.
current_requests <- current_matches |>
  distinct(source_family, project_id, component_pin, target_year, predecessor_status)
stopifnot(!anyDuplicated(current_requests[c("source_family", "project_id", "component_pin", "target_year")]))
review$current_project_id <- review$project_id
for (i in which(review$initial_checkpoint_pending)) {
  matches <- current_requests |> filter(source_family == review$source_family[i],
    component_pin == review$component_pin[i], target_year == review$target_year[i])
  if (review$project_id[i] %in% matches$project_id) {
    matches <- matches |> filter(project_id == review$project_id[i])
  }
  if (nrow(matches) == 1L) {
    review$current_project_id[i] <- matches$project_id
    review$current_predecessor_status[i] <- matches$predecessor_status
  }
}

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
  left_join(scope, by = c("source_family", "current_project_id" = "project_id", "target_year"), relationship = "many-to-one") |>
  left_join(history, by = c("project_id", "component_pin", "target_year"), relationship = "many-to-one") |>
  mutate(next_year_exact_pin_candidate = coalesce(history_year == target_year + 1L, FALSE)) |>
  arrange(source_family, project_id, target_year, component_pin)

# The production fallback also checks the earlier exact-PIN history extract.
fallback_evidence <- current_matches |> filter(!is.na(history_coordinate_year)) |>
  distinct(project_id, component_pin, target_year, history_coordinate_year,
    history_coordinate_row_id, reference_x_3435, reference_y_3435)
stopifnot(!anyDuplicated(fallback_evidence[c("project_id", "component_pin", "target_year")]))
review <- review |> left_join(fallback_evidence,
  by = c("current_project_id" = "project_id", "component_pin", "target_year"), relationship = "many-to-one") |>
  mutate(history_year = coalesce(history_year, history_coordinate_year),
    history_year_gap = abs(history_year - target_year),
    history_row_id = coalesce(history_row_id, history_coordinate_row_id),
    history_x_3435 = coalesce(history_x_3435, reference_x_3435),
    history_y_3435 = coalesce(history_y_3435, reference_y_3435),
    history_status = if_else(is.finite(history_x_3435) & is.finite(history_y_3435),
      "exact_pin_history_available_for_review", history_status),
    next_year_exact_pin_candidate = coalesce(history_year == target_year + 1L, FALSE)) |>
  select(-history_coordinate_year, -history_coordinate_row_id, -reference_x_3435, -reference_y_3435)

# Distinguish duplicate database records from genuinely different candidate land areas.
parcels <- st_read("../input/current_predecessor_parcel_source.gpkg", quiet = TRUE)
history_parcels <- st_read("../input/history_reference_parcels.gpkg", quiet = TRUE)
history_parcels <- history_parcels[, names(parcels)]
history_queries <- read_csv("../input/history_reference_queries.csv", show_col_types = FALSE)
overlap <- match(paste(history_parcels$target_year, history_parcels$object_id),
  paste(parcels$target_year, parcels$object_id))
for (i in which(!is.na(overlap))) {
  stopifnot(length(st_equals(history_parcels[i, ], parcels[overlap[i], ])[[1]]) == 1L)
}
parcels <- rbind(parcels, history_parcels[is.na(overlap), ])
stopifnot(st_crs(parcels)$epsg == 3435,
  !anyDuplicated(st_drop_geometry(parcels)[c("target_year", "object_id")]))
review$predecessor_min_area_sqft <- NA_real_
review$predecessor_max_area_sqft <- NA_real_
review$predecessor_shapes_equivalent <- NA
for (i in which(review$initial_checkpoint_pending |
    review$current_predecessor_status == "multiple_predecessor_polygons")) {
  matches <- current_matches |> filter(project_id == review$current_project_id[i],
    component_pin == review$component_pin[i], target_year == review$target_year[i], !is.na(object_id))
  if (!nrow(matches)) next
  shapes <- parcels |> filter(target_year == review$target_year[i], object_id %in% matches$object_id)
  stopifnot(nrow(shapes) == nrow(matches))
  review$predecessor_min_area_sqft[i] <- min(as.numeric(st_area(shapes)))
  review$predecessor_max_area_sqft[i] <- max(as.numeric(st_area(shapes)))
  review$predecessor_shapes_equivalent[i] <- all(lengths(st_equals(shapes)) == nrow(shapes))
}
review <- review |> mutate(
  initial_checkpoint_resolved = initial_checkpoint_pending &
    coalesce(current_predecessor_status %in% c("equivalent_predecessor_geometry", "unique_predecessor_polygon") &
      predecessor_shapes_equivalent, FALSE),
  initial_checkpoint_pending = initial_checkpoint_pending & !initial_checkpoint_resolved)

residential <- read_csv("../input/preferred_residential_project_candidates.csv", show_col_types = FALSE)
commercial <- read_csv("../input/preferred_commercial_project_candidates.csv", show_col_types = FALSE)
reviewed_replacement_ids <- residential |>
  filter(replacement_check == "reviewed_same_building_identity") |>
  pull(replacement_project_ids) |> unique()
candidate_reasons <- bind_rows(residential, commercial) |>
  select(source_family, current_project_id = project_id, candidate_decision_reason = decision_reason,
    upstream_candidate_status = candidate_status, upstream_project_kind = project_kind,
    candidate_construction_year = construction_year, candidate_assessor_land_sqft = land_sqft)
stopifnot(!anyDuplicated(candidate_reasons[c("source_family", "current_project_id")]))
review <- review |> left_join(candidate_reasons,
  by = c("source_family", "current_project_id"), relationship = "many-to-one") |>
  mutate(candidate_status = coalesce(candidate_status, upstream_candidate_status),
    project_kind = coalesce(project_kind, upstream_project_kind)) |>
  select(-upstream_candidate_status, -upstream_project_kind)
stopifnot(all(is.na(review$candidate_construction_year[review$construction_year_unresolved])))

cards <- read_csv("../input/residential_multicard_cards.csv",
  col_types = cols(pin = col_character(), .default = col_guess())) |>
  group_by(pin) |> summarise(
    reported_construction_years = paste(sort(unique(year_built)), collapse = "/"),
    reported_assessment_years = paste(sort(unique(tax_year)), collapse = "/"), .groups = "drop") |>
  mutate(current_project_id = paste0("residential_multicard_", pin)) |> select(-pin)
lineages <- read_csv("../input/residential_tieback_temporal_lineage_evidence.csv", show_col_types = FALSE) |>
  transmute(current_project_id = tieback_lineage_id,
    reported_construction_years = as.character(candidate_construction_years),
    lineage_reason = temporal_reason)
year_evidence <- bind_rows(cards, lineages)
stopifnot(!anyDuplicated(year_evidence$current_project_id))
review <- review |> left_join(year_evidence, by = "current_project_id", relationship = "many-to-one")

# Older coordinate candidates remain evidence; only the approved one-year rule enters production.
review$history_candidate_polygon_count <- NA_integer_
review$history_candidate_parcel_pins <- NA_character_
for (i in which(is.finite(review$history_x_3435) & is.finite(review$history_y_3435))) {
  point <- st_as_sf(review[i, ], coords = c("history_x_3435", "history_y_3435"), crs = 3435)
  stopifnot(any(history_queries$target_year == review$target_year[i] &
    abs(history_queries$reference_x_3435 - review$history_x_3435[i]) < 1e-6 &
    abs(history_queries$reference_y_3435 - review$history_y_3435[i]) < 1e-6))
  shapes <- history_parcels |> filter(target_year == review$target_year[i])
  hits <- st_within(point, shapes)[[1]]
  review$history_candidate_polygon_count[i] <- length(hits)
  review$history_candidate_parcel_pins[i] <- paste(sort(unique(shapes$predecessor_pin14[hits])), collapse = "/")
}

review <- review |> mutate(
  review_route = case_when(
    grepl("^exclude", candidate_status) ~ "closed_excluded_source_record",
    current_project_id %in% reviewed_replacement_ids &
      geography_status == "complete_construction_year_geometry" ~ "resolved_reviewed_identity_and_location",
    geography_status == "reviewed_permit_location" ~ "resolved_by_reviewed_permit_location",
    initial_checkpoint_resolved ~ "resolved_by_general_parcel_rule",
    construction_year_unresolved ~ "reconcile_construction_episode_before_assigning_year",
    centroid_outside_parcel & polygon_parts == 1L ~ "concave_shape_explains_exterior_centroid_check_project_land_scope",
    centroid_outside_parcel ~ "check_multipart_project_land_scope",
    current_predecessor_status == "multiple_predecessor_polygons" ~ "reconcile_overlapping_historical_parcels",
    history_candidate_polygon_count == 0L ~ "historical_map_has_no_polygon_at_exact_pin_location",
    history_status == "exact_pin_history_available_for_review" ~ "evaluate_exact_pin_history_before_manual_geocoding",
    TRUE ~ "obtain_missing_location_evidence"),
  production_owner = case_when(
    construction_year_unresolved ~ "build_residential_assessor_projects.R",
    centroid_outside_parcel ~ "build_preferred_project_geography.R; upstream project component producer",
    current_predecessor_status %in% c("multiple_predecessor_polygons", "equivalent_predecessor_geometry") ~
      "recover_preferred_historical_predecessors.R",
    TRUE ~ "build_preferred_predecessor_reference_points.R"))

# An old single-card identifier and its current multicard identifier are one review case.
# Carry the original checkpoint identity forward before removing identical audit records.
review <- review |> group_by(source_family, current_project_id, component_pin, target_year) |>
  mutate(initial_project_ids = paste(sort(unique(project_id[
      initial_checkpoint_pending | initial_checkpoint_resolved])), collapse = "/"),
    initial_predecessor_status = if (all(is.na(initial_predecessor_status))) NA_character_ else
      paste(sort(unique(na.omit(initial_predecessor_status))), collapse = "/"),
    initial_checkpoint_pending = any(initial_checkpoint_pending),
    initial_checkpoint_resolved = any(initial_checkpoint_resolved)) |>
  ungroup() |> mutate(project_id = current_project_id,
    case_id = paste(source_family, project_id, coalesce(component_pin, "project"),
      coalesce(as.character(target_year), "year_unresolved"), sep = "|")) |>
  select(-current_project_id) |> distinct() |> arrange(source_family, project_id, target_year, component_pin)
stopifnot(!anyDuplicated(review$case_id))
# Coordinate candidates are evidence only; this audit does not assign locations or exclusions.
write_csv(review, "../output/candidate_geography_review_queue.csv")
