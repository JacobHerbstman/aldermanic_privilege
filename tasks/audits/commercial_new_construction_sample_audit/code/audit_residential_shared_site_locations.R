# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

candidates <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

components <- readr::read_csv(
  "../output/preferred_residential_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

suppressions <- readr::read_csv(
  "../adjudication/residential_candidate_suppressions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(candidate_project_id = readr::col_character(), .default = readr::col_guess())
)

additional_decisions <- readr::read_csv(
  "../adjudication/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(candidate_project_id = readr::col_character(), .default = readr::col_guess())
)

retained_mechanical <- candidates %>%
  filter(candidate_status == "retain_mechanical") %>%
  anti_join(suppressions, by = c("project_id" = "candidate_project_id")) %>%
  anti_join(
    additional_decisions %>%
      filter(decision != "retain_with_override") %>%
      select(candidate_project_id),
    by = c("project_id" = "candidate_project_id")
  ) %>%
  select(project_id, construction_year)

single_component_projects <- components %>%
  semi_join(retained_mechanical, by = "project_id") %>%
  add_count(project_id, name = "component_count") %>%
  filter(component_count == 1) %>%
  select(project_id, component_pin)

component_geometry <- sf::st_read(
  "../output/preferred_project_component_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(source_family == "residential") %>%
  inner_join(single_component_projects, by = c("project_id", "component_pin")) %>%
  inner_join(retained_mechanical, by = c("project_id", "target_year" = "construction_year"))

if (anyDuplicated(component_geometry$project_id) > 0) {
  stop("A single-component project has more than one selected site polygon.", call. = FALSE)
}

shared_site_keys <- component_geometry %>%
  sf::st_drop_geometry() %>%
  distinct(project_id, target_year, parcel_object_ids) %>%
  add_count(target_year, parcel_object_ids, name = "projects_on_selected_site") %>%
  filter(projects_on_selected_site > 1)

shared_sites <- component_geometry %>%
  inner_join(
    shared_site_keys,
    by = c("project_id", "target_year", "parcel_object_ids"),
    relationship = "one-to-one"
  )

reference_points <- readr::read_csv(
  "../output/preferred_predecessor_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    reference_status == "reference_point_available",
    is.finite(reference_x_3435),
    is.finite(reference_y_3435)
  ) %>%
  select(
    project_id,
    component_pin,
    target_year,
    reference_source,
    reference_x_3435,
    reference_y_3435
  )

if (anyDuplicated(reference_points[c("project_id", "component_pin", "target_year")]) > 0) {
  stop("Accepted predecessor reference points are not unique.", call. = FALSE)
}

shared_site_review <- shared_sites %>%
  left_join(
    reference_points,
    by = c("project_id", "component_pin", "target_year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    construction_year = target_year,
    old_geometry_source = "construction_year_predecessor_site_centroid"
  )

accepted_points <- shared_site_review %>%
  filter(is.finite(reference_x_3435), is.finite(reference_y_3435)) %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(
    coords = c("reference_x_3435", "reference_y_3435"),
    crs = 3435,
    remove = FALSE
  )

selected_site_polygons <- shared_site_review %>%
  filter(is.finite(reference_x_3435), is.finite(reference_y_3435))

old_points <- sf::st_centroid(selected_site_polygons)
old_coordinates <- sf::st_coordinates(old_points)
old_points <- old_points %>%
  mutate(
    old_x_3435 = old_coordinates[, "X"],
    old_y_3435 = old_coordinates[, "Y"],
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

accepted_points$reference_point_in_selected_site <- diag(
  sf::st_within(accepted_points, selected_site_polygons, sparse = FALSE)
)

accepted_points <- accepted_points %>%
  left_join(
    sf::st_drop_geometry(old_points) %>%
      select(project_id, old_x_3435, old_y_3435),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    shift_ft = sqrt(
      (reference_x_3435 - old_x_3435)^2 +
        (reference_y_3435 - old_y_3435)^2
    ),
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

reference_assignment <- assign_points_to_boundaries(
  points_sf = accepted_points,
  era_values = accepted_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
) %>%
  rename(
    reference_ward = ward,
    reference_neighbor_ward = neighbor_ward,
    reference_ward_pair = ward_pair_id,
    reference_distance_to_boundary_ft = dist_ft
  )

old_assignment <- assign_points_to_boundaries(
  points_sf = old_points,
  era_values = old_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
) %>%
  rename(
    old_ward = ward,
    old_neighbor_ward = neighbor_ward,
    old_ward_pair = ward_pair_id,
    old_distance_to_boundary_ft = dist_ft
  )

review <- bind_cols(
  sf::st_drop_geometry(accepted_points),
  reference_assignment,
  old_assignment
) %>%
  mutate(
    old_within_1500ft = old_distance_to_boundary_ft <= 1500,
    old_within_500ft = old_distance_to_boundary_ft <= 500,
    reference_within_1500ft = reference_distance_to_boundary_ft <= 1500,
    reference_within_500ft = reference_distance_to_boundary_ft <= 500,
    ward_changed = reference_ward != old_ward,
    ward_pair_changed = reference_ward_pair != old_ward_pair,
    enters_1500ft = reference_within_1500ft & !old_within_1500ft,
    leaves_1500ft = !reference_within_1500ft & old_within_1500ft,
    enters_500ft = reference_within_500ft & !old_within_500ft,
    leaves_500ft = !reference_within_500ft & old_within_500ft
  ) %>%
  select(
    project_id,
    component_pin,
    construction_year,
    match_method,
    parcel_object_ids,
    projects_on_selected_site,
    reference_source,
    reference_point_in_selected_site,
    old_geometry_source,
    old_x_3435,
    old_y_3435,
    reference_x_3435,
    reference_y_3435,
    shift_ft,
    old_ward,
    reference_ward,
    old_ward_pair,
    reference_ward_pair,
    old_distance_to_boundary_ft,
    reference_distance_to_boundary_ft,
    old_within_1500ft,
    reference_within_1500ft,
    old_within_500ft,
    reference_within_500ft,
    ward_changed,
    ward_pair_changed,
    enters_1500ft,
    leaves_1500ft,
    enters_500ft,
    leaves_500ft
  ) %>%
  arrange(desc(shift_ft), project_id)

if (any(!review$reference_point_in_selected_site) ||
    any(!is.finite(review$reference_distance_to_boundary_ft)) ||
    any(is.na(review$reference_ward)) ||
    any(is.na(review$reference_ward_pair))) {
  stop("An accepted reference point is outside its selected historical site.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "shared_site_single_component_projects",
    "projects_with_accepted_reference_point",
    "projects_without_accepted_reference_point",
    "reference_points_inside_selected_site",
    "location_shift_above_25ft",
    "location_shift_above_100ft",
    "location_shift_above_500ft",
    "ward_assignments_changed",
    "ward_pairs_changed",
    "projects_entering_1500ft",
    "projects_leaving_1500ft",
    "projects_entering_500ft",
    "projects_leaving_500ft"
  ),
  value = c(
    nrow(shared_sites),
    nrow(review),
    nrow(shared_sites) - nrow(review),
    sum(review$reference_point_in_selected_site),
    sum(review$shift_ft > 25),
    sum(review$shift_ft > 100),
    sum(review$shift_ft > 500),
    sum(review$ward_changed),
    sum(review$ward_pair_changed),
    sum(review$enters_1500ft),
    sum(review$leaves_1500ft),
    sum(review$enters_500ft),
    sum(review$leaves_500ft)
  )
)

summary <- bind_rows(
  summary,
  review %>%
    count(reference_source, name = "value") %>%
    transmute(
      metric = paste0("reference_source:", reference_source),
      value
    )
)

readr::write_csv(
  review,
  "../output/residential_shared_site_location_review.csv"
)
readr::write_csv(
  summary,
  "../output/residential_shared_site_location_summary.csv"
)
