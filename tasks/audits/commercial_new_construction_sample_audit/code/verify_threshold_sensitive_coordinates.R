# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

decisions <- readr::read_csv(
  "../adjudication/threshold_sensitive_coordinate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (nrow(decisions) != 5L ||
    anyDuplicated(decisions$project_id) ||
    any(!decisions$cutoff_ft %in% c(500, 1500))) {
  stop("Threshold-sensitive coordinate decisions are invalid.", call. = FALSE)
}

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(decisions, by = "project_id") |>
  dplyr::transmute(
    project_id,
    construction_year,
    source_addresses,
    candidate_source = "selected_project_coordinate",
    candidate_detail = geometry_source,
    x_3435,
    y_3435
  )

if (nrow(ledger) != nrow(decisions) ||
    any(!is.finite(ledger$x_3435)) ||
    any(!is.finite(ledger$y_3435))) {
  stop("A threshold-sensitive project lacks its selected coordinate.", call. = FALSE)
}

predecessor_centroids <- sf::st_read(
  "../output/preferred_project_year_centroids.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::semi_join(decisions, by = "project_id")

predecessor_xy <- sf::st_coordinates(predecessor_centroids)
predecessor_points <- predecessor_centroids |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    project_id,
    construction_year = target_year,
    source_addresses = NA_character_,
    candidate_source = "construction_year_predecessor_centroid",
    candidate_detail = "selected_construction_year_project_polygon",
    x_3435 = predecessor_xy[, "X"],
    y_3435 = predecessor_xy[, "Y"]
  )

permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)

permit_points <- decisions |>
  tidyr::separate_longer_delim(permit_numbers, delim = "/") |>
  dplyr::rename(permit_number = permit_numbers) |>
  dplyr::inner_join(
    permits |>
      dplyr::select(
        permit_number,
        permit_address,
        permit_x_3435,
        permit_y_3435
      ),
    by = "permit_number",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    ledger |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::transmute(
    project_id,
    construction_year,
    source_addresses = permit_address,
    candidate_source = "new_construction_permit_point",
    candidate_detail = permit_number,
    x_3435 = permit_x_3435,
    y_3435 = permit_y_3435
  )

expected_permits <- sum(
  stringr::str_count(decisions$permit_numbers, "/") + 1L
)
if (nrow(permit_points) != expected_permits ||
    any(!is.finite(permit_points$x_3435)) ||
    any(!is.finite(permit_points$y_3435))) {
  stop("Threshold-sensitive permit coordinates are incomplete.", call. = FALSE)
}

candidates <- dplyr::bind_rows(
  ledger,
  predecessor_points,
  permit_points
) |>
  dplyr::mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

candidate_points <- sf::st_as_sf(
  candidates,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE) |>
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers(
  "../input/ward_pair_boundaries.gpkg"
)

assignment <- assign_points_to_boundaries(
  points_sf = candidate_points,
  era_values = candidate_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines
)

evidence <- dplyr::bind_cols(
  sf::st_drop_geometry(candidate_points),
  assignment
) |>
  dplyr::left_join(
    decisions,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    within_cutoff = dist_ft <= cutoff_ft,
    accepted_coordinate =
      candidate_source == "selected_project_coordinate"
  ) |>
  dplyr::select(
    project_id,
    construction_year,
    source_addresses,
    candidate_source,
    candidate_detail,
    x_3435,
    y_3435,
    ward,
    neighbor_ward,
    ward_pair = ward_pair_id,
    distance_to_boundary_ft = dist_ft,
    cutoff_ft,
    within_cutoff,
    accepted_coordinate,
    expected_within_cutoff,
    decision_reason
  ) |>
  dplyr::arrange(project_id, dplyr::desc(accepted_coordinate), candidate_source)

accepted <- evidence |>
  dplyr::filter(accepted_coordinate)

if (nrow(accepted) != nrow(decisions) ||
    any(accepted$within_cutoff != accepted$expected_within_cutoff)) {
  stop("A frozen threshold decision conflicts with the selected coordinate.", call. = FALSE)
}

summary <- evidence |>
  dplyr::group_by(
    project_id,
    cutoff_ft,
    expected_within_cutoff,
    decision_reason
  ) |>
  dplyr::summarise(
    selected_distance_to_boundary_ft =
      distance_to_boundary_ft[accepted_coordinate],
    minimum_candidate_distance_ft =
      min(distance_to_boundary_ft),
    maximum_candidate_distance_ft =
      max(distance_to_boundary_ft),
    candidate_sources_agree =
      dplyr::n_distinct(within_cutoff) == 1L,
    candidate_sources = paste(
      sort(unique(candidate_source)),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(project_id)

readr::write_csv(
  evidence,
  "../output/threshold_sensitive_coordinate_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/threshold_sensitive_coordinate_summary.csv"
)
