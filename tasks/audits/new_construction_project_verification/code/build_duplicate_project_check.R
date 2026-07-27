# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    construction_year = readr::col_double(),
    x_3435 = readr::col_double(),
    y_3435 = readr::col_double(),
    review_address = readr::col_character(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    audit_decision = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(audit_decision != "exclude_after_manual_review") |>
  dplyr::mutate(
    construction_year = audit_construction_year,
    dwelling_units = audit_dwelling_units,
    building_sqft = audit_building_sqft,
    address_key = review_address |>
      stringr::str_to_upper() |>
      stringr::str_remove("\\bCHICAGO\\b.*$") |>
      stringr::str_remove("\\b(APT|UNIT|SUITE)\\b.*$") |>
      stringr::str_remove_all("[^A-Z0-9]")
  )

if (anyDuplicated(projects$project_id)) {
  stop("The reviewed project ledger is not unique by project_id.")
}

project_pins <- projects |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::filter(!is.na(component_pins), component_pins != "")

pin_groups <- split(project_pins$project_id, project_pins$component_pins)
shared_pin_rows <- vector("list", length(pin_groups))
for (i in seq_along(pin_groups)) {
  project_ids <- sort(unique(pin_groups[[i]]))
  if (length(project_ids) < 2L) {
    next
  }
  project_pairs <- t(utils::combn(project_ids, 2))
  shared_pin_rows[[i]] <- tibble::tibble(
    project_id_a = project_pairs[, 1],
    project_id_b = project_pairs[, 2],
    duplicate_reason = "shared_component_pin",
    detail = names(pin_groups)[i]
  )
}
shared_pins <- dplyr::bind_rows(shared_pin_rows)

address_rows <- projects |>
  dplyr::filter(!is.na(address_key), address_key != "") |>
  dplyr::select(project_id, address_key)
address_groups <- split(address_rows$project_id, address_rows$address_key)
same_address_rows <- vector("list", length(address_groups))
for (i in seq_along(address_groups)) {
  project_ids <- sort(unique(address_groups[[i]]))
  if (length(project_ids) < 2L) {
    next
  }
  project_pairs <- t(utils::combn(project_ids, 2))
  same_address_rows[[i]] <- tibble::tibble(
    project_id_a = project_pairs[, 1],
    project_id_b = project_pairs[, 2],
    duplicate_reason = "same_normalized_address",
    detail = names(address_groups)[i]
  )
}
same_addresses <- dplyr::bind_rows(same_address_rows)

project_points <- sf::st_as_sf(
  projects,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
nearby_indices <- sf::st_is_within_distance(
  project_points,
  project_points,
  dist = 25
)

nearby_pairs <- vector("list", nrow(project_points))
for (i in seq_len(nrow(project_points))) {
  candidate_rows <- nearby_indices[[i]]
  candidate_rows <- candidate_rows[candidate_rows > i]
  if (length(candidate_rows) == 0L) {
    next
  }
  year_gap <- abs(
    project_points$construction_year[candidate_rows] -
      project_points$construction_year[i]
  )
  candidate_rows <- candidate_rows[
    is.finite(year_gap) & year_gap <= 2
  ]
  if (length(candidate_rows) == 0L) {
    next
  }
  nearby_pairs[[i]] <- tibble::tibble(
    project_id_a = project_points$project_id[i],
    project_id_b = project_points$project_id[candidate_rows],
    duplicate_reason = "within_25ft_and_2_years",
    detail = sprintf(
      "%.2fft",
      as.numeric(sf::st_distance(
        project_points[i, ],
        project_points[candidate_rows, ],
        by_element = FALSE
      ))
    )
  )
}
nearby_pairs <- dplyr::bind_rows(nearby_pairs)

duplicate_candidates <- dplyr::bind_rows(
  shared_pins,
  same_addresses,
  nearby_pairs
) |>
  dplyr::distinct()

project_polygons <- sf::st_read(
  "../input/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::filter(target_year == construction_year) |>
  dplyr::select(project_id, geom)

if (anyDuplicated(project_polygons$project_id)) {
  stop("The historical project polygons are not unique by project_id.")
}

polygon_rows <- match(
  duplicate_candidates$project_id_a,
  project_polygons$project_id
)
polygon_columns <- match(
  duplicate_candidates$project_id_b,
  project_polygons$project_id
)
overlap_share <- rep(NA_real_, nrow(duplicate_candidates))
for (i in seq_len(nrow(duplicate_candidates))) {
  if (is.na(polygon_rows[i]) || is.na(polygon_columns[i])) {
    next
  }
  polygon_a <- sf::st_geometry(project_polygons[polygon_rows[i], ])
  polygon_b <- sf::st_geometry(project_polygons[polygon_columns[i], ])
  intersection_area <- as.numeric(sf::st_area(
    suppressWarnings(sf::st_intersection(polygon_a, polygon_b))
  ))
  if (length(intersection_area) == 0L) {
    overlap_share[i] <- 0
    next
  }
  smaller_polygon_area <- min(
    as.numeric(sf::st_area(polygon_a)),
    as.numeric(sf::st_area(polygon_b))
  )
  overlap_share[i] <- sum(intersection_area) / smaller_polygon_area
}

duplicate_candidates <- duplicate_candidates |>
  dplyr::mutate(
    smaller_polygon_overlap_share = overlap_share
  ) |>
  dplyr::left_join(
    projects |>
      sf::st_drop_geometry() |>
      dplyr::select(
        project_id_a = project_id,
        address_a = review_address,
        construction_year_a = construction_year,
        dwelling_units_a = dwelling_units,
        building_sqft_a = building_sqft
      ),
    by = "project_id_a",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      sf::st_drop_geometry() |>
      dplyr::select(
        project_id_b = project_id,
        address_b = review_address,
        construction_year_b = construction_year,
        dwelling_units_b = dwelling_units,
        building_sqft_b = building_sqft
      ),
    by = "project_id_b",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(project_id_a, project_id_b, duplicate_reason)

readr::write_csv(
  duplicate_candidates,
  "../output/duplicate_project_candidates.csv",
  na = ""
)

duplicate_summary <- duplicate_candidates |>
  dplyr::count(duplicate_reason, name = "candidate_pairs")

readr::write_csv(
  duplicate_summary,
  "../output/duplicate_project_summary.csv",
  na = ""
)
