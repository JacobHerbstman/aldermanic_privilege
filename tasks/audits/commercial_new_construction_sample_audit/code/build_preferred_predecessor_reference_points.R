# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

coverage <- readr::read_csv(
  "../output/preferred_historical_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(coverage_status %in% c("missing_pin10", "ambiguous_pin10")) %>%
  mutate(request_id = paste(project_id, component_pin, target_year, sep = "|"))

historical_coordinates <- readr::read_csv(
  "../input/density_historical_coordinates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_transform(3435)

historical_matrix <- sf::st_coordinates(historical_coordinates)
historical_coordinates <- historical_coordinates %>%
  sf::st_drop_geometry() %>%
  transmute(
    component_pin = pin,
    target_year = construction_year,
    reference_x_3435 = historical_matrix[, "X"],
    reference_y_3435 = historical_matrix[, "Y"],
    reference_source = coordinate_source
  )

if (anyDuplicated(historical_coordinates[c("component_pin", "target_year")]) > 0) {
  stop("Historical coordinate reference is not unique by PIN-year.", call. = FALSE)
}

current_coordinates <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  filter(
    is.finite(centroid_x_crs_3435),
    is.finite(centroid_y_crs_3435)
  ) %>%
  transmute(
    component_pin = pin,
    current_x_3435 = centroid_x_crs_3435,
    current_y_3435 = centroid_y_crs_3435
  )

if (anyDuplicated(current_coordinates$component_pin) > 0) {
  stop("Current parcel coordinate reference is not unique by PIN.", call. = FALSE)
}

historical_address_coordinates <- readr::read_csv(
  "../output/preferred_historical_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    tiger_line_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(census_status == "accepted_reference_point") %>%
  select(
    request_id,
    address_x_3435 = census_x_3435,
    address_y_3435 = census_y_3435,
    selected_address,
    selected_address_year,
    matched_address,
    tiger_line_id
  )

if (anyDuplicated(historical_address_coordinates$request_id) > 0) {
  stop("Accepted historical address coordinates are not unique by request.", call. = FALSE)
}

chicago_address_coordinates <- readr::read_csv(
  "../output/preferred_chicago_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(chicago_status == "accepted_reference_point") %>%
  select(
    request_id,
    chicago_x_3435,
    chicago_y_3435,
    chicago_matched_address,
    chicago_score,
    chicago_locator
  )

if (anyDuplicated(chicago_address_coordinates$request_id) > 0) {
  stop("Accepted Chicago address coordinates are not unique by request.", call. = FALSE)
}

permit_coordinates <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    plausible_application_window | plausible_issue_window,
    is.finite(permit_x_3435),
    is.finite(permit_y_3435)
  ) %>%
  group_by(component_pin) %>%
  summarise(
    permit_points = n_distinct(paste(permit_x_3435, permit_y_3435)),
    permit_x_min = min(permit_x_3435),
    permit_x_max = max(permit_x_3435),
    permit_y_min = min(permit_y_3435),
    permit_y_max = max(permit_y_3435),
    permit_x_3435 = median(permit_x_3435),
    permit_y_3435 = median(permit_y_3435),
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    .groups = "drop"
  ) %>%
  mutate(
    permit_point_spread_ft = sqrt(
      (permit_x_max - permit_x_min)^2 +
        (permit_y_max - permit_y_min)^2
    )
  ) %>%
  filter(permit_point_spread_ft <= 100)

reference_points <- coverage %>%
  left_join(
    historical_coordinates,
    by = c("component_pin", "target_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    current_coordinates,
    by = "component_pin",
    relationship = "many-to-one"
  ) %>%
  left_join(
    historical_address_coordinates,
    by = "request_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    chicago_address_coordinates,
    by = "request_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    permit_coordinates,
    by = "component_pin",
    relationship = "many-to-one"
  ) %>%
  mutate(
    reference_source = case_when(
      is.finite(reference_x_3435) & is.finite(reference_y_3435) ~ reference_source,
      is.finite(current_x_3435) & is.finite(current_y_3435) ~ "parcel_universe_2025_exact_pin",
      is.finite(chicago_x_3435) & is.finite(chicago_y_3435) ~
        "chicago_point_geocode_of_cook_county_historical_address",
      is.finite(address_x_3435) & is.finite(address_y_3435) ~
        "census_geocode_of_cook_county_historical_address",
      is.finite(permit_x_3435) & is.finite(permit_y_3435) ~ "issued_permit_exact_component_pin10",
      TRUE ~ NA_character_
    ),
    reference_x_3435 = coalesce(
      reference_x_3435,
      current_x_3435,
      chicago_x_3435,
      address_x_3435,
      permit_x_3435
    ),
    reference_y_3435 = coalesce(
      reference_y_3435,
      current_y_3435,
      chicago_y_3435,
      address_y_3435,
      permit_y_3435
    ),
    reference_status = if_else(
      is.finite(reference_x_3435) & is.finite(reference_y_3435),
      "reference_point_available",
      "reference_point_unresolved"
    )
  ) %>%
  select(
    request_id,
    source_family,
    project_id,
    project_kind,
    candidate_status,
    component_pin,
    pin10,
    target_year,
    coverage_status,
    reference_status,
    reference_source,
    reference_x_3435,
    reference_y_3435,
    selected_address,
    selected_address_year,
    matched_address,
    tiger_line_id,
    chicago_matched_address,
    chicago_score,
    chicago_locator,
    permit_ids,
    permit_point_spread_ft
  ) %>%
  arrange(target_year, project_id, component_pin)

if (anyDuplicated(reference_points$request_id) > 0) {
  stop("Preferred predecessor reference requests are not unique.", call. = FALSE)
}

summary <- bind_rows(
  reference_points %>%
    count(reference_status, reference_source, name = "value") %>%
    transmute(
      metric = paste(reference_status, coalesce(reference_source, "none"), sep = ":"),
      value
    ),
  tibble::tibble(
    metric = "missing_or_ambiguous_project_component_years",
    value = nrow(reference_points)
  )
)

readr::write_csv(
  reference_points,
  "../output/preferred_predecessor_reference_points.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_predecessor_reference_summary.csv"
)
