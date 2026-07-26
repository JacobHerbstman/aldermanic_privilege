# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

scope <- readr::read_csv(
  "../output/residential_unresolved_scope_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  select(project_id, component_pins)

components <- scope %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  rename(pin = component_pins) %>%
  distinct(project_id, pin)

inventory_points <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    coordinate_x_3435 = readr::col_double(),
    coordinate_y_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  filter(is.finite(coordinate_x_3435), is.finite(coordinate_y_3435)) %>%
  transmute(
    project_id,
    reference_source = paste0("candidate_inventory:", pin),
    x_3435 = coordinate_x_3435,
    y_3435 = coordinate_y_3435
  )

current_points <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) %>%
  transmute(
    project_id,
    reference_source = paste0("current_parcel:", pin),
    x_3435 = centroid_x_crs_3435,
    y_3435 = centroid_y_crs_3435
  )

historical_points <- readr::read_csv(
  "../input/density_historical_exact_parcel_records.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    year = readr::col_double(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  inner_join(components, by = "pin", relationship = "many-to-one") %>%
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) %>%
  transmute(
    project_id,
    reference_source = paste0("historical_parcel:", pin, "@", year),
    x_3435 = centroid_x_crs_3435,
    y_3435 = centroid_y_crs_3435
  )

address_points <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(geocode_status == "accepted_reference_point") %>%
  transmute(
    project_id,
    reference_source = paste0("address_geocode:", address),
    x_3435,
    y_3435
  )

exact_project_permits <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(scope, by = "project_id") %>%
  distinct(project_id, permit_id, permit_x_3435, permit_y_3435)

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>%
  mutate(
    permit_id = as.character(id),
    permit_pin = str_replace_all(coalesce(as.character(pin), ""), "[^0-9]", ""),
    application_year = lubridate::year(application_start_date),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    new_construction_text = str_detect(
      coalesce(work_description, ""),
      regex(
        "NEW CONSTRUCTION|CONSTRUCT NEW|ERECT( A)? NEW|NEW [A-Z0-9 -]*(RESIDENCE|BUILDING|HOME|TOWNHOUSE)",
        ignore_case = TRUE
      )
    )
  ) %>%
  filter(
    between(application_year, 2004L, 2023L),
    new_construction_text | permit_id %in% exact_project_permits$permit_id
  )
permits <- permits[!sf::st_is_empty(permits), ]

direct_permit_points <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential", directly_matched) %>%
  inner_join(
    components %>% mutate(source_project_id = paste0("residential_", pin)),
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) %>%
  transmute(project_id = project_id.y, permit_id) %>%
  inner_join(
    sf::st_drop_geometry(permits) %>% select(permit_id, xcoordinate, ycoordinate),
    by = "permit_id",
    relationship = "many-to-one"
  ) %>%
  filter(is.finite(xcoordinate), is.finite(ycoordinate)) %>%
  transmute(
    project_id,
    reference_source = paste0("direct_permit:", permit_id),
    x_3435 = xcoordinate,
    y_3435 = ycoordinate
  )

exact_permit_points <- exact_project_permits %>%
  filter(is.finite(permit_x_3435), is.finite(permit_y_3435)) %>%
  transmute(
    project_id,
    reference_source = paste0("exact_pin10_permit:", permit_id),
    x_3435 = permit_x_3435,
    y_3435 = permit_y_3435
  ) %>%
  distinct()

reference_points <- bind_rows(
  inventory_points,
  current_points,
  historical_points,
  address_points,
  direct_permit_points,
  exact_permit_points
) %>%
  mutate(
    x_rounded = round(x_3435),
    y_rounded = round(y_3435)
  ) %>%
  group_by(project_id, x_rounded, y_rounded) %>%
  summarise(
    reference_sources = paste(sort(unique(reference_source)), collapse = " || "),
    x_3435 = mean(x_3435),
    y_3435 = mean(y_3435),
    .groups = "drop"
  ) %>%
  mutate(reference_id = paste0("residential_reference_", row_number()))

reference_sf <- sf::st_as_sf(
  reference_points,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

permit_index <- sf::st_is_within_distance(reference_sf, permits, dist = 250)
links <- purrr::map2_dfr(
  seq_len(nrow(reference_sf)),
  permit_index,
  function(reference_row, permit_rows) {
    if (length(permit_rows) == 0) return(tibble::tibble())
    distances <- sf::st_distance(reference_sf[reference_row, ], permits[permit_rows, ])
    component_pin10 <- unique(str_sub(
      components$pin[components$project_id == reference_sf$project_id[reference_row]],
      1,
      10
    ))
    sf::st_drop_geometry(permits[permit_rows, ]) %>%
      transmute(
        reference_id = reference_sf$reference_id[reference_row],
        project_id = reference_sf$project_id[reference_row],
        reference_sources = reference_sf$reference_sources[reference_row],
        distance_ft = as.numeric(units::set_units(distances[1, ], "ft")),
        permit_id,
        permit,
        permit_pin,
        exact_component_pin10 = vapply(
          permit_pin,
          function(value) any(str_detect(value, fixed(component_pin10)), na.rm = TRUE),
          logical(1)
        ),
        application_start_date,
        issue_date,
        application_year,
        permit_status,
        permit_type,
        permit_address,
        work_description
      )
  }
) %>%
  group_by(project_id, permit_id) %>%
  arrange(distance_ft, .by_group = TRUE) %>%
  summarise(
    reference_id = first(reference_id),
    reference_sources = first(reference_sources),
    minimum_distance_ft = first(distance_ft),
    permit = first(permit),
    permit_pin = first(permit_pin),
    exact_component_pin10 = any(exact_component_pin10),
    application_start_date = first(application_start_date),
    issue_date = first(issue_date),
    application_year = first(application_year),
    permit_status = first(permit_status),
    permit_type = first(permit_type),
    permit_address = first(permit_address),
    work_description = first(work_description),
    .groups = "drop"
  ) %>%
  left_join(
    exact_project_permits %>%
      distinct(project_id, permit_id) %>%
      mutate(upstream_exact_pin10_match = TRUE),
    by = c("project_id", "permit_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    exact_component_pin10 = exact_component_pin10 |
      coalesce(upstream_exact_pin10_match, FALSE)
  ) %>%
  select(-upstream_exact_pin10_match) %>%
  arrange(project_id, minimum_distance_ft, application_start_date, permit_id)

if (anyDuplicated(reference_points$reference_id) > 0 ||
    anyDuplicated(links[c("project_id", "permit_id")]) > 0) {
  stop("Unresolved permit evidence violates reference or project-permit keys.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "projects",
      "projects_with_reference_point",
      "reference_points",
      "nearby_new_construction_permits",
      "projects_with_nearby_new_construction_permit",
      "projects_with_exact_pin10_new_construction_permit"
    ),
    value = c(
      nrow(scope),
      n_distinct(reference_points$project_id),
      nrow(reference_points),
      nrow(links),
      n_distinct(links$project_id),
      n_distinct(links$project_id[links$exact_component_pin10])
    )
  )
)

readr::write_csv(reference_points, "../output/residential_unresolved_reference_points.csv")
readr::write_csv(links, "../output/residential_unresolved_nearby_permits.csv")
readr::write_csv(summary, "../output/residential_unresolved_nearby_permit_summary.csv")
