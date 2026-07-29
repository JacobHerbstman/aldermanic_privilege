# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review_projects <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  select(project_id, project_kind)

if (anyDuplicated(review_projects$project_id) > 0) {
  stop("Residential review projects are not unique.", call. = FALSE)
}

project_geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  filter(project_id %in% review_projects$project_id) %>%
  select(project_id, target_year)

if (anyDuplicated(project_geometry$project_id) > 0) {
  stop("Residential review geometry is not unique by project.", call. = FALSE)
}

missing_geometry <- review_projects %>%
  sf::st_drop_geometry() %>%
  anti_join(
    project_geometry %>% sf::st_drop_geometry() %>% select(project_id),
    by = "project_id"
  )

if (nrow(missing_geometry) > 0) {
  stop("Residential review projects are missing construction-year geometry.", call. = FALSE)
}

current_parcels <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "pin10", "tax_year", "class",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) %>%
  as_tibble() %>%
  transmute(
    pin = str_pad(str_replace_all(pin, "[^0-9]", ""), 14, pad = "0"),
    pin10 = str_pad(str_replace_all(pin10, "[^0-9]", ""), 10, pad = "0"),
    tax_year = as.integer(tax_year),
    class = str_squish(class),
    x_3435 = as.numeric(centroid_x_crs_3435),
    y_3435 = as.numeric(centroid_y_crs_3435)
  ) %>%
  filter(!is.na(x_3435), !is.na(y_3435))

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel PINs are not unique.", call. = FALSE)
}

current_parcels_sf <- current_parcels %>%
  sf::st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)

intersections <- sf::st_intersects(current_parcels_sf, project_geometry)
linked_rows <- which(lengths(intersections) > 0)

current_parcel_links <- tibble::tibble(
  current_row = rep(linked_rows, lengths(intersections[linked_rows])),
  project_row = unlist(intersections[linked_rows], use.names = FALSE)
) %>%
  mutate(
    project_id = project_geometry$project_id[project_row],
    target_year = project_geometry$target_year[project_row],
    pin = current_parcels$pin[current_row],
    pin10 = current_parcels$pin10[current_row],
    tax_year = current_parcels$tax_year[current_row],
    class = current_parcels$class[current_row]
  ) %>%
  select(project_id, target_year, pin, pin10, tax_year, class) %>%
  distinct() %>%
  group_by(pin) %>%
  mutate(projects_per_current_pin = n_distinct(project_id)) %>%
  ungroup() %>%
  arrange(project_id, pin)

spatial_condo_requests <- current_parcel_links %>%
  filter(class == "299") %>%
  left_join(review_projects, by = "project_id", relationship = "many-to-one") %>%
  distinct(project_id, project_kind, target_year, pin10) %>%
  mutate(
    link_method = "current_centroid_in_construction_year_polygon",
    link_reason = NA_character_
  )

condo_overrides <- readr::read_csv(
  "../adjudication/residential_successor_condo_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  mutate(
    pin10 = str_pad(str_replace_all(pin10, "[^0-9]", ""), 10, pad = "0")
  )

if (anyDuplicated(condo_overrides[c("project_id", "pin10")]) > 0) {
  stop("Residential successor condo overrides are not unique.", call. = FALSE)
}

unknown_override_projects <- condo_overrides %>%
  anti_join(review_projects, by = "project_id")

if (nrow(unknown_override_projects) > 0) {
  stop("Residential successor condo overrides contain unknown projects.", call. = FALSE)
}

manual_condo_requests <- condo_overrides %>%
  left_join(review_projects, by = "project_id", relationship = "many-to-one") %>%
  left_join(
    project_geometry %>% sf::st_drop_geometry() %>% select(project_id, target_year),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  transmute(
    project_id,
    project_kind,
    target_year,
    pin10,
    link_method = "manual_successor_override",
    link_reason = reason
  )

condo_requests <- bind_rows(spatial_condo_requests, manual_condo_requests) %>%
  arrange(project_id, pin10, desc(link_method == "manual_successor_override")) %>%
  distinct(project_id, pin10, .keep_all = TRUE) %>%
  group_by(pin10) %>%
  mutate(projects_per_condo_base = n_distinct(project_id)) %>%
  ungroup() %>%
  arrange(pin10, project_id)

ambiguous_links <- condo_requests %>%
  filter(projects_per_condo_base > 1)

summary <- tibble::tibble(
  metric = c(
    "review_projects",
    "review_projects_with_current_parcels",
    "current_parcel_links",
    "current_pins_linked_to_multiple_projects",
    "review_projects_with_condo_successors",
    "distinct_condo_bases_requested",
    "condo_bases_linked_to_multiple_projects"
  ),
  value = c(
    nrow(review_projects),
    n_distinct(current_parcel_links$project_id),
    nrow(current_parcel_links),
    n_distinct(current_parcel_links$pin[current_parcel_links$projects_per_current_pin > 1]),
    n_distinct(condo_requests$project_id),
    n_distinct(condo_requests$pin10),
    n_distinct(ambiguous_links$pin10)
  )
)

readr::write_csv(
  current_parcel_links,
  "../output/residential_review_current_parcel_links.csv"
)
readr::write_csv(
  condo_requests,
  "../output/residential_successor_condo_requests.csv"
)
readr::write_csv(
  ambiguous_links,
  "../output/residential_successor_condo_ambiguous_links.csv"
)
readr::write_csv(
  summary,
  "../output/residential_successor_condo_request_summary.csv"
)
