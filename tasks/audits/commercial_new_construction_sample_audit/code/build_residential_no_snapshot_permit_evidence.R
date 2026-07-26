# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review <- readr::read_csv(
  "../output/residential_tieback_no_snapshot_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_id = readr::col_character(),
    component_pins = readr::col_character(),
    candidate_year = readr::col_double(),
    .default = readr::col_guess()
  )
)

project_points <- sf::st_read(
  "../output/preferred_project_year_centroids.gpkg",
  quiet = TRUE
) %>%
  filter(project_id %in% review$source_project_id) %>%
  select(source_project_id = project_id)

permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  query = paste0(
    "SELECT fid, geom, id, pin, permit, permit_status, permit_type, ",
    "application_start_date, issue_date, street_number, street_direction, ",
    "street_name, work_description ",
    "FROM building_permits_clean ",
    "WHERE permit_type IN ('PERMIT - NEW CONSTRUCTION', ",
    "'PERMIT - WRECKING/DEMOLITION')"
  ),
  quiet = TRUE
) %>%
  sf::st_transform(3435)

if (anyDuplicated(review$source_project_id) > 0 ||
    anyDuplicated(project_points$source_project_id) > 0 ||
    anyDuplicated(permits$id) > 0) {
  stop("Nearby-permit evidence inputs violate their declared keys.", call. = FALSE)
}
if (!setequal(review$source_project_id, project_points$source_project_id)) {
  stop("A no-snapshot tieback project lacks its construction-year centroid.", call. = FALSE)
}

nearby_indices <- sf::st_is_within_distance(project_points, permits, dist = 300)

nearby <- purrr::map2_dfr(
  project_points$source_project_id,
  seq_along(nearby_indices),
  function(source_project_id, i) {
    permit_rows <- nearby_indices[[i]]
    if (length(permit_rows) == 0) {
      return(tibble::tibble())
    }

    distances <- as.numeric(sf::st_distance(
      project_points[i, ],
      permits[permit_rows, ],
      by_element = FALSE
    )[1, ])

    sf::st_drop_geometry(permits[permit_rows, ]) %>%
      mutate(
        source_project_id = source_project_id,
        distance_ft = distances
      )
  }
) %>%
  left_join(
    review %>% select(source_project_id, candidate_year),
    by = "source_project_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    application_start_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_year = lubridate::year(coalesce(issue_date, application_start_date)),
    years_from_candidate = permit_year - candidate_year,
    address = str_squish(paste(street_number, street_direction, street_name))
  ) %>%
  filter(abs(years_from_candidate) <= 5) %>%
  select(
    source_project_id,
    candidate_year,
    distance_ft,
    permit_year,
    years_from_candidate,
    id,
    permit,
    permit_status,
    permit_type,
    application_start_date,
    issue_date,
    pin,
    address,
    work_description
  ) %>%
  arrange(source_project_id, abs(years_from_candidate), distance_ft, permit_year, id)

summary <- review %>%
  select(source_project_id, candidate_year) %>%
  left_join(
    nearby %>%
      group_by(source_project_id) %>%
      summarise(
        nearby_permits = n(),
        nearby_new_construction = sum(permit_type == "PERMIT - NEW CONSTRUCTION"),
        nearby_demolitions = sum(permit_type == "PERMIT - WRECKING/DEMOLITION"),
        nearest_permit_ft = min(distance_ft),
        .groups = "drop"
      ),
    by = "source_project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    nearby_permits = coalesce(nearby_permits, 0L),
    nearby_new_construction = coalesce(nearby_new_construction, 0L),
    nearby_demolitions = coalesce(nearby_demolitions, 0L)
  ) %>%
  arrange(source_project_id)

readr::write_csv(
  nearby,
  "../output/residential_tieback_no_snapshot_nearby_permits.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_no_snapshot_permit_summary.csv"
)
