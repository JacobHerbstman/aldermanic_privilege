# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

project_geometry <- sf::st_read(
  "../output/historical_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(issue_date),
    !is.na(application_start_date)
  ) %>%
  mutate(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    application_year = lubridate::year(application_date),
    issue_year = lubridate::year(issue_date),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    referenced_permit_numbers = purrr::map2_chr(
      str_extract_all(
        str_to_upper(coalesce(work_description, "")),
        "(?<![0-9])10[0-9]{7}(?![0-9])"
      ),
      permit_number,
      ~ paste(sort(setdiff(unique(.x), .y)), collapse = "/")
    )
  ) %>%
  select(
    permit_id,
    permit_number,
    application_date,
    issue_date,
    application_year,
    issue_year,
    permit_status,
    permit_address,
    referenced_permit_numbers,
    work_description
  )

exact_links <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  distinct(source_family, project_id, permit_id) %>%
  mutate(exact_pin_match = TRUE)

candidate_index <- sf::st_is_within_distance(project_geometry, permits, dist = 200)

spatial_matches <- purrr::map2_dfr(
  seq_len(nrow(project_geometry)),
  candidate_index,
  function(project_row, permit_rows) {
    if (length(permit_rows) == 0) {
      return(tibble::tibble())
    }
    project <- project_geometry[project_row, ]
    permit_candidates <- permits[permit_rows, ]
    polygon_distance_ft <- as.numeric(sf::st_distance(
      project[rep(1, length(permit_rows)), ],
      permit_candidates,
      by_element = TRUE
    ))
    bind_cols(
      sf::st_drop_geometry(project[rep(1, length(permit_rows)), ]) %>%
        select(source_family, project_id, target_year),
      sf::st_drop_geometry(permit_candidates) %>%
        select(
          permit_id,
          permit_number,
          application_date,
          issue_date,
          application_year,
          issue_year,
          permit_status,
          permit_address,
          referenced_permit_numbers,
          work_description
        ),
      tibble::tibble(polygon_distance_ft)
    )
  }
) %>%
  mutate(
    application_year_gap = target_year - application_year,
    issue_year_gap = target_year - issue_year,
    plausible_application_window = between(application_year_gap, -2, 6),
    plausible_issue_window = is.na(issue_year_gap) | between(issue_year_gap, -2, 4)
  ) %>%
  filter(plausible_application_window, plausible_issue_window) %>%
  left_join(
    exact_links,
    by = c("source_family", "project_id", "permit_id"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    exact_pin_match = coalesce(exact_pin_match, FALSE),
    spatial_match_method = case_when(
      exact_pin_match ~ "exact_pin_and_project_polygon",
      polygon_distance_ft == 0 ~ "inside_project_polygon",
      TRUE ~ "within_200ft_of_project_polygon"
    )
  ) %>%
  distinct(source_family, project_id, target_year, permit_id, .keep_all = TRUE) %>%
  arrange(source_family, project_id, target_year, polygon_distance_ft, application_date, permit_id)

project_summary <- project_geometry %>%
  sf::st_drop_geometry() %>%
  select(source_family, project_id, target_year) %>%
  left_join(
    exact_links %>%
      count(source_family, project_id, name = "plausible_exact_permits"),
    by = c("source_family", "project_id"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    spatial_matches %>%
      group_by(source_family, project_id, target_year) %>%
      summarise(
        plausible_spatial_permits = n_distinct(permit_id),
        inside_polygon_permits = n_distinct(permit_id[polygon_distance_ft == 0]),
        nonexact_inside_polygon_permits = n_distinct(
          permit_id[polygon_distance_ft == 0 & !exact_pin_match]
        ),
        nearest_permit_distance_ft = min(polygon_distance_ft),
        spatial_permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
        .groups = "drop"
      ),
    by = c("source_family", "project_id", "target_year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    plausible_exact_permits = coalesce(plausible_exact_permits, 0L),
    plausible_spatial_permits = coalesce(plausible_spatial_permits, 0L),
    inside_polygon_permits = coalesce(inside_polygon_permits, 0L),
    nonexact_inside_polygon_permits = coalesce(nonexact_inside_polygon_permits, 0L),
    spatial_permit_ids = coalesce(spatial_permit_ids, "")
  )

summary <- bind_rows(
  project_summary %>%
    filter(plausible_exact_permits == 0, nonexact_inside_polygon_permits > 0) %>%
    count(source_family, name = "value") %>%
    transmute(metric = paste0(source_family, "_projects_recovered_by_inside_polygon_permit"), value),
  project_summary %>%
    filter(plausible_exact_permits == 0, plausible_spatial_permits > 0) %>%
    count(source_family, name = "value") %>%
    transmute(metric = paste0(source_family, "_projects_recovered_within_200ft"), value),
  tibble::tibble(metric = "plausible_project_permit_links", value = nrow(spatial_matches)),
  tibble::tibble(metric = "nonexact_inside_polygon_links", value = sum(
    spatial_matches$polygon_distance_ft == 0 & !spatial_matches$exact_pin_match
  ))
)

readr::write_csv(summary, "../output/spatial_permit_evidence_summary.csv")
readr::write_csv(spatial_matches, "../output/new_construction_spatial_permit_matches.csv")
readr::write_csv(project_summary, "../output/new_construction_spatial_permit_summary.csv")
