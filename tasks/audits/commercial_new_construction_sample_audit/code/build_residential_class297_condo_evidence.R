# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    project_kind == "class_297" |
      str_detect(coalesce(candidate_review_categories, ""), fixed("class_297"))
  ) %>%
  select(project_id, component_pins, construction_year)

if (anyDuplicated(projects$project_id) > 0) {
  stop("Residential projects containing class 297 are not unique.", call. = FALSE)
}

requests <- readr::read_csv(
  "../output/residential_successor_condo_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_id %in% projects$project_id)

base_years <- readr::read_csv(
  "../output/residential_successor_condo_base_year_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

cohort_evidence <- purrr::map_dfr(seq_len(nrow(requests)), function(i) {
  request <- requests[i, ]
  target_year <- as.integer(request$target_year)
  evidence <- base_years %>%
    filter(pin10 == request$pin10)

  if (nrow(evidence) == 0) {
    return(request %>% mutate(condo_evidence_status = "missing_condo_history"))
  }

  eligible <- evidence %>% filter(year >= target_year)
  if (nrow(eligible) == 0) {
    eligible <- evidence
  }

  bind_cols(
    request,
    eligible %>%
      slice_min(year, n = 1, with_ties = FALSE) %>%
      select(-pin10)
  ) %>%
    mutate(condo_evidence_status = "cohort_year_selected")
}) %>%
  arrange(project_id, pin10)

current_condo_bases <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin10", "class", "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) %>%
  as_tibble() %>%
  transmute(
    pin10 = str_pad(str_replace_all(pin10, "[^0-9]", ""), 10, pad = "0"),
    class = str_squish(class),
    condo_x_3435 = as.numeric(centroid_x_crs_3435),
    condo_y_3435 = as.numeric(centroid_y_crs_3435)
  ) %>%
  filter(class == "299", !is.na(condo_x_3435), !is.na(condo_y_3435)) %>%
  distinct(pin10, condo_x_3435, condo_y_3435)

ambiguous_base_coordinates <- current_condo_bases %>%
  count(pin10) %>%
  filter(n > 1)

if (nrow(ambiguous_base_coordinates) > 0) {
  stop("Current condo bases have multiple centroids.", call. = FALSE)
}

project_permit_keys <- projects %>%
  transmute(
    preferred_project_id = project_id,
    permit_project_id = component_pins
  ) %>%
  tidyr::separate_longer_delim(permit_project_id, delim = "/") %>%
  mutate(
    permit_project_id = paste0(
      "residential_",
      str_pad(str_replace_all(permit_project_id, "[^0-9]", ""), 14, pad = "0")
    )
  ) %>%
  distinct()

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(
    project_permit_keys,
    by = c("project_id" = "permit_project_id"),
    relationship = "many-to-one"
  ) %>%
  filter(str_detect(str_to_upper(work_description), "NEW|ERECT|FOUNDATION")) %>%
  distinct(preferred_project_id, permit_number, .keep_all = TRUE)

permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) %>%
  filter(permit %in% permit_links$permit_number) %>%
  select(permit)

if (anyDuplicated(permits$permit) > 0) {
  stop("Permit numbers are not unique in the clean permit file.", call. = FALSE)
}

permit_coordinates <- permits %>%
  mutate(
    permit_x_3435 = sf::st_coordinates(.)[, 1],
    permit_y_3435 = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry() %>%
  rename(permit_number = permit)

permit_links <- permit_links %>%
  left_join(
    permit_coordinates,
    by = "permit_number",
    relationship = "many-to-one"
  )

project_bases <- requests %>%
  select(project_id, pin10) %>%
  left_join(
    current_condo_bases,
    by = "pin10",
    relationship = "many-to-one"
  )

permit_base_distances <- purrr::map_dfr(projects$project_id, function(id) {
  project_permits <- permit_links %>%
    filter(preferred_project_id == id)
  project_condos <- project_bases %>%
    filter(project_id == id)

  if (nrow(project_permits) == 0 || nrow(project_condos) == 0) {
    return(tibble::tibble())
  }

  tidyr::crossing(
    project_permits %>%
      select(
        permit_number, application_date, issue_date, permit_status,
        permit_address, work_description, permit_x_3435, permit_y_3435
      ),
    project_condos %>%
      select(pin10, condo_x_3435, condo_y_3435)
  ) %>%
    mutate(
      project_id = id,
      permit_to_condo_distance_ft = sqrt(
        (permit_x_3435 - condo_x_3435)^2 +
          (permit_y_3435 - condo_y_3435)^2
      )
    ) %>%
    select(project_id, everything())
}) %>%
  group_by(project_id, permit_number) %>%
  mutate(nearest_condo_base_for_permit = permit_to_condo_distance_ft == min(permit_to_condo_distance_ft)) %>%
  ungroup() %>%
  arrange(project_id, permit_number, permit_to_condo_distance_ft, pin10)

summary <- tibble::tibble(
  metric = c(
    "projects_containing_class_297",
    "projects_with_successor_condo_candidates",
    "successor_condo_candidate_bases",
    "projects_with_permit_to_condo_distances",
    "permit_to_condo_distance_rows"
  ),
  value = c(
    nrow(projects),
    n_distinct(requests$project_id),
    n_distinct(requests$pin10),
    n_distinct(permit_base_distances$project_id),
    nrow(permit_base_distances)
  )
)

readr::write_csv(
  cohort_evidence,
  "../output/residential_class297_condo_cohort_evidence.csv"
)
readr::write_csv(
  permit_base_distances,
  "../output/residential_class297_permit_condo_distances.csv"
)
readr::write_csv(
  summary,
  "../output/residential_class297_condo_evidence_summary.csv"
)
