# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

missing_episodes <- readr::read_csv(
  "../output/residential_unresolved_historical_parcel_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(coverage_status == "missing_pin10") %>%
  distinct(episode_id, project_id, target_year)

existing_points <- readr::read_csv(
  "../output/residential_unresolved_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    reference_sources = readr::col_character(),
    reference_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    project_id,
    x_3435 = as.numeric(x_3435),
    y_3435 = as.numeric(y_3435),
    reference_source = reference_sources,
    source_record_id = reference_id
  )

address_points <- bind_rows(
  readr::read_csv(
    "../output/residential_unresolved_address_geocodes.csv",
    show_col_types = FALSE,
    col_types = readr::cols(
      request_id = readr::col_character(),
      project_id = readr::col_character(),
      address = readr::col_character(),
      geocode_status = readr::col_character(),
      .default = readr::col_guess()
    )
  ) %>%
    mutate(reference_kind = "accepted_project_address"),
  readr::read_csv(
    "../output/residential_unresolved_permit_address_geocodes.csv",
    show_col_types = FALSE,
    col_types = readr::cols(
      request_id = readr::col_character(),
      project_id = readr::col_character(),
      address = readr::col_character(),
      geocode_status = readr::col_character(),
      .default = readr::col_guess()
    )
  ) %>%
    mutate(reference_kind = "accepted_permit_address")
) %>%
  filter(
    geocode_status == "accepted_reference_point",
    is.finite(x_3435),
    is.finite(y_3435)
  ) %>%
  transmute(
    project_id,
    x_3435 = as.numeric(x_3435),
    y_3435 = as.numeric(y_3435),
    reference_source = paste(reference_kind, address, sep = ":"),
    source_record_id = request_id
  )

natural_points <- bind_rows(existing_points, address_points) %>%
  filter(is.finite(x_3435), is.finite(y_3435)) %>%
  mutate(
    x_key = round(x_3435, 1),
    y_key = round(y_3435, 1)
  ) %>%
  group_by(project_id, x_key, y_key) %>%
  summarise(
    x_3435 = mean(x_3435),
    y_3435 = mean(y_3435),
    reference_sources = paste(sort(unique(reference_source)), collapse = " || "),
    source_record_ids = paste(sort(unique(source_record_id)), collapse = "/"),
    .groups = "drop"
  )

reference_overrides <- readr::read_csv(
  "../adjudication/residential_unresolved_predecessor_reference_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

if (anyDuplicated(reference_overrides$project_id) > 0) {
  stop("Predecessor reference overrides are not unique by target project.", call. = FALSE)
}

override_points <- reference_overrides %>%
  inner_join(
    natural_points,
    by = c("reference_project_id" = "project_id"),
    relationship = "one-to-many"
  ) %>%
  transmute(
    project_id,
    x_key,
    y_key,
    x_3435,
    y_3435,
    reference_sources = paste0(
      "shared_project_reference:", reference_project_id, " || ",
      reference_sources
    ),
    source_record_ids = paste0("manual_reference_override/", source_record_ids),
    override_reason = reason,
    override_evidence = evidence
  )

points <- bind_rows(
  natural_points %>%
    mutate(override_reason = NA_character_, override_evidence = NA_character_),
  override_points
) %>%
  semi_join(missing_episodes %>% distinct(project_id), by = "project_id") %>%
  arrange(project_id, x_key, y_key) %>%
  group_by(project_id) %>%
  mutate(project_point_number = row_number()) %>%
  ungroup() %>%
  mutate(point_id = paste(project_id, project_point_number, sep = "|"))

if (anyDuplicated(points$point_id) > 0) {
  stop("Residential unresolved predecessor point IDs are not unique.", call. = FALSE)
}

points_by_project <- points %>%
  group_by(project_id) %>%
  summarise(point_rows = list(pick(everything())), .groups = "drop")

requests <- missing_episodes %>%
  left_join(points_by_project, by = "project_id", relationship = "many-to-one") %>%
  mutate(
    point_rows = purrr::map(
      point_rows,
      ~ if (is.null(.x)) tibble::tibble() else .x
    )
  ) %>%
  tidyr::unnest(point_rows, keep_empty = TRUE) %>%
  mutate(
    point_request_id = if_else(
      is.na(point_id),
      paste(episode_id, "no_reference_point", sep = "|"),
      paste(episode_id, point_id, sep = "|")
    ),
    reference_status = if_else(
      is.na(point_id),
      "reference_point_unresolved",
      "reference_point_available"
    )
  ) %>%
  arrange(project_id, target_year, project_point_number)

if (anyDuplicated(requests$point_request_id) > 0) {
  stop("Residential unresolved predecessor requests are not unique.", call. = FALSE)
}
if (n_distinct(requests$episode_id) != nrow(missing_episodes)) {
  stop("A missing residential episode disappeared from the reference table.", call. = FALSE)
}

summary <- bind_rows(
  tibble::tibble(
    metric = c(
      "episodes_with_missing_source_pin",
      "projects_with_missing_source_pin",
      "distinct_reference_points",
      "episode_point_requests",
      "episodes_without_reference_point"
    ),
    value = c(
      nrow(missing_episodes),
      n_distinct(missing_episodes$project_id),
      nrow(points),
      sum(requests$reference_status == "reference_point_available"),
      n_distinct(requests$episode_id[requests$reference_status == "reference_point_unresolved"])
    )
  ),
  requests %>%
    distinct(episode_id, reference_status) %>%
    count(reference_status, name = "value") %>%
    transmute(metric = paste0("episodes_", reference_status), value)
)

readr::write_csv(
  requests,
  "../output/residential_unresolved_predecessor_reference_points.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_predecessor_reference_summary.csv"
)
