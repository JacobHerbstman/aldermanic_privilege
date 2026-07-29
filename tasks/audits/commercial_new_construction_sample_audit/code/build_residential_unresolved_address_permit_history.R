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
  transmute(
    project_id,
    component_pin = component_pins,
    component_pin10 = str_sub(component_pins, 1, 10)
  ) %>%
  distinct()

address_requests <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    request_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    project_id,
    requested_address = address,
    address_normalized = str_squish(str_to_upper(address_normalized))
  ) %>%
  distinct()

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  transmute(
    permit_id = as.character(id),
    permit_number = as.character(permit),
    permit_pin10 = str_sub(str_replace_all(coalesce(as.character(pin), ""), "[^0-9]", ""), 1, 10),
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    permit_address_normalized = str_squish(str_to_upper(permit_address)),
    application_start_date,
    issue_date,
    permit_status,
    permit_type,
    work_description = str_squish(coalesce(work_description, ""))
  ) %>%
  filter(
    !is.na(application_start_date),
    between(lubridate::year(application_start_date), 1999L, 2025L)
  )

permit_address_groups <- permits %>%
  filter(permit_address_normalized != "") %>%
  tidyr::nest(
    permit_rows = c(
      permit_id,
      permit_number,
      permit_pin10,
      permit_address,
      application_start_date,
      issue_date,
      permit_status,
      permit_type,
      work_description
    ),
    .by = permit_address_normalized
  )

address_matches <- address_requests %>%
  inner_join(
    permit_address_groups,
    by = c("address_normalized" = "permit_address_normalized"),
    relationship = "many-to-one"
  ) %>%
  tidyr::unnest(permit_rows) %>%
  transmute(
    project_id,
    permit_id,
    permit_number,
    permit_pin10,
    permit_address,
    application_start_date,
    issue_date,
    permit_status,
    permit_type,
    work_description,
    match_method = "exact_normalized_address",
    match_evidence = requested_address
  )

permit_pin_groups <- permits %>%
  filter(permit_pin10 != "") %>%
  tidyr::nest(
    permit_rows = c(
      permit_id,
      permit_number,
      permit_address,
      application_start_date,
      issue_date,
      permit_status,
      permit_type,
      work_description
    ),
    .by = permit_pin10
  )

pin_matches <- components %>%
  filter(component_pin10 != "") %>%
  inner_join(
    permit_pin_groups,
    by = c("component_pin10" = "permit_pin10"),
    relationship = "many-to-one"
  ) %>%
  tidyr::unnest(permit_rows) %>%
  transmute(
    project_id,
    permit_id,
    permit_number,
    permit_pin10 = component_pin10,
    permit_address,
    application_start_date,
    issue_date,
    permit_status,
    permit_type,
    work_description,
    match_method = "exact_component_pin10",
    match_evidence = component_pin
  )

history <- bind_rows(address_matches, pin_matches) %>%
  group_by(project_id, permit_id) %>%
  summarise(
    permit_number = dplyr::first(permit_number),
    permit_pin10 = dplyr::first(permit_pin10[permit_pin10 != ""], default = NA_character_),
    permit_address = dplyr::first(permit_address),
    application_start_date = dplyr::first(application_start_date),
    issue_date = dplyr::first(issue_date),
    permit_status = dplyr::first(permit_status),
    permit_type = dplyr::first(permit_type),
    work_description = dplyr::first(work_description),
    match_methods = paste(sort(unique(match_method)), collapse = "/"),
    match_evidence = paste(sort(unique(match_evidence)), collapse = "/"),
    .groups = "drop"
  ) %>%
  arrange(project_id, application_start_date, permit_id)

project_summary <- history %>%
  mutate(
    permit_evidence = paste0(
      "permit=", permit_number,
      "; applied=", application_start_date,
      "; issued=", coalesce(as.character(issue_date), "missing"),
      "; status=", permit_status,
      "; type=", permit_type,
      "; address=", permit_address,
      "; match=", match_methods,
      "; work=", work_description
    )
  ) %>%
  group_by(project_id) %>%
  summarise(
    exact_address_or_pin_permits = n(),
    earliest_permit_application = min(application_start_date),
    latest_permit_application = max(application_start_date),
    complete_permits = sum(permit_status == "COMPLETE", na.rm = TRUE),
    cancelled_or_expired_permits = sum(
      permit_status %in% c("CANCELLED", "EXPIRED", "REVOKED"),
      na.rm = TRUE
    ),
    exact_address_or_pin_permit_evidence = paste(permit_evidence, collapse = " || "),
    .groups = "drop"
  ) %>%
  right_join(scope %>% select(project_id), by = "project_id", relationship = "one-to-one") %>%
  mutate(
    exact_address_or_pin_permits = coalesce(exact_address_or_pin_permits, 0L),
    complete_permits = coalesce(complete_permits, 0L),
    cancelled_or_expired_permits = coalesce(cancelled_or_expired_permits, 0L)
  ) %>%
  arrange(project_id)

if (anyDuplicated(history[c("project_id", "permit_id")]) > 0 ||
    nrow(project_summary) != nrow(scope) ||
    anyDuplicated(project_summary$project_id) > 0 ||
    !setequal(project_summary$project_id, scope$project_id)) {
  stop("Exact residential permit histories violate project-permit keys or coverage.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "projects",
    "projects_with_exact_address_or_pin_permit",
    "exact_project_permit_links",
    "unique_permits",
    "duplicate_project_permit_links",
    "unaccounted_projects"
  ),
  value = c(
    nrow(scope),
    sum(project_summary$exact_address_or_pin_permits > 0),
    nrow(history),
    n_distinct(history$permit_id),
    anyDuplicated(history[c("project_id", "permit_id")]),
    nrow(scope) - nrow(project_summary)
  )
)

readr::write_csv(
  history,
  "../output/residential_unresolved_address_permit_history.csv"
)
readr::write_csv(
  project_summary,
  "../output/residential_unresolved_address_permit_project_summary.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_address_permit_summary.csv"
)
