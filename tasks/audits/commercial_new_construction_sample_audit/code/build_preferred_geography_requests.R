# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

residential <- readr::read_csv(
  "../output/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    candidate_status = readr::col_character(),
    construction_year = readr::col_double(),
    component_pins = readr::col_character(),
    .default = readr::col_skip()
  )
)

commercial <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    candidate_status = readr::col_character(),
    construction_year = readr::col_double(),
    component_pins = readr::col_character(),
    .default = readr::col_skip()
  )
)

project_columns <- c(
  "source_family", "project_id", "project_kind", "candidate_status",
  "construction_year", "component_pins"
)

all_candidates <- bind_rows(
  residential %>% select(all_of(project_columns)),
  commercial %>% select(all_of(project_columns))
)

projects <- all_candidates %>%
  filter(
    candidate_status != "exclude_outside_period",
    between(construction_year, 2006L, 2022L)
  )

if (anyDuplicated(projects$project_id) > 0) {
  stop("Preferred geography projects are not unique by project ID.", call. = FALSE)
}

requests <- projects %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  transmute(
    source_family,
    project_id,
    project_kind,
    candidate_status,
    target_year = as.integer(construction_year),
    component_pin = component_pins,
    pin10 = str_sub(component_pins, 1, 10)
  ) %>%
  distinct(project_id, component_pin, target_year, .keep_all = TRUE) %>%
  arrange(target_year, pin10, project_id, component_pin)

if (any(is.na(requests$component_pin) | str_length(requests$component_pin) != 14)) {
  stop("Preferred geography request contains an invalid component PIN.", call. = FALSE)
}
if (anyDuplicated(requests[c("project_id", "component_pin", "target_year")]) > 0) {
  stop("Preferred project-component-year requests are not unique.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "study_period_projects_with_finite_year",
    "requested_project_component_years",
    "distinct_year_pin10_requests",
    "study_period_candidates_missing_year"
  ),
  value = c(
    nrow(projects),
    nrow(requests),
    nrow(distinct(requests, target_year, pin10)),
    sum(all_candidates$candidate_status != "exclude_outside_period" &
      is.na(all_candidates$construction_year))
  )
)

readr::write_csv(requests, "../output/preferred_project_geography_requests.csv")
readr::write_csv(summary, "../output/preferred_project_geography_request_summary.csv")
