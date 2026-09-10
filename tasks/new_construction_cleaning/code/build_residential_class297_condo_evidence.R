# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

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

stopifnot(!anyDuplicated(requests[c("project_id", "pin10")]),
  !anyDuplicated(base_years[c("pin10", "year")]))

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
    mutate(condo_evidence_status = if_else(year >= target_year,
      "cohort_year_selected", "earliest_available_before_target"))
}) %>%
  arrange(project_id, pin10)

readr::write_csv(cohort_evidence, "../output/residential_class297_condo_cohort_evidence.csv")
