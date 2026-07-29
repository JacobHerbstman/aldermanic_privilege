# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review <- readr::read_csv(
  "../output/residential_tieback_no_snapshot_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(source_project_id, candidate_year)

requests <- readr::read_csv(
  "../output/residential_successor_condo_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), pin10 = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(project_id %in% review$source_project_id) %>%
  transmute(
    source_project_id = project_id,
    condo_base = pin10,
    target_year,
    link_method,
    link_reason
  )

base_years <- readr::read_csv(
  "../output/residential_successor_condo_base_year_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin10 = readr::col_character(), .default = readr::col_guess())
) %>%
  rename(condo_base = pin10)

if (anyDuplicated(review$source_project_id) > 0 ||
    anyDuplicated(requests[c("source_project_id", "condo_base")]) > 0 ||
    anyDuplicated(base_years[c("condo_base", "year")]) > 0) {
  stop("No-snapshot condominium evidence violates its declared keys.", call. = FALSE)
}

evidence <- purrr::map_dfr(seq_len(nrow(requests)), function(i) {
  request <- requests[i, ]
  history <- base_years %>% filter(condo_base == request$condo_base)

  if (nrow(history) == 0) {
    return(request %>% mutate(condo_evidence_status = "missing_condo_history"))
  }

  eligible <- history %>% filter(year >= request$target_year)
  if (nrow(eligible) == 0) {
    eligible <- history
  }

  bind_cols(
    request,
    eligible %>%
      slice_min(year, n = 1, with_ties = FALSE) %>%
      select(-condo_base)
  ) %>%
    mutate(condo_evidence_status = "cohort_year_selected")
}) %>%
  arrange(source_project_id, condo_base)

if (nrow(evidence) != nrow(requests) ||
    anyDuplicated(evidence[c("source_project_id", "condo_base")]) > 0) {
  stop("No-snapshot condominium evidence does not cover each request once.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "no_snapshot_projects",
    "projects_with_successor_condo_bases",
    "successor_condo_bases",
    "missing_condo_histories"
  ),
  value = c(
    nrow(review),
    n_distinct(requests$source_project_id),
    nrow(requests),
    sum(evidence$condo_evidence_status == "missing_condo_history")
  )
)

readr::write_csv(
  evidence,
  "../output/residential_tieback_no_snapshot_condo_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_no_snapshot_condo_summary.csv"
)
