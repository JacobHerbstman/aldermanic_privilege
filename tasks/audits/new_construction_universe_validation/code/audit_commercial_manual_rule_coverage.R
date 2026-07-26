# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

same_number <- function(x, y, tolerance = 1e-8) {
  (is.na(x) & is.na(y)) |
    (!is.na(x) & !is.na(y) & abs(x - y) <= tolerance)
}

collapse_numbers <- function(x) {
  values <- sort(unique(x[is.finite(x)]))
  if (length(values) == 0) NA_character_ else paste(values, collapse = "/")
}

candidates <- readr::read_csv(
  "../input/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    candidate_status,
    candidate_reason = decision_reason,
    candidate_year = construction_year,
    candidate_units = dwelling_units,
    candidate_building_sqft = building_sqft,
    candidate_land_sqft = land_sqft
  )

preferred <- readr::read_csv(
  "../input/preferred_commercial_project_ledger.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(decision_source == "manual_ledger") |>
  dplyr::select(
    project_id,
    final_year = construction_year,
    final_units = dwelling_units,
    final_building_sqft = building_sqft,
    final_land_sqft = land_sqft,
    decision_action,
    confidence,
    decision_reason,
    evidence_ids,
    evidence_urls
  )

scope <- readr::read_csv(
  "../input/final_density_model_input.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    distance_to_boundary_ft,
    within_500ft,
    within_1500ft
  )

unit_evidence <- readr::read_csv(
  "../input/commercial_unit_adjudication_evidence.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    recommended_units,
    recommended_units_source,
    resolved_address_unit_sum,
    resolved_permit_unit_values,
    unit_review_required,
    unit_review_reason
  )

completion <- readr::read_csv(
  "../input/commercial_completion_evidence.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    completion_evidence_status,
    issued_new_construction_permits,
    completed_new_construction_permits,
    later_assessor_report_after_permit
  )

land_evidence <- readr::read_csv(
  "../input/commercial_land_adjudication_evidence.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    project_land_area_sqft,
    geography_status,
    land_review_required
  )

permit_units <- readr::read_csv(
  "../input/new_construction_permit_unit_mentions.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(source_family == "commercial") |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    permit_unit_values = collapse_numbers(unit_count),
    permit_unit_value_count = dplyr::n_distinct(unit_count[is.finite(unit_count)]),
    unique_permit_units = dplyr::if_else(
      permit_unit_value_count == 1L,
      min(unit_count, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

permit_years <- readr::read_csv(
  "../input/project_permit_chain_links.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(source_family == "commercial", directly_matched) |>
  dplyr::mutate(
    work_description = stringr::str_to_upper(
      dplyr::coalesce(work_description, "")
    ),
    explicit_new_building = stringr::str_detect(
      work_description,
      paste0(
        "\\bNEW CONSTRUCTION\\b|",
        "\\bCONSTRUCTION OF (?:A |AN )?NEW\\b|",
        "\\bCONSTRUCT(?:ION)? (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?[0-9]+[ -]?STORY\\b|",
        "\\bFULL BUILDING PERMIT\\b"
      )
    ),
    application_year = lubridate::year(as.Date(application_date)),
    issue_year = lubridate::year(as.Date(issue_date))
  ) |>
  dplyr::filter(explicit_new_building) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    permit_application_years = collapse_numbers(application_year),
    permit_issue_years = collapse_numbers(issue_year),
    permit_construction_years = list(unique(c(
      application_year,
      issue_year,
      issue_year + 1L
    ))),
    .groups = "drop"
  )

for (x in list(
  candidates,
  preferred,
  scope,
  unit_evidence,
  completion,
  land_evidence,
  permit_units,
  permit_years
)) {
  if (anyDuplicated(x$project_id)) {
    stop("A commercial-rule input is not uniquely keyed by project_id.")
  }
}

coverage <- preferred |>
  dplyr::left_join(candidates, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(scope, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(
    unit_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(completion, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(
    land_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(permit_units, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(permit_years, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    within_500ft = dplyr::coalesce(within_500ft, FALSE),
    within_1500ft = dplyr::coalesce(within_1500ft, FALSE),
    year_matches_candidate = same_number(final_year, candidate_year),
    year_matches_permit = purrr::map2_lgl(
      final_year,
      permit_construction_years,
      ~ is.numeric(.y) && any(.x == .y, na.rm = TRUE)
    ),
    units_match_candidate = same_number(final_units, candidate_units),
    building_matches_candidate = same_number(
      final_building_sqft,
      candidate_building_sqft
    ),
    land_matches_candidate = same_number(final_land_sqft, candidate_land_sqft),
    land_matches_polygon = dplyr::if_else(
      is.finite(final_land_sqft) &
        is.finite(project_land_area_sqft) &
        final_land_sqft > 0 &
        project_land_area_sqft > 0,
      abs(final_land_sqft - project_land_area_sqft) / final_land_sqft <= 0.01,
      FALSE
    ),
    units_match_recommended = same_number(final_units, recommended_units),
    units_match_address_sum = same_number(
      final_units,
      resolved_address_unit_sum
    ),
    units_match_unique_permit = same_number(final_units, unique_permit_units),
    strong_completion_evidence =
      dplyr::coalesce(issued_new_construction_permits > 0, FALSE) |
      dplyr::coalesce(completed_new_construction_permits > 0, FALSE) |
      dplyr::coalesce(
        completion_evidence_status %in% c(
          "issued_new_permit_and_later_assessor",
          "city_building_year_support"
        ),
        FALSE
      ),
    all_fields_match_candidate =
      year_matches_candidate &
      units_match_candidate &
      building_matches_candidate &
      land_matches_candidate,
    all_fields_have_programmatic_support =
      (year_matches_candidate | year_matches_permit) &
      (units_match_candidate |
        units_match_recommended |
        units_match_address_sum |
        units_match_unique_permit) &
      building_matches_candidate &
      (land_matches_candidate | land_matches_polygon),
    strict_rule_status = dplyr::case_when(
      decision_action == "merge_source_projects" ~
        "manual_project_membership_exception",
      candidate_status == "retain_mechanical" &
        all_fields_match_candidate ~
        "automatic_candidate_rule",
      all_fields_match_candidate &
        strong_completion_evidence ~
        "automatic_candidate_with_completion_evidence",
      year_matches_permit &
        units_match_candidate &
        building_matches_candidate &
        land_matches_candidate &
        strong_completion_evidence ~
        "automatic_permit_year",
      !is.finite(candidate_land_sqft) &
        land_matches_polygon &
        year_matches_candidate &
        units_match_candidate &
        building_matches_candidate ~
        "automatic_polygon_land_recovery",
      TRUE ~ "manual_exception_required"
    ),
    support_status = dplyr::case_when(
      strict_rule_status != "manual_exception_required" ~ strict_rule_status,
      all_fields_have_programmatic_support ~
        "available_sources_support_all_final_fields",
      TRUE ~ "final_field_not_reproduced_by_current_rules"
    ),
    unsupported_fields = purrr::pmap_chr(
      list(
        year_matches_candidate | year_matches_permit,
        units_match_candidate |
          units_match_recommended |
          units_match_address_sum |
          units_match_unique_permit,
        building_matches_candidate,
        land_matches_candidate | land_matches_polygon
      ),
      function(year_supported, units_supported, building_supported,
               land_supported) {
        fields <- c("year", "units", "building_sqft", "land_sqft")
        supported <- c(
          year_supported,
          units_supported,
          building_supported,
          land_supported
        )
        paste(fields[!supported], collapse = "/")
      }
    ),
    unsupported_fields = dplyr::na_if(unsupported_fields, "")
  ) |>
  dplyr::arrange(
    dplyr::desc(within_500ft),
    strict_rule_status,
    project_id
  ) |>
  dplyr::select(-permit_construction_years)

summary <- dplyr::bind_rows(
  coverage |>
    dplyr::filter(within_1500ft) |>
    dplyr::count(
      section = "strict_rule_status",
      status = strict_rule_status,
      within_500ft,
      name = "projects"
    ),
  coverage |>
    dplyr::filter(within_1500ft) |>
    dplyr::count(
      section = "support_status",
      status = support_status,
      within_500ft,
      name = "projects"
    ),
  tibble::tibble(
    section = "field_differences",
    status = c(
      "year",
      "units",
      "building_sqft",
      "land_sqft"
    ),
    within_500ft = NA,
    projects = c(
      sum(coverage$within_1500ft & !coverage$year_matches_candidate),
      sum(coverage$within_1500ft & !coverage$units_match_candidate),
      sum(coverage$within_1500ft & !coverage$building_matches_candidate),
      sum(coverage$within_1500ft & !coverage$land_matches_candidate)
    )
  )
) |>
  dplyr::arrange(section, status, within_500ft)

readr::write_csv(
  coverage,
  "../output/commercial_manual_rule_coverage.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/commercial_manual_rule_summary.csv",
  na = ""
)
