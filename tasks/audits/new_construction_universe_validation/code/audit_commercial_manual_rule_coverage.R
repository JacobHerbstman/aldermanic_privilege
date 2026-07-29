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

extract_unit_count <- function(description) {
  matches <- stringr::str_match_all(
    stringr::str_to_upper(dplyr::coalesce(description, "")),
    paste0(
      "\\b([0-9]{1,4})\\s*(?:TOTAL\\s+)?",
      "(?:DWELLING\\s+|RESIDENTIAL\\s+|APARTMENT\\s+|",
      "EFFICIENCY\\s+)?(?:UNITS?|D\\.?U\\.?)\\b"
    )
  )[[1]]
  counts <- suppressWarnings(as.numeric(matches[, 2]))
  if (length(counts) == 0 || all(is.na(counts))) {
    NA_real_
  } else {
    max(counts, na.rm = TRUE)
  }
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
  dplyr::select(
    project_id,
    source_project_ids,
    decision_source,
    decision_id,
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

project_aliases <- preferred |>
  dplyr::select(project_id, source_project_ids) |>
  tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
  dplyr::transmute(
    final_project_id = project_id,
    source_project_id = stringr::str_trim(source_project_ids)
  ) |>
  dplyr::filter(source_project_id != "") |>
  dplyr::distinct()

unambiguous_project_aliases <- project_aliases |>
  dplyr::add_count(source_project_id, name = "final_project_count") |>
  dplyr::filter(final_project_count == 1L) |>
  dplyr::select(-final_project_count)

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

permit_unit_mentions <- readr::read_csv(
  "../input/new_construction_permit_unit_mentions.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(source_family == "commercial") |>
  dplyr::mutate(permit_number = as.character(permit_number))

permit_units <- permit_unit_mentions |>
  dplyr::inner_join(
    unambiguous_project_aliases,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id = final_project_id) |>
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

connection <- DBI::dbConnect(
  RSQLite::SQLite(),
  "../input/building_permits_clean.gpkg",
  flags = RSQLite::SQLITE_RO
)
on.exit(DBI::dbDisconnect(connection), add = TRUE)

permit_metadata <- DBI::dbGetQuery(
  connection,
  paste(
    "SELECT",
    "CAST(id AS TEXT) AS permit_id,",
    "CAST(permit AS TEXT) AS permit_number,",
    "permit_type,",
    "permit_status,",
    "issue_date,",
    "street_number,",
    "street_direction,",
    "street_name,",
    "work_description",
    "FROM building_permits_clean"
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    permit_address = stringr::str_squish(
      paste(street_number, street_direction, street_name)
    ),
    parsed_unit_count = purrr::map_dbl(
      work_description,
      extract_unit_count
    )
  )

valid_permits <- permit_metadata |>
  dplyr::filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING")
  )

evidence_permits <- preferred |>
  dplyr::transmute(
    project_id,
    permit_number = stringr::str_extract_all(
      dplyr::coalesce(evidence_ids, ""),
      "(?<=permit_)[0-9]{7,9}"
    )
  ) |>
  tidyr::unnest_longer(permit_number) |>
  dplyr::filter(!is.na(permit_number)) |>
  dplyr::inner_join(
    valid_permits |>
      dplyr::select(permit_number),
    by = "permit_number",
    relationship = "many-to-one"
  ) |>
  dplyr::distinct()

evidence_permit_units <- evidence_permits |>
  dplyr::inner_join(
    valid_permits |>
      dplyr::select(
        permit_number,
        permit_address,
        unit_count = parsed_unit_count
      ) |>
      dplyr::filter(is.finite(unit_count)),
    by = "permit_number",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id, permit_address) |>
  dplyr::summarise(
    address_unit_count = max(unit_count),
    .groups = "drop"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    evidence_permit_unit_values = collapse_numbers(address_unit_count),
    evidence_permit_unit_value_count =
      dplyr::n_distinct(address_unit_count),
    unique_evidence_permit_units = dplyr::if_else(
      evidence_permit_unit_value_count == 1L,
      min(address_unit_count),
      NA_real_
    ),
    evidence_permit_address_sum = sum(address_unit_count),
    .groups = "drop"
  )

permit_years <- readr::read_csv(
  "../input/project_permit_chain_links.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(source_family == "commercial", directly_matched) |>
  dplyr::mutate(permit_id = as.character(permit_id)) |>
  dplyr::inner_join(
    unambiguous_project_aliases,
    by = c("project_id" = "source_project_id"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    permit_metadata |>
      dplyr::select(
        permit_id,
        source_permit_type = permit_type,
        source_permit_status = permit_status
      ),
    by = "permit_id",
    relationship = "many-to-one"
  ) |>
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
    valid_new_construction_permit =
      source_permit_type == "PERMIT - NEW CONSTRUCTION" &
      source_permit_status %in% c(
        "COMPLETE",
        "ACTIVE",
        "PHASED PERMITTING"
      ),
    application_year = lubridate::year(as.Date(application_date)),
    issue_year = lubridate::year(as.Date(issue_date))
  ) |>
  dplyr::filter(explicit_new_building | valid_new_construction_permit) |>
  dplyr::group_by(project_id = final_project_id) |>
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

evidence_permit_years <- preferred |>
  dplyr::transmute(
    project_id,
    permit_number = stringr::str_extract_all(
      dplyr::coalesce(evidence_ids, ""),
      "(?<=permit_)[0-9]{7,9}"
    )
  ) |>
  tidyr::unnest_longer(permit_number) |>
  dplyr::filter(!is.na(permit_number)) |>
  dplyr::inner_join(
    valid_permits,
    by = "permit_number",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    evidence_permit_issue_years = collapse_numbers(
      lubridate::year(as.Date(issue_date))
    ),
    evidence_permit_construction_years = list(unique(c(
      lubridate::year(as.Date(issue_date)),
      lubridate::year(as.Date(issue_date)) + 1L
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
  evidence_permit_units,
  permit_years,
  evidence_permit_years
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
  dplyr::left_join(
    evidence_permit_units,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(permit_years, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(
    evidence_permit_years,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    within_500ft = dplyr::coalesce(within_500ft, FALSE),
    within_1500ft = dplyr::coalesce(within_1500ft, FALSE),
    year_matches_candidate = same_number(final_year, candidate_year),
    year_matches_permit = purrr::map2_lgl(
      final_year,
      permit_construction_years,
      ~ is.numeric(.y) && any(.x == .y, na.rm = TRUE)
    ),
    year_matches_evidence_permit = purrr::map2_lgl(
      final_year,
      evidence_permit_construction_years,
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
    units_match_evidence_permit = same_number(
      final_units,
      unique_evidence_permit_units
    ),
    units_match_evidence_permit_address_sum = same_number(
      final_units,
      evidence_permit_address_sum
    ),
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
      (
        year_matches_candidate |
          year_matches_permit |
          year_matches_evidence_permit
      ) &
      (units_match_candidate |
        units_match_recommended |
        units_match_address_sum |
        units_match_unique_permit |
        units_match_evidence_permit |
        units_match_evidence_permit_address_sum) &
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
      (year_matches_permit | year_matches_evidence_permit) &
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
        year_matches_candidate |
          year_matches_permit |
          year_matches_evidence_permit,
        units_match_candidate |
          units_match_recommended |
          units_match_address_sum |
          units_match_unique_permit |
          units_match_evidence_permit |
          units_match_evidence_permit_address_sum,
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
  dplyr::select(
    -permit_construction_years,
    -evidence_permit_construction_years
  )

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

minimal_exceptions <- coverage |>
  dplyr::filter(
    within_1500ft,
    support_status %in% c(
      "final_field_not_reproduced_by_current_rules",
      "manual_project_membership_exception"
    )
  ) |>
  dplyr::group_by(
    decision_source,
    decision_action,
    decision_id
  ) |>
  dplyr::summarise(
    output_projects = dplyr::n(),
    projects_within_500ft = sum(within_500ft),
    project_ids = paste(sort(unique(project_id)), collapse = "/"),
    unsupported_fields = paste(
      sort(unique(stats::na.omit(unsupported_fields))),
      collapse = "/"
    ),
    evidence_ids = paste(
      sort(unique(stats::na.omit(evidence_ids))),
      collapse = "/"
    ),
    decision_reason = paste(
      sort(unique(stats::na.omit(decision_reason))),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    unsupported_fields = dplyr::na_if(unsupported_fields, ""),
    evidence_ids = dplyr::na_if(evidence_ids, "")
  ) |>
  dplyr::arrange(
    dplyr::desc(projects_within_500ft),
    decision_source,
    decision_action,
    decision_id
  )

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
readr::write_csv(
  minimal_exceptions,
  "../output/commercial_minimal_exception_ledger.csv",
  na = ""
)
