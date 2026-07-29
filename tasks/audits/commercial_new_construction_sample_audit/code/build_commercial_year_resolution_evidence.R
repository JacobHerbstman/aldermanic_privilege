# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

vintages <- readr::read_csv(
  "../output/commercial_family_vintage_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_family_id = readr::col_character(), .default = readr::col_guess())
)

ground_up <- readr::read_csv(
  "../output/commercial_ground_up_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

exact_permits <- readr::read_csv(
  "../output/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    permit_status == "COMPLETE",
    plausible_application_window,
    plausible_issue_window
  ) %>%
  mutate(evidence_match = "component_pin")

spatial_permits <- readr::read_csv(
  "../output/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    source_family == "commercial",
    permit_status == "COMPLETE",
    polygon_distance_ft == 0,
    plausible_application_window,
    plausible_issue_window
  ) %>%
  mutate(evidence_match = "inside_project_polygon")

residential_construction_pattern <- regex(
  paste(
    "ERECT.*(DWELLING|RESIDENTIAL|APARTMENT|UNIT|DORMITORY|SENIOR|ASSISTED)",
    "NEW.*(DWELLING|RESIDENTIAL|APARTMENT|UNIT|DORMITORY|SENIOR|ASSISTED)",
    "NEW CONSTRUCTION.*(DWELLING|RESIDENTIAL|APARTMENT|UNIT|DORMITORY|SENIOR|ASSISTED)",
    sep = "|"
  ),
  ignore_case = TRUE
)

permit_evidence <- bind_rows(
  exact_permits %>%
    select(
      project_id, permit_id, permit_number, application_year, issue_year,
      permit_status, permit_address, work_description, evidence_match
    ),
  spatial_permits %>%
    select(
      project_id, permit_id, permit_number, application_year, issue_year,
      permit_status, permit_address, work_description, evidence_match
    )
) %>%
  filter(str_detect(coalesce(work_description, ""), residential_construction_pattern)) %>%
  distinct(project_id, permit_id, .keep_all = TRUE) %>%
  arrange(project_id, application_year, issue_year, permit_number)

permit_summary <- permit_evidence %>%
  group_by(project_id) %>%
  summarise(
    supporting_permits = n_distinct(permit_id),
    supporting_permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    permit_application_years = paste(sort(unique(application_year)), collapse = "/"),
    permit_issue_years = paste(sort(unique(issue_year[is.finite(issue_year)])), collapse = "/"),
    earliest_application_year = min(application_year, na.rm = TRUE),
    latest_application_year = max(application_year, na.rm = TRUE),
    earliest_issue_year = if_else(
      any(is.finite(issue_year)), min(issue_year, na.rm = TRUE), NA_integer_
    ),
    latest_issue_year = if_else(
      any(is.finite(issue_year)), max(issue_year, na.rm = TRUE), NA_integer_
    ),
    permit_evidence = paste(
      sort(unique(paste0(permit_number, " [", evidence_match, "] ", permit_address))),
      collapse = " / "
    ),
    .groups = "drop"
  )

vintage_years <- vintages %>%
  select(project_family_id, valuation_year, yearbuilt) %>%
  tidyr::pivot_wider(
    names_from = valuation_year,
    values_from = yearbuilt,
    names_prefix = "assessor_year_"
  )

year_evidence <- candidates %>%
  filter(between(construction_year, 2006L, 2022L)) %>%
  left_join(
    vintage_years,
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    ground_up %>%
      select(
        project_id,
        city_year_built_values,
        city_year_coverage_share,
        city_near_target_share,
        city_old_building_share,
        ground_up_status
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(permit_summary, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    assessor_year_conflict =
      is.finite(assessor_year_2021) & is.finite(assessor_year_2024) &
      assessor_year_2021 != assessor_year_2024,
    permit_supported = coalesce(supporting_permits, 0L) > 0,
    city_supported =
      city_year_coverage_share >= 0.75 & city_near_target_share >= 0.75,
    city_contradicts =
      city_year_coverage_share >= 0.75 & city_old_building_share >= 0.75,
    year_resolution = case_when(
      !assessor_year_conflict ~ "retain_assessor_year_no_cross_vintage_conflict",
      permit_supported & city_supported ~ "retain_2024_year_permit_and_city_supported",
      permit_supported ~ "retain_2024_year_permit_supported",
      city_supported ~ "retain_2024_year_city_supported",
      abs(assessor_year_2024 - assessor_year_2021) <= 3 ~
        "retain_2024_year_small_vintage_revision",
      city_contradicts ~ "manual_year_review_city_record_predates_assessor",
      TRUE ~ "manual_year_review_no_external_date"
    ),
    year_resolution_reason = case_when(
      year_resolution == "retain_assessor_year_no_cross_vintage_conflict" ~
        "No conflicting assessor construction year across available vintages",
      year_resolution == "retain_2024_year_permit_and_city_supported" ~
        "Issued residential new-construction permit and City building years support the 2024 report",
      year_resolution == "retain_2024_year_permit_supported" ~
        "Issued residential new-construction permit is temporally consistent with the 2024 report",
      year_resolution == "retain_2024_year_city_supported" ~
        "City building years support the 2024 report",
      year_resolution == "retain_2024_year_small_vintage_revision" ~
        "The latest assessor report revises the earlier year by no more than three years",
      year_resolution == "manual_year_review_city_record_predates_assessor" ~
        "The assessor vintages conflict and the City footprint record predates both reports; the City field may be stale",
      TRUE ~ "The assessor vintages conflict and neither permit nor City building evidence resolves the year"
    )
  ) %>%
  arrange(desc(current_within_1500ft), project_id)

if (anyDuplicated(year_evidence$project_id) > 0) {
  stop("Commercial year evidence is not unique by project.", call. = FALSE)
}

readr::write_csv(
  permit_evidence,
  "../output/commercial_year_permit_evidence.csv"
)
readr::write_csv(
  year_evidence,
  "../output/commercial_year_resolution_evidence.csv"
)
readr::write_csv(
  bind_rows(
    year_evidence %>%
      count(year_resolution, name = "value") %>%
      transmute(section = "all", metric = year_resolution, value),
    year_evidence %>%
      filter(current_within_1500ft) %>%
      count(year_resolution, name = "value") %>%
      transmute(section = "within_1500ft", metric = year_resolution, value)
  ),
  "../output/commercial_year_resolution_summary.csv"
)
