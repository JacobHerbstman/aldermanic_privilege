# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    audit_construction_year = readr::col_integer(),
    .default = readr::col_guess()
  )
)

pending_validation <- readr::read_csv(
  "../output/assessor_only_validation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    validation_cluster = readr::col_integer(),
    .default = readr::col_guess()
  )
) |>
  dplyr::group_by(validation_cluster) |>
  dplyr::mutate(site_id = min(project_id)) |>
  dplyr::ungroup() |>
  dplyr::select(
    project_id,
    validation_cluster,
    site_id,
    sale_evidence,
    cluster_project_count,
    cluster_sale_support
  )

site_reviews <- readr::read_csv(
  "../adjudication/assessor_default_site_reviews.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    site_id = readr::col_character(),
    site_decision = readr::col_character(),
    corrected_year = readr::col_integer(),
    .default = readr::col_character()
  )
)

project_exceptions <- readr::read_csv(
  "../adjudication/assessor_default_project_exceptions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    project_decision = readr::col_character(),
    corrected_year = readr::col_integer(),
    .default = readr::col_character()
  )
)

final_overrides <- readr::read_csv(
  "../adjudication/final_project_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    project_decision = readr::col_character(),
    corrected_year = readr::col_integer(),
    .default = readr::col_character()
  )
)

exact_permits <- readr::read_csv(
  "../output/assessor_only_exact_permit_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    residential_new_construction_permits,
    nearest_new_construction_year,
    existing_building_permits_near_selected_year,
    demolition_permits,
    exact_permit_review_flag = review_flag,
    exact_permit_numbers = evidence_permit_numbers
  )

if (nrow(projects) != 795L || anyDuplicated(projects$project_id)) {
  stop("The reviewed project ledger must contain 795 unique projects.")
}
if (
  nrow(pending_validation) != 149L ||
    anyDuplicated(pending_validation$project_id)
) {
  stop("The final Assessor-principal review must contain 149 unique projects.")
}
if (
  nrow(site_reviews) != 56L ||
    anyDuplicated(site_reviews$site_id)
) {
  stop("The site review file must contain 56 unique sites.")
}
if (anyDuplicated(project_exceptions$project_id)) {
  stop("Project exceptions are not uniquely keyed by project_id.")
}
if (anyDuplicated(final_overrides$project_id)) {
  stop("Final overrides are not uniquely keyed by project_id.")
}
if (nrow(dplyr::anti_join(
  final_overrides,
  projects,
  by = "project_id"
)) > 0L) {
  stop("Final overrides contain unknown project IDs.")
}

pending_sites <- pending_validation |>
  dplyr::distinct(site_id)

if (
  nrow(dplyr::anti_join(pending_sites, site_reviews, by = "site_id")) > 0L ||
    nrow(dplyr::anti_join(site_reviews, pending_sites, by = "site_id")) > 0L
) {
  stop("The site review file does not exactly cover the pending sites.")
}

project_specific_rows <- pending_validation |>
  dplyr::semi_join(
    site_reviews |>
      dplyr::filter(site_decision == "project_specific"),
    by = "site_id"
  ) |>
  dplyr::select(project_id)

if (
  nrow(dplyr::anti_join(
    project_specific_rows,
    project_exceptions,
    by = "project_id"
  )) > 0L ||
    nrow(dplyr::anti_join(
      project_exceptions,
      project_specific_rows,
      by = "project_id"
    )) > 0L
) {
  stop("Project exceptions do not exactly cover the project-specific sites.")
}

final_projects <- projects |>
  dplyr::left_join(
    pending_validation,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    site_reviews |>
      dplyr::rename(
        site_corrected_year = corrected_year,
        site_evidence_type = evidence_type,
        site_evidence_ids = evidence_ids,
        site_evidence_url = evidence_url,
        site_notes = notes
      ),
    by = "site_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_exceptions |>
      dplyr::rename(
        project_corrected_year = corrected_year,
        project_evidence_type = evidence_type,
        project_evidence_url = evidence_url,
        project_notes = notes
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    final_overrides |>
      dplyr::rename(
        final_override_decision = project_decision,
        final_override_year = corrected_year,
        final_override_evidence_type = evidence_type,
        final_override_evidence_ids = evidence_ids,
        final_override_evidence_url = evidence_url,
        final_override_notes = notes
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    exact_permits,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    final_include = dplyr::case_when(
      !is.na(final_override_decision) ~
        final_override_decision != "exclude",
      audit_decision == "exclude_after_manual_review" ~ FALSE,
      audit_decision != "retain_assessor_only_pending_review" ~ TRUE,
      !is.na(project_decision) ~ project_decision != "exclude",
      site_decision == "exclude" ~ FALSE,
      TRUE ~ TRUE
    ),
    final_construction_year = dplyr::coalesce(
      final_override_year,
      project_corrected_year,
      site_corrected_year,
      audit_construction_year
    ),
    final_decision = dplyr::case_when(
      !final_include ~ "exclude",
      final_construction_year != audit_construction_year ~
        "retain_corrected_year",
      TRUE ~ "retain_selected_year"
    ),
    final_evidence_type = dplyr::coalesce(
      final_override_evidence_type,
      project_evidence_type,
      site_evidence_type,
      evidence_type,
      dplyr::case_when(
        dplyr::coalesce(exact_address_new_building_permit, FALSE) ~
          "city_new_construction_permit",
        dplyr::coalesce(extended_permit_support, FALSE) ~
          "city_new_construction_permit",
        spatial_new_building_permit ~
          "city_new_construction_permit",
        TRUE ~ NA_character_
      ),
      verification_strength
    ),
    final_evidence_url = dplyr::coalesce(
      final_override_evidence_url,
      project_evidence_url,
      site_evidence_url,
      evidence_url,
      external_source_1_url
    ),
    final_review_notes = dplyr::coalesce(
      final_override_notes,
      project_notes,
      site_notes,
      notes,
      external_reviewer_notes,
      reviewer_notes
    ),
    final_evidence_tier = dplyr::case_when(
      !final_include ~ "reviewed_exclusion",
      !is.na(final_override_decision) ~ "manual_project_review",
      audit_decision == "retain_with_independent_support" ~
        "independent_project_evidence",
      audit_decision == "retain_with_corroboration" ~
        "corroborating_project_evidence",
      audit_decision %in% c(
        "retain_after_manual_review",
        "retain_with_corrected_year"
      ) ~ "manual_project_review",
      audit_decision == "retain_assessor_only_pending_review" ~
        "manual_site_review",
      TRUE ~ "reviewed_assessor_record"
    ),
    valid_far = final_include &
      !is.na(audit_building_sqft) &
      !is.na(audit_land_sqft) &
      audit_building_sqft > 0 &
      audit_land_sqft > 0,
    valid_dupac = final_include &
      !is.na(audit_dwelling_units) &
      !is.na(audit_land_sqft) &
      audit_dwelling_units > 0 &
      audit_land_sqft > 0,
    residential_new_construction_permits = dplyr::coalesce(
      residential_new_construction_permits,
      0L
    ),
    existing_building_permits_near_selected_year = dplyr::coalesce(
      existing_building_permits_near_selected_year,
      0L
    ),
    demolition_permits = dplyr::coalesce(
      demolition_permits,
      0L
    )
  )

if (
  nrow(final_projects) != 795L ||
    anyDuplicated(final_projects$project_id)
) {
  stop("Final review changed the 795-project candidate ledger.")
}
if (any(is.na(final_projects$final_include))) {
  stop("At least one project lacks a final inclusion decision.")
}
if (any(
  final_projects$final_include &
    !dplyr::between(final_projects$final_construction_year, 2006L, 2022L)
)) {
  stop("A retained project has an invalid final construction year.")
}
invalid_density_projects <- final_projects |>
  dplyr::filter(
    final_include,
    !valid_far & !valid_dupac
  )

if (nrow(invalid_density_projects) > 0L) {
  stop(
    "Retained projects lack both FAR and DUPAC inputs: ",
    paste(invalid_density_projects$project_id, collapse = ", ")
  )
}

retained_pins <- final_projects |>
  dplyr::filter(final_include) |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    pin = stringr::str_pad(component_pins, 14, pad = "0")
  )

if (anyDuplicated(retained_pins$pin)) {
  stop("A component PIN is assigned to more than one retained project.")
}

readr::write_csv(
  final_projects |>
    dplyr::arrange(
      dplyr::desc(final_include),
      final_construction_year,
      project_id
    ),
  "../output/final_project_verification_ledger.csv",
  na = ""
)

final_summary <- dplyr::bind_rows(
  final_projects |>
    dplyr::count(
      final_include,
      final_decision,
      final_evidence_tier,
      audit_current_multifamily,
      name = "projects"
    ) |>
    dplyr::mutate(summary = "decision_and_evidence"),
  final_projects |>
    dplyr::summarise(
      projects = dplyr::n(),
      final_include = NA,
      final_decision = "all_candidates",
      final_evidence_tier = NA_character_,
      audit_current_multifamily = NA
    ) |>
    dplyr::mutate(summary = "candidate_total")
) |>
  dplyr::select(
    summary,
    final_include,
    final_decision,
    final_evidence_tier,
    audit_current_multifamily,
    projects
  )

readr::write_csv(
  final_summary,
  "../output/final_project_verification_summary.csv",
  na = ""
)
