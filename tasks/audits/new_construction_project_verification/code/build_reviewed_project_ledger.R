# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    verification_strength = readr::col_character(),
    .default = readr::col_guess()
  )
)

validated <- readr::read_csv(
  "../input/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    external_multifamily = readr::col_logical(),
    .default = readr::col_skip()
  )
) |>
  dplyr::select(
    project_id,
    validated_dwelling_units = dwelling_units,
    validated_building_sqft = building_sqft,
    validated_land_sqft = land_sqft,
    validated_multifamily = external_multifamily
  )

manual_reviews <- readr::read_csv(
  "../adjudication/project_manual_reviews.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    review_status = readr::col_character(),
    project_exists = readr::col_character(),
    new_construction = readr::col_character(),
    construction_year_status = readr::col_character(),
    reviewed_construction_year = readr::col_double(),
    confidence = readr::col_character(),
    evidence_type = readr::col_character(),
    evidence_ids = readr::col_character(),
    evidence_url = readr::col_character(),
    notes = readr::col_character(),
    reviewed_dwelling_units = readr::col_double(),
    reviewed_building_sqft = readr::col_double(),
    reviewed_land_sqft = readr::col_double(),
    field_notes = readr::col_character(),
    reviewed_multifamily = readr::col_logical()
  )
)

exact_address_permits <- readr::read_csv(
  "../output/project_permit_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_type = readr::col_character(),
    permit_status = readr::col_character(),
    permit_description = readr::col_character(),
    address_match = readr::col_logical(),
    application_year_gap = readr::col_double(),
    candidate_project_count = readr::col_double(),
    .default = readr::col_skip()
  )
) |>
  dplyr::mutate(
    permit_description_upper = permit_description |>
      dplyr::coalesce("") |>
      stringr::str_to_upper() |>
      stringr::str_squish(),
    explicit_new_building = stringr::str_detect(
      permit_description_upper,
      paste0(
        "NEW CONSTRUCTION|",
        "CONSTRUCTION OF (A |AN )?NEW|",
        "CONSTRUCT(ION)? (A |AN )?NEW|",
        "ERECT (A |AN )?NEW|",
        "ERECT (A )?[0-9]+ STORY|",
        "ERECT (A |AN )?[0-9]+[ -]?(UNIT|D[.]?U[.]?)"
      )
    ),
    residential_building = stringr::str_detect(
      permit_description_upper,
      paste0(
        "DWELLING|(^|[^A-Z])DU([^A-Z]|$)|D[.] ?U[.]|",
        "APARTMENT|RESIDENTIAL|TOWN ?HOME|SINGLE FAMILY|",
        "TWO FLAT|THREE FLAT|2 FLAT|3 FLAT"
      )
    ),
    exact_address_new_building_permit =
      address_match &
      candidate_project_count == 1 &
      dplyr::coalesce(permit_status != "CANCELLED", TRUE) &
      dplyr::between(application_year_gap, -4, 1) &
      (
        explicit_new_building |
          permit_type == "PERMIT - NEW CONSTRUCTION"
      ) &
      residential_building &
      !stringr::str_detect(
        permit_description_upper,
        "ADDITION.*EXISTING"
      )
  ) |>
  dplyr::filter(exact_address_new_building_permit) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    exact_address_new_building_permit = TRUE,
    exact_address_new_building_permit_ids = paste(
      sort(unique(permit_number)),
      collapse = "/"
    ),
    .groups = "drop"
  )

extended_permits <- readr::read_csv(
  "../output/extended_permit_candidate_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    extended_permit_support = readr::col_logical(),
    extended_permit_numbers = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::filter(extended_permit_support) |>
  dplyr::select(
    project_id,
    extended_permit_support,
    extended_permit_numbers
  )

if (anyDuplicated(projects$project_id)) {
  stop("The project ledger is not uniquely keyed by project_id.")
}
if (anyDuplicated(validated$project_id)) {
  stop("The validated analysis file is not uniquely keyed by project_id.")
}
if (nrow(dplyr::anti_join(projects, validated, by = "project_id")) > 0L) {
  stop("A reviewed project is absent from the validated analysis file.")
}
if (anyDuplicated(manual_reviews$project_id)) {
  stop("The manual review file is not uniquely keyed by project_id.")
}
if (nrow(dplyr::anti_join(
  manual_reviews,
  projects,
  by = "project_id"
)) > 0L) {
  stop("The manual review file contains unknown project IDs.")
}
if (anyDuplicated(extended_permits$project_id)) {
  stop("The extended permit evidence is not uniquely keyed by project_id.")
}

reviewed_projects <- projects |>
  dplyr::left_join(
    validated,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    manual_reviews,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
      exact_address_permits,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    extended_permits,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    exact_address_new_building_permit = dplyr::coalesce(
      exact_address_new_building_permit,
      FALSE
    ),
    audit_decision = dplyr::case_when(
      review_status == "exclude" ~ "exclude_after_manual_review",
      review_status == "retain" &
        construction_year_status == "corrected" ~
        "retain_with_corrected_year",
      review_status == "retain" ~ "retain_after_manual_review",
      review_status == "needs_further_review" ~
        "manual_review_unresolved",
      exact_address_new_building_permit ~
        "retain_with_independent_support",
      dplyr::coalesce(extended_permit_support, FALSE) ~
        "retain_with_independent_support",
      verification_strength == "strong_independent" ~
        "retain_with_independent_support",
      verification_strength == "corroborating_not_independent" ~
        "retain_with_corroboration",
      TRUE ~ "retain_assessor_only_pending_review"
    ),
    audit_construction_year = dplyr::coalesce(
      reviewed_construction_year,
      construction_year
    ),
    audit_dwelling_units = dplyr::coalesce(
      reviewed_dwelling_units,
      validated_dwelling_units
    ),
    audit_building_sqft = dplyr::coalesce(
      reviewed_building_sqft,
      validated_building_sqft
    ),
    audit_land_sqft = dplyr::coalesce(
      reviewed_land_sqft,
      validated_land_sqft
    ),
    audit_current_multifamily = dplyr::coalesce(
      reviewed_multifamily,
      validated_multifamily
    )
  )

if (nrow(reviewed_projects) != 795L) {
  stop("The reviewed ledger does not contain the expected 795 projects.")
}
if (
  any(
    is.na(reviewed_projects$reviewed_dwelling_units) &
      reviewed_projects$audit_dwelling_units !=
        reviewed_projects$validated_dwelling_units,
    na.rm = TRUE
  ) ||
    any(
      is.na(reviewed_projects$reviewed_building_sqft) &
        reviewed_projects$audit_building_sqft !=
          reviewed_projects$validated_building_sqft,
      na.rm = TRUE
    ) ||
    any(
      is.na(reviewed_projects$reviewed_land_sqft) &
        reviewed_projects$audit_land_sqft !=
          reviewed_projects$validated_land_sqft,
      na.rm = TRUE
    ) ||
    any(
      is.na(reviewed_projects$reviewed_multifamily) &
        reviewed_projects$audit_current_multifamily !=
          reviewed_projects$validated_multifamily,
      na.rm = TRUE
    )
) {
  stop("An unreviewed validated field changed during project review.")
}

readr::write_csv(
  reviewed_projects,
  "../output/reviewed_project_ledger.csv",
  na = ""
)

review_summary <- reviewed_projects |>
  dplyr::count(
    audit_decision,
    audit_current_multifamily,
    name = "projects"
  ) |>
  dplyr::arrange(
    audit_decision,
    dplyr::desc(audit_current_multifamily)
  )

readr::write_csv(
  review_summary,
  "../output/reviewed_project_summary.csv",
  na = ""
)
