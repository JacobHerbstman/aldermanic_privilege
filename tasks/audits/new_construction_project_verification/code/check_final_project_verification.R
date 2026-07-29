# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/final_project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

duplicate_queue <- readr::read_csv(
  "../../new_construction_universe_validation/output/unresolved_duplicate_candidates.csv",
  show_col_types = FALSE
)

final_overrides <- readr::read_csv(
  "../adjudication/final_project_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_character()
  )
)

retained_pins <- projects |>
  dplyr::filter(final_include) |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    pin = stringr::str_pad(component_pins, 14, pad = "0")
  )

unresolved <- dplyr::bind_rows(
  projects |>
    dplyr::filter(
      is.na(final_include) |
        is.na(final_evidence_type) |
        final_evidence_type == "unresolved"
    ) |>
    dplyr::transmute(
      project_id,
      check = "missing_final_decision_or_evidence",
      detail = final_evidence_type
    ),
  projects |>
    dplyr::filter(final_include, !valid_far, !valid_dupac) |>
    dplyr::transmute(
      project_id,
      check = "missing_density_outcome",
      detail = NA_character_
    ),
  projects |>
    dplyr::filter(is.na(distance_to_boundary_ft) | distance_to_boundary_ft > 500) |>
    dplyr::transmute(
      project_id,
      check = "outside_500ft_verification_scope",
      detail = as.character(distance_to_boundary_ft)
    ),
  projects |>
    dplyr::filter(
      final_include,
      existing_building_permits_near_selected_year > 0,
      !project_id %in% final_overrides$project_id
    ) |>
    dplyr::transmute(
      project_id,
      check = "existing_building_permit_not_adjudicated",
      detail = exact_permit_numbers
    ),
  projects |>
    dplyr::filter(
      !final_include,
      residential_new_construction_permits > 0,
      is.na(final_review_notes)
    ) |>
    dplyr::transmute(
      project_id,
      check = "excluded_new_building_permit_not_adjudicated",
      detail = exact_permit_numbers
    )
)

if (anyDuplicated(projects$project_id)) {
  unresolved <- dplyr::bind_rows(
    unresolved,
    tibble::tibble(
      project_id = NA_character_,
      check = "duplicate_project_id",
      detail = as.character(sum(duplicated(projects$project_id)))
    )
  )
}
if (anyDuplicated(retained_pins$pin)) {
  unresolved <- dplyr::bind_rows(
    unresolved,
    tibble::tibble(
      project_id = NA_character_,
      check = "component_pin_reused",
      detail = as.character(sum(duplicated(retained_pins$pin)))
    )
  )
}
if (nrow(duplicate_queue) > 0L) {
  unresolved <- dplyr::bind_rows(
    unresolved,
    tibble::tibble(
      project_id = NA_character_,
      check = "unresolved_duplicate_pair",
      detail = as.character(nrow(duplicate_queue))
    )
  )
}

checks <- tibble::tibble(
  check = c(
    "candidate_projects",
    "retained_projects",
    "excluded_projects",
    "retained_multifamily_projects",
    "retained_far_projects",
    "retained_dupac_projects",
    "retained_corrected_years",
    "unique_retained_component_pins",
    "unresolved_review_items"
  ),
  value = c(
    nrow(projects),
    sum(projects$final_include),
    sum(!projects$final_include),
    sum(projects$final_include & projects$audit_current_multifamily),
    sum(projects$valid_far),
    sum(projects$valid_dupac),
    sum(
      projects$final_include &
        projects$final_construction_year !=
          projects$audit_construction_year
    ),
    dplyr::n_distinct(retained_pins$pin),
    nrow(unresolved)
  )
)

readr::write_csv(
  checks,
  "../output/final_project_verification_checks.csv",
  na = ""
)
readr::write_csv(
  unresolved,
  "../output/final_project_verification_unresolved.csv",
  na = ""
)

if (nrow(unresolved) > 0L) {
  print(unresolved)
  stop("The final project verification audit has unresolved items.")
}
