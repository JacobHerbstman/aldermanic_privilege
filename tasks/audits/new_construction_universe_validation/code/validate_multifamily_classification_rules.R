# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE
) |>
  dplyr::left_join(
    readr::read_csv(
      "../output/eligibility_rule_validation.csv",
      show_col_types = FALSE,
      col_select = c(project_id, proposed_action),
      col_types = readr::cols(
        project_id = readr::col_character(),
        proposed_action = readr::col_character()
      )
    ) |>
      dplyr::select(project_id, proposed_action),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    spatial_permit_dwelling_units = as.numeric(
      stringr::str_match(
        stringr::str_to_upper(
          dplyr::coalesce(strong_spatial_permit_descriptions, "")
        ),
        "([0-9]+)\\s*(?:DWELLING\\s+UNITS?|D\\.?\\s*U\\.?)"
      )[, 2]
    ),
    spatial_permit_dwelling_units = dplyr::coalesce(
      spatial_permit_dwelling_units,
      dplyr::case_when(
        stringr::str_detect(
          stringr::str_to_upper(
            dplyr::coalesce(strong_spatial_permit_descriptions, "")
          ),
          "\\bTWO[- ]DWELLING"
        ) ~ 2,
        stringr::str_detect(
          stringr::str_to_upper(
            dplyr::coalesce(strong_spatial_permit_descriptions, "")
          ),
          "\\bTHREE[- ]DWELLING"
        ) ~ 3,
        stringr::str_detect(
          stringr::str_to_upper(
            dplyr::coalesce(strong_spatial_permit_descriptions, "")
          ),
          "\\bFOUR[- ]DWELLING"
        ) ~ 4,
        TRUE ~ NA_real_
      )
    ),
    exact_permit_dwelling_units = dplyr::if_else(
      is.finite(permit_unit_min) & permit_unit_min == permit_unit_max,
      permit_unit_max,
      NA_real_
    ),
    explicit_permit_dwelling_units = dplyr::coalesce(
      exact_permit_dwelling_units,
      spatial_permit_dwelling_units
    ),
    explicit_permit_unit_source = dplyr::case_when(
      is.finite(exact_permit_dwelling_units) ~ paste0(
        "exact_pin_permit:",
        dplyr::coalesce(
          exact_pin_positive_permit_numbers,
          exact_pin_permit_numbers
        )
      ),
      is.finite(spatial_permit_dwelling_units) ~ paste0(
        "spatial_new_building_permit:",
        strong_spatial_permit_numbers
      ),
      TRUE ~ NA_character_
    ),
    stable_permit_unit_count =
      is.finite(explicit_permit_dwelling_units) &
      explicit_permit_dwelling_units > 1,
    corroborated_permit_unit_count =
      stable_permit_unit_count &
      (
        positive_new_building_permit |
          exact_pin_post_construction_existing_work |
          exact_pin_negative_existing_building |
          exact_negative_existing_work
      ),
    permit_unit_recovery_eligible =
      (class_211_212 | class_values == "EX") &
      dplyr::coalesce(dwelling_units <= 1, TRUE) &
      corroborated_permit_unit_count,
    resolved_dwelling_units = dplyr::if_else(
      permit_unit_recovery_eligible,
      explicit_permit_dwelling_units,
      dwelling_units
    ),
    resolved_unit_source = dplyr::if_else(
      permit_unit_recovery_eligible,
      explicit_permit_unit_source,
      units_source
    ),
    unit_count_rule = dplyr::case_when(
      permit_unit_recovery_eligible &
        is.finite(exact_permit_dwelling_units) ~
        "explicit_exact_new_building_permit",
      permit_unit_recovery_eligible &
        is.finite(spatial_permit_dwelling_units) ~
        "explicit_spatial_new_building_permit",
      TRUE ~ "retained_assessor_value"
    )
) |>
  dplyr::mutate(
    recognized_single_family_class = stringr::str_detect(
      dplyr::coalesce(class_values, ""),
      "(^|/)(202|203|204|205|206|207|208|209|210|219|234|278|295)($|/)"
    ),
    class_first_multifamily = dplyr::case_when(
      source_family == "commercial" ~
        dplyr::coalesce(resolved_dwelling_units > 1, FALSE),
      project_kind == "recovered_completed_residential_building" ~
        resolved_dwelling_units > 1,
      class_211_212 | class_297 ~ TRUE,
      class_values == "EX" & is.finite(permit_unit_max) ~
        permit_unit_max > 1,
      class_values == "EX" &
        stringr::str_detect(
          dplyr::coalesce(exact_permit_descriptions, ""),
          "\\bTWO[- ](?:DWELLING|UNIT)|\\b2[- ](?:DWELLING|UNIT)"
        ) ~ TRUE,
      class_values == "EX" &
        stringr::str_detect(
          dplyr::coalesce(exact_permit_descriptions, ""),
          "SINGLE[- ]FAMILY"
        ) ~ FALSE,
      class_values == "EX" ~ resolved_dwelling_units > 1,
      class_values == "OA2" ~ resolved_dwelling_units > 1,
      recognized_single_family_class ~ FALSE,
      TRUE ~ NA
    ),
    externally_reviewed_classification =
      multifamily_disposition %in% c("include", "exclude"),
    reviewed_multifamily = dplyr::case_when(
      multifamily_disposition == "include" ~ TRUE,
      multifamily_disposition == "exclude" ~ FALSE,
      TRUE ~ NA
    ),
    class_first_error = dplyr::coalesce(
      externally_reviewed_classification &
        class_first_multifamily != reviewed_multifamily,
      FALSE
    ),
    classification_review_reason = dplyr::case_when(
      proposed_action == "exclude" ~ "ineligible_project",
      class_first_error ~ "externally_validated_class_exception",
      is.na(class_first_multifamily) ~ "unrecognized_or_exception_class",
      class_211_212 &
        (!is.finite(resolved_dwelling_units) |
          resolved_dwelling_units <= 1) ~
        "multifamily_unit_count_requires_recovery",
      TRUE ~ "programmatic_classification"
    ),
    requires_classification_review =
      !classification_review_reason %in%
        c("programmatic_classification", "ineligible_project"),
    proposed_multifamily = dplyr::case_when(
      class_first_error ~ reviewed_multifamily,
      TRUE ~ class_first_multifamily
    ),
    resolved_dwelling_units = dplyr::case_when(
      !proposed_multifamily &
        project_kind == "single_pin_single_card" ~ 1,
      TRUE ~ resolved_dwelling_units
    ),
    unit_count_rule = dplyr::case_when(
      !proposed_multifamily &
        project_kind == "single_pin_single_card" ~
        "single_family_single_record",
      TRUE ~ unit_count_rule
    ),
    resolved_unit_source = dplyr::case_when(
      !proposed_multifamily &
        project_kind == "single_pin_single_card" ~
        "single_family_class_rule",
      TRUE ~ resolved_unit_source
    )
  )

if (
  any(
    projects$project_kind == "single_pin_single_card" &
      !projects$proposed_multifamily &
      projects$resolved_dwelling_units != 1,
    na.rm = TRUE
  )
) {
  stop("A single-record single-family project has more than one dwelling.")
}

reviewed <- projects |>
  dplyr::filter(externally_reviewed_classification) |>
  dplyr::arrange(project_id)

set.seed(20260726)
retrospective_holdout_ids <- sample(
  reviewed$project_id,
  size = min(50L, nrow(reviewed)),
  replace = FALSE
)
retrospective_holdout <- reviewed |>
  dplyr::filter(project_id %in% retrospective_holdout_ids)

validation <- dplyr::bind_rows(
  reviewed |>
    dplyr::summarise(
      rule = "reported_unit_count",
      reviewed_projects = dplyr::n(),
      programmatically_decided = dplyr::n(),
      correct = sum(current_multifamily == reviewed_multifamily),
      errors = sum(current_multifamily != reviewed_multifamily),
      accuracy = mean(current_multifamily == reviewed_multifamily),
      left_for_review = 0L
    ),
  reviewed |>
    dplyr::summarise(
      rule = "assessor_class_first",
      reviewed_projects = dplyr::n(),
      programmatically_decided = sum(!is.na(class_first_multifamily)),
      correct = sum(
        class_first_multifamily == reviewed_multifamily,
        na.rm = TRUE
      ),
      errors = sum(
        class_first_multifamily != reviewed_multifamily,
        na.rm = TRUE
      ),
      accuracy = mean(
        class_first_multifamily == reviewed_multifamily,
        na.rm = TRUE
      ),
      left_for_review = sum(is.na(class_first_multifamily))
    ),
  retrospective_holdout |>
    dplyr::summarise(
      rule = "assessor_class_first_retrospective_holdout",
      reviewed_projects = dplyr::n(),
      programmatically_decided = sum(!is.na(class_first_multifamily)),
      correct = sum(
        class_first_multifamily == reviewed_multifamily,
        na.rm = TRUE
      ),
      errors = sum(
        class_first_multifamily != reviewed_multifamily,
        na.rm = TRUE
      ),
      accuracy = mean(
        class_first_multifamily == reviewed_multifamily,
        na.rm = TRUE
      ),
      left_for_review = sum(is.na(class_first_multifamily))
    ),
  reviewed |>
    dplyr::summarise(
      rule = "conservative_review_screen",
      reviewed_projects = dplyr::n(),
      programmatically_decided = sum(!requires_classification_review),
      correct = sum(
        !requires_classification_review &
          proposed_multifamily == reviewed_multifamily
      ),
      errors = sum(
        !requires_classification_review &
          proposed_multifamily != reviewed_multifamily
      ),
      accuracy = mean(
        proposed_multifamily[!requires_classification_review] ==
          reviewed_multifamily[!requires_classification_review]
      ),
      left_for_review = sum(requires_classification_review)
  )
)

error_modes <- projects |>
  dplyr::mutate(
    classification_mode = dplyr::case_when(
      class_278_295 & dwelling_units > 1 ~
        "single_family_class_with_multiple_reported_dwellings",
      class_211_212 ~ "class_211_212",
      TRUE ~ "other"
    )
  ) |>
  dplyr::group_by(
    classification_mode,
    externally_reviewed_classification
  ) |>
  dplyr::summarise(
    projects = dplyr::n(),
    reviewed_errors = sum(class_first_error),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    classification_mode,
    dplyr::desc(externally_reviewed_classification)
  )

set.seed(20260726)
mode_b_candidates <- projects |>
  dplyr::filter(
    !externally_reviewed_classification,
    class_211_212,
    proposed_action != "exclude"
  ) |>
  dplyr::arrange(project_id)
mode_b_review_sample <- mode_b_candidates |>
  dplyr::slice_sample(n = min(50L, nrow(mode_b_candidates))) |>
  dplyr::arrange(project_id) |>
  dplyr::transmute(
    project_id,
    source_family,
    component_pins,
    project_kind,
    construction_year,
    class_values,
    dwelling_units,
    building_sqft,
    current_property_addresses,
    addresses,
    exact_permit_addresses,
    strong_spatial_permit_addresses,
    exact_pin_permit_addresses,
    exact_pin_permit_numbers,
    exact_pin_positive_descriptions,
    exact_pin_broad_negative_descriptions,
    history_year_values,
    history_building_area_values,
    history_unit_count_values,
    class_first_multifamily,
    reviewed_multifamily = NA,
    review_source = NA_character_,
    reviewer_notes = NA_character_
  )

review_summary <- projects |>
  dplyr::filter(within_1500ft) |>
  dplyr::count(
    requires_classification_review,
    externally_reviewed_classification,
    classification_review_reason,
    name = "projects"
  ) |>
  dplyr::arrange(
    dplyr::desc(requires_classification_review),
    externally_reviewed_classification,
    classification_review_reason
  )

readr::write_csv(
  validation,
  "../output/multifamily_classification_rule_validation.csv",
  na = ""
)
readr::write_csv(
  error_modes,
  "../output/multifamily_classification_error_modes.csv",
  na = ""
)
readr::write_csv(
  mode_b_review_sample,
  "../output/multifamily_classification_mode_b_review_sample.csv",
  na = ""
)
readr::write_csv(
  review_summary,
  "../output/multifamily_classification_review_summary.csv",
  na = ""
)
readr::write_csv(
  projects |>
    dplyr::filter(
      within_1500ft,
      requires_classification_review,
      !externally_reviewed_classification
    ) |>
    dplyr::arrange(
      within_500ft,
      classification_review_reason,
      project_id
    ),
  "../output/multifamily_classification_new_review_queue.csv",
  na = ""
)
readr::write_csv(
  projects |>
    dplyr::filter(class_first_error) |>
    dplyr::arrange(project_id),
  "../output/multifamily_classification_known_exceptions.csv",
  na = ""
)
readr::write_csv(
  projects,
  "../output/multifamily_classification_decisions.csv",
  na = ""
)
