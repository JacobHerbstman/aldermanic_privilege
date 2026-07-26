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
    explicit_permit_dwelling_units = dplyr::coalesce(
      dplyr::if_else(
        is.finite(permit_unit_min) & permit_unit_min == permit_unit_max,
        permit_unit_max,
        NA_real_
      ),
      spatial_permit_dwelling_units
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
    unit_count_rule = dplyr::case_when(
      stable_permit_unit_count ~ "explicit_new_building_permit",
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
    )
  )

reviewed <- projects |>
  dplyr::filter(externally_reviewed_classification)

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
