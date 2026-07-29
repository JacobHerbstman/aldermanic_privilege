# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/residual_permit_footprint_2022_assessor_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    successor_assessor_status ==
      "complete_unrepresented_successor_assessor_candidate",
    density_field_source == "residential_assessor",
    candidate_building_sqft >= 100,
    !stringr::str_detect(
      stringr::str_to_upper(representative_description),
      "ADDITION"
    )
  )
overrides <- readr::read_csv(
  "../adjudication/residual_footprint_candidate_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
history <- readr::read_csv(
  "../output/residual_permit_footprint_2022_residential_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin14_2022 = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::arrange(
    pin14_2022,
    card_num,
    tax_year,
    row_id
  ) |>
  dplyr::group_by(pin14_2022, card_num, tax_year) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::group_by(pin14_2022, tax_year) |>
  dplyr::summarise(
    building_sqft = sum(building_sqft, na.rm = TRUE),
    dwelling_units = sum(dwelling_units, na.rm = TRUE),
    land_sqft = suppressWarnings(max(land_sqft, na.rm = TRUE)),
    year_built_values = paste(
      sort(unique(year_built[is.finite(year_built)])),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    land_sqft = dplyr::if_else(
      is.infinite(land_sqft),
      NA_real_,
      land_sqft
    )
  )

transition_evidence <- candidates |>
  dplyr::select(
    permit_chain_id,
    pin14_2022,
    representative_address,
    representative_application_date,
    representative_issue_date,
    representative_description,
    application_boundary_distance_ft,
    application_year,
    city_year_built,
    city_units,
    city_shape_area_sqft,
    assessor_class_values,
    candidate_dwelling_units,
    candidate_building_sqft,
    candidate_land_sqft
  ) |>
  dplyr::left_join(
    history,
    by = "pin14_2022",
    relationship = "one-to-many"
  ) |>
  dplyr::group_by(
    permit_chain_id,
    pin14_2022,
    representative_address,
    representative_application_date,
    representative_issue_date,
    representative_description,
    application_boundary_distance_ft,
    application_year,
    city_year_built,
    city_units,
    city_shape_area_sqft,
    assessor_class_values,
    candidate_dwelling_units,
    candidate_building_sqft,
    candidate_land_sqft
  ) |>
  dplyr::summarise(
    first_tax_year = suppressWarnings(min(
      tax_year[is.finite(tax_year)],
      na.rm = TRUE
    )),
    first_final_building_year = suppressWarnings(min(
      tax_year[
        is.finite(building_sqft) &
          building_sqft == candidate_building_sqft
      ],
      na.rm = TRUE
    )),
    first_final_unit_year = suppressWarnings(min(
      tax_year[
        is.finite(dwelling_units) &
          dwelling_units == candidate_dwelling_units
      ],
      na.rm = TRUE
    )),
    pre_application_building_values = paste(
      sort(unique(
        building_sqft[
          tax_year < application_year &
            is.finite(building_sqft)
        ]
      )),
      collapse = "/"
    ),
    pre_application_unit_values = paste(
      sort(unique(
        dwelling_units[
          tax_year < application_year &
            is.finite(dwelling_units)
        ]
      )),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        first_tax_year,
        first_final_building_year,
        first_final_unit_year
      ),
      ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
    ),
    final_building_appears_after_application =
      is.finite(first_final_building_year) &
        dplyr::between(
          first_final_building_year,
          application_year,
          application_year + 4L
        ),
    no_pre_application_assessor_record =
      is.finite(first_tax_year) &
        first_tax_year >= application_year,
    mechanical_decision = dplyr::case_when(
      final_building_appears_after_application ~ "include",
      TRUE ~ "exclude_density_fields_stale"
    )
  ) |>
  dplyr::left_join(
    overrides,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    final_decision = dplyr::coalesce(
      decision,
      mechanical_decision
    ),
    final_dwelling_units = dplyr::coalesce(
      unit_override,
      candidate_dwelling_units
    ),
    completion_year = dplyr::case_when(
      final_building_appears_after_application ~
        first_final_building_year,
      final_decision == "include" &
        is.finite(city_year_built) ~ city_year_built,
      TRUE ~ NA_real_
    ),
    completion_date_start = as.Date(paste0(
      completion_year,
      "-01-01"
    )),
    completion_date_end = as.Date(paste0(
      completion_year,
      "-12-31"
    )),
    application_to_completion_min_days = as.numeric(
      completion_date_start - representative_application_date
    ),
    application_to_completion_max_days = as.numeric(
      completion_date_end - representative_application_date
    ),
    issue_to_completion_min_days = as.numeric(
      completion_date_start - representative_issue_date
    ),
    issue_to_completion_max_days = as.numeric(
      completion_date_end - representative_issue_date
    ),
    final_decision_reason = dplyr::coalesce(
      decision_reason,
      "Current whole-building assessor fields first appear within four years of the new-construction application"
    ),
    final_evidence = dplyr::coalesce(
      evidence,
      "permit_city_footprint_and_assessor_field_transition"
    )
  )

summary <- transition_evidence |>
  dplyr::count(final_decision, name = "value") |>
  dplyr::transmute(
    section = "final_decision",
    metric = final_decision,
    value
  )

readr::write_csv(
  transition_evidence,
  "../output/residual_footprint_field_transitions.csv"
)
readr::write_csv(
  summary,
  "../output/residual_footprint_field_transition_summary.csv"
)
