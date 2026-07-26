# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

dispositions <- readr::read_csv(
  "../input/final_residual_permit_chain_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    representative_application_date = readr::col_date(),
    representative_issue_date = readr::col_date(),
    representative_address = readr::col_character(),
    representative_description = readr::col_character(),
    application_boundary_distance_ft = readr::col_double(),
    maximum_parsed_unit_mention = readr::col_double(),
    historical_reconciliation_status = readr::col_character(),
    successor_assessor_statuses = readr::col_character(),
    final_disposition = readr::col_character(),
    final_disposition_reason = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::filter(
    final_disposition == "completed_but_density_fields_unusable"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(
      representative_application_date
    ),
    inside_500ft = application_boundary_distance_ft <= 500
  )

footprint_evidence <- readr::read_csv(
  "../input/residual_permit_footprint_2022_assessor_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    city_units = readr::col_double(),
    city_shape_area_sqft = readr::col_double(),
    candidate_dwelling_units = readr::col_double(),
    candidate_building_sqft = readr::col_double(),
    candidate_land_sqft = readr::col_double(),
    completion_year = readr::col_double(),
    successor_assessor_status = readr::col_character(),
    .default = readr::col_skip()
  )
)

footprint_summary <- footprint_evidence |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    successor_footprints = dplyr::n_distinct(
      footprint_id[!is.na(footprint_id)]
    ),
    successor_pins = dplyr::n_distinct(
      pin14_2022[!is.na(pin14_2022)]
    ),
    city_unit_max = if (all(is.na(city_units))) {
      NA_real_
    } else {
      max(city_units, na.rm = TRUE)
    },
    city_footprint_sqft_max =
      if (all(is.na(city_shape_area_sqft))) {
        NA_real_
      } else {
        max(city_shape_area_sqft, na.rm = TRUE)
      },
    successor_unit_max =
      if (all(is.na(candidate_dwelling_units))) {
        NA_real_
      } else {
        max(candidate_dwelling_units, na.rm = TRUE)
      },
    successor_building_sqft_max =
      if (all(is.na(candidate_building_sqft))) {
        NA_real_
      } else {
        max(candidate_building_sqft, na.rm = TRUE)
      },
    successor_land_sqft_max =
      if (all(is.na(candidate_land_sqft))) {
        NA_real_
      } else {
        max(candidate_land_sqft, na.rm = TRUE)
      },
    successor_has_complete_density_fields = any(
      candidate_dwelling_units > 0 &
        candidate_building_sqft > 0 &
        candidate_land_sqft > 0,
      na.rm = TRUE
    ),
    successor_completion_supported = any(
      is.finite(completion_year)
    ),
    successor_statuses = paste(
      sort(unique(successor_assessor_status)),
      collapse = "/"
    ),
    .groups = "drop"
  )

field_transitions <- readr::read_csv(
  "../input/residual_footprint_field_transitions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    final_decision = readr::col_character(),
    final_decision_reason = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::transmute(
    permit_chain_id,
    field_transition_decision = final_decision,
    field_transition_reason = final_decision_reason
  )

historical_pins <- readr::read_csv(
  "../input/residual_permit_historical_pin_reconciliation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    historical_parcel_match_method = readr::col_character(),
    historical_parcel_distance_ft = readr::col_double(),
    .default = readr::col_skip()
  )
) |>
  dplyr::select(
    permit_chain_id,
    historical_pin14,
    historical_parcel_match_method,
    historical_parcel_distance_ft
  )

residential_history <- readr::read_csv(
  "../input/residual_permit_historical_residential_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    historical_pin14 = readr::col_character(),
    tax_year = readr::col_integer(),
    card_num = readr::col_integer(),
    year_built = readr::col_integer(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    .default = readr::col_skip()
  )
)

residential_history_by_pin <- residential_history |>
  dplyr::group_by(historical_pin14) |>
  tidyr::nest(history = -historical_pin14) |>
  dplyr::ungroup()

historical_residential_candidates <- dispositions |>
  dplyr::select(permit_chain_id, application_year) |>
  dplyr::inner_join(
    historical_pins,
    by = "permit_chain_id",
    relationship = "one-to-many"
  ) |>
  dplyr::inner_join(
    residential_history_by_pin,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(history) |>
  dplyr::filter(
    is.finite(year_built),
    dplyr::between(year_built, 2006L, 2022L),
    dplyr::between(year_built - application_year, -1L, 4L),
    dwelling_units > 0,
    building_sqft > 0,
    land_sqft > 0
  )

historical_residential_summary <- historical_residential_candidates |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    historical_residential_pins = dplyr::n_distinct(
      historical_pin14
    ),
    historical_residential_cards = dplyr::n_distinct(
      paste(historical_pin14, card_num)
    ),
    historical_residential_years = dplyr::n_distinct(year_built),
    historical_residential_pin_values = paste(
      sort(unique(historical_pin14)),
      collapse = "/"
    ),
    historical_residential_unit_values = paste(
      sort(unique(dwelling_units)),
      collapse = "/"
    ),
    historical_residential_building_values = paste(
      sort(unique(building_sqft)),
      collapse = "/"
    ),
    historical_residential_land_values = paste(
      sort(unique(land_sqft)),
      collapse = "/"
    ),
    historical_residential_building_min = min(building_sqft),
    .groups = "drop"
  )

historical_pin_chain_counts <- historical_residential_candidates |>
  dplyr::distinct(permit_chain_id, historical_pin14) |>
  dplyr::count(
    historical_pin14,
    name = "permit_chains_for_historical_pin"
  )

historical_residential_summary <- historical_residential_summary |>
  dplyr::left_join(
    historical_residential_candidates |>
      dplyr::distinct(permit_chain_id, historical_pin14) |>
      dplyr::left_join(
        historical_pin_chain_counts,
        by = "historical_pin14",
        relationship = "many-to-one"
      ) |>
      dplyr::group_by(permit_chain_id) |>
      dplyr::summarise(
        historical_pin_max_permit_chains = max(
          permit_chains_for_historical_pin
        ),
        .groups = "drop"
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  )

commercial_history <- readr::read_csv(
  "../input/residual_permit_historical_commercial_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    historical_pin14 = readr::col_character(),
    keypin = readr::col_character(),
    year_built = readr::col_integer(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    .default = readr::col_skip()
  )
)

commercial_history_by_pin <- commercial_history |>
  dplyr::group_by(historical_pin14) |>
  tidyr::nest(history = -historical_pin14) |>
  dplyr::ungroup()

historical_commercial_summary <- dispositions |>
  dplyr::select(permit_chain_id, application_year) |>
  dplyr::inner_join(
    historical_pins,
    by = "permit_chain_id",
    relationship = "one-to-many"
  ) |>
  dplyr::inner_join(
    commercial_history_by_pin,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(history) |>
  dplyr::filter(
    is.finite(year_built),
    dplyr::between(year_built, 2006L, 2022L),
    dplyr::between(year_built - application_year, -1L, 4L),
    dwelling_units > 0,
    building_sqft > 0,
    land_sqft > 0
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    historical_commercial_candidates = dplyr::n_distinct(keypin),
    .groups = "drop"
  )

attrition <- dispositions |>
  dplyr::left_join(
    footprint_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    field_transitions,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    historical_residential_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    historical_commercial_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        successor_footprints,
        successor_pins,
        historical_residential_pins,
        historical_residential_cards,
        historical_residential_years,
        historical_pin_max_permit_chains,
        historical_commercial_candidates
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    dplyr::across(
      c(
        successor_has_complete_density_fields,
        successor_completion_supported
      ),
      ~ dplyr::coalesce(.x, FALSE)
    ),
    possible_multifamily = dplyr::coalesce(
      maximum_parsed_unit_mention >= 2,
      FALSE
    ) |
      dplyr::coalesce(city_unit_max >= 2, FALSE),
    strict_assessor_recovery = dplyr::case_when(
      field_transition_decision ==
        "exclude_density_fields_stale" ~ FALSE,
      historical_residential_pins == 1L &
        historical_residential_cards == 1L &
        historical_residential_years == 1L &
        historical_pin_max_permit_chains == 1L &
        historical_residential_building_min >= 200 ~ TRUE,
      historical_commercial_candidates == 1L ~ TRUE,
      TRUE ~ FALSE
    ),
    attrition_reason = dplyr::case_when(
      field_transition_decision ==
        "exclude_density_fields_stale" ~
        "assessor_density_fields_predate_construction",
      historical_residential_pins > 0L &
        historical_pin_max_permit_chains > 1L ~
        "one_assessor_record_linked_to_multiple_permit_chains",
      historical_residential_pins > 0L &
        historical_residential_building_min < 200 ~
        "historical_assessor_density_fields_implausible",
      successor_has_complete_density_fields ~
        "complete_successor_fields_do_not_describe_new_building",
      successor_completion_supported ~
        "completion_supported_but_assessor_density_fields_missing",
      historical_reconciliation_status ==
        "unrepresented_assessor_completion_candidate" ~
        "historical_completion_but_comparable_density_fields_missing",
      TRUE ~ "completion_supported_but_comparable_density_fields_missing"
    )
  )

if (anyDuplicated(attrition$permit_chain_id)) {
  stop("Residual completed-project audit is not one row per chain.")
}

summary <- dplyr::bind_rows(
  attrition |>
    dplyr::count(attrition_reason, name = "all_chains") |>
    dplyr::left_join(
      attrition |>
        dplyr::filter(inside_500ft) |>
        dplyr::count(attrition_reason, name = "inside_500ft"),
      by = "attrition_reason",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      attrition |>
        dplyr::filter(possible_multifamily) |>
        dplyr::count(
          attrition_reason,
          name = "possible_multifamily_all"
        ),
      by = "attrition_reason",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      attrition |>
        dplyr::filter(inside_500ft, possible_multifamily) |>
        dplyr::count(
          attrition_reason,
          name = "possible_multifamily_inside_500ft"
        ),
      by = "attrition_reason",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dplyr::across(
        -attrition_reason,
        ~ dplyr::coalesce(.x, 0L)
      ),
      section = "attrition_reason"
    ) |>
    dplyr::rename(metric = attrition_reason),
  tibble::tibble(
    section = "totals",
    metric = c(
      "completed_density_unusable_chains",
      "inside_500ft",
      "possible_multifamily",
      "possible_multifamily_inside_500ft",
      "strict_assessor_recoveries"
    ),
    all_chains = c(
      nrow(attrition),
      sum(attrition$inside_500ft),
      sum(attrition$possible_multifamily),
      sum(attrition$inside_500ft & attrition$possible_multifamily),
      sum(attrition$strict_assessor_recovery)
    ),
    inside_500ft = NA_integer_,
    possible_multifamily_all = NA_integer_,
    possible_multifamily_inside_500ft = NA_integer_
  )
)

readr::write_csv(
  attrition,
  "../output/residual_completed_density_attrition.csv"
)
readr::write_csv(
  summary,
  "../output/residual_completed_density_attrition_summary.csv"
)
