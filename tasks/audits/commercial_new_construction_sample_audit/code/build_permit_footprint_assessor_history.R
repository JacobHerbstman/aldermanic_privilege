# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14L, digits, NA_character_)
}

candidate_links <- readr::read_csv(
  "../output/permit_footprint_current_parcel_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    candidate_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::mutate(
    candidate_pin = normalize_pin(candidate_pin),
    application_year = lubridate::year(representative_application_date),
    issue_year = lubridate::year(representative_issue_date)
  )

if (anyDuplicated(
  candidate_links[c("permit_chain_id", "footprint_id", "candidate_pin")]
)) {
  stop("Footprint-current parcel candidate keys must be unique.", call. = FALSE)
}

assessor_history <- readr::read_csv(
  "../input/residential_improvement_characteristics_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character()),
  col_select = c(
    pin,
    year,
    card,
    class,
    tieback_key_pin,
    cdu,
    pin_is_multicard,
    pin_num_cards,
    pin_is_multiland,
    pin_num_landlines,
    char_yrblt,
    char_bldg_sf,
    char_land_sf,
    char_beds,
    char_rooms,
    char_fbath,
    char_hbath,
    char_frpl,
    char_type_resd,
    char_cnst_qlty,
    char_apts,
    char_ncu,
    row_id
  )
) |>
  dplyr::mutate(
    candidate_pin = normalize_pin(pin),
    year = suppressWarnings(as.integer(year)),
    dplyr::across(
      c(
        char_yrblt,
        char_bldg_sf,
        char_land_sf,
        char_beds,
        char_rooms,
        char_fbath,
        char_hbath,
        char_frpl,
        char_apts,
        char_ncu
      ),
      ~ suppressWarnings(as.double(.x))
    )
  ) |>
  dplyr::filter(candidate_pin %in% candidate_links$candidate_pin) |>
  dplyr::select(
    candidate_pin,
    report_year = year,
    card,
    assessor_class = class,
    tieback_key_pin,
    cdu,
    pin_is_multicard,
    pin_num_cards,
    pin_is_multiland,
    pin_num_landlines,
    reported_year_built = char_yrblt,
    reported_building_sqft = char_bldg_sf,
    reported_land_sqft = char_land_sf,
    reported_bedrooms = char_beds,
    reported_rooms = char_rooms,
    reported_full_baths = char_fbath,
    reported_half_baths = char_hbath,
    reported_fireplaces = char_frpl,
    residence_type = char_type_resd,
    construction_quality = char_cnst_qlty,
    reported_apartments = char_apts,
    reported_units_in_building = char_ncu,
    row_id
  ) |>
  dplyr::distinct()

if (anyDuplicated(assessor_history$row_id)) {
  stop("Assessor history row IDs must be unique.", call. = FALSE)
}

assessor_year_history <- assessor_history |>
  dplyr::group_by(candidate_pin, report_year) |>
  dplyr::summarise(
    assessor_rows = dplyr::n(),
    assessor_cards = dplyr::n_distinct(card),
    assessor_classes = paste(sort(unique(assessor_class)), collapse = "/"),
    reported_year_built_values = paste(
      sort(unique(reported_year_built[!is.na(reported_year_built)])),
      collapse = "/"
    ),
    reported_year_built_min = suppressWarnings(min(
      reported_year_built,
      na.rm = TRUE
    )),
    reported_year_built_max = suppressWarnings(max(
      reported_year_built,
      na.rm = TRUE
    )),
    reported_building_sqft_sum = sum(reported_building_sqft, na.rm = TRUE),
    reported_building_sqft_max = suppressWarnings(max(
      reported_building_sqft,
      na.rm = TRUE
    )),
    reported_land_sqft_sum = sum(reported_land_sqft, na.rm = TRUE),
    reported_land_sqft_max = suppressWarnings(max(
      reported_land_sqft,
      na.rm = TRUE
    )),
    reported_apartments_sum = sum(reported_apartments, na.rm = TRUE),
    reported_units_in_building_sum = sum(
      reported_units_in_building,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        reported_year_built_min,
        reported_year_built_max,
        reported_building_sqft_max,
        reported_land_sqft_max
      ),
      ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  dplyr::arrange(candidate_pin, report_year) |>
  dplyr::group_by(candidate_pin) |>
  dplyr::mutate(
    prior_report_year = dplyr::lag(report_year),
    prior_reported_year_built_min = dplyr::lag(reported_year_built_min),
    prior_reported_building_sqft_sum = dplyr::lag(
      reported_building_sqft_sum
    ),
    prior_reported_building_sqft_max = dplyr::lag(
      reported_building_sqft_max
    ),
    prior_reported_apartments_sum = dplyr::lag(reported_apartments_sum),
    reported_building_sqft_sum_change = reported_building_sqft_sum -
      prior_reported_building_sqft_sum,
    reported_building_sqft_max_change = reported_building_sqft_max -
      prior_reported_building_sqft_max,
    reported_apartments_change = reported_apartments_sum -
      prior_reported_apartments_sum,
    reported_year_built_changed = !is.na(prior_reported_year_built_min) &
      reported_year_built_min != prior_reported_year_built_min
  ) |>
  dplyr::ungroup()

history_rows_nested <- assessor_history |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    assessor_rows_nested = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

candidate_assessor_rows <- candidate_links |>
  dplyr::left_join(
    history_rows_nested,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(assessor_rows_nested, keep_empty = TRUE) |>
  dplyr::mutate(
    report_year_minus_application_year = report_year - application_year,
    reported_year_minus_application_year = reported_year_built -
      application_year,
    reported_year_minus_footprint_year = reported_year_built -
      city_year_built
  ) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    report_year,
    card,
    row_id
  )

year_history_nested <- assessor_year_history |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    year_rows = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

candidate_year_history <- candidate_links |>
  dplyr::left_join(
    year_history_nested,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(year_rows, keep_empty = TRUE) |>
  dplyr::mutate(
    report_year_minus_application_year = report_year - application_year,
    reported_year_minus_application_year = reported_year_built_min -
      application_year,
    reported_year_minus_footprint_year = reported_year_built_min -
      city_year_built,
    assessor_sqft_to_city_building_ratio = dplyr::if_else(
      city_building_sqft > 0,
      reported_building_sqft_sum / city_building_sqft,
      NA_real_
    ),
    assessor_sqft_to_footprint_ratio = dplyr::if_else(
      city_shape_area_sqft > 0,
      reported_building_sqft_sum / city_shape_area_sqft,
      NA_real_
    )
  ) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    report_year
  )

nearest_reported_year <- candidate_assessor_rows |>
  dplyr::filter(!is.na(reported_year_built)) |>
  dplyr::mutate(
    footprint_year_gap = abs(reported_year_built - city_year_built),
    application_year_gap = abs(reported_year_built - application_year),
    report_year_after_application = report_year >= application_year
  ) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    footprint_year_gap,
    application_year_gap,
    dplyr::desc(report_year_after_application),
    report_year,
    card
  ) |>
  dplyr::group_by(permit_chain_id, footprint_id, candidate_pin) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    selected_assessor_completion_year = reported_year_built,
    selected_assessor_report_year = report_year,
    selected_assessor_card = card,
    selected_assessor_class = assessor_class,
    selected_assessor_building_sqft = reported_building_sqft,
    selected_assessor_land_sqft = reported_land_sqft,
    selected_assessor_apartments = reported_apartments,
    selected_assessor_units_in_building = reported_units_in_building,
    selected_assessor_footprint_year_gap = footprint_year_gap,
    selected_assessor_application_year_gap = application_year_gap
  )

pre_application_fields <- candidate_year_history |>
  dplyr::filter(!is.na(report_year), report_year < application_year) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    dplyr::desc(report_year)
  ) |>
  dplyr::group_by(permit_chain_id, footprint_id, candidate_pin) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    pre_application_report_year = report_year,
    pre_application_year_built = reported_year_built_min,
    pre_application_building_sqft_sum = reported_building_sqft_sum,
    pre_application_building_sqft_max = reported_building_sqft_max,
    pre_application_apartments = reported_apartments_sum
  )

post_application_fields <- candidate_year_history |>
  dplyr::filter(!is.na(report_year), report_year >= application_year) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    report_year
  ) |>
  dplyr::group_by(permit_chain_id, footprint_id, candidate_pin) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::select(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    first_post_application_report_year = report_year,
    first_post_application_year_built = reported_year_built_min,
    first_post_application_building_sqft_sum = reported_building_sqft_sum,
    first_post_application_building_sqft_max = reported_building_sqft_max,
    first_post_application_apartments = reported_apartments_sum
  )

candidate_evidence <- candidate_links |>
  dplyr::left_join(
    candidate_assessor_rows |>
      dplyr::group_by(permit_chain_id, footprint_id, candidate_pin) |>
      dplyr::summarise(
        assessor_history_rows = sum(!is.na(row_id)),
        assessor_report_year_min = suppressWarnings(min(
          report_year,
          na.rm = TRUE
        )),
        assessor_report_year_max = suppressWarnings(max(
          report_year,
          na.rm = TRUE
        )),
        reported_year_built_values = paste(
          sort(unique(reported_year_built[!is.na(reported_year_built)])),
          collapse = "/"
        ),
        assessor_year_within_one_of_application = any(
          abs(reported_year_built - application_year) <= 1,
          na.rm = TRUE
        ),
        assessor_year_within_two_of_application = any(
          abs(reported_year_built - application_year) <= 2,
          na.rm = TRUE
        ),
        assessor_year_within_one_of_footprint = any(
          abs(reported_year_built - city_year_built) <= 1,
          na.rm = TRUE
        ),
        assessor_year_within_two_of_footprint = any(
          abs(reported_year_built - city_year_built) <= 2,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        assessor_report_year_min = dplyr::if_else(
          is.infinite(assessor_report_year_min),
          NA_real_,
          assessor_report_year_min
        ),
        assessor_report_year_max = dplyr::if_else(
          is.infinite(assessor_report_year_max),
          NA_real_,
          assessor_report_year_max
        )
      ),
    by = c("permit_chain_id", "footprint_id", "candidate_pin"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    nearest_reported_year,
    by = c("permit_chain_id", "footprint_id", "candidate_pin"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    pre_application_fields,
    by = c("permit_chain_id", "footprint_id", "candidate_pin"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    post_application_fields,
    by = c("permit_chain_id", "footprint_id", "candidate_pin"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    assessor_history_rows = dplyr::coalesce(assessor_history_rows, 0L),
    footprint_completion_year_start = as.Date(paste0(
      city_year_built,
      "-01-01"
    )),
    footprint_completion_year_end = as.Date(paste0(
      city_year_built,
      "-12-31"
    )),
    assessor_completion_year_start = as.Date(paste0(
      selected_assessor_completion_year,
      "-01-01"
    )),
    assessor_completion_year_end = as.Date(paste0(
      selected_assessor_completion_year,
      "-12-31"
    )),
    footprint_application_to_completion_min_days = as.numeric(
      footprint_completion_year_start - representative_application_date
    ),
    footprint_application_to_completion_max_days = as.numeric(
      footprint_completion_year_end - representative_application_date
    ),
    footprint_issue_to_completion_min_days = as.numeric(
      footprint_completion_year_start - representative_issue_date
    ),
    footprint_issue_to_completion_max_days = as.numeric(
      footprint_completion_year_end - representative_issue_date
    ),
    assessor_application_to_completion_min_days = as.numeric(
      assessor_completion_year_start - representative_application_date
    ),
    assessor_application_to_completion_max_days = as.numeric(
      assessor_completion_year_end - representative_application_date
    ),
    assessor_issue_to_completion_min_days = as.numeric(
      assessor_completion_year_start - representative_issue_date
    ),
    assessor_issue_to_completion_max_days = as.numeric(
      assessor_completion_year_end - representative_issue_date
    ),
    building_sqft_sum_change_at_first_post_report =
      first_post_application_building_sqft_sum -
      pre_application_building_sqft_sum,
    building_sqft_max_change_at_first_post_report =
      first_post_application_building_sqft_max -
      pre_application_building_sqft_max,
    apartments_change_at_first_post_report =
      first_post_application_apartments -
      pre_application_apartments,
    assessor_completion_evidence = dplyr::case_when(
      assessor_history_rows == 0L ~ "no_residential_assessor_history",
      assessor_year_within_one_of_application &
        assessor_year_within_one_of_footprint ~
        "assessor_year_matches_permit_and_footprint",
      assessor_year_within_two_of_application &
        assessor_year_within_two_of_footprint ~
        "assessor_year_near_permit_and_footprint",
      assessor_year_within_two_of_footprint ~
        "assessor_year_near_footprint_only",
      assessor_year_within_two_of_application ~
        "assessor_year_near_permit_only",
      TRUE ~ "assessor_year_disagrees"
    ),
    completion_date_precision = "calendar_year_interval"
  ) |>
  dplyr::arrange(
    factor(
      assessor_completion_evidence,
      levels = c(
        "assessor_year_matches_permit_and_footprint",
        "assessor_year_near_permit_and_footprint",
        "assessor_year_near_footprint_only",
        "assessor_year_near_permit_only",
        "assessor_year_disagrees",
        "no_residential_assessor_history"
      )
    ),
    application_boundary_distance_ft,
    permit_chain_id,
    footprint_id,
    candidate_pin
  )

permit_rows <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(permit_chain_id %in% candidate_links$permit_chain_id) |>
  dplyr::arrange(permit_chain_id, application_date, issue_date, permit_number) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_rows_nested = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

timeline_detail <- candidate_evidence |>
  dplyr::left_join(
    permit_rows,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(permit_rows_nested, keep_empty = TRUE) |>
  dplyr::mutate(
    application_to_issue_days = as.numeric(issue_date - application_date),
    footprint_application_to_completion_min_days = as.numeric(
      footprint_completion_year_start - application_date
    ),
    footprint_application_to_completion_max_days = as.numeric(
      footprint_completion_year_end - application_date
    ),
    footprint_issue_to_completion_min_days = as.numeric(
      footprint_completion_year_start - issue_date
    ),
    footprint_issue_to_completion_max_days = as.numeric(
      footprint_completion_year_end - issue_date
    ),
    assessor_application_to_completion_min_days = as.numeric(
      assessor_completion_year_start - application_date
    ),
    assessor_application_to_completion_max_days = as.numeric(
      assessor_completion_year_end - application_date
    ),
    assessor_issue_to_completion_min_days = as.numeric(
      assessor_completion_year_start - issue_date
    ),
    assessor_issue_to_completion_max_days = as.numeric(
      assessor_completion_year_end - issue_date
    )
  ) |>
  dplyr::arrange(
    permit_chain_id,
    footprint_id,
    candidate_pin,
    application_date,
    issue_date,
    permit_number
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = "footprint_current_parcel_candidate_links",
    value = nrow(candidate_links)
  ),
  tibble::tibble(
    metric = "candidate_pins",
    value = dplyr::n_distinct(candidate_links$candidate_pin)
  ),
  tibble::tibble(
    metric = "candidate_links_with_residential_assessor_history",
    value = sum(candidate_evidence$assessor_history_rows > 0L)
  ),
  candidate_evidence |>
    dplyr::count(assessor_completion_evidence, name = "value") |>
    dplyr::transmute(
      metric = paste0(
        "assessor_completion_evidence:",
        assessor_completion_evidence
      ),
      value
    ),
  tibble::tibble(
    metric = "permit_timeline_rows",
    value = nrow(timeline_detail)
  )
)

readr::write_csv(
  candidate_year_history,
  "../output/permit_footprint_current_parcel_assessor_history.csv",
  na = ""
)
readr::write_csv(
  candidate_evidence,
  "../output/permit_footprint_current_parcel_evidence.csv",
  na = ""
)
readr::write_csv(
  timeline_detail,
  "../output/permit_footprint_current_parcel_timeline_detail.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_footprint_assessor_history_summary.csv",
  na = ""
)
