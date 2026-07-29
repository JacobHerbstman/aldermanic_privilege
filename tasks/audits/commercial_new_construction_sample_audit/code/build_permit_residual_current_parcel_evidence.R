# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14L, digits, NA_character_)
}

normalize_address <- function(x) {
  out <- stringr::str_to_upper(dplyr::coalesce(as.character(x), ""))
  out <- stringr::str_replace_all(out, "\\bCHICAGO\\b", "")
  out <- stringr::str_replace_all(out, "\\bSTREET\\b", "ST")
  out <- stringr::str_replace_all(out, "\\bAVENUE\\b", "AVE")
  out <- stringr::str_replace_all(out, "\\bBOULEVARD\\b", "BLVD")
  out <- stringr::str_replace_all(out, "\\bROAD\\b", "RD")
  out <- stringr::str_replace_all(out, "\\bDRIVE\\b", "DR")
  out <- stringr::str_replace_all(out, "\\bPLACE\\b", "PL")
  out <- stringr::str_replace_all(out, "\\bCOURT\\b", "CT")
  out <- stringr::str_replace_all(out, "\\bPARKWAY\\b", "PKWY")
  out <- stringr::str_replace_all(out, "[^A-Z0-9 -]", " ")
  stringr::str_squish(out)
}

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    representative_permit_id = readr::col_character(),
    representative_permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
)
parcel_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
assessor_history <- readr::read_csv(
  "../input/residential_improvement_characteristics_full.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    tieback_key_pin = readr::col_character(),
    row_id = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
)
represented_pins <- readr::read_csv(
  "../output/residential_candidate_pin_representation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_pin = readr::col_character(),
    represented_project_ids = readr::col_character()
  )
) |>
  dplyr::transmute(
    candidate_pin = normalize_pin(candidate_pin),
    represented_project_ids
  )

if (anyDuplicated(chains$permit_chain_id)) {
  stop("Residual permit chains must be unique.", call. = FALSE)
}
if (anyDuplicated(represented_pins$candidate_pin)) {
  stop("Represented candidate PINs must be unique.", call. = FALSE)
}

address_pin_map <- parcel_addresses |>
  dplyr::transmute(
    normalized_address = normalize_address(prop_address_full),
    candidate_pin = normalize_pin(pin)
  ) |>
  dplyr::filter(normalized_address != "", !is.na(candidate_pin)) |>
  dplyr::distinct(normalized_address, candidate_pin) |>
  dplyr::arrange(normalized_address, candidate_pin) |>
  dplyr::group_by(normalized_address) |>
  dplyr::summarise(
    address_pins = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

chain_address_pins <- chains |>
  dplyr::transmute(
    permit_chain_id,
    normalized_address = representative_normalized_address,
    representative_address,
    representative_application_date,
    representative_issue_date,
    application_year = lubridate::year(representative_application_date)
  ) |>
  dplyr::left_join(
    address_pin_map,
    by = "normalized_address",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(address_pins, keep_empty = TRUE) |>
  dplyr::filter(!is.na(candidate_pin))

requested_pins <- unique(chain_address_pins$candidate_pin)
history_by_pin <- assessor_history |>
  dplyr::mutate(candidate_pin = normalize_pin(pin)) |>
  dplyr::filter(candidate_pin %in% requested_pins) |>
  dplyr::transmute(
    candidate_pin,
    assessor_row_id = row_id,
    assessor_report_year = as.integer(year),
    assessor_card_num = as.integer(card),
    assessor_class = class,
    assessor_tieback_pin = normalize_pin(tieback_key_pin),
    assessor_year_built = as.integer(char_yrblt),
    assessor_building_sqft = as.numeric(char_bldg_sf),
    assessor_land_sqft = as.numeric(char_land_sf),
    assessor_units = suppressWarnings(as.numeric(char_apts)),
    assessor_residence_type = as.character(char_type_resd),
    assessor_use = as.character(char_use),
    pin_is_multicard = as.logical(pin_is_multicard),
    pin_num_cards = as.integer(pin_num_cards),
    pin_is_multiland = as.logical(pin_is_multiland),
    pin_num_landlines = as.integer(pin_num_landlines)
  ) |>
  dplyr::filter(
    dplyr::between(assessor_year_built, 2006L, 2022L),
    is.finite(assessor_building_sqft),
    assessor_building_sqft > 0
  ) |>
  dplyr::arrange(
    candidate_pin,
    assessor_card_num,
    assessor_year_built,
    assessor_report_year,
    assessor_row_id
  ) |>
  dplyr::group_by(candidate_pin) |>
  dplyr::summarise(
    history_rows = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

address_assessor_rows <- chain_address_pins |>
  dplyr::left_join(
    history_by_pin,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(history_rows, keep_empty = TRUE) |>
  dplyr::mutate(
    construction_year_gap = assessor_year_built - application_year,
    plausible_construction_year = dplyr::between(
      construction_year_gap,
      -1L,
      6L
    )
  )

plausible_episode_rows <- address_assessor_rows |>
  dplyr::filter(plausible_construction_year) |>
  dplyr::arrange(
    permit_chain_id,
    candidate_pin,
    assessor_card_num,
    assessor_year_built,
    dplyr::desc(assessor_report_year),
    assessor_row_id
  ) |>
  dplyr::group_by(
    permit_chain_id,
    candidate_pin,
    assessor_card_num,
    assessor_year_built
  ) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    represented_pins,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    represented_in_preferred_ledger = !is.na(represented_project_ids),
    completion_year_start = as.Date(paste0(assessor_year_built, "-01-01")),
    completion_year_end = as.Date(paste0(assessor_year_built, "-12-31")),
    completion_date_precision = "assessor_construction_year_interval",
    application_to_issue_days = as.numeric(
      representative_issue_date - representative_application_date
    ),
    application_to_completion_min_days = as.numeric(
      completion_year_start - representative_application_date
    ),
    application_to_completion_max_days = as.numeric(
      completion_year_end - representative_application_date
    ),
    issue_to_completion_min_days = as.numeric(
      completion_year_start - representative_issue_date
    ),
    issue_to_completion_max_days = as.numeric(
      completion_year_end - representative_issue_date
    ),
    timeline_order_status = dplyr::case_when(
      assessor_year_built < application_year ~
        "reported_construction_year_precedes_application",
      assessor_year_built == application_year ~
        "application_and_reported_construction_same_year",
      TRUE ~ "reported_construction_after_application"
    )
  )

chain_episode_summary <- plausible_episode_rows |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_exact_address_pins = dplyr::n_distinct(candidate_pin),
    plausible_exact_address_pin_cards = dplyr::n_distinct(
      paste(candidate_pin, assessor_card_num, sep = ":")
    ),
    represented_exact_address_pins = dplyr::n_distinct(
      candidate_pin[represented_in_preferred_ledger]
    ),
    unrepresented_exact_address_pins = dplyr::n_distinct(
      candidate_pin[!represented_in_preferred_ledger]
    ),
    unrepresented_exact_address_pin_cards = dplyr::n_distinct(
      paste(
        candidate_pin[!represented_in_preferred_ledger],
        assessor_card_num[!represented_in_preferred_ledger],
        sep = ":"
      )
    ),
    exact_address_candidate_pins = paste(
      sort(unique(candidate_pin)),
      collapse = "/"
    ),
    unrepresented_exact_address_candidate_pins = paste(
      sort(unique(candidate_pin[!represented_in_preferred_ledger])),
      collapse = "/"
    ),
    .groups = "drop"
  )

address_pin_counts <- chain_address_pins |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    current_exact_address_pins = dplyr::n_distinct(candidate_pin),
    current_exact_address_pin_values = paste(
      sort(unique(candidate_pin)),
      collapse = "/"
    ),
    .groups = "drop"
  )

chain_review <- chains |>
  dplyr::left_join(
    address_pin_counts,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    chain_episode_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        current_exact_address_pins,
        plausible_exact_address_pins,
        plausible_exact_address_pin_cards,
        represented_exact_address_pins,
        unrepresented_exact_address_pins,
        unrepresented_exact_address_pin_cards
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    current_address_assessor_status = dplyr::case_when(
      unrepresented_exact_address_pins == 1L &
        unrepresented_exact_address_pin_cards == 1L ~
        "one_unrepresented_exact_address_assessor_episode",
      unrepresented_exact_address_pins == 1L &
        unrepresented_exact_address_pin_cards > 1L ~
        "one_unrepresented_pin_multiple_card_episodes",
      unrepresented_exact_address_pins > 1L ~
        "multiple_unrepresented_exact_address_pins",
      represented_exact_address_pins > 0L ~
        "exact_address_assessor_episode_already_represented",
      current_exact_address_pins > 0L ~
        "exact_current_address_without_plausible_assessor_episode",
      TRUE ~ "no_current_exact_address_pin"
    )
  ) |>
  dplyr::arrange(
    factor(
      current_address_assessor_status,
      levels = c(
        "one_unrepresented_exact_address_assessor_episode",
        "one_unrepresented_pin_multiple_card_episodes",
        "multiple_unrepresented_exact_address_pins",
        "exact_address_assessor_episode_already_represented",
        "exact_current_address_without_plausible_assessor_episode",
        "no_current_exact_address_pin"
      )
    ),
    application_boundary_distance_ft,
    representative_application_date,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "residual_permit_chains", value = nrow(chains)),
  tibble::tibble(
    metric = "chains_with_current_exact_address_pin",
    value = dplyr::n_distinct(chain_address_pins$permit_chain_id)
  ),
  tibble::tibble(
    metric = "plausible_chain_pin_card_episode_rows",
    value = nrow(plausible_episode_rows)
  ),
  chain_review |>
    dplyr::count(current_address_assessor_status, name = "value") |>
    dplyr::transmute(
      metric = paste0("chain_status:", current_address_assessor_status),
      value
    ),
  plausible_episode_rows |>
    dplyr::count(timeline_order_status, name = "value") |>
    dplyr::transmute(metric = paste0("timeline_order:", timeline_order_status), value)
)

readr::write_csv(
  plausible_episode_rows,
  "../output/permit_residual_current_address_assessor_episodes.csv",
  na = ""
)
readr::write_csv(
  chain_review,
  "../output/permit_residual_current_address_assessor_chain_review.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_residual_current_address_assessor_summary.csv",
  na = ""
)
