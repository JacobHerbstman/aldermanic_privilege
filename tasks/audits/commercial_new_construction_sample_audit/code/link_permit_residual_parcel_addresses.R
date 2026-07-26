# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin10 <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(
    stringr::str_length(digits) >= 10,
    stringr::str_sub(digits, 1, 10),
    NA_character_
  )
}

normalize_address <- function(x) {
  out <- stringr::str_to_upper(dplyr::coalesce(as.character(x), ""))
  out <- stringr::str_replace_all(out, "\\bNORTH\\b", "N")
  out <- stringr::str_replace_all(out, "\\bSOUTH\\b", "S")
  out <- stringr::str_replace_all(out, "\\bEAST\\b", "E")
  out <- stringr::str_replace_all(out, "\\bWEST\\b", "W")
  out <- stringr::str_replace_all(out, "\\bSTREET\\b", "ST")
  out <- stringr::str_replace_all(out, "\\bAVENUE\\b", "AVE")
  out <- stringr::str_replace_all(out, "\\bBOULEVARD\\b", "BLVD")
  out <- stringr::str_replace_all(out, "\\bROAD\\b", "RD")
  out <- stringr::str_replace_all(out, "\\bDRIVE\\b", "DR")
  out <- stringr::str_replace_all(out, "\\bPLACE\\b", "PL")
  out <- stringr::str_replace_all(out, "\\bCOURT\\b", "CT")
  out <- stringr::str_replace_all(out, "\\bPARKWAY\\b", "PKWY")
  out <- stringr::str_replace_all(out, "\\bTERRACE\\b", "TER")
  out <- stringr::str_replace_all(out, "\\bCHICAGO\\b|\\bIL\\b", " ")
  out <- stringr::str_replace_all(out, "[^A-Z0-9 -]", " ")
  out <- stringr::str_squish(out)
  street_address <- stringr::str_extract(
    out,
    "^[0-9]+(?:-[0-9]+)?(?: [NSEW])? .+?\\b(?:ST|AVE|BLVD|RD|DR|PL|CT|PKWY|TER)\\b"
  )
  dplyr::if_else(!is.na(street_address), street_address, out)
}

chains <- readr::read_csv(
  "../output/permit_first_unmatched_residential_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
permits <- readr::read_csv(
  "../output/permit_first_permit_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_chain_id = readr::col_character(),
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
candidates <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    row_id = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    dplyr::between(year_built, 2006L, 2022L),
    is.finite(building_sqft),
    building_sqft > 0
  ) |>
  dplyr::transmute(
    candidate_pin = stringr::str_replace_all(pin, "[^0-9]", ""),
    candidate_pin10 = normalize_pin10(pin),
    candidate_row_id = row_id,
    candidate_tax_year = tax_year,
    candidate_card_num = card_num,
    candidate_class = class,
    candidate_year_built = year_built,
    candidate_building_sqft = building_sqft,
    candidate_land_sqft = land_sqft,
    candidate_units = num_apartments,
    candidate_review_category = review_category,
    candidate_mechanical_status = mechanical_status
  )
represented_pins <- readr::read_csv(
  "../output/residential_candidate_pin_representation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_pin = readr::col_character(),
    represented_project_ids = readr::col_character()
  )
)

if (anyDuplicated(chains$permit_chain_id) > 0 ||
    anyDuplicated(candidates$candidate_row_id) > 0 ||
    anyDuplicated(represented_pins$candidate_pin) > 0) {
  stop("Permit chains, assessor candidate rows, and represented PINs must be unique.", call. = FALSE)
}

current_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    prop_address_full = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::transmute(
    candidate_pin = stringr::str_replace_all(pin, "[^0-9]", ""),
    assessor_address = prop_address_full,
    address_source = "parcel_address_2025"
  )

historical_addresses <- readr::read_csv(
  "../input/density_parcel_address_selected_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    selected_address = readr::col_character(),
    selected_address_year = readr::col_integer(),
    .default = readr::col_skip()
  )
) |>
  dplyr::transmute(
    candidate_pin = stringr::str_replace_all(pin, "[^0-9]", ""),
    assessor_address = selected_address,
    address_source = paste0("historical_parcel_address_", selected_address_year)
  )

candidate_addresses <- dplyr::bind_rows(
  current_addresses,
  historical_addresses
) |>
  dplyr::filter(!is.na(candidate_pin), candidate_pin != "") |>
  dplyr::mutate(normalized_address = normalize_address(assessor_address)) |>
  dplyr::filter(normalized_address != "") |>
  dplyr::group_by(candidate_pin, normalized_address) |>
  dplyr::summarise(
    assessor_address = paste(sort(unique(assessor_address)), collapse = "/"),
    address_source = paste(sort(unique(address_source)), collapse = "/"),
    .groups = "drop"
  ) |>
  dplyr::inner_join(
    candidates |>
      dplyr::group_by(candidate_pin) |>
      dplyr::summarise(
        candidate_rows = list(dplyr::pick(dplyr::everything())),
        .groups = "drop"
      ),
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(candidate_rows) |>
  dplyr::distinct(
    normalized_address,
    candidate_pin,
    candidate_row_id,
    .keep_all = TRUE
  ) |>
  dplyr::group_by(normalized_address) |>
  dplyr::summarise(
    candidate_rows = list(dplyr::pick(dplyr::everything())),
    .groups = "drop"
  )

permit_addresses <- permits |>
  dplyr::filter(permit_chain_id %in% chains$permit_chain_id) |>
  dplyr::transmute(
    permit_chain_id,
    permit_address,
    normalized_address = normalize_address(permit_address)
  ) |>
  dplyr::filter(normalized_address != "") |>
  dplyr::distinct(permit_chain_id, normalized_address, permit_address)

chain_pin10 <- permits |>
  dplyr::filter(permit_chain_id %in% chains$permit_chain_id) |>
  dplyr::select(permit_chain_id, pin) |>
  tidyr::separate_rows(pin, sep = "\\s*\\|\\s*") |>
  dplyr::mutate(permit_pin10 = normalize_pin10(pin)) |>
  dplyr::filter(!is.na(permit_pin10)) |>
  dplyr::distinct(permit_chain_id, permit_pin10) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    permit_pin10s = paste(sort(unique(permit_pin10)), collapse = "/"),
    .groups = "drop"
  )

address_links <- permit_addresses |>
  dplyr::inner_join(
    candidate_addresses,
    by = "normalized_address",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(candidate_rows) |>
  dplyr::left_join(
    chains |>
      dplyr::select(
        permit_chain_id,
        representative_permit_number,
        representative_application_date,
        representative_issue_date,
        representative_address,
        representative_description,
        maximum_unit_mention,
        any_single_family_signal,
        application_ward_pair,
        application_boundary_distance_ft,
        review_priority
      ),
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    represented_pins,
    by = "candidate_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chain_pin10,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    application_year = lubridate::year(representative_application_date),
    construction_year_gap = candidate_year_built - application_year,
    plausible_construction_year = dplyr::between(construction_year_gap, -1L, 6L),
    exact_permit_pin10 = purrr::map2_lgl(
      candidate_pin10,
      dplyr::coalesce(permit_pin10s, ""),
      ~ !is.na(.x) && .y != "" && .x %in% stringr::str_split_1(.y, "/")
    ),
    represented_in_preferred_ledger = !is.na(represented_project_ids)
  ) |>
  dplyr::filter(plausible_construction_year) |>
  dplyr::arrange(
    permit_chain_id,
    dplyr::desc(exact_permit_pin10),
    abs(construction_year_gap),
    candidate_pin,
    candidate_card_num
  ) |>
  dplyr::group_by(permit_chain_id, candidate_pin) |>
  dplyr::mutate(candidate_pin_cards = dplyr::n_distinct(candidate_card_num)) |>
  dplyr::slice(1L) |>
  dplyr::ungroup()

chain_address_summary <- address_links |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    plausible_address_candidate_pins = dplyr::n_distinct(candidate_pin),
    represented_address_candidate_pins = dplyr::n_distinct(
      candidate_pin[represented_in_preferred_ledger]
    ),
    unrepresented_address_candidate_pins = dplyr::n_distinct(
      candidate_pin[!represented_in_preferred_ledger]
    ),
    exact_permit_pin_address_candidates = sum(exact_permit_pin10),
    .groups = "drop"
  )

chain_review <- chains |>
  dplyr::left_join(
    chain_address_summary,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        plausible_address_candidate_pins,
        represented_address_candidate_pins,
        unrepresented_address_candidate_pins,
        exact_permit_pin_address_candidates
      ),
      ~ dplyr::coalesce(.x, 0L)
    ),
    address_review_status = dplyr::case_when(
      unrepresented_address_candidate_pins == 1L &
        represented_address_candidate_pins == 0L ~
        "one_unrepresented_exact_address_completion_candidate",
      unrepresented_address_candidate_pins > 0L ~
        "manual_review_unrepresented_address_candidates",
      represented_address_candidate_pins > 0L ~
        "address_candidates_already_represented",
      TRUE ~ "no_exact_assessor_address_candidate"
    )
  ) |>
  dplyr::arrange(
    factor(
      address_review_status,
      levels = c(
        "one_unrepresented_exact_address_completion_candidate",
        "manual_review_unrepresented_address_candidates",
        "address_candidates_already_represented",
        "no_exact_assessor_address_candidate"
      )
    ),
    application_boundary_distance_ft,
    permit_chain_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "residual_permit_chains", value = nrow(chains)),
  tibble::tibble(
    metric = "plausible_chain_assessor_address_links",
    value = nrow(address_links)
  ),
  chain_review |>
    dplyr::count(address_review_status, name = "value") |>
    dplyr::transmute(metric = paste0("chain_status:", address_review_status), value)
)

readr::write_csv(
  address_links,
  "../output/permit_residential_assessor_address_links.csv",
  na = ""
)
readr::write_csv(
  chain_review,
  "../output/permit_residential_assessor_address_chain_review.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_residential_assessor_address_summary.csv",
  na = ""
)
