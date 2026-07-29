# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  stringr::str_to_upper(dplyr::coalesce(as.character(x), "")) |>
    stringr::str_replace_all("\\bCHICAGO\\b", "") |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_replace_all(
      paste0(
        "\\b(STREET|ST|AVENUE|AVE|ROAD|RD|BOULEVARD|BLVD|",
        "COURT|CT|PLACE|PL|DRIVE|DR|PARKWAY|PKWY)\\b"
      ),
      ""
    ) |>
    stringr::str_squish()
}

parse_address_range <- function(x) {
  normalized <- normalize_address(x)
  parts <- stringr::str_match(
    normalized,
    "^([0-9]+)(?:\\s+([0-9]+))?\\s+(.+)$"
  )
  first_number <- suppressWarnings(as.integer(parts[, 2]))
  second_number <- suppressWarnings(as.integer(parts[, 3]))
  tibble::tibble(
    address_number_min = pmin(
      first_number,
      dplyr::coalesce(second_number, first_number)
    ),
    address_number_max = pmax(
      first_number,
      dplyr::coalesce(second_number, first_number)
    ),
    street_key = stringr::str_squish(parts[, 4])
  )
}

chain_status <- readr::read_csv(
  "../output/residual_permit_footprint_2022_assessor_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    successor_assessor_status ==
      "completion_supported_but_density_fields_incomplete"
  ) |>
  dplyr::distinct(
    permit_chain_id,
    representative_application_date,
    representative_address,
    representative_description
  )

footprint_links <- readr::read_csv(
  "../output/permit_residual_city_building_footprint_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    footprint_id = readr::col_character(),
    harris_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(
    chain_status,
    by = "permit_chain_id"
  ) |>
  dplyr::filter(
    strong_footprint_match,
    !represented_in_preferred_ledger
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::filter(
    dplyr::case_when(
      any(address_range_match) ~ address_range_match,
      any(exact_harris_pin) ~ exact_harris_pin,
      TRUE ~ permit_point_inside_footprint
    )
  ) |>
  dplyr::ungroup()

permit_ranges <- chain_status |>
  dplyr::select(
    permit_chain_id,
    source_address = representative_address
  ) |>
  dplyr::bind_cols(
    parse_address_range(chain_status$representative_address)
  ) |>
  dplyr::mutate(address_range_source = "permit_address")
footprint_ranges <- footprint_links |>
  dplyr::transmute(
    permit_chain_id,
    source_address = city_address,
    address_number_min = as.integer(address_from),
    address_number_max = as.integer(address_to),
    street_key = normalize_address(city_street),
    address_range_source = "city_footprint_address"
  )
candidate_ranges <- dplyr::bind_rows(
  permit_ranges,
  footprint_ranges
) |>
  dplyr::filter(
    is.finite(address_number_min),
    is.finite(address_number_max),
    street_key != ""
  ) |>
  dplyr::distinct(
    permit_chain_id,
    address_number_min,
    address_number_max,
    street_key,
    .keep_all = TRUE
  )

parcel_pins <- readr::read_csv(
  "../output/residual_permit_footprint_2022_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    footprint_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    pin10_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  tidyr::nest(parcel_rows = -footprint_id)
parcel_pins <- footprint_links |>
  dplyr::distinct(permit_chain_id, footprint_id) |>
  dplyr::inner_join(
    parcel_pins,
    by = "footprint_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(
    parcel_rows
  ) |>
  dplyr::transmute(
    permit_chain_id,
    candidate_pin14 = pin14_2022,
    candidate_pin10 = stringr::str_sub(pin14_2022, 1, 10)
  )
historical_pins <- readr::read_csv(
  "../output/residual_permit_historical_pin_reconciliation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(
    chain_status,
    by = "permit_chain_id"
  ) |>
  dplyr::transmute(
    permit_chain_id,
    candidate_pin14 = historical_pin14,
    candidate_pin10 = stringr::str_sub(historical_pin14, 1, 10)
  )
candidate_pins <- dplyr::bind_rows(
  parcel_pins,
  historical_pins
) |>
  dplyr::filter(stringr::str_length(candidate_pin14) == 14L) |>
  dplyr::distinct()
candidate_pin_lists <- candidate_pins |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    candidate_pin14s = list(sort(unique(candidate_pin14))),
    candidate_pin10s = list(sort(unique(candidate_pin10))),
    .groups = "drop"
  )

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))
commercial <- DBI::dbGetQuery(con, "
SELECT
  row_number() OVER () AS commercial_row_id,
  regexp_replace(trim(keypin), '[^0-9]', '', 'g') AS keypin,
  trim(pins) AS source_pins,
  try_cast(numeric_text(year) AS INTEGER) AS assessor_report_year,
  try_cast(numeric_text(yearbuilt) AS INTEGER) AS year_built,
  try_cast(numeric_text(tot_units) AS DOUBLE) AS dwelling_units,
  try_cast(numeric_text(bldgsf) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(landsf) AS DOUBLE) AS land_sqft,
  trim(address) AS assessor_address,
  trim(property_type_use) AS property_type_use,
  trim(category) AS category
FROM read_csv(
  '../input/commercial_value_raw.csv',
  all_varchar = true,
  header = true,
  ignore_errors = false,
  max_line_size = 10000000
)
WHERE try_cast(numeric_text(year) AS INTEGER) <= 2025;
")
commercial <- dplyr::bind_cols(
  commercial,
  parse_address_range(commercial$assessor_address) |>
    dplyr::rename(
      commercial_address_number_min = address_number_min,
      commercial_address_number_max = address_number_max
    )
) |>
  dplyr::mutate(
    keypin10 = stringr::str_sub(keypin, 1, 10),
    component_pin14s = purrr::map2(
      stringr::str_extract_all(
        dplyr::coalesce(source_pins, ""),
        "[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{4}|[0-9]{14}"
      ),
      keypin,
      \(pins, key_pin) sort(unique(c(
        stringr::str_replace_all(pins, "[^0-9]", ""),
        key_pin
      )))
    ),
    component_pin10s = purrr::map(
      component_pin14s,
      \(pins) sort(unique(stringr::str_sub(pins, 1, 10)))
    )
  ) |>
  dplyr::filter(
    is.finite(commercial_address_number_min),
    street_key != ""
  ) |>
  dplyr::group_by(keypin) |>
  dplyr::arrange(
    dplyr::desc(assessor_report_year),
    commercial_row_id,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    street_key,
    commercial_address_number_min,
    dplyr::desc(assessor_report_year),
    commercial_row_id
  )

commercial_by_street <- commercial |>
  tidyr::nest(commercial_rows = -street_key)
address_candidates <- candidate_ranges |>
  dplyr::inner_join(
    commercial_by_street,
    by = "street_key",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(commercial_rows) |>
  dplyr::filter(
    commercial_address_number_min <= address_number_max,
    commercial_address_number_max >= address_number_min
  ) |>
  dplyr::distinct(
    permit_chain_id,
    commercial_row_id,
    .keep_all = TRUE
  ) |>
  dplyr::left_join(
    candidate_pin_lists,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    chain_status,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    exact_pin14_overlap = any(
      component_pin14s %in% candidate_pin14s
    ),
    pin10_overlap = any(
      component_pin10s %in% candidate_pin10s
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    application_year = lubridate::year(
      representative_application_date
    ),
    commercial_year_gap = year_built - application_year,
    plausible_commercial_year =
      dplyr::between(commercial_year_gap, -1L, 4L),
    complete_density_fields =
      is.finite(dwelling_units) & dwelling_units > 0 &
      is.finite(building_sqft) & building_sqft > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    condo_record = stringr::str_detect(
      paste(category, property_type_use),
      stringr::regex("condo", ignore_case = TRUE)
    ),
    exact_permit_address =
      commercial_address_number_min ==
        address_number_min &
      commercial_address_number_max ==
        address_number_max &
      address_range_source == "permit_address",
    evidence_rank = dplyr::case_when(
      condo_record ~ 0L,
      exact_pin14_overlap & complete_density_fields ~ 5L,
      pin10_overlap & complete_density_fields ~ 4L,
      exact_permit_address &
        plausible_commercial_year &
        complete_density_fields ~ 3L,
      plausible_commercial_year &
        complete_density_fields ~ 2L,
      TRUE ~ 1L
    )
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::arrange(
    dplyr::desc(evidence_rank),
    dplyr::desc(plausible_commercial_year),
    dplyr::desc(assessor_report_year),
    commercial_row_id,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    top_evidence_rank = max(evidence_rank),
    top_rank_candidate_count = sum(
      evidence_rank == top_evidence_rank
    ),
    selected_commercial_address_candidate =
      dplyr::row_number() == 1L &
      top_evidence_rank >= 2L &
      top_rank_candidate_count == 1L
  ) |>
  dplyr::ungroup()

chain_summary <- chain_status |>
  dplyr::left_join(
    address_candidates |>
      dplyr::group_by(permit_chain_id) |>
      dplyr::summarise(
        commercial_address_candidates = dplyr::n_distinct(
          commercial_row_id
        ),
        top_evidence_rank = max(top_evidence_rank),
        top_rank_candidate_count = max(top_rank_candidate_count),
        selected_commercial_keypin = dplyr::first(
          keypin[selected_commercial_address_candidate],
          default = NA_character_
        ),
        selected_commercial_address = dplyr::first(
          assessor_address[
            selected_commercial_address_candidate
          ],
          default = NA_character_
        ),
        selected_commercial_year_built = dplyr::first(
          year_built[selected_commercial_address_candidate],
          default = NA_real_
        ),
        selected_commercial_dwelling_units = dplyr::first(
          dwelling_units[selected_commercial_address_candidate],
          default = NA_real_
        ),
        selected_commercial_building_sqft = dplyr::first(
          building_sqft[selected_commercial_address_candidate],
          default = NA_real_
        ),
        selected_commercial_land_sqft = dplyr::first(
          land_sqft[selected_commercial_address_candidate],
          default = NA_real_
        ),
        commercial_address_recovery_status = dplyr::case_when(
          any(selected_commercial_address_candidate) ~
            "unique_supported_commercial_address_candidate",
          top_evidence_rank == 0L ~
            "condo_records_only",
          top_evidence_rank >= 2L ~
            "multiple_supported_commercial_address_candidates",
          commercial_address_candidates > 0L ~
            "address_candidates_without_complete_supported_fields",
          TRUE ~ "no_commercial_address_candidate"
        ),
        .groups = "drop"
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    commercial_address_candidates = dplyr::coalesce(
      commercial_address_candidates,
      0L
    ),
    commercial_address_recovery_status = dplyr::coalesce(
      commercial_address_recovery_status,
      "no_commercial_address_candidate"
    )
  )

summary <- dplyr::bind_rows(
  chain_summary |>
    dplyr::count(
      commercial_address_recovery_status,
      name = "value"
    ) |>
    dplyr::transmute(
      section = "recovery_status",
      metric = commercial_address_recovery_status,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "incomplete_footprint_candidate_chains",
      "commercial_address_candidate_rows",
      "uniquely_recovered_chains"
    ),
    value = c(
      nrow(chain_status),
      nrow(address_candidates),
      sum(
        chain_summary$commercial_address_recovery_status ==
          "unique_supported_commercial_address_candidate"
      )
    )
  )
)

readr::write_csv(
  address_candidates,
  "../output/residual_footprint_commercial_address_candidates.csv"
)
readr::write_csv(
  chain_summary,
  "../output/residual_footprint_commercial_address_chain_summary.csv"
)
readr::write_csv(
  summary,
  "../output/residual_footprint_commercial_address_summary.csv"
)
