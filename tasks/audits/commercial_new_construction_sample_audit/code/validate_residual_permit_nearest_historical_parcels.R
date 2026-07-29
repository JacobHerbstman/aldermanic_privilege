# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_replace_all(
      "\\b(AVENUE|AVE)\\b",
      "AVE"
    ) |>
    stringr::str_replace_all(
      "\\b(STREET|ST)\\b",
      "ST"
    ) |>
    stringr::str_replace_all(
      "\\b(ROAD|RD)\\b",
      "RD"
    ) |>
    stringr::str_replace_all(
      "\\b(BOULEVARD|BLVD)\\b",
      "BLVD"
    ) |>
    stringr::str_replace_all(
      "\\b(PLACE|PL)\\b",
      "PL"
    ) |>
    stringr::str_replace_all(
      "\\b(PARKWAY|PKWY)\\b",
      "PKWY"
    ) |>
    stringr::str_replace_all(
      "\\b(COURT|CT)\\b",
      "CT"
    ) |>
    stringr::str_replace_all(
      "\\b(DRIVE|DR)\\b",
      "DR"
    ) |>
    stringr::str_squish()
}

matches <- readr::read_csv(
  "../output/residual_permit_nearest_historical_parcel_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    historical_pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(stringr::str_length(historical_pin14) == 14L)
chains <- readr::read_csv(
  "../output/residual_permit_chain_semantics.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    permit_chain_id,
    representative_address,
    semantic_building_class
  )
addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_character()
  )
) |>
  dplyr::transmute(
    historical_pin14 = pin,
    current_property_address = prop_address_full
  )

if (anyDuplicated(chains$permit_chain_id) ||
    anyDuplicated(addresses$historical_pin14)) {
  stop("Nearest parcel validation keys are invalid.", call. = FALSE)
}

candidate_matches <- matches |>
  dplyr::left_join(
    chains,
    by = "permit_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    addresses,
    by = "historical_pin14",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    normalized_permit_address = normalize_address(
      representative_address
    ),
    normalized_property_address = normalize_address(
      current_property_address
    ),
    exact_current_address_match =
      !is.na(normalized_property_address) &
        normalized_permit_address == normalized_property_address
  )

request_validation <- candidate_matches |>
  dplyr::group_by(request_id) |>
  dplyr::mutate(
    exact_address_match_count = sum(
      exact_current_address_match,
      na.rm = TRUE
    ),
    unique_address_selected =
      exact_current_address_match &
        exact_address_match_count == 1L,
    validated_nearest_match = dplyr::case_when(
      unique_address_selected ~ TRUE,
      nearest_match_status == "unique_nearest_parcel" &
        nearest_distance_ft <= 25 ~ TRUE,
      TRUE ~ FALSE
    ),
    nearest_validation_method = dplyr::case_when(
      unique_address_selected &
        nearest_match_status == "ambiguous_nearest_parcels" ~
        "address_resolved_ambiguous_nearest",
      unique_address_selected &
        nearest_distance_ft > 25 ~
        "address_validated_long_distance_nearest",
      unique_address_selected ~
        "address_confirmed_unique_nearest",
      nearest_match_status == "unique_nearest_parcel" &
        nearest_distance_ft <= 25 ~
        "unique_nearest_within_25ft",
      nearest_match_status == "ambiguous_nearest_parcels" ~
        "unresolved_ambiguous_nearest",
      TRUE ~ "unresolved_nearest_beyond_25ft"
    )
  ) |>
  dplyr::ungroup()

validated_matches <- request_validation |>
  dplyr::filter(validated_nearest_match) |>
  dplyr::distinct(
    request_id,
    object_id,
    historical_pin14,
    .keep_all = TRUE
  )
chain_coverage <- chains |>
  dplyr::semi_join(
    matches |>
      dplyr::distinct(permit_chain_id),
    by = "permit_chain_id"
  ) |>
  dplyr::left_join(
    validated_matches |>
      dplyr::group_by(permit_chain_id) |>
      dplyr::summarise(
        validated_year_requests = dplyr::n_distinct(target_year),
        validated_historical_pins = dplyr::n_distinct(
          historical_pin14,
          na.rm = TRUE
        ),
        minimum_validated_distance_ft = min(nearest_distance_ft),
        maximum_validated_distance_ft = max(nearest_distance_ft),
        validated_pin_values = paste(
          sort(unique(historical_pin14)),
          collapse = "/"
        ),
        validation_methods = paste(
          sort(unique(nearest_validation_method)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    validated_year_requests = dplyr::coalesce(
      validated_year_requests,
      0L
    ),
    validated_historical_pins = dplyr::coalesce(
      validated_historical_pins,
      0L
    )
  )

summary <- dplyr::bind_rows(
  request_validation |>
    dplyr::distinct(request_id, nearest_validation_method) |>
    dplyr::count(nearest_validation_method, name = "value") |>
    dplyr::transmute(
      section = "request_status",
      metric = nearest_validation_method,
      value
    ),
  tibble::tibble(
    section = "coverage",
    metric = c(
      "chains_requiring_fallback",
      "chains_with_validated_fallback_pin",
      "chains_without_validated_fallback_pin",
      "validated_historical_pins"
    ),
    value = c(
      nrow(chain_coverage),
      sum(chain_coverage$validated_historical_pins > 0),
      sum(chain_coverage$validated_historical_pins == 0),
      dplyr::n_distinct(validated_matches$historical_pin14)
    )
  )
)

readr::write_csv(
  request_validation,
  "../output/residual_permit_nearest_historical_parcel_validation.csv"
)
readr::write_csv(
  validated_matches,
  "../output/residual_permit_validated_nearest_historical_parcel_matches.csv"
)
readr::write_csv(
  chain_coverage,
  "../output/residual_permit_validated_nearest_historical_parcel_coverage.csv"
)
readr::write_csv(
  summary,
  "../output/residual_permit_validated_nearest_historical_parcel_summary.csv"
)
