# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

address_key <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_replace_all(
      "\\b(N|S|E|W|NORTH|SOUTH|EAST|WEST)\\b",
      " "
    ) |>
    stringr::str_replace_all(
      "\\b(STREET|ST|AVENUE|AVE|BOULEVARD|BLVD|ROAD|RD|DRIVE|DR|PLACE|PL|COURT|CT|PARKWAY|PKWY)\\b",
      " "
    ) |>
    stringr::str_squish()
}

chain_evidence <- readr::read_csv(
  "../output/residual_permit_historical_chain_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin_values = readr::col_character(),
    represented_project_ids = readr::col_character(),
    unrepresented_historical_pin_values =
      readr::col_character(),
    plausible_unrepresented_residential_pin_values =
      readr::col_character(),
    plausible_residential_year_values =
      readr::col_character(),
    plausible_unrepresented_commercial_pin_values =
      readr::col_character(),
    plausible_commercial_year_values =
      readr::col_character(),
    .default = readr::col_guess()
  )
)
footprint_transitions <- readr::read_csv(
  "../output/residual_footprint_field_transitions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
)
footprint_assessor <- readr::read_csv(
  "../output/residual_permit_footprint_2022_assessor_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    represented_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::summarise(
    successor_assessor_statuses = paste(
      sort(unique(successor_assessor_status)),
      collapse = "/"
    ),
    .groups = "drop"
  )
multi_parcel <- readr::read_csv(
  "../output/residual_footprint_multi_parcel_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    pin14_2022 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::distinct(permit_chain_id) |>
  dplyr::mutate(multi_parcel_footprint_candidate = TRUE)
historical_overrides <- readr::read_csv(
  "../adjudication/residual_historical_candidate_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    physical_project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
original_ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)
duplicate_dispositions <- readr::read_csv(
  "../output/preferred_project_duplicate_dispositions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(project_id, duplicate_disposition)

if (anyDuplicated(chain_evidence$permit_chain_id) ||
    anyDuplicated(footprint_transitions$permit_chain_id) ||
    anyDuplicated(original_ledger$project_id) ||
    anyDuplicated(duplicate_dispositions$project_id) ||
    anyDuplicated(historical_overrides$permit_chain_id)) {
  stop("Final audit-ledger inputs have invalid keys.", call. = FALSE)
}

parcel_coordinates <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) |>
  dplyr::transmute(
    component_pin = stringr::str_pad(pin, 14L, pad = "0"),
    x_3435 = centroid_x_crs_3435,
    y_3435 = centroid_y_crs_3435
  ) |>
  dplyr::filter(
    stringr::str_length(component_pin) == 14L,
    is.finite(x_3435),
    is.finite(y_3435)
  ) |>
  dplyr::distinct(component_pin, .keep_all = TRUE)
historical_reconciliation <- readr::read_csv(
  "../output/residual_permit_historical_pin_reconciliation.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    permit_chain_id = readr::col_character(),
    historical_pin14 = readr::col_character(),
    represented_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    !is.na(historical_pin14),
    historical_pin14 != ""
  ) |>
  dplyr::arrange(
    permit_chain_id,
    historical_parcel_distance_ft,
    target_year
  ) |>
  dplyr::group_by(permit_chain_id) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()
historical_parcels <- sf::st_read(
  "../output/residual_permit_historical_parcels.gpkg",
  quiet = TRUE
)
historical_centroids <- sf::st_centroid(
  sf::st_geometry(historical_parcels)
)
historical_parcel_coordinates_exact <- historical_parcels |>
  dplyr::mutate(
    x_3435_historical =
      sf::st_coordinates(historical_centroids)[, 1],
    y_3435_historical =
      sf::st_coordinates(historical_centroids)[, 2]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    target_year,
    historical_pin14,
    x_3435_historical,
    y_3435_historical
  ) |>
  dplyr::distinct(
    target_year,
    historical_pin14,
    .keep_all = TRUE
  )
nearest_historical_parcels <- sf::st_read(
  "../output/residual_permit_nearest_historical_parcels.gpkg",
  quiet = TRUE
)
nearest_historical_centroids <- sf::st_centroid(
  sf::st_geometry(nearest_historical_parcels)
)
historical_parcel_coordinates_nearest <-
  nearest_historical_parcels |>
  dplyr::mutate(
    x_3435_historical =
      sf::st_coordinates(nearest_historical_centroids)[, 1],
    y_3435_historical =
      sf::st_coordinates(nearest_historical_centroids)[, 2]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    target_year,
    historical_pin14,
    x_3435_historical,
    y_3435_historical
  ) |>
  dplyr::distinct(
    target_year,
    historical_pin14,
    .keep_all = TRUE
  )
historical_parcel_coordinates <- dplyr::bind_rows(
  historical_parcel_coordinates_exact,
  historical_parcel_coordinates_nearest
) |>
  dplyr::distinct(
    target_year,
    historical_pin14,
    .keep_all = TRUE
  )
historical_chain_coordinates <- historical_reconciliation |>
  dplyr::left_join(
    historical_parcel_coordinates,
    by = c("target_year", "historical_pin14"),
    relationship = "many-to-one"
  ) |>
  dplyr::select(
    geometry_chain_id = permit_chain_id,
    historical_pin14,
    historical_parcel_match_method,
    x_3435_historical,
    y_3435_historical
  )

footprint_additions <- footprint_transitions |>
  dplyr::filter(final_decision == "include") |>
  dplyr::transmute(
    project_id = paste0(
      "missing_footprint_",
      stringr::str_remove(permit_chain_id, "^permit_chain_")
    ),
    permit_chain_ids = permit_chain_id,
    geometry_chain_id = permit_chain_id,
    source_family = "residential",
    source_addresses = representative_address,
    component_pin = pin14_2022,
    project_kind = "recovered_completed_residential_building",
    construction_year = completion_year,
    dwelling_units = final_dwelling_units,
    building_sqft = candidate_building_sqft,
    land_sqft = candidate_land_sqft,
    membership_source =
      "issued_new_construction_permit_and_city_building_footprint",
    year_source =
      "assessor_field_transition_or_verified_building_record",
    units_source = dplyr::if_else(
      is.finite(unit_override),
      "new_construction_permit",
      "successor_assessor_or_city_building_record"
    ),
    building_source = "successor_residential_assessor",
    land_source = "successor_residential_assessor",
    decision_reason = final_decision_reason,
    evidence = final_evidence,
    representative_application_date,
    representative_issue_date,
    completion_date_start,
    completion_date_end,
    application_to_completion_min_days,
    application_to_completion_max_days,
    issue_to_completion_min_days,
    issue_to_completion_max_days
  )
historical_additions <- historical_overrides |>
  dplyr::filter(decision %in% c("include_project", "include_phase")) |>
  dplyr::group_by(physical_project_id) |>
  dplyr::summarise(
    project_id = dplyr::first(physical_project_id),
    permit_chain_ids = paste(
      sort(unique(permit_chain_id)),
      collapse = "/"
    ),
    geometry_chain_id = permit_chain_id[
      decision == "include_project"
    ][1],
    source_family = "residential",
    source_addresses = chain_evidence$representative_address[
      match(
        permit_chain_id[decision == "include_project"],
        chain_evidence$permit_chain_id
      )
    ][1],
    component_pin = component_pin[
      decision == "include_project"
    ][1],
    project_kind = "recovered_completed_residential_building",
    construction_year = completion_year[
      decision == "include_project"
    ][1],
    dwelling_units = dwelling_units[
      decision == "include_project"
    ][1],
    building_sqft = building_sqft[
      decision == "include_project"
    ][1],
    land_sqft = land_sqft[
      decision == "include_project"
    ][1],
    membership_source =
      "issued_new_construction_permit_and_historical_assessor_completion",
    year_source = "historical_assessor_completion",
    units_source = dplyr::if_else(
      project_id == "missing_project_30_e_balbo",
      "new_construction_permit",
      "historical_residential_assessor"
    ),
    building_source = dplyr::if_else(
      project_id == "missing_project_30_e_balbo",
      "historical_commercial_whole_building_record",
      "historical_residential_assessor"
    ),
    land_source = building_source,
    decision_reason = decision_reason[
      decision == "include_project"
    ][1],
    evidence = paste(
      sort(unique(permit_chain_id)),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    representative_application_date = purrr::map(
      permit_chain_ids,
      ~ min(
        chain_evidence$representative_application_date[
          chain_evidence$permit_chain_id %in%
            stringr::str_split_1(.x, "/")
        ],
        na.rm = TRUE
      )
    ) |>
      unlist() |>
      as.Date(origin = "1970-01-01"),
    representative_issue_date = purrr::map(
      permit_chain_ids,
      ~ min(
        chain_evidence$representative_issue_date[
          chain_evidence$permit_chain_id %in%
            stringr::str_split_1(.x, "/")
        ],
        na.rm = TRUE
      )
    ) |>
      unlist() |>
      as.Date(origin = "1970-01-01"),
    completion_date_start = as.Date(paste0(
      construction_year,
      "-01-01"
    )),
    completion_date_end = as.Date(paste0(
      construction_year,
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
    )
  )
additions <- dplyr::bind_rows(
  footprint_additions,
  historical_additions
) |>
  dplyr::left_join(
    parcel_coordinates,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    historical_chain_coordinates,
    by = "geometry_chain_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    x_3435 = dplyr::coalesce(
      x_3435_historical,
      x_3435
    ),
    y_3435 = dplyr::coalesce(
      y_3435_historical,
      y_3435
    ),
    geometry_source = dplyr::if_else(
      is.finite(x_3435_historical),
      "construction_year_historical_parcel_centroid",
      "current_successor_parcel_centroid"
    ),
    geometry_evidence = dplyr::if_else(
      is.finite(x_3435_historical),
      paste0(
        historical_parcel_match_method,
        ":",
        historical_pin14
      ),
      component_pin
    ),
    address_key = address_key(source_addresses),
    allow_far = is.finite(building_sqft) & building_sqft > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    allow_dupac = is.finite(dwelling_units) & dwelling_units > 0 &
      is.finite(land_sqft) & land_sqft > 0
  )

if (nrow(additions) != 35L ||
    anyDuplicated(additions$project_id) ||
    anyDuplicated(additions$component_pin) ||
    any(!is.finite(additions$x_3435)) ||
    any(!is.finite(additions$y_3435)) ||
    any(!additions$allow_far) ||
    any(!additions$allow_dupac)) {
  stop(
    paste0(
      "Accepted missing-project additions failed validation: rows=",
      nrow(additions),
      ", duplicate_ids=",
      anyDuplicated(additions$project_id),
      ", duplicate_pins=",
      anyDuplicated(additions$component_pin),
      ", missing_x=",
      sum(!is.finite(additions$x_3435)),
      ", missing_y=",
      sum(!is.finite(additions$y_3435)),
      ", invalid_far=",
      sum(!additions$allow_far),
      ", invalid_dupac=",
      sum(!additions$allow_dupac),
      "."
    ),
    call. = FALSE
  )
}

retained_ledger <- original_ledger |>
  dplyr::left_join(
    duplicate_dispositions,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    is.na(duplicate_disposition) |
      duplicate_disposition == "retain"
  )
retained_components <- retained_ledger |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(component_pin = component_pins) |>
  dplyr::filter(!is.na(component_pin), component_pin != "")

spatial_dedupe <- tidyr::crossing(
  addition_project_id = additions$project_id,
  retained_project_id = retained_ledger$project_id
) |>
  dplyr::left_join(
    additions |>
      dplyr::select(
        addition_project_id = project_id,
        addition_component_pin = component_pin,
        addition_address_key = address_key,
        addition_year = construction_year,
        addition_units = dwelling_units,
        addition_building_sqft = building_sqft,
        addition_x = x_3435,
        addition_y = y_3435
      ),
    by = "addition_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    retained_ledger |>
      dplyr::transmute(
        retained_project_id = project_id,
        retained_address_key = address_key(source_addresses),
        retained_year = construction_year,
        retained_units = dwelling_units,
        retained_building_sqft = building_sqft,
        retained_x = x_3435,
        retained_y = y_3435
      ),
    by = "retained_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    distance_ft = sqrt(
      (addition_x - retained_x)^2 +
        (addition_y - retained_y)^2
    ),
    year_gap = abs(addition_year - retained_year),
    same_address =
      !is.na(addition_address_key) &
        addition_address_key == retained_address_key,
    plausible_duplicate =
      year_gap <= 4L &
        (
          distance_ft <= 5 |
            same_address
        )
  )
addition_dedupe_screen <- spatial_dedupe |>
  dplyr::group_by(addition_project_id) |>
  dplyr::arrange(distance_ft, year_gap, .by_group = TRUE) |>
  dplyr::summarise(
    nearest_retained_project_id = dplyr::first(
      retained_project_id
    ),
    nearest_retained_distance_ft = dplyr::first(distance_ft),
    nearest_retained_year_gap = dplyr::first(year_gap),
    plausible_retained_duplicate_count = sum(
      plausible_duplicate,
      na.rm = TRUE
    ),
    plausible_retained_duplicate_ids = paste(
      sort(unique(
        retained_project_id[plausible_duplicate]
      )),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    additions |>
      dplyr::select(
        project_id,
        source_addresses,
        component_pin,
        construction_year,
        dwelling_units,
        building_sqft,
        land_sqft
      ),
    by = c("addition_project_id" = "project_id"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    component_pin_already_retained =
      component_pin %in% retained_components$component_pin
  )

addition_pairs <- tidyr::crossing(
  additions |>
    dplyr::transmute(
      project_id_1 = project_id,
      source_addresses_1 = source_addresses,
      component_pin_1 = component_pin,
      construction_year_1 = construction_year,
      dwelling_units_1 = dwelling_units,
      building_sqft_1 = building_sqft,
      land_sqft_1 = land_sqft,
      address_key_1 = address_key,
      x_3435_1 = x_3435,
      y_3435_1 = y_3435
    ),
  additions |>
    dplyr::transmute(
      project_id_2 = project_id,
      source_addresses_2 = source_addresses,
      component_pin_2 = component_pin,
      construction_year_2 = construction_year,
      dwelling_units_2 = dwelling_units,
      building_sqft_2 = building_sqft,
      land_sqft_2 = land_sqft,
      address_key_2 = address_key,
      x_3435_2 = x_3435,
      y_3435_2 = y_3435
    )
) |>
  dplyr::filter(project_id_1 < project_id_2) |>
  dplyr::mutate(
    distance_ft = sqrt(
      (x_3435_1 - x_3435_2)^2 +
        (y_3435_1 - y_3435_2)^2
    ),
    year_gap = abs(construction_year_1 - construction_year_2),
    same_address =
      !is.na(address_key_1) &
        address_key_1 == address_key_2,
    both_addresses_observed =
      !is.na(address_key_1) & !is.na(address_key_2),
    plausible_duplicate =
      component_pin_1 == component_pin_2 |
        (
          year_gap <= 4L &
            (
              same_address |
                (
                  distance_ft <= 5 &
                    !both_addresses_observed
                )
            )
        )
  )

if (any(
  addition_dedupe_screen$component_pin_already_retained |
    addition_dedupe_screen$plausible_retained_duplicate_count > 0L
) ||
    any(addition_pairs$plausible_duplicate)) {
  stop(
    paste0(
      "Accepted missing projects still contain a plausible duplicate. ",
      "Existing-ledger candidates: ",
      paste(
        addition_dedupe_screen$addition_project_id[
          addition_dedupe_screen$component_pin_already_retained |
            addition_dedupe_screen$
              plausible_retained_duplicate_count > 0L
        ],
        collapse = "/"
      ),
      ". Addition-pair candidates: ",
      paste(
        paste0(
          addition_pairs$project_id_1[
            addition_pairs$plausible_duplicate
          ],
          ":",
          addition_pairs$project_id_2[
            addition_pairs$plausible_duplicate
          ]
        ),
        collapse = "/"
      ),
      "."
    ),
    call. = FALSE
  )
}

final_audit_ledger <- dplyr::bind_rows(
  retained_ledger |>
    dplyr::mutate(
      ledger_action = "retain_existing",
      permit_chain_ids = NA_character_
    ),
  additions |>
    dplyr::transmute(
      project_id,
      source_family,
      source_project_ids = permit_chain_ids,
      source_addresses,
      component_pins = component_pin,
      project_kind,
      construction_year,
      dwelling_units,
      building_sqft,
      land_sqft,
      allow_far,
      allow_dupac,
      membership_source,
      year_source,
      units_source,
      building_source,
      land_source,
      evidence_ids = evidence,
      decision_reason,
      confidence = "high",
      decision_source = "final_permit_completion_audit",
      decision_action = "include_recovered_project",
      geometry_source,
      geometry_evidence,
      x_3435,
      y_3435,
      duplicate_disposition = NA_character_,
      ledger_action = "add_recovered_project",
      permit_chain_ids
    )
)
final_components <- final_audit_ledger |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::filter(!is.na(component_pins), component_pins != "")

if (anyDuplicated(final_audit_ledger$project_id) ||
    anyDuplicated(final_components$component_pins)) {
  stop("Final audit ledger still contains duplicate keys.", call. = FALSE)
}

chain_dispositions <- chain_evidence |>
  dplyr::left_join(
    footprint_transitions |>
      dplyr::select(
        permit_chain_id,
        footprint_final_decision = final_decision,
        footprint_decision_reason = final_decision_reason
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    footprint_assessor,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    multi_parcel,
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    historical_overrides |>
      dplyr::select(
        permit_chain_id,
        historical_override_decision = decision,
        physical_project_id,
        historical_override_reason = decision_reason
      ),
    by = "permit_chain_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    multi_parcel_footprint_candidate = dplyr::coalesce(
      multi_parcel_footprint_candidate,
      FALSE
    ),
    final_disposition = dplyr::case_when(
      footprint_final_decision == "include" ~
        "add_recovered_project",
      historical_override_decision == "include_project" ~
        "add_recovered_project",
      historical_override_decision == "include_phase" ~
        "collapse_into_recovered_project",
      footprint_final_decision ==
        "exclude_not_completed" ~
        "permit_not_completed",
      footprint_final_decision ==
        "exclude_density_fields_stale" ~
        "completed_but_density_fields_unusable",
      stringr::str_detect(
        successor_assessor_statuses,
        "already_represented"
      ) ~ "already_represented",
      historical_reconciliation_status ==
        "already_represented" ~ "already_represented",
      multi_parcel_footprint_candidate ~
        "completed_but_density_fields_unusable",
      stringr::str_detect(
        successor_assessor_statuses,
        "density_fields_incomplete"
      ) ~ "completed_but_density_fields_unusable",
      historical_reconciliation_status ==
        "unrepresented_assessor_completion_candidate" ~
        "completed_but_density_fields_unusable",
      historical_reconciliation_status ==
        "unrepresented_footprint_candidate" ~
        "completion_or_fields_not_sufficient",
      historical_reconciliation_status ==
        "not_a_completed_residential_building_candidate" ~
        "not_a_residential_building",
      historical_reconciliation_status ==
        "full_building_permit_without_completion_match" ~
        "no_independent_completion_evidence",
      historical_reconciliation_status ==
        "ambiguous_permit_without_completion_match" ~
        "ambiguous_without_completion_evidence",
      TRUE ~ "requires_final_review"
    ),
    final_disposition_reason = dplyr::coalesce(
      footprint_decision_reason,
      historical_override_reason,
      historical_reconciliation_status
    )
  )

if (any(chain_dispositions$final_disposition ==
        "requires_final_review")) {
  stop("Residual permit chains remain unresolved.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  tibble::tibble(
    section = "final_ledger",
    metric = c(
      "original_projects",
      "suppressed_duplicate_projects",
      "retained_original_projects",
      "recovered_projects_added",
      "final_projects",
      "duplicate_project_ids",
      "duplicate_component_pins"
    ),
    value = c(
      nrow(original_ledger),
      sum(
        duplicate_dispositions$duplicate_disposition ==
          "suppress_duplicate"
      ),
      nrow(retained_ledger),
      nrow(additions),
      nrow(final_audit_ledger),
      anyDuplicated(final_audit_ledger$project_id),
      anyDuplicated(final_components$component_pins)
    )
  ),
  chain_dispositions |>
    dplyr::count(final_disposition, name = "value") |>
    dplyr::transmute(
      section = "permit_chain_disposition",
      metric = final_disposition,
      value
    )
)

readr::write_csv(
  additions,
  "../output/final_recovered_missing_projects.csv"
)
readr::write_csv(
  addition_dedupe_screen,
  "../output/final_recovered_missing_project_dedupe_screen.csv"
)
readr::write_csv(
  addition_pairs,
  "../output/final_recovered_missing_project_pair_screen.csv"
)
readr::write_csv(
  chain_dispositions,
  "../output/final_residual_permit_chain_dispositions.csv"
)
readr::write_csv(
  final_audit_ledger,
  "../output/final_new_construction_audit_ledger.csv"
)
readr::write_csv(
  summary,
  "../output/final_new_construction_audit_summary.csv"
)
