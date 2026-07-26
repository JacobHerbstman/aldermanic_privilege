# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

library(sf)

normalize_address <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

pairs_within_groups <- function(data, group, member, evidence) {
  data |>
    dplyr::filter(
      !is.na(.data[[group]]),
      .data[[group]] != ""
    ) |>
    dplyr::distinct(
      group_value = .data[[group]],
      member_value = .data[[member]]
    ) |>
    dplyr::group_split(group_value) |>
    purrr::map_dfr(function(rows) {
      members <- sort(unique(rows$member_value))
      if (length(members) < 2L) {
        return(tibble::tibble())
      }
      combinations <- t(utils::combn(members, 2))
      tibble::tibble(
        project_id_1 = combinations[, 1],
        project_id_2 = combinations[, 2],
        evidence_type = evidence
      )
    })
}

projects <- readr::read_csv(
  "../output/multicard_adjudicated_density_model_input.csv",
  show_col_types = FALSE
)
geography <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::select(project_id, x_3435, y_3435)
ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    source_addresses = readr::col_character(),
    component_pins = readr::col_character(),
    permit_chain_ids = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    source_project_ids,
    source_addresses,
    component_pins,
    permit_chain_ids
  )
multicard_evidence <- readr::read_csv(
  "../output/multicard_final_review_bundle.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    review_address,
    permit_addresses,
    current_addresses
  )

projects <- projects |>
  dplyr::left_join(
    geography,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    ledger,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    multicard_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    review_addresses = dplyr::coalesce(
      review_address,
      permit_addresses,
      current_addresses
    )
  )

project_components <- projects |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(component_pin = component_pins) |>
  dplyr::filter(
    !is.na(component_pin),
    component_pin != ""
  ) |>
  dplyr::distinct()
if (anyDuplicated(project_components$component_pin)) {
  stop("A component PIN belongs to more than one retained project.", call. = FALSE)
}

parcel_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::transmute(
    component_pin = pin,
    current_address = normalize_address(prop_address_full)
  ) |>
  dplyr::filter(
    !is.na(current_address),
    current_address != ""
  ) |>
  dplyr::distinct()
if (anyDuplicated(parcel_addresses$component_pin)) {
  stop("Current parcel addresses are not unique by PIN.", call. = FALSE)
}

project_address_summary <- project_components |>
  dplyr::inner_join(
    parcel_addresses,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    parcel_addresses = paste(
      sort(unique(current_address)),
      collapse = "/"
    ),
    .groups = "drop"
  )

new_construction_permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    !is.na(pin),
    pin != ""
  ) |>
  dplyr::transmute(
    pin10 = as.character(pin),
    permit_id = as.character(id),
    permit_address = normalize_address(
      paste(street_number, street_direction, street_name)
    ),
    application_start_date = as.character(application_start_date),
    issue_date = as.character(issue_date)
  ) |>
  dplyr::distinct()

permit_pin10_summary <- new_construction_permits |>
  dplyr::group_by(pin10) |>
  dplyr::summarise(
    exact_pin_new_construction_permits = paste(
      sort(unique(permit_id)),
      collapse = "/"
    ),
    exact_pin_permit_addresses = paste(
      sort(unique(permit_address)),
      collapse = "/"
    ),
    exact_pin_application_dates = paste(
      sort(unique(application_start_date)),
      collapse = "/"
    ),
    exact_pin_issue_dates = paste(
      sort(unique(issue_date)),
      collapse = "/"
    ),
    .groups = "drop"
  )

project_permit_summary <- project_components |>
  dplyr::mutate(pin10 = substr(component_pin, 1, 10)) |>
  dplyr::distinct(project_id, pin10) |>
  dplyr::inner_join(
    permit_pin10_summary,
    by = "pin10",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    exact_pin_new_construction_permits = paste(
      sort(unique(exact_pin_new_construction_permits)),
      collapse = "/"
    ),
    exact_pin_permit_addresses = paste(
      sort(unique(exact_pin_permit_addresses)),
      collapse = "/"
    ),
    exact_pin_application_dates = paste(
      sort(unique(exact_pin_application_dates)),
      collapse = "/"
    ),
    exact_pin_issue_dates = paste(
      sort(unique(exact_pin_issue_dates)),
      collapse = "/"
    ),
    .groups = "drop"
  )

projects <- projects |>
  dplyr::left_join(
    project_address_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_permit_summary,
    by = "project_id",
    relationship = "one-to-one"
  )

if (
  anyDuplicated(projects$project_id) ||
    any(!is.finite(projects$x_3435)) ||
    any(!is.finite(projects$y_3435)) ||
    any(is.na(projects$source_project_ids))
) {
  stop("The final multicard duplicate-audit inputs are invalid.", call. = FALSE)
}

membership_pairs <- dplyr::bind_rows(
  projects |>
    dplyr::select(project_id, source_project_ids) |>
    tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
    pairs_within_groups(
      "source_project_ids",
      "project_id",
      "shared_source_project"
    ),
  projects |>
    dplyr::select(project_id, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    pairs_within_groups(
      "component_pins",
      "project_id",
      "shared_component_pin"
    ),
  projects |>
    dplyr::select(project_id, permit_chain_ids) |>
    tidyr::separate_longer_delim(permit_chain_ids, delim = "/") |>
    pairs_within_groups(
      "permit_chain_ids",
      "project_id",
      "shared_permit_chain"
    ),
  projects |>
    dplyr::select(project_id, source_addresses) |>
    tidyr::separate_longer_delim(source_addresses, delim = "/") |>
    dplyr::mutate(
      normalized_address = normalize_address(source_addresses)
    ) |>
    dplyr::filter(
      !normalized_address %in% c("", "UNKNOWN", "0 UNKNOWN UNKNOWN")
    ) |>
    pairs_within_groups(
      "normalized_address",
      "project_id",
      "shared_source_address"
    ),
  projects |>
    dplyr::mutate(
      coordinate_key = paste(
        round(x_3435, 2),
        round(y_3435, 2),
        sep = ":"
      )
    ) |>
    pairs_within_groups(
      "coordinate_key",
      "project_id",
      "same_centroid"
    )
)

multicard_projects <- projects |>
  dplyr::filter(project_kind == "same_pin_multiple_cards")

nearby_pairs <- dplyr::cross_join(
  multicard_projects |>
    dplyr::select(
      project_id_1 = project_id,
      year_1 = construction_year,
      units_1 = dwelling_units,
      building_1 = building_sqft,
      land_1 = land_sqft,
      x_1 = x_3435,
      y_1 = y_3435
    ),
  projects |>
    dplyr::select(
      project_id_2 = project_id,
      year_2 = construction_year,
      units_2 = dwelling_units,
      building_2 = building_sqft,
      land_2 = land_sqft,
      x_2 = x_3435,
      y_2 = y_3435
    )
) |>
  dplyr::filter(project_id_1 != project_id_2) |>
  dplyr::mutate(
    distance_ft = sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2),
    year_gap = abs(year_1 - year_2),
    building_ratio = pmin(building_1, building_2) /
      pmax(building_1, building_2),
    land_ratio = pmin(land_1, land_2) / pmax(land_1, land_2)
  ) |>
  dplyr::filter(
    distance_ft <= 200,
    year_gap <= 4,
    (
      units_1 == units_2 &
        is.finite(building_ratio) &
        building_ratio >= 0.90
    ) |
      (
        is.finite(building_ratio) &
          building_ratio >= 0.90 &
          is.finite(land_ratio) &
          land_ratio >= 0.90
      )
  ) |>
  dplyr::mutate(
    pair_id_1 = pmin(project_id_1, project_id_2),
    pair_id_2 = pmax(project_id_1, project_id_2)
  ) |>
  dplyr::transmute(
    project_id_1 = pair_id_1,
    project_id_2 = pair_id_2,
    evidence_type = "nearby_similar_project"
  )

candidate_keys <- dplyr::bind_rows(
  membership_pairs,
  nearby_pairs
) |>
  dplyr::mutate(
    pair_id_1 = pmin(project_id_1, project_id_2),
    pair_id_2 = pmax(project_id_1, project_id_2)
  ) |>
  dplyr::transmute(
    project_id_1 = pair_id_1,
    project_id_2 = pair_id_2,
    evidence_type
  ) |>
  dplyr::filter(
    project_id_1 %in% multicard_projects$project_id |
      project_id_2 %in% multicard_projects$project_id
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(project_id_1, project_id_2) |>
  dplyr::summarise(
    evidence_types = paste(sort(unique(evidence_type)), collapse = "/"),
    .groups = "drop"
  )

pair_details <- candidate_keys |>
  dplyr::left_join(
    projects |>
      dplyr::rename_with(~ paste0(.x, "_1")),
    by = "project_id_1",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::rename_with(~ paste0(.x, "_2")),
    by = "project_id_2",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    distance_ft = sqrt(
      (x_3435_1 - x_3435_2)^2 +
        (y_3435_1 - y_3435_2)^2
    ),
    year_gap = abs(construction_year_1 - construction_year_2),
    building_ratio = pmin(building_sqft_1, building_sqft_2) /
      pmax(building_sqft_1, building_sqft_2),
    land_ratio = pmin(land_sqft_1, land_sqft_2) /
      pmax(land_sqft_1, land_sqft_2),
    pair_scope = dplyr::if_else(
      project_kind_1 == "same_pin_multiple_cards" &
        project_kind_2 == "same_pin_multiple_cards",
      "multicard_vs_multicard",
      "multicard_vs_other"
    )
  ) |>
  dplyr::select(
    project_id_1,
    project_id_2,
    pair_scope,
    evidence_types,
    source_addresses_1,
    source_addresses_2,
    review_addresses_1,
    review_addresses_2,
    parcel_addresses_1,
    parcel_addresses_2,
    exact_pin_new_construction_permits_1,
    exact_pin_new_construction_permits_2,
    exact_pin_permit_addresses_1,
    exact_pin_permit_addresses_2,
    exact_pin_application_dates_1,
    exact_pin_application_dates_2,
    exact_pin_issue_dates_1,
    exact_pin_issue_dates_2,
    component_pins_1,
    component_pins_2,
    permit_chain_ids_1,
    permit_chain_ids_2,
    project_kind_1,
    project_kind_2,
    construction_year_1,
    construction_year_2,
    dwelling_units_1,
    dwelling_units_2,
    building_sqft_1,
    building_sqft_2,
    land_sqft_1,
    land_sqft_2,
    distance_ft,
    year_gap,
    building_ratio,
    land_ratio
  ) |>
  dplyr::arrange(pair_scope, distance_ft, project_id_1, project_id_2)

multicard_decisions <- readr::read_csv(
  "../adjudication/multicard_parent_pair_decisions.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    pair_id_1 = pmin(project_id_1, project_id_2),
    pair_id_2 = pmax(project_id_1, project_id_2)
  ) |>
  dplyr::transmute(
    project_id_1 = pair_id_1,
    project_id_2 = pair_id_2,
    disposition,
    evidence
  ) |>
  dplyr::semi_join(
    pair_details,
    by = c("project_id_1", "project_id_2")
  )
cross_decisions <- readr::read_csv(
  "../adjudication/multicard_cross_pair_decisions.csv",
  show_col_types = FALSE
)
cross_decision_keys <- paste(
  cross_decisions$project_id_1,
  cross_decisions$project_id_2,
  sep = "::"
)

decisions <- dplyr::bind_rows(
  multicard_decisions,
  cross_decisions
)
if (anyDuplicated(decisions[c("project_id_1", "project_id_2")])) {
  stop("Multicard duplicate decisions contain duplicate pair keys.", call. = FALSE)
}

pair_details <- pair_details |>
  dplyr::left_join(
    decisions,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    deterministic_distinct_single_family =
      pair_scope == "multicard_vs_other" &
        dwelling_units_1 == 1 &
        dwelling_units_2 == 1 &
        evidence_types == "nearby_similar_project" &
        !is.na(parcel_addresses_1) &
        !is.na(parcel_addresses_2) &
        parcel_addresses_1 != parcel_addresses_2 &
        component_pins_1 != component_pins_2,
    disposition = dplyr::coalesce(
      disposition,
      dplyr::if_else(
        deterministic_distinct_single_family,
        "retain_distinct_projects",
        NA_character_
      )
    ),
    evidence = dplyr::coalesce(
      evidence,
      dplyr::if_else(
        deterministic_distinct_single_family,
        paste0(
          "Separate one-unit projects at ",
          parcel_addresses_1,
          " and ",
          parcel_addresses_2,
          " have distinct retained PINs, current addresses, and centroids; ",
          "the final ledger records no shared source project, component PIN, ",
          "source address, permit chain, or centroid."
        ),
        NA_character_
      )
    ),
    decision_source = dplyr::case_when(
      paste(project_id_1, project_id_2, sep = "::") %in%
        cross_decision_keys ~
        "manual_cross_project_review",
      pair_scope == "multicard_vs_multicard" ~
        "manual_multicard_parent_review",
      deterministic_distinct_single_family ~
        "deterministic_distinct_current_parcels",
      TRUE ~ NA_character_
    )
  )

unresolved <- pair_details |>
  dplyr::filter(is.na(disposition) | disposition == "")
unused_decisions <- cross_decisions |>
  dplyr::anti_join(
    pair_details,
    by = c("project_id_1", "project_id_2")
  )

readr::write_csv(
  pair_details,
  "../output/multicard_retained_duplicate_candidates.csv",
  na = ""
)
readr::write_csv(
  unresolved,
  "../output/multicard_retained_duplicate_unresolved.csv",
  na = ""
)

retained_dispositions <- c(
  "retain_both",
  "retain_both_with_overrides",
  "retain_distinct_projects"
)
if (
  nrow(unresolved) > 0L ||
    nrow(unused_decisions) > 0L ||
    any(!pair_details$disposition %in% retained_dispositions)
) {
  print(unresolved)
  print(unused_decisions)
  stop("The retained multicard duplicate review is incomplete.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "retained_projects",
    "retained_multicard_projects",
    "candidate_pairs",
    "multicard_vs_multicard_pairs",
    "multicard_vs_other_pairs",
    "deterministic_single_family_resolutions",
    "manual_cross_project_resolutions",
    "resolved_pairs",
    "retained_pair_dispositions",
    "unresolved_pairs",
    "unused_pair_decisions"
  ),
  value = c(
    nrow(projects),
    nrow(multicard_projects),
    nrow(pair_details),
    sum(pair_details$pair_scope == "multicard_vs_multicard"),
    sum(pair_details$pair_scope == "multicard_vs_other"),
    sum(
      pair_details$decision_source ==
        "deterministic_distinct_current_parcels"
    ),
    sum(
      pair_details$decision_source ==
        "manual_cross_project_review"
    ),
    sum(!is.na(pair_details$disposition)),
    sum(pair_details$disposition %in% retained_dispositions),
    nrow(unresolved),
    nrow(unused_decisions)
  )
)

readr::write_csv(
  summary,
  "../output/multicard_retained_duplicate_validation.csv",
  na = ""
)
