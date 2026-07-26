# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

pairs_within_groups <- function(data, group, member, evidence) {
  data |>
    dplyr::select(
      group_value = dplyr::all_of(group),
      member_value = dplyr::all_of(member)
    ) |>
    dplyr::filter(
      !is.na(group_value),
      group_value != "",
      !is.na(member_value),
      member_value != ""
    ) |>
    dplyr::distinct() |>
    dplyr::group_split(group_value) |>
    purrr::map_dfr(function(group_rows) {
      members <- sort(unique(group_rows$member_value))
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
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    source_addresses = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)
components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
parcel_addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_character()
  )
) |>
  dplyr::transmute(
    component_pin = pin,
    current_address = prop_address_full |>
      stringr::str_to_upper() |>
      stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
      stringr::str_squish()
  ) |>
  dplyr::filter(
    !is.na(current_address),
    current_address != ""
  )

if (anyDuplicated(projects$project_id) ||
    anyDuplicated(components$component_pin) ||
    anyDuplicated(components[c("project_id", "component_pin")]) ||
    anyDuplicated(parcel_addresses$component_pin) ||
    !setequal(projects$project_id, components$project_id) ||
    any(!is.finite(projects$x_3435)) ||
    any(!is.finite(projects$y_3435))) {
  stop("Preferred project ledger keys or locations are invalid.", call. = FALSE)
}

project_current_addresses <- components |>
  dplyr::inner_join(
    parcel_addresses,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::distinct(project_id, current_address)
project_current_address_summary <- project_current_addresses |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    current_addresses = paste(
      sort(unique(current_address)),
      collapse = "/"
    ),
    current_address_count = dplyr::n_distinct(current_address),
    .groups = "drop"
  )
projects <- projects |>
  dplyr::left_join(
    project_current_address_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    current_address_count = dplyr::coalesce(
      current_address_count,
      0L
    )
  )

source_membership <- projects |>
  dplyr::select(final_project_id = project_id, source_project_ids) |>
  tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
  dplyr::rename(source_project_id = source_project_ids) |>
  dplyr::filter(
    !is.na(source_project_id),
    source_project_id != ""
  ) |>
  dplyr::distinct()

source_id_pairs <- pairs_within_groups(
  source_membership,
  "source_project_id",
  "final_project_id",
  "shared_source_project"
)

address_membership <- projects |>
  dplyr::select(final_project_id = project_id, source_addresses) |>
  tidyr::separate_longer_delim(source_addresses, delim = "/") |>
  dplyr::mutate(
    normalized_address = source_addresses |>
      stringr::str_to_upper() |>
      stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
      stringr::str_squish()
  ) |>
  dplyr::filter(
    !is.na(normalized_address),
    normalized_address != "",
    !normalized_address %in% c("0 UNKNOWN UNKNOWN", "UNKNOWN")
  ) |>
  dplyr::distinct(final_project_id, normalized_address)

address_pairs <- pairs_within_groups(
  address_membership,
  "normalized_address",
  "final_project_id",
  "shared_normalized_address"
)
current_address_pairs <- pairs_within_groups(
  project_current_addresses |>
    dplyr::rename(final_project_id = project_id),
  "current_address",
  "final_project_id",
  "shared_current_parcel_address"
)

coordinate_membership <- projects |>
  dplyr::mutate(
    coordinate_key = paste(
      round(x_3435, 2),
      round(y_3435, 2),
      sep = ":"
    )
  ) |>
  dplyr::select(final_project_id = project_id, coordinate_key)
exact_coordinate_pairs <- pairs_within_groups(
  coordinate_membership,
  "coordinate_key",
  "final_project_id",
  "same_centroid"
)

project_points <- sf::st_as_sf(
  projects |>
    dplyr::select(project_id, x_3435, y_3435),
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
nearby_rows <- sf::st_is_within_distance(
  project_points,
  project_points,
  dist = units::set_units(100, "ft")
)
nearby_pairs <- purrr::map2_dfr(
  seq_along(nearby_rows),
  nearby_rows,
  function(left_row, right_rows) {
    right_rows <- right_rows[right_rows > left_row]
    if (length(right_rows) == 0) {
      return(tibble::tibble())
    }
    tibble::tibble(
      project_id_1 = projects$project_id[left_row],
      project_id_2 = projects$project_id[right_rows]
    )
  }
) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id_1 = project_id,
        x_1 = x_3435,
        y_1 = y_3435,
        year_1 = construction_year,
        units_1 = dwelling_units,
        building_1 = building_sqft,
        land_1 = land_sqft
      ),
    by = "project_id_1",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id_2 = project_id,
        x_2 = x_3435,
        y_2 = y_3435,
        year_2 = construction_year,
        units_2 = dwelling_units,
        building_2 = building_sqft,
        land_2 = land_sqft
      ),
    by = "project_id_2",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    distance_ft = sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2),
    year_gap = abs(year_1 - year_2),
    equal_units = is.finite(units_1) & units_1 == units_2,
    building_ratio = pmin(building_1, building_2) /
      pmax(building_1, building_2),
    land_ratio = pmin(land_1, land_2) / pmax(land_1, land_2),
    similar_building = is.finite(building_ratio) & building_ratio >= 0.90,
    similar_land = is.finite(land_ratio) & land_ratio >= 0.90
  ) |>
  dplyr::filter(
    year_gap <= 3L,
    (equal_units & similar_building) |
      (similar_building & similar_land)
  ) |>
  dplyr::transmute(
    project_id_1,
    project_id_2,
    evidence_type = "nearby_similar_project"
  )

exact_pin_permit_links <- permit_links |>
  dplyr::filter(
    directly_matched,
    direct_match_method == "exact_pin"
  ) |>
  dplyr::select(source_project_id = project_id, permit_chain_id) |>
  dplyr::distinct()
source_membership_nested <- source_membership |>
  dplyr::group_by(source_project_id) |>
  dplyr::summarise(
    final_project_ids = list(sort(unique(final_project_id))),
    .groups = "drop"
  )
exact_pin_permit_links <- exact_pin_permit_links |>
  dplyr::inner_join(
    source_membership_nested,
    by = "source_project_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(
    final_project_ids,
    values_to = "final_project_id"
  ) |>
  dplyr::select(permit_chain_id, final_project_id) |>
  dplyr::distinct()

permit_pairs <- pairs_within_groups(
  exact_pin_permit_links,
  "permit_chain_id",
  "final_project_id",
  "shared_exact_pin_permit_chain"
)

pair_evidence <- dplyr::bind_rows(
  source_id_pairs,
  address_pairs,
  current_address_pairs,
  exact_coordinate_pairs,
  nearby_pairs,
  permit_pairs
) |>
  dplyr::distinct() |>
  dplyr::group_by(project_id_1, project_id_2) |>
  dplyr::summarise(
    evidence_types = paste(sort(unique(evidence_type)), collapse = "/"),
    evidence_count = dplyr::n_distinct(evidence_type),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::rename_with(
        ~ paste0(.x, "_1"),
        -project_id
      ) |>
      dplyr::rename(project_id_1 = project_id),
    by = "project_id_1",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::rename_with(
        ~ paste0(.x, "_2"),
        -project_id
      ) |>
      dplyr::rename(project_id_2 = project_id),
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
    documented_disaggregation =
      source_project_ids_1 == source_project_ids_2 &
      stringr::str_detect(
        paste(decision_reason_1, decision_reason_2),
        stringr::regex(
          paste0(
            "separately measured|disaggregat|separately permitted|",
            "one completed single-family building"
          ),
          ignore_case = TRUE
        )
      ),
    shared_current_address = stringr::str_detect(
      evidence_types,
      "shared_current_parcel_address"
    ),
    strong_shared_identity = stringr::str_detect(
      evidence_types,
      paste0(
        "shared_current_parcel_address|shared_normalized_address|",
        "shared_exact_pin_permit_chain|shared_source_project"
      )
    ),
    distinct_current_addresses =
      current_address_count_1 > 0L &
      current_address_count_2 > 0L &
      !shared_current_address,
    exact_duplicate_fields =
      construction_year_1 == construction_year_2 &
      dwelling_units_1 == dwelling_units_2 &
      building_sqft_1 == building_sqft_2 &
      land_sqft_1 == land_sqft_2,
    likely_lineage_repetition =
      distance_ft <= 0.02 &
      year_gap <= 3L &
      !documented_disaggregation &
      (
        (is.finite(building_ratio) & building_ratio >= 0.98) |
          (is.finite(land_ratio) & land_ratio >= 0.98)
      ),
    nearby_possible_lineage_repetition =
      distance_ft <= 100 &
      year_gap <= 3L &
      dwelling_units_1 == dwelling_units_2 &
      is.finite(building_ratio) &
      building_ratio >= 0.90 &
      !documented_disaggregation &
      !distinct_current_addresses,
    duplicate_review_priority = dplyr::case_when(
      documented_disaggregation ~
        "documented_multiple_buildings",
      exact_duplicate_fields &
        distance_ft <= 25 &
        strong_shared_identity ~
        "highest_exact_fields_and_site",
      strong_shared_identity ~
        "high_multiple_duplicate_signals",
      likely_lineage_repetition &
        !distinct_current_addresses ~
        "high_possible_predecessor_successor_repetition",
      nearby_possible_lineage_repetition ~
        "nearby_possible_predecessor_successor_repetition",
      stringr::str_detect(evidence_types, "same_centroid") &
        (
          dwelling_units_1 > 1 |
            dwelling_units_2 > 1 |
            !distinct_current_addresses
        ) ~
        "shared_site_requires_lineage_review",
      distinct_current_addresses ~
        "resolved_distinct_current_addresses",
      TRUE ~ "single_duplicate_signal_requires_review"
    )
  ) |>
  dplyr::arrange(
    factor(
      duplicate_review_priority,
      levels = c(
        "highest_exact_fields_and_site",
        "high_possible_predecessor_successor_repetition",
        "nearby_possible_predecessor_successor_repetition",
        "high_multiple_duplicate_signals",
        "shared_site_requires_lineage_review",
        "single_duplicate_signal_requires_review",
        "documented_multiple_buildings",
        "resolved_distinct_current_addresses"
      )
    ),
    distance_ft,
    project_id_1,
    project_id_2
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    section = "ledger",
    metric = c(
      "projects",
      "component_pins",
      "duplicate_project_ids",
      "duplicate_component_pins"
    ),
    value = c(
      nrow(projects),
      nrow(components),
      anyDuplicated(projects$project_id),
      anyDuplicated(components$component_pin)
    )
  ),
  pair_evidence |>
    dplyr::count(duplicate_review_priority, name = "value") |>
    dplyr::transmute(
      section = "candidate_pairs",
      metric = duplicate_review_priority,
      value
    ),
  tibble::tibble(
    section = "candidate_pairs",
    metric = "candidate_pairs_total",
    value = nrow(pair_evidence)
  )
)

readr::write_csv(
  pair_evidence,
  "../output/preferred_project_duplicate_candidate_pairs.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_duplicate_audit_summary.csv"
)
