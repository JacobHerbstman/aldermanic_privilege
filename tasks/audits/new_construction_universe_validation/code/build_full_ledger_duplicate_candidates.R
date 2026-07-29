# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    exact_permit_numbers = readr::col_character(),
    exact_permit_addresses = readr::col_character(),
    source_addresses = readr::col_character(),
    current_property_addresses = readr::col_character(),
    addresses = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    x_3435 = as.numeric(x_3435),
    y_3435 = as.numeric(y_3435)
  )

analysis_project_ids <- readr::read_csv(
  "../input/multicard_external_reviewed_model_input.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_skip()
  ),
  show_col_types = FALSE
) |>
  dplyr::pull(project_id)

projects <- projects |>
  dplyr::filter(project_id %in% analysis_project_ids)

if (nrow(projects) != length(unique(analysis_project_ids))) {
  stop("The reviewed analysis file does not map one-to-one to evidence.", call. = FALSE)
}

component_crosswalk <- projects |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(component_pin = component_pins) |>
  dplyr::distinct()

project_aliases <- dplyr::bind_rows(
  projects |>
    dplyr::transmute(project_id, alias = project_id),
  projects |>
    dplyr::select(project_id, source_project_ids) |>
    tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
    dplyr::transmute(project_id, alias = source_project_ids),
  projects |>
    dplyr::select(project_id, source_family, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0(source_family, "_", component_pins)
    ),
  projects |>
    dplyr::filter(source_family == "residential") |>
    dplyr::select(project_id, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0("residential_multicard_", component_pins)
    )
) |>
  dplyr::filter(!is.na(alias), alias != "") |>
  dplyr::distinct()

unique_project_aliases <- project_aliases |>
  dplyr::add_count(alias, name = "alias_project_count") |>
  dplyr::filter(alias_project_count == 1L) |>
  dplyr::select(-alias_project_count)

exact_permit_links <- readr::read_csv(
  "../input/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(source_project_id = project_id) |>
  dplyr::inner_join(
    component_crosswalk,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::transmute(
    project_id,
    permit_number = as.character(permit_number),
    permit_match = "exact_component_pin"
  ) |>
  dplyr::distinct()

spatial_permit_links_raw <- readr::read_csv(
  "../input/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    exact_pin_match |
      spatial_match_method %in% c(
        "inside_project_polygon",
        "exact_pin_and_project_polygon"
      ) |
      polygon_distance_ft <= 5
  ) |>
  dplyr::rename(source_project_id = project_id)

spatial_permit_links <- dplyr::bind_rows(
  spatial_permit_links_raw |>
    dplyr::filter(source_project_id %in% projects$project_id) |>
    dplyr::rename(project_id = source_project_id),
  spatial_permit_links_raw |>
    dplyr::filter(!source_project_id %in% projects$project_id) |>
    dplyr::inner_join(
      unique_project_aliases,
      by = c("source_project_id" = "alias"),
      relationship = "many-to-one"
    )
) |>
  dplyr::transmute(
    project_id,
    permit_number = as.character(permit_number),
    permit_match = dplyr::case_when(
      exact_pin_match ~ "spatial_file_exact_pin",
      spatial_match_method == "exact_pin_and_project_polygon" ~
        "exact_pin_and_project_polygon",
      spatial_match_method == "inside_project_polygon" ~
        "inside_project_polygon",
      TRUE ~ "within_5ft_of_project_polygon"
    )
  ) |>
  dplyr::distinct()

if (
  anyDuplicated(projects$project_id) ||
    anyDuplicated(component_crosswalk$component_pin) ||
    any(!is.finite(projects$x_3435)) ||
    any(!is.finite(projects$y_3435))
) {
  stop("The full-ledger duplicate input failed validation.", call. = FALSE)
}

project_points <- sf::st_as_sf(
  projects,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

nearby <- sf::st_is_within_distance(
  project_points,
  project_points,
  dist = units::set_units(500, "ft")
)

pairs <- purrr::imap_dfr(
  nearby,
  function(matches, row_id) {
    matches <- matches[matches > row_id]
    if (length(matches) == 0) {
      return(NULL)
    }
    tibble::tibble(row_id_1 = row_id, row_id_2 = matches)
  }
)

pair_data <- pairs |>
  dplyr::mutate(
    project_id_1 = projects$project_id[row_id_1],
    project_id_2 = projects$project_id[row_id_2],
    distance_ft = as.numeric(
      sf::st_distance(
        project_points[row_id_1, ],
        project_points[row_id_2, ],
        by_element = TRUE
      )
    )
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
    year_gap = abs(construction_year_1 - construction_year_2),
    same_units = dwelling_units_1 == dwelling_units_2,
    same_building_sqft = building_sqft_1 == building_sqft_2,
    same_land_sqft = land_sqft_1 == land_sqft_2,
    same_physical_record =
      same_units & same_building_sqft & same_land_sqft,
    near_identical_physical_record =
      year_gap <= 2 &
      dplyr::between(
        pmin(building_sqft_1, building_sqft_2) /
          pmax(building_sqft_1, building_sqft_2),
        0.98,
        1
      ) &
      dplyr::between(
        pmin(land_sqft_1, land_sqft_2) /
          pmax(land_sqft_1, land_sqft_2),
        0.98,
        1
      ),
    co_located = distance_ft <= 5,
    normalized_address_1 = stringr::str_squish(
      stringr::str_to_upper(
        dplyr::coalesce(
          current_property_addresses_1,
          addresses_1,
          ""
        )
      )
    ),
    normalized_address_2 = stringr::str_squish(
      stringr::str_to_upper(
        dplyr::coalesce(
          current_property_addresses_2,
          addresses_2,
          ""
        )
      )
    ),
    same_nonempty_address =
      normalized_address_1 != "" &
      normalized_address_1 == normalized_address_2,
    different_supported_addresses =
      (
        normalized_address_1 != "" &
          normalized_address_2 != "" &
          normalized_address_1 != normalized_address_2
      ) |
      (
        dplyr::coalesce(exact_permit_addresses_1, "") != "" &
          dplyr::coalesce(exact_permit_addresses_2, "") != "" &
          exact_permit_addresses_1 != exact_permit_addresses_2
      ) |
      (
        dplyr::coalesce(source_addresses_1, "") != "" &
          dplyr::coalesce(source_addresses_2, "") != "" &
          source_addresses_1 != source_addresses_2
      ),
    distinct_exact_permit_evidence =
      dplyr::coalesce(exact_permit_numbers_1, "") != "" &
      dplyr::coalesce(exact_permit_numbers_2, "") != "" &
      exact_permit_numbers_1 != exact_permit_numbers_2,
    physical_duplicate_candidate =
      same_nonempty_address &
      year_gap <= 2 &
      (
        same_physical_record |
        near_identical_physical_record |
        co_located
      )
  )

permit_links <- dplyr::bind_rows(
  exact_permit_links,
  spatial_permit_links
) |>
  dplyr::distinct()

prior_pair_review <- readr::read_csv(
  "../input/preferred_project_duplicate_pair_dispositions.csv",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    raw_project_id_1 = project_id_1,
    raw_project_id_2 = project_id_2,
    project_id_1 = pmin(raw_project_id_1, raw_project_id_2),
    project_id_2 = pmax(raw_project_id_1, raw_project_id_2),
    prior_pair_disposition = pair_disposition,
    prior_temporal_lineage_status = temporal_lineage_status,
    prior_distinct_supported_addresses = distinct_supported_addresses,
    prior_shared_direct_permit_addresses =
      shared_direct_permit_address_count,
    prior_duplicate_review_group_id = duplicate_review_group_id
  ) |>
  dplyr::distinct(project_id_1, project_id_2, .keep_all = TRUE)

shared_permit_groups <- permit_links |>
  dplyr::group_by(permit_number) |>
  dplyr::summarise(
    project_ids = list(sort(unique(project_id))),
    .groups = "drop"
  ) |>
  dplyr::filter(lengths(project_ids) > 1)

shared_permits <- purrr::pmap_dfr(
  shared_permit_groups,
  function(permit_number, project_ids) {
    pairs <- utils::combn(project_ids, 2)
    tibble::tibble(
      permit_number = permit_number,
      project_id_1 = pairs[1, ],
      project_id_2 = pairs[2, ],
      permit_match_1 = permit_links$permit_match[
        match(
          paste(permit_number, pairs[1, ]),
          paste(permit_links$permit_number, permit_links$project_id)
        )
      ],
      permit_match_2 = permit_links$permit_match[
        match(
          paste(permit_number, pairs[2, ]),
          paste(permit_links$permit_number, permit_links$project_id)
        )
      ]
    )
  }
) |>
  dplyr::group_by(project_id_1, project_id_2) |>
  dplyr::summarise(
    shared_permit_numbers = paste(
      sort(unique(permit_number)),
      collapse = " | "
    ),
    shared_permit_matches = paste(
      sort(unique(paste(permit_match_1, permit_match_2, sep = " + "))),
      collapse = " | "
    ),
    .groups = "drop"
  )

candidates <- pair_data |>
  dplyr::left_join(
    shared_permits,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    prior_pair_review,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    shared_permit_candidate =
      !is.na(shared_permit_numbers) &
      year_gap <= 3 &
      stringr::str_detect(
        shared_permit_matches,
        paste0(
          "exact_component_pin \\+ exact_component_pin|",
          "spatial_file_exact_pin \\+ spatial_file_exact_pin|",
          "exact_pin_and_project_polygon \\+ ",
          "exact_pin_and_project_polygon"
        )
      ),
    candidate_reason = paste0(
      dplyr::if_else(co_located, "co_located;", ""),
      dplyr::if_else(
        same_physical_record,
        "same_physical_record;",
        ""
      ),
      dplyr::if_else(
        near_identical_physical_record,
        "near_identical_physical_record;",
        ""
      ),
      dplyr::if_else(
        shared_permit_candidate,
        "shared_permit;",
        ""
      )
    ),
    duplicate_review_priority = dplyr::case_when(
      within_500ft_1 &
        within_500ft_2 &
        distance_ft <= 50 &
        (dwelling_units_1 > 1 | dwelling_units_2 > 1) &
        shared_permit_candidate &
        (
          land_sqft_1 == land_sqft_2 |
          pmin(building_sqft_1, building_sqft_2) /
            pmax(building_sqft_1, building_sqft_2) >= 0.9
        ) ~ 1L,
      within_500ft_1 &
        within_500ft_2 &
        (dwelling_units_1 > 1 | dwelling_units_2 > 1) &
        shared_permit_candidate ~ 2L,
      (within_500ft_1 | within_500ft_2) &
        shared_permit_candidate ~ 3L,
      physical_duplicate_candidate ~ 4L,
      TRUE ~ 5L
    ),
    duplicate_rule_status = dplyr::case_when(
      distinct_exact_permit_evidence &
        different_supported_addresses ~
        "retain_distinct_exact_permits_and_addresses",
      prior_temporal_lineage_status %in% c(
        "contemporaneous_distinct_address_parcels",
        "contemporaneous_distinct_assessor_parcels",
        "contemporaneous_class_295_rowhouse_parcels"
      ) ~
        "retain_distinct_contemporaneous_parcels",
      different_supported_addresses &
        !co_located ~
        "retain_distinct_supported_addresses",
      stringr::str_detect(
        dplyr::coalesce(prior_pair_disposition, ""),
        "^retain"
      ) ~ "retain_prior_pair_review_only",
      physical_duplicate_candidate &
        distance_ft <= 25 &
        year_gap <= 3 &
        project_kind_1 != project_kind_2 ~
        "review_possible_parent_successor_overlap",
      shared_permit_candidate ~ "review_shared_permit_without_separation",
      physical_duplicate_candidate ~ "review_physical_duplicate_candidate",
      TRUE ~ "retain_no_strong_duplicate_evidence"
    )
  ) |>
  dplyr::filter(
    within_1500ft_1 | within_1500ft_2,
    physical_duplicate_candidate | shared_permit_candidate
  ) |>
  dplyr::select(
    project_id_1,
    project_id_2,
    source_family_1,
    source_family_2,
    project_kind_1,
    project_kind_2,
    class_values_1,
    class_values_2,
    component_pins_1,
    component_pins_2,
    construction_year_1,
    construction_year_2,
    dwelling_units_1,
    dwelling_units_2,
    building_sqft_1,
    building_sqft_2,
    land_sqft_1,
    land_sqft_2,
    current_property_addresses_1,
    current_property_addresses_2,
    addresses_1,
    addresses_2,
    external_structure_class_1,
    external_structure_class_2,
    multifamily_disposition_1,
    multifamily_disposition_2,
    reviewer_notes_1,
    reviewer_notes_2,
    within_500ft_1,
    within_500ft_2,
    ward_pair_1,
    ward_pair_2,
    distance_ft,
    year_gap,
    same_physical_record,
    near_identical_physical_record,
    co_located,
    same_nonempty_address,
    different_supported_addresses,
    distinct_exact_permit_evidence,
    shared_permit_numbers,
    shared_permit_matches,
    prior_pair_disposition,
    prior_temporal_lineage_status,
    prior_distinct_supported_addresses,
    prior_shared_direct_permit_addresses,
    prior_duplicate_review_group_id,
    duplicate_rule_status,
    duplicate_review_priority,
    candidate_reason
  ) |>
  dplyr::arrange(
    duplicate_review_priority,
    distance_ft,
    project_id_1,
    project_id_2
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    metric = c(
      "candidate_pairs",
      "shared_permit_pairs",
      "co_located_pairs",
      "same_physical_record_pairs",
      "near_identical_physical_record_pairs",
      "candidate_pairs_touching_500ft_sample"
    ),
    value = c(
      nrow(candidates),
      sum(!is.na(candidates$shared_permit_numbers)),
      sum(candidates$co_located),
      sum(candidates$same_physical_record),
      sum(candidates$near_identical_physical_record),
      sum(candidates$within_500ft_1 | candidates$within_500ft_2)
    )
  ),
  candidates |>
    dplyr::count(
      metric = "candidate_reason",
      value = candidate_reason,
      name = "projects"
    ) |>
    dplyr::transmute(
      metric = paste0(metric, ":", value),
      value = projects
    )
)

readr::write_csv(
  candidates,
  "../output/full_ledger_duplicate_candidates.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/full_ledger_duplicate_summary.csv",
  na = ""
)
readr::write_csv(
  candidates |>
    dplyr::filter(duplicate_review_priority == 1L),
  "../output/high_priority_duplicate_candidates.csv",
  na = ""
)
readr::write_csv(
  candidates |>
    dplyr::filter(stringr::str_starts(duplicate_rule_status, "review_")),
  "../output/unresolved_duplicate_candidates.csv",
  na = ""
)
