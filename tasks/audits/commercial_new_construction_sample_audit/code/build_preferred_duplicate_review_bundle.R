# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

pairs <- readr::read_csv(
  "../output/preferred_project_duplicate_candidate_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    .default = readr::col_guess()
  )
)
projects <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
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
boundary_scope <- readr::read_csv(
  "../output/preferred_new_construction_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id,
    ward,
    neighbor_ward,
    ward_pair,
    distance_to_boundary_ft,
    within_1500ft,
    within_500ft
  )
history <- readr::read_csv(
  "../output/residential_project_history_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    .default = readr::col_guess()
  )
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
    component_pin = pin,
    current_property_address = prop_address_full
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
tieback_lineages <- readr::read_csv(
  "../output/residential_tieback_temporal_lineage_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_lineage_id = readr::col_character(),
    all_lineage_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(tieback_lineage_id, all_lineage_pins) |>
  tidyr::separate_longer_delim(all_lineage_pins, delim = "/") |>
  dplyr::rename(component_pin = all_lineage_pins) |>
  dplyr::filter(
    !is.na(component_pin),
    component_pin != ""
  ) |>
  dplyr::distinct()

if (anyDuplicated(projects$project_id) ||
    anyDuplicated(components$component_pin) ||
    anyDuplicated(boundary_scope$project_id) ||
    anyDuplicated(addresses$component_pin) ||
    anyDuplicated(history$pin) ||
    anyDuplicated(tieback_lineages$component_pin)) {
  stop("Duplicate-review source keys are invalid.", call. = FALSE)
}

review_pairs <- pairs |>
  dplyr::filter(
    !duplicate_review_priority %in% c(
      "documented_multiple_buildings",
      "resolved_distinct_current_addresses",
      "single_duplicate_signal_requires_review"
    )
  )
review_graph <- igraph::graph_from_data_frame(
  review_pairs |>
    dplyr::select(project_id_1, project_id_2),
  directed = FALSE
)
membership <- igraph::components(review_graph)$membership
group_membership <- tibble::tibble(
  project_id = names(membership),
  component_number = as.integer(membership)
) |>
  dplyr::group_by(component_number) |>
  dplyr::mutate(
    first_project_id = min(project_id)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(first_project_id, project_id) |>
  dplyr::mutate(
    duplicate_review_group_id = sprintf(
      "duplicate_review_group_%03d",
      dplyr::dense_rank(first_project_id)
    )
  ) |>
  dplyr::select(duplicate_review_group_id, project_id)

review_pairs <- review_pairs |>
  dplyr::left_join(
    group_membership |>
      dplyr::rename(project_id_1 = project_id),
    by = "project_id_1",
    relationship = "many-to-one"
  )
if (any(is.na(review_pairs$duplicate_review_group_id))) {
  stop("A duplicate candidate pair lacks a review group.", call. = FALSE)
}

project_components <- components |>
  dplyr::left_join(
    history,
    by = c("component_pin" = "pin"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    addresses,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    tieback_lineages,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::inner_join(
    group_membership,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::arrange(
    duplicate_review_group_id,
    project_id,
    component_pin
  )

review_members <- group_membership |>
  dplyr::inner_join(
    projects,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    boundary_scope,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_components |>
      dplyr::group_by(
        duplicate_review_group_id,
        project_id
      ) |>
      dplyr::summarise(
        current_property_addresses = paste(
          sort(unique(current_property_address[
            !is.na(current_property_address)
          ])),
          collapse = "/"
        ),
        tieback_lineage_ids = paste(
          sort(unique(tieback_lineage_id[
            !is.na(tieback_lineage_id)
          ])),
          collapse = "/"
        ),
        history_year_values = paste(
          sort(unique(source_years[
            !is.na(source_years)
          ])),
          collapse = "/"
        ),
        history_building_area_values = paste(
          sort(unique(source_building_areas[
            !is.na(source_building_areas)
          ])),
          collapse = "/"
        ),
        history_land_area_values = paste(
          sort(unique(source_land_areas[
            !is.na(source_land_areas)
          ])),
          collapse = "/"
        ),
        history_unit_values = paste(
          sort(unique(source_unit_counts[
            !is.na(source_unit_counts)
          ])),
          collapse = "/"
        ),
        .groups = "drop"
      ) |>
      dplyr::select(-duplicate_review_group_id),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(duplicate_review_group_id, project_id)

source_membership <- review_members |>
  dplyr::select(final_project_id = project_id, source_project_ids) |>
  tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
  dplyr::rename(source_project_id = source_project_ids) |>
  dplyr::filter(
    !is.na(source_project_id),
    source_project_id != ""
  ) |>
  dplyr::distinct()
source_membership_nested <- source_membership |>
  dplyr::group_by(source_project_id) |>
  dplyr::summarise(
    final_project_ids = list(sort(unique(final_project_id))),
    .groups = "drop"
  )

review_permits <- permit_links |>
  dplyr::filter(directly_matched) |>
  dplyr::rename(source_project_id = project_id) |>
  dplyr::inner_join(
    source_membership_nested,
    by = "source_project_id",
    relationship = "many-to-one"
  ) |>
  tidyr::unnest_longer(
    final_project_ids,
    values_to = "project_id"
  ) |>
  dplyr::inner_join(
    group_membership,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::distinct(
    duplicate_review_group_id,
    project_id,
    permit_chain_id,
    permit_id,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    duplicate_review_group_id,
    project_id,
    application_date,
    permit_chain_id,
    permit_id
  )

group_summary <- review_members |>
  dplyr::group_by(duplicate_review_group_id) |>
  dplyr::summarise(
    projects = dplyr::n(),
    source_families = paste(
      sort(unique(source_family)),
      collapse = "/"
    ),
    construction_year_values = paste(
      sort(unique(construction_year)),
      collapse = "/"
    ),
    current_address_values = paste(
      sort(unique(current_property_addresses[
        !is.na(current_property_addresses) &
          current_property_addresses != ""
      ])),
      collapse = " | "
    ),
    tieback_lineage_values = paste(
      sort(unique(tieback_lineage_ids[
        !is.na(tieback_lineage_ids) &
          tieback_lineage_ids != ""
      ])),
      collapse = "/"
    ),
    projects_with_current_address = sum(
      !is.na(current_property_addresses) &
        current_property_addresses != ""
    ),
    projects_within_1500ft = sum(within_1500ft),
    projects_within_500ft = sum(within_500ft),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    review_pairs |>
      dplyr::group_by(duplicate_review_group_id) |>
      dplyr::summarise(
        pair_priorities = paste(
          sort(unique(duplicate_review_priority)),
          collapse = "/"
        ),
        pair_evidence_types = paste(
          sort(unique(evidence_types)),
          collapse = " | "
        ),
        candidate_pairs = dplyr::n(),
        .groups = "drop"
      ),
    by = "duplicate_review_group_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    review_permits |>
      dplyr::group_by(duplicate_review_group_id) |>
      dplyr::summarise(
        directly_linked_permit_chains =
          dplyr::n_distinct(permit_chain_id),
        directly_linked_permit_addresses = paste(
          sort(unique(permit_address[
            !is.na(permit_address)
          ])),
          collapse = " | "
        ),
        .groups = "drop"
      ),
    by = "duplicate_review_group_id",
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(
    dplyr::desc(projects_within_1500ft),
    pair_priorities,
    duplicate_review_group_id
  )

summary <- dplyr::bind_rows(
  tibble::tibble(
    section = "review",
    metric = c(
      "review_groups",
      "review_projects",
      "review_pairs",
      "groups_touching_1500ft",
      "groups_touching_500ft"
    ),
    value = c(
      nrow(group_summary),
      nrow(review_members),
      nrow(review_pairs),
      sum(group_summary$projects_within_1500ft > 0L),
      sum(group_summary$projects_within_500ft > 0L)
    )
  ),
  review_pairs |>
    dplyr::count(duplicate_review_priority, name = "value") |>
    dplyr::transmute(
      section = "pair_priority",
      metric = duplicate_review_priority,
      value
    )
)

readr::write_csv(
  review_pairs,
  "../output/preferred_project_duplicate_review_pairs.csv"
)
readr::write_csv(
  group_summary,
  "../output/preferred_project_duplicate_review_groups.csv"
)
readr::write_csv(
  review_members,
  "../output/preferred_project_duplicate_review_members.csv"
)
readr::write_csv(
  project_components,
  "../output/preferred_project_duplicate_review_components.csv"
)
readr::write_csv(
  review_permits,
  "../output/preferred_project_duplicate_review_permits.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_duplicate_review_summary.csv"
)
