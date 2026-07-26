# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

address_key <- function(x) {
  value <- x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
  number <- stringr::str_extract(value, "^[0-9]+")
  street <- value |>
    stringr::str_remove("^[0-9]+\\s*") |>
    stringr::str_replace_all(
      "\\b(N|S|E|W|NORTH|SOUTH|EAST|WEST)\\b",
      " "
    ) |>
    stringr::str_replace_all(
      "\\b(ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|DR|DRIVE|PL|PLACE|CT|COURT|PKWY|PARKWAY|HWY|HIGHWAY)\\b",
      " "
    ) |>
    stringr::str_replace("\\s+[A-Z]$", "") |>
    stringr::str_squish()
  dplyr::if_else(
    !is.na(number) & street != "",
    paste(number, street),
    NA_character_
  )
}

pairs <- readr::read_csv(
  "../output/preferred_project_duplicate_temporal_pairs.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    tieback_group_values_1 = readr::col_character(),
    tieback_group_values_2 = readr::col_character(),
    .default = readr::col_guess()
  )
)
projects <- readr::read_csv(
  "../output/preferred_project_duplicate_review_members.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
all_projects <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
direct_permits <- readr::read_csv(
  "../output/preferred_project_duplicate_review_permits.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(directly_matched) |>
  dplyr::select(project_id, address = permit_address) |>
  dplyr::filter(!is.na(address), address != "")
nearby_groups <- readr::read_csv(
  "../output/preferred_project_duplicate_nearby_permit_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    duplicate_review_group_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
transition_edges <- readr::read_csv(
  "../output/preferred_project_duplicate_transition_edges.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id_1 = readr::col_character(),
    project_id_2 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(
    project_id_1,
    project_id_2,
    transition_candidate_source,
    predecessor_project_id,
    successor_project_id,
    successor_to_predecessor_parcel_ft,
    transition_parcel_status
  )
overrides <- readr::read_csv(
  "../adjudication/preferred_project_duplicate_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    duplicate_disposition = readr::col_character(),
    disposition_reason = readr::col_character(),
    evidence = readr::col_character()
  )
)

if (anyDuplicated(projects$project_id) ||
    anyDuplicated(all_projects$project_id) ||
    anyDuplicated(pairs[c("project_id_1", "project_id_2")]) ||
    anyDuplicated(
      transition_edges[c("project_id_1", "project_id_2")]
    ) ||
    anyDuplicated(overrides$project_id) ||
    any(!overrides$project_id %in% all_projects$project_id) ||
    any(!overrides$duplicate_disposition %in%
          c("retain", "suppress_duplicate"))) {
  stop("Duplicate-disposition inputs have invalid keys.", call. = FALSE)
}

current_addresses <- projects |>
  dplyr::select(
    project_id,
    address = current_property_addresses
  ) |>
  dplyr::filter(!is.na(address), address != "") |>
  tidyr::separate_longer_delim(address, delim = "/")
address_evidence <- dplyr::bind_rows(
  current_addresses |>
    dplyr::mutate(address_source = "current_parcel"),
  direct_permits |>
    dplyr::mutate(address_source = "direct_new_construction_permit")
) |>
  dplyr::mutate(address_key = address_key(address)) |>
  dplyr::filter(!is.na(address_key), address_key != "") |>
  dplyr::distinct(
    project_id,
    address_source,
    address_key,
    address
  )
address_sets <- address_evidence |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    address_keys = list(sort(unique(address_key))),
    direct_permit_address_keys = list(sort(unique(
      address_key[
        address_source == "direct_new_construction_permit"
      ]
    ))),
    current_address_count = dplyr::n_distinct(
      address_key[address_source == "current_parcel"]
    ),
    direct_permit_address_count = dplyr::n_distinct(
      address_key[
        address_source == "direct_new_construction_permit"
      ]
    ),
    address_count = dplyr::n_distinct(address_key),
    addresses = paste(sort(unique(address)), collapse = "/"),
    .groups = "drop"
  )
address_list <- split(address_sets$address_keys, address_sets$project_id)
direct_permit_address_list <- split(
  address_sets$direct_permit_address_keys,
  address_sets$project_id
)

pair_dispositions <- pairs |>
  dplyr::left_join(
    transition_edges,
    by = c("project_id_1", "project_id_2"),
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    address_sets |>
      dplyr::select(
        project_id_1 = project_id,
        support_current_address_count_1 = current_address_count,
        direct_permit_address_count_1 =
          direct_permit_address_count,
        address_count_1 = address_count,
        support_addresses_1 = addresses
      ),
    by = "project_id_1",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    address_sets |>
      dplyr::select(
        project_id_2 = project_id,
        support_current_address_count_2 = current_address_count,
        direct_permit_address_count_2 =
          direct_permit_address_count,
        address_count_2 = address_count,
        support_addresses_2 = addresses
      ),
    by = "project_id_2",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    nearby_groups |>
      dplyr::select(
        duplicate_review_group_id,
        project_count,
        nearby_permit_address_count
    ),
    by = "duplicate_review_group_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    overrides |>
      dplyr::select(
        project_id_1 = project_id,
        override_disposition_1 = duplicate_disposition
      ),
    by = "project_id_1",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    overrides |>
      dplyr::select(
        project_id_2 = project_id,
        override_disposition_2 = duplicate_disposition
      ),
    by = "project_id_2",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        support_current_address_count_1,
        direct_permit_address_count_1,
        address_count_1,
        support_current_address_count_2,
        direct_permit_address_count_2,
        address_count_2,
        nearby_permit_address_count
      ),
      ~ dplyr::coalesce(.x, 0L)
    )
  )

pair_dispositions$shared_address_count <- purrr::map2_int(
  pair_dispositions$project_id_1,
  pair_dispositions$project_id_2,
  function(project_1, project_2) {
    length(intersect(
      unlist(address_list[[project_1]]),
      unlist(address_list[[project_2]])
    ))
  }
)
pair_dispositions$shared_direct_permit_address_count <-
  purrr::map2_int(
    pair_dispositions$project_id_1,
    pair_dispositions$project_id_2,
    function(project_1, project_2) {
      length(intersect(
        unlist(direct_permit_address_list[[project_1]]),
        unlist(direct_permit_address_list[[project_2]])
      ))
    }
  )
pair_dispositions <- pair_dispositions |>
  dplyr::mutate(
    confirmed_transition =
      is.finite(successor_to_predecessor_parcel_ft) &
        successor_to_predecessor_parcel_ft <= 5,
    both_addressed = address_count_1 > 0L & address_count_2 > 0L,
    distinct_supported_addresses =
      both_addressed & shared_address_count == 0L,
    distinct_direct_permit_addresses =
      direct_permit_address_count_1 > 0L &
        direct_permit_address_count_2 > 0L &
        shared_direct_permit_address_count == 0L,
    nearby_permits_support_group =
      is.finite(project_count) &
        nearby_permit_address_count >= project_count,
    transition_suppress_project = dplyr::case_when(
      !confirmed_transition ~ NA_character_,
      address_count_1 == 0L & address_count_2 > 0L ~
        project_id_1,
      address_count_1 > 0L & address_count_2 == 0L ~
        project_id_2,
      both_addressed & shared_address_count > 0L ~
        predecessor_project_id,
      address_count_1 == 0L &
        address_count_2 == 0L &
        dwelling_units_1 == dwelling_units_2 &
        is.finite(building_ratio) &
        building_ratio >= 0.80 ~
        predecessor_project_id,
      TRUE ~ NA_character_
    ),
    pair_disposition = dplyr::case_when(
      override_disposition_1 == "suppress_duplicate" |
        override_disposition_2 == "suppress_duplicate" ~
        "suppress_manual_duplicate",
      !is.na(override_disposition_1) &
        !is.na(override_disposition_2) ~
        "retain_manual_distinct_projects",
      temporal_lineage_status %in% c(
        "contemporaneous_class_295_rowhouse_parcels",
        "contemporaneous_distinct_assessor_parcels",
        "contemporaneous_distinct_address_parcels"
      ) ~ "retain_distinct_contemporaneous_projects",
      !is.na(transition_suppress_project) ~
        "suppress_confirmed_transition_duplicate",
      confirmed_transition & distinct_supported_addresses ~
        "retain_distinct_addressed_transition_projects",
      distinct_direct_permit_addresses ~
        "retain_distinct_permitted_projects",
      !is.na(transition_parcel_status) &
        successor_to_predecessor_parcel_ft > 5 ~
        "retain_spatially_distinct_projects",
      distinct_supported_addresses ~
        "retain_distinct_addressed_projects",
      nearby_permits_support_group ~
        "retain_multiple_permitted_projects",
      TRUE ~ "requires_final_review"
    )
  )

suppressions <- pair_dispositions |>
  dplyr::filter(
    pair_disposition ==
      "suppress_confirmed_transition_duplicate"
  ) |>
  dplyr::transmute(
    project_id = transition_suppress_project,
    duplicate_review_group_id,
    disposition_reason = dplyr::case_when(
      address_count_1 == 0L & project_id_1 == project_id ~
        "retired_pin_without_independent_address_or_permit",
      address_count_2 == 0L & project_id_2 == project_id ~
        "retired_pin_without_independent_address_or_permit",
      both_addressed & shared_address_count > 0L ~
        "same_physical_address_across_pin_transition",
      TRUE ~
        "same_fields_inside_predecessor_parcel_without_separate_address"
    )
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    duplicate_review_group_ids = paste(
      sort(unique(duplicate_review_group_id)),
      collapse = "/"
    ),
    disposition_reason = paste(
      sort(unique(disposition_reason)),
      collapse = "/"
    ),
    .groups = "drop"
  )
pair_dispositions <- pair_dispositions |>
  dplyr::mutate(
    pair_disposition = dplyr::if_else(
      pair_disposition == "requires_final_review" &
        (project_id_1 %in% suppressions$project_id |
           project_id_2 %in% suppressions$project_id),
      "resolved_by_project_suppression",
      pair_disposition
    )
  )
unresolved_projects <- pair_dispositions |>
  dplyr::filter(
    pair_disposition == "requires_final_review",
    !project_id_1 %in% suppressions$project_id,
    !project_id_2 %in% suppressions$project_id
  ) |>
  dplyr::select(
    duplicate_review_group_id,
    project_id_1,
    project_id_2
  ) |>
  tidyr::pivot_longer(
    c(project_id_1, project_id_2),
    values_to = "project_id"
  ) |>
  dplyr::select(-name) |>
  dplyr::distinct()

project_dispositions <- all_projects |>
  dplyr::select(
    project_id,
    source_family,
    project_kind,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id,
        within_1500ft,
        within_500ft,
        current_property_addresses
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    address_sets |>
      dplyr::select(
        project_id,
        current_address_count,
        direct_permit_address_count,
        address_count,
        addresses
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    suppressions,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    overrides |>
      dplyr::rename(
        override_disposition = duplicate_disposition,
        override_reason = disposition_reason,
        override_evidence = evidence
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    unresolved_duplicate_review =
      project_id %in% unresolved_projects$project_id &
        is.na(disposition_reason) &
        is.na(override_disposition),
    disposition_reason = dplyr::coalesce(
      disposition_reason,
      override_reason
    ),
    duplicate_disposition = dplyr::case_when(
      !is.na(override_disposition) ~ override_disposition,
      !is.na(disposition_reason) ~ "suppress_duplicate",
      unresolved_duplicate_review ~ "requires_final_review",
      TRUE ~ "retain"
    )
  )

if (any(project_dispositions$duplicate_disposition ==
        "requires_final_review")) {
  stop("Duplicate adjudication still has unresolved projects.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  pair_dispositions |>
    dplyr::count(pair_disposition, name = "value") |>
    dplyr::transmute(
      section = "pair_dispositions",
      metric = pair_disposition,
      value
    ),
  project_dispositions |>
    dplyr::count(duplicate_disposition, name = "value") |>
    dplyr::transmute(
      section = "project_dispositions",
      metric = duplicate_disposition,
      value
    ),
  project_dispositions |>
    dplyr::filter(within_1500ft) |>
    dplyr::count(duplicate_disposition, name = "value") |>
    dplyr::transmute(
      section = "within_1500ft",
      metric = duplicate_disposition,
      value
    ),
  project_dispositions |>
    dplyr::filter(within_500ft) |>
    dplyr::count(duplicate_disposition, name = "value") |>
    dplyr::transmute(
      section = "within_500ft",
      metric = duplicate_disposition,
      value
    )
)

readr::write_csv(
  address_evidence,
  "../output/preferred_project_duplicate_address_evidence.csv"
)
readr::write_csv(
  pair_dispositions,
  "../output/preferred_project_duplicate_pair_dispositions.csv"
)
readr::write_csv(
  project_dispositions,
  "../output/preferred_project_duplicate_dispositions.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_project_duplicate_disposition_summary.csv"
)
