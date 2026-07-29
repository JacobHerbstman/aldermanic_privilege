# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

adjudication <- readr::read_csv(
  "../output/multicard_final_adjudication.csv",
  show_col_types = FALSE
)
cards <- readr::read_csv(
  "../output/multicard_card_snapshot.csv",
  show_col_types = FALSE
)
reviews <- readr::read_csv(
  "../adjudication/multicard_external_web_reviews.csv",
  show_col_types = FALSE
)
successors <- readr::read_csv(
  "../output/multicard_current_successor_project_summary.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(successor_addresses = current_addresses)

if (
  anyDuplicated(adjudication$project_id) ||
    anyDuplicated(reviews$project_id) ||
    anyDuplicated(successors$project_id)
) {
  stop("External-review inputs contain duplicate project IDs.", call. = FALSE)
}

card_types <- cards |>
  dplyr::filter(target_card) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    target_cards_confirmed = dplyr::n(),
    card_units_min = min(card_units, na.rm = TRUE),
    card_units_max = max(card_units, na.rm = TRUE),
    all_cards_single_unit = all(card_units == 1),
    any_card_multifamily = any(card_units > 1),
    .groups = "drop"
  )

queue <- adjudication |>
  dplyr::left_join(
    card_types,
    by = "pin",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    successors,
    by = c(
      "project_id",
      "pin" = "project_pin",
      "construction_year",
      "within_500ft"
    ),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    pin = sprintf("%.0f", pin),
    site_units_exceed_one = final_units > 1,
    building_type_rule = dplyr::case_when(
      final_disposition != "retain" ~ "suppressed",
      any_card_multifamily ~ "multifamily_building",
      all_cards_single_unit ~ "single_family_buildings",
      TRUE ~ "external_review_required"
    ),
    successor_sqft_ratio = current_noncondo_assessor_building_sqft /
      final_building_sqft,
    internal_evidence = dplyr::case_when(
      final_disposition != "retain" ~ "suppressed",
      all_cards_single_unit & current_parcels >= 2 ~
        "multiple_successor_parcels",
      all_cards_single_unit & current_footprints >= 2 ~
        "multiple_building_footprints",
      all_cards_single_unit &
        stringr::str_count(dplyr::coalesce(permit_addresses, ""), " / ") >= 1 ~
        "multiple_permit_addresses",
      any_card_multifamily &
        current_noncondo_assessor_units == final_units &
        dplyr::between(successor_sqft_ratio, 0.98, 1.02) ~
        "successor_total_matches",
      any_card_multifamily & current_condo_pins == final_units ~
        "successor_condo_count_matches",
      TRUE ~ "manual_external_review"
    ),
    google_maps_url = paste0(
      "https://www.google.com/maps/search/?api=1&query=",
      utils::URLencode(
        paste(review_address, "Chicago IL"),
        reserved = TRUE
      )
    ),
    google_search_query = paste0(
      "\"",
      review_address,
      "\" Chicago \"",
      pin,
      "\""
    ),
    review_priority = dplyr::case_when(
      project_id == "residential_multicard_20211200290000" ~ 1L,
      within_500ft & site_units_exceed_one ~ 2L,
      site_units_exceed_one ~ 3L,
      within_500ft ~ 4L,
      TRUE ~ 5L
    )
  ) |>
  dplyr::left_join(
    reviews,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    review_status = dplyr::coalesce(review_status, "pending"),
    external_structure_class = dplyr::case_when(
      review_status != "complete" ~ NA_character_,
      google_visual_type %in% c(
        "detached_single_family_collection",
        "single_detached_single_family_house",
        "single_mixed_use_one_dwelling_building",
        "two_detached_single_family_houses",
        "three_detached_single_family_houses",
        "four_detached_single_family_houses"
      ) ~ "detached_single_family_collection",
      google_visual_type %in% c(
        "attached_single_family_collection",
        "townhouse_development",
        "eight_unit_townhouse_development",
        "five_attached_single_family_townhouses",
        "two_attached_single_family_townhouses",
        "sixteen_unit_townhouse_development",
        "townhouse_components_within_larger_development"
      ) ~ "attached_single_family_collection",
      google_visual_type %in% c(
        "addition_to_existing_single_family",
        "expired_unbuilt_single_family_permits",
        "garage_card_existing_historic_building",
        "predecessor_townhouse_parent_duplicate",
        "spurious_small_card_existing_historic_multifamily",
        "spurious_small_card_existing_historic_single_family",
        "spurious_new_construction_card_existing_historic_building",
        "vacant_site_no_completed_project"
      ) ~
        "no_completed_project",
      google_visual_type %in% c(
        "single_apartment_building",
        "single_condominium_building",
        "single_mixed_use_apartment_building",
        "single_six_unit_condominium_building",
        "two_family_property",
        "new_two_unit_rear_component",
        "new_three_unit_component"
      ) ~ "single_multifamily_building",
      google_visual_type %in% c(
        "multifamily_building_collection",
        "two_adjacent_apartment_buildings",
        "three_adjacent_apartment_buildings",
        "four_adjacent_apartment_buildings",
        "two_of_three_adjacent_three_unit_buildings",
        "two_adjacent_six_unit_apartment_buildings",
        "three_adjacent_three_unit_apartment_buildings",
        "two_adjacent_four_unit_apartment_buildings",
        "two_adjacent_two_unit_condominium_buildings",
        "three_adjacent_four_unit_apartment_buildings",
        "three_adjacent_two_unit_buildings",
        "two_adjacent_three_unit_apartment_buildings"
      ) ~ "multiple_multifamily_buildings",
      TRUE ~ "unclassified"
    ),
    multifamily_disposition = dplyr::case_when(
      review_status != "complete" ~ "pending",
      external_structure_class %in% c(
        "single_multifamily_building",
        "multiple_multifamily_buildings"
      ) ~ "include",
      external_structure_class %in% c(
        "detached_single_family_collection",
        "attached_single_family_collection"
      ) ~ "exclude",
      external_structure_class == "no_completed_project" ~ "suppress",
      TRUE ~ "unresolved"
    )
  ) |>
  dplyr::select(
    review_priority,
    project_id,
    pin,
    construction_year,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    review_address,
    target_cards,
    target_card_numbers,
    target_classes,
    card_units_min,
    card_units_max,
    all_cards_single_unit,
    any_card_multifamily,
    final_units,
    final_building_sqft,
    land_sqft,
    site_units_exceed_one,
    building_type_rule,
    current_footprints,
    current_footprints_near_construction,
    current_footprint_area_sum,
    permit_addresses,
    permit_unit_values,
    current_parcels,
    current_noncondo_parcels,
    current_condo_pins,
    distinct_current_addresses,
    successor_addresses,
    current_noncondo_assessor_units,
    current_noncondo_assessor_building_sqft,
    successor_sqft_ratio,
    internal_evidence,
    adjudication_reason,
    adjudication_confidence,
    google_maps_url,
    google_search_query,
    review_status,
    google_visual_type,
    external_structure_class,
    multifamily_disposition,
    external_building_count,
    external_unit_count,
    external_building_sqft,
    source_1_url,
    source_2_url,
    supports_building_type,
    supports_final_units,
    reviewer_notes,
    review_date
  ) |>
  dplyr::arrange(review_priority, project_id)

if (
  nrow(queue) != nrow(adjudication) ||
    anyDuplicated(queue$project_id) ||
    any(is.na(queue$building_type_rule)) ||
    any(queue$review_status == "complete" &
      (is.na(queue$supports_building_type) |
        is.na(queue$supports_final_units))) ||
    any(queue$review_status == "complete" &
      queue$multifamily_disposition == "unresolved")
) {
  stop("The external multicard review queue failed validation.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  queue |>
    dplyr::count(
      section = "building_type_rule",
      value = building_type_rule,
      name = "projects"
    ),
  queue |>
    dplyr::count(
      section = "review_status",
      value = review_status,
      name = "projects"
    ),
  queue |>
    dplyr::count(
      section = "multifamily_disposition",
      value = multifamily_disposition,
      name = "projects"
    ),
  queue |>
    dplyr::count(
      section = "internal_evidence",
      value = internal_evidence,
      name = "projects"
    ),
  queue |>
    dplyr::count(
      section = "priority",
      value = as.character(review_priority),
      name = "projects"
    )
)

readr::write_csv(
  queue,
  "../output/multicard_external_review_queue.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/multicard_external_review_summary.csv",
  na = ""
)
