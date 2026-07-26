# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

scope <- readr::read_csv(
  "../output/residential_unresolved_scope_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

history <- readr::read_csv(
  "../output/residential_unresolved_scope_assessor_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    card_num = readr::col_character(),
    property_class = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

card_evidence <- readr::read_csv(
  "../output/residential_unresolved_card_project_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

episodes <- readr::read_csv(
  "../output/residential_unresolved_construction_episodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    episode_component_pins = readr::col_character(),
    episode_source_rows = readr::col_character(),
    .default = readr::col_guess()
  )
)

permits <- readr::read_csv(
  "../output/residential_unresolved_nearby_permits.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit = readr::col_character(),
    permit_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

reference_points <- readr::read_csv(
  "../output/residential_unresolved_reference_points.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    reference_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

geocodes <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    request_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

city_buildings <- readr::read_csv(
  "../output/residential_unresolved_city_building_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

permit_history <- readr::read_csv(
  "../output/residential_unresolved_address_permit_project_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

if (anyDuplicated(scope$project_id) > 0 ||
    !setequal(scope$project_id, history$project_id) ||
    any(!card_evidence$project_id %in% scope$project_id) ||
    any(!episodes$project_id %in% scope$project_id) ||
    any(!permits$project_id %in% scope$project_id) ||
    any(!reference_points$project_id %in% scope$project_id) ||
    any(!geocodes$project_id %in% scope$project_id) ||
    anyDuplicated(city_buildings$project_id) > 0 ||
    any(!city_buildings$project_id %in% scope$project_id) ||
    anyDuplicated(permit_history$project_id) > 0 ||
    !setequal(permit_history$project_id, scope$project_id)) {
  stop("Unresolved residential evidence violates project keys or coverage.", call. = FALSE)
}

history_summary <- history %>%
  group_by(project_id) %>%
  summarise(
    assessor_history_rows = n(),
    assessor_source_pins = n_distinct(pin),
    assessor_source_cards = n_distinct(paste(pin, card_num)),
    assessor_first_tax_year = min(tax_year, na.rm = TRUE),
    assessor_last_tax_year = max(tax_year, na.rm = TRUE),
    assessor_min_year_built = min(year_built, na.rm = TRUE),
    assessor_max_year_built = max(year_built, na.rm = TRUE),
    assessor_study_period_rows = sum(between(year_built, 2006, 2022), na.rm = TRUE),
    assessor_study_period_years = paste(
      sort(unique(year_built[between(year_built, 2006, 2022)])),
      collapse = "/"
    ),
    assessor_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    .groups = "drop"
  )

episode_summary <- episodes %>%
  arrange(project_id, construction_year, tax_year) %>%
  group_by(project_id) %>%
  summarise(
    usable_episode_rows = n(),
    usable_episode_years = paste(sort(unique(construction_year)), collapse = "/"),
    usable_episode_evidence = paste0(
      "year=", construction_year,
      "; tax_year=", tax_year,
      "; cards=", physical_cards,
      "; building_sqft=", episode_building_sqft,
      "; units=", episode_dwelling_units,
      "; land_sqft=", site_land_sqft,
      "; FAR_ready=", far_fields_complete,
      "; DUPAC_ready=", dupac_fields_complete,
      collapse = " || "
    ),
    .groups = "drop"
  )

permit_summary <- permits %>%
  mutate(
    exact_component_pin10 = coalesce(exact_component_pin10, FALSE),
    within_25ft = minimum_distance_ft <= 25,
    permit_evidence = paste0(
      "permit=", permit,
      "; application_year=", application_year,
      "; status=", permit_status,
      "; address=", permit_address,
      "; distance_ft=", round(minimum_distance_ft, 1),
      "; work=", str_squish(work_description)
    )
  ) %>%
  group_by(project_id) %>%
  summarise(
    nearby_new_construction_permits = n_distinct(permit_id),
    exact_pin10_permits = n_distinct(permit_id[exact_component_pin10]),
    within_25ft_permits = n_distinct(permit_id[within_25ft]),
    exact_pin10_permit_evidence = paste(
      unique(permit_evidence[exact_component_pin10]),
      collapse = " || "
    ),
    within_25ft_permit_evidence = paste(
      unique(permit_evidence[within_25ft]),
      collapse = " || "
    ),
    nearest_permit_distance_ft = min(minimum_distance_ft),
    nearest_permit_evidence = permit_evidence[which.min(minimum_distance_ft)],
    .groups = "drop"
  )

reference_summary <- reference_points %>%
  group_by(project_id) %>%
  summarise(
    independent_reference_points = n(),
    independent_reference_sources = paste(
      sort(unique(unlist(str_split(reference_sources, fixed("/"))))),
      collapse = "/"
    ),
    .groups = "drop"
  )

geocode_summary <- geocodes %>%
  group_by(project_id) %>%
  summarise(
    address_requests = n(),
    accepted_address_points = sum(geocode_status == "accepted_reference_point"),
    accepted_address_evidence = paste0(
      address[geocode_status == "accepted_reference_point"],
      " -> ", matched_address[geocode_status == "accepted_reference_point"],
      collapse = " || "
    ),
    .groups = "drop"
  )

evidence <- scope %>%
  select(
    project_id,
    project_kind,
    component_pins,
    construction_year,
    review_scope,
    geography_status,
    distance_to_boundary_ft,
    assessor_year_values,
    study_period_year_values,
    source_pin_count,
    source_card_count,
    exact_permit_evidence,
    permit_unit_evidence,
    current_addresses,
    historical_addresses,
    historical_point_evidence
  ) %>%
  left_join(history_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(
    card_evidence %>%
      select(
        project_id,
        card_evidence_status = evidence_status,
        selected_tax_year,
        selected_study_period_episodes,
        selected_study_period_years,
        selected_active_physical_cards,
        selected_site_land_sqft,
        selected_snapshot_reason,
        selected_card_evidence,
        selected_episode_evidence,
        selected_all_far_fields_complete,
        selected_all_dupac_fields_complete
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(episode_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(reference_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(geocode_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(city_buildings, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_history, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    assessor_study_period_years = coalesce(assessor_study_period_years, ""),
    study_period_year_values = coalesce(study_period_year_values, ""),
    nearby_new_construction_permits = coalesce(nearby_new_construction_permits, 0L),
    exact_pin10_permits = coalesce(exact_pin10_permits, 0L),
    within_25ft_permits = coalesce(within_25ft_permits, 0L),
    independent_reference_points = coalesce(independent_reference_points, 0L),
    address_requests = coalesce(address_requests, 0L),
    accepted_address_points = coalesce(accepted_address_points, 0L),
    reference_addresses = coalesce(reference_addresses, 0L),
    uniquely_matched_addresses = coalesce(uniquely_matched_addresses, 0L),
    ambiguous_address_matches = coalesce(ambiguous_address_matches, 0L),
    nearest_only_addresses = coalesce(nearest_only_addresses, 0L),
    addresses_without_nearby_footprint = coalesce(
      addresses_without_nearby_footprint,
      0L
    ),
    review_category = case_when(
      assessor_study_period_rows == 0 & assessor_max_year_built < 2006 ~
        "outside_period_candidate",
      card_evidence_status == "one_episode_card_evidence" ~
        "single_episode_review",
      card_evidence_status == "multiple_construction_episodes_review" ~
        "multiple_episode_review",
      card_evidence_status == "distinct_buildings_share_card_number_review" ~
        "card_identity_review",
      TRUE ~ "manual_project_review"
    ),
    location_review_category = case_when(
      independent_reference_points == 0 ~ "no_independent_reference_point",
      exact_pin10_permits > 0 | within_25ft_permits > 0 ~ "permit_supported_location",
      accepted_address_points > 0 ~ "exact_address_geocode_only",
      TRUE ~ "parcel_or_candidate_point_only"
    )
  ) %>%
  arrange(review_category, project_id)

if (nrow(evidence) != nrow(scope) ||
    anyDuplicated(evidence$project_id) > 0 ||
    !setequal(evidence$project_id, scope$project_id) ||
    any(!is.finite(evidence$assessor_history_rows)) ||
    any(evidence$assessor_history_rows <= 0) ||
    any(evidence$review_category == "outside_period_candidate" &
          (evidence$assessor_study_period_rows > 0 |
             evidence$assessor_max_year_built >= 2006))) {
  stop("The unresolved residential adjudication ledger failed validation.", call. = FALSE)
}

summary <- bind_rows(
  evidence %>%
    count(review_category, name = "value") %>%
    transmute(section = "review_category", metric = review_category, value),
  evidence %>%
    count(location_review_category, name = "value") %>%
    transmute(section = "location_review", metric = location_review_category, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "source_projects",
      "projects_with_study_period_year",
      "outside_period_candidates",
      "projects_with_exact_pin10_permit",
      "projects_with_permit_within_25ft",
      "projects_with_exact_address_or_pin_permit_history",
      "projects_with_unique_address_range_footprint",
      "projects_without_independent_reference_point",
      "duplicate_project_ids",
      "unaccounted_projects"
    ),
    value = c(
      nrow(evidence),
      sum(evidence$assessor_study_period_rows > 0),
      sum(evidence$review_category == "outside_period_candidate"),
      sum(evidence$exact_pin10_permits > 0),
      sum(evidence$within_25ft_permits > 0),
      sum(evidence$exact_address_or_pin_permits > 0),
      sum(evidence$uniquely_matched_addresses > 0),
      sum(evidence$independent_reference_points == 0),
      anyDuplicated(evidence$project_id),
      nrow(scope) - nrow(evidence)
    )
  )
)

readr::write_csv(
  evidence,
  "../output/residential_unresolved_adjudication_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_adjudication_evidence_summary.csv"
)
