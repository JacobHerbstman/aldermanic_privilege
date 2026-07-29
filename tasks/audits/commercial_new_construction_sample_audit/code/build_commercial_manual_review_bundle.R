# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/commercial_adjudication_queue.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

review_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "commercial", review_scope == "review_within_1500ft") %>%
  select(project_id, distance_to_boundary_ft, within_500ft, review_scope)

ground_up <- readr::read_csv(
  "../output/commercial_ground_up_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    ground_up_status,
    ground_up_review_required,
    matched_city_footprints,
    city_footprint_addresses,
    city_year_built_values,
    city_year_coverage_share,
    city_near_target_share,
    city_old_building_share,
    exact_new_construction_permit_numbers,
    inside_new_construction_permit_numbers,
    address_new_construction_permit_numbers,
    chain_new_construction_permit_numbers
  )

units <- readr::read_csv(
  "../output/commercial_unit_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    units_2021,
    units_2024,
    unit_permit_counts,
    candidate_permit_unit_evidence = permit_unit_evidence,
    recommended_units,
    recommended_units_source,
    unit_review_required,
    unit_review_reason
  )

land <- readr::read_csv(
  "../output/commercial_land_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    assessor_land_sqft,
    project_land_area_sqft,
    assessor_to_parcel_land_ratio,
    geography_status,
    requested_components,
    resolved_components,
    collapsed_components,
    land_review_required,
    land_review_reason
  )

permit_chains <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial") %>%
  group_by(project_id) %>%
  summarise(
    permit_chain_evidence = paste(
      paste0(
        permit_number,
        " [", permit_status, "] ",
        str_squish(work_description)
      ),
      collapse = " || "
    ),
    .groups = "drop"
  )

address_permits <- readr::read_csv(
  "../output/commercial_address_permit_history_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

project_overlaps <- readr::read_csv(
  "../output/project_overlap_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "commercial") %>%
  select(-source_family)

if (
  anyDuplicated(candidates$project_id) > 0 ||
  anyDuplicated(ground_up$project_id) > 0 ||
  anyDuplicated(units$project_id) > 0 ||
  anyDuplicated(land$project_id) > 0 ||
  anyDuplicated(permit_chains$project_id) > 0 ||
  anyDuplicated(address_permits$project_id) > 0 ||
  anyDuplicated(project_overlaps$project_id) > 0
) {
  stop("Commercial review inputs are not unique by project.", call. = FALSE)
}

review <- candidates %>%
  inner_join(review_scope, by = "project_id", relationship = "one-to-one") %>%
  left_join(ground_up, by = "project_id", relationship = "one-to-one") %>%
  left_join(units, by = "project_id", relationship = "one-to-one") %>%
  left_join(land, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_chains, by = "project_id", relationship = "one-to-one") %>%
  left_join(address_permits, by = "project_id", relationship = "one-to-one") %>%
  left_join(project_overlaps, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    ground_up_review_required = coalesce(ground_up_review_required, FALSE),
    unit_review_required = coalesce(unit_review_required, FALSE),
    land_review_required = coalesce(land_review_required, FALSE),
    review_fields = purrr::pmap_chr(
      list(ground_up_review_required, unit_review_required, land_review_required),
      function(project_type, dwelling_units, land_area) {
        paste(
          c(
            if (project_type) "project_type" else character(),
            if (dwelling_units) "dwelling_units" else character(),
            if (land_area) "land_area" else character()
          ),
          collapse = "/"
        )
      }
    )
  ) %>%
  select(
    project_id,
    candidate_status,
    decision_reason,
    review_fields,
    review_scope,
    distance_to_boundary_ft,
    within_500ft,
    current_distance_m,
    construction_year,
    selected_source_addresses,
    selected_property_type_use,
    selected_property_description,
    component_pins,
    component_count,
    dwelling_units,
    building_sqft,
    land_sqft,
    selected_vintage,
    observed_2021,
    observed_2024,
    stable_component_membership,
    ground_up_status,
    matched_city_footprints,
    city_footprint_addresses,
    city_year_built_values,
    city_year_coverage_share,
    city_near_target_share,
    city_old_building_share,
    exact_new_construction_permit_numbers,
    inside_new_construction_permit_numbers,
    address_new_construction_permit_numbers,
    chain_new_construction_permit_numbers,
    permit_chain_evidence,
    address_permits,
    residential_new_construction_permits,
    other_new_construction_permits,
    conversion_addition_permits,
    demolition_permits,
    permit_history,
    units_2021,
    units_2024,
    unit_permit_counts,
    candidate_permit_unit_evidence,
    recommended_units,
    recommended_units_source,
    unit_review_reason,
    assessor_land_sqft,
    project_land_area_sqft,
    assessor_to_parcel_land_ratio,
    geography_status,
    requested_components,
    resolved_components,
    collapsed_components,
    land_review_reason,
    overlap_candidates,
    project_overlap_evidence
  ) %>%
  arrange(project_id)

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(
  names(review),
  regex(paste(prohibited_review_columns, collapse = "|"), ignore_case = TRUE)
))) {
  stop("Commercial review bundle contains a prohibited analysis field.", call. = FALSE)
}
if (anyDuplicated(review$project_id) > 0) {
  stop("Commercial review bundle is not unique by project.", call. = FALSE)
}
if (nrow(review) != nrow(review_scope)) {
  stop("Commercial review bundle does not cover the full construction-year review scope.", call. = FALSE)
}

summary <- bind_rows(
  review %>% count(review_fields, name = "value") %>% transmute(metric = review_fields, value),
  tibble::tibble(metric = "projects", value = nrow(review))
)

readr::write_csv(review, "../output/commercial_manual_review_bundle.csv")
readr::write_csv(summary, "../output/commercial_manual_review_summary.csv")
