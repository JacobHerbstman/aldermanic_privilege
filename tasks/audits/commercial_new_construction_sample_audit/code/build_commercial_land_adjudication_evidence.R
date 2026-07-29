# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(between(construction_year, 2006L, 2022L))

project_geography <- readr::read_csv(
  "../output/preferred_project_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial") %>%
  select(
    project_id,
    geography_status,
    requested_components,
    resolved_components,
    collapsed_components,
    project_land_area_sqft,
    within_1500ft,
    within_500ft
  )

if (anyDuplicated(commercial_projects$project_id) > 0) {
  stop("Commercial projects are not unique by project ID.", call. = FALSE)
}
if (anyDuplicated(project_geography$project_id) > 0) {
  stop("Commercial project geography is not unique by project ID.", call. = FALSE)
}

land_evidence <- commercial_projects %>%
  left_join(project_geography, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    assessor_land_available = is.finite(land_sqft) & land_sqft > 0,
    parcel_land_available = is.finite(project_land_area_sqft) & project_land_area_sqft > 0,
    assessor_to_parcel_land_ratio = if_else(
      assessor_land_available & parcel_land_available,
      land_sqft / project_land_area_sqft,
      NA_real_
    ),
    land_area_agrees_within_10pct =
      is.finite(assessor_to_parcel_land_ratio) &
      between(assessor_to_parcel_land_ratio, 0.90, 1.10),
    land_area_agrees_within_25pct =
      is.finite(assessor_to_parcel_land_ratio) &
      between(assessor_to_parcel_land_ratio, 0.80, 1.25),
    exact_construction_year_components =
      geography_status == "complete_construction_year_geometry" &
      collapsed_components == 0,
    land_review_reason = case_when(
      geography_status != "complete_construction_year_geometry" ~
        "construction_year_geometry_unresolved",
      exact_construction_year_components ~
        "use_exact_construction_year_parcel_union",
      !parcel_land_available ~ "predecessor_parcel_union_land_missing",
      land_area_agrees_within_25pct ~
        "predecessor_parcel_union_validated_by_assessor_land",
      TRUE ~ "predecessor_parcel_union_requires_review"
    ),
    land_review_required = land_review_reason %in% c(
      "construction_year_geometry_unresolved",
      "predecessor_parcel_union_land_missing",
      "predecessor_parcel_union_requires_review"
    )
  ) %>%
  select(
    project_id,
    construction_year,
    selected_source_addresses,
    component_pins,
    component_count,
    selected_vintage,
    source_row_ids,
    assessor_land_sqft = land_sqft,
    project_land_area_sqft,
    assessor_to_parcel_land_ratio,
    land_area_agrees_within_10pct,
    land_area_agrees_within_25pct,
    exact_construction_year_components,
    geography_status,
    requested_components,
    resolved_components,
    collapsed_components,
    within_1500ft,
    within_500ft,
    candidate_status,
    decision_reason,
    land_review_required,
    land_review_reason
  ) %>%
  arrange(desc(within_1500ft), desc(within_500ft), project_id)

if (anyDuplicated(land_evidence$project_id) > 0) {
  stop("Commercial land evidence is not unique by project ID.", call. = FALSE)
}

land_review_queue <- land_evidence %>%
  filter(land_review_required)

summary <- bind_rows(
  land_evidence %>%
    count(land_review_reason, name = "value") %>%
    transmute(metric = paste0("all:", land_review_reason), value),
  land_evidence %>%
    filter(within_1500ft) %>%
    count(land_review_reason, name = "value") %>%
    transmute(metric = paste0("within_1500ft:", land_review_reason), value),
  land_evidence %>%
    filter(within_500ft) %>%
    count(land_review_reason, name = "value") %>%
    transmute(metric = paste0("within_500ft:", land_review_reason), value)
)

readr::write_csv(
  land_evidence,
  "../output/commercial_land_adjudication_evidence.csv"
)
readr::write_csv(
  land_review_queue,
  "../output/commercial_land_adjudication_queue.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_land_adjudication_summary.csv"
)
