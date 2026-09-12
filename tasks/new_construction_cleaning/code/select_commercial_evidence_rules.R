# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv", show_col_types = FALSE,
  col_types = readr::cols(project_id = "c", component_pins = "c", source_row_ids = "c")
)
vintages <- readr::read_csv(
  "../output/commercial_family_vintage_summary.csv", show_col_types = FALSE,
  col_types = readr::cols(project_family_id = "c", component_pin_list = "c", source_units = "c")
) %>%
  select(project_family_id, valuation_year, reported_units)
stopifnot(!anyDuplicated(vintages[c("project_family_id", "valuation_year")]))
vintages <- vintages %>%
  tidyr::pivot_wider(names_from = valuation_year, values_from = reported_units,
                     names_prefix = "units_")

# Permits already linked by parcel number, parcel geometry, or a revision chain.
linked <- read_csv("../output/project_permit_chain_links.csv",
  col_types = cols(project_id = "c", permit_id = "c", permit_number = "c", .default = col_guess()),
  show_col_types = FALSE) %>%
  filter(source_family == "commercial", permit_type == "PERMIT - NEW CONSTRUCTION") %>%
  transmute(project_id, permit_id, permit_number, permit_type, issue_date = as.Date(issue_date),
    work_description = str_squish(coalesce(work_description, "")), linked = TRUE)
address <- read_csv("../output/commercial_address_permit_history.csv",
  col_types = cols(project_id = "c", permit_id = "c", permit_number = "c", .default = col_guess()),
  show_col_types = FALSE) %>%
  transmute(project_id, permit_id, permit_number, permit_type, issue_date = as.Date(issue_date),
    work_description = str_squish(coalesce(work_description, "")), linked = TRUE)
spatial <- read_csv("../output/new_construction_spatial_permit_matches.csv",
  col_types = cols(project_id = "c", permit_id = "c", permit_number = "c", .default = col_guess()),
  show_col_types = FALSE) %>%
  filter(source_family == "commercial", polygon_distance_ft <= 100) %>%
  transmute(project_id, permit_id, permit_number, permit_type = "PERMIT - NEW CONSTRUCTION",
    issue_date = as.Date(issue_date), work_description = str_squish(coalesce(work_description, "")),
    linked = polygon_distance_ft == 0)

# Combine descriptions of the same permit before identifying residential work.
# An inside-parcel or recorded address/chain match is required for acceptance.
issued <- bind_rows(linked, address, spatial) %>%
  group_by(project_id, permit_id, permit_number) %>%
  summarise(permit_type = paste(sort(unique(permit_type)), collapse = "/"),
    issue_date = min(issue_date, na.rm = TRUE),
    work_description = paste(unique(work_description), collapse = " || "),
    linked = any(linked), .groups = "drop") %>%
  filter(linked, !is.na(issue_date), str_detect(permit_type, fixed("PERMIT - NEW CONSTRUCTION")),
    str_detect(work_description, regex(
      "DWELLING|RESIDENTIAL|APARTMENT|MULTI[- ]?FAMILY|HOUSING|DORMITOR|SENIOR LIVING|\\b[0-9]{1,4}\\s*[- ]?\\s*(?:TOTAL\\s+)?(?:UNITS?|D\\.?U\\.?)\\b",
      ignore_case = TRUE))) %>%
  group_by(project_id) %>%
  summarise(latest_new_construction_issue = max(issue_date), .groups = "drop")

completion <- candidates %>% filter(between(construction_year, 2006, 2022)) %>%
  left_join(issued, by = "project_id", relationship = "one-to-one") %>%
  transmute(project_id, later_assessor_report_after_permit = !is.na(latest_new_construction_issue) &
    is.finite(selected_vintage) & selected_vintage >= lubridate::year(latest_new_construction_issue))

land <- readr::read_csv(
  "../output/preferred_project_boundary_scope.csv", show_col_types = FALSE,
  col_types = readr::cols(project_id = "c", component_pins = "c")
) %>%
  filter(source_family == "commercial") %>%
  select(project_id, geography_status, project_land_area_sqft, collapsed_components)
scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv", show_col_types = FALSE,
  col_types = readr::cols(project_id = "c")
) %>%
  filter(source_family == "commercial") %>%
  select(project_id, review_scope)
manual <- readr::read_csv(
  "../input/commercial_manual_decisions.csv", show_col_types = FALSE,
  col_types = readr::cols(.default = "c")
) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = ";") %>%
  transmute(project_id = str_trim(source_project_ids), manual_action = action)

stopifnot(!anyDuplicated(candidates$project_id), !anyDuplicated(vintages$project_family_id),
          !anyDuplicated(completion$project_id), !anyDuplicated(land$project_id),
          !anyDuplicated(scope$project_id), !anyDuplicated(manual$project_id))

resolution <- candidates %>%
  left_join(vintages, by = c("project_id" = "project_family_id"), relationship = "one-to-one") %>%
  left_join(completion, by = "project_id", relationship = "one-to-one") %>%
  left_join(land, by = "project_id", relationship = "one-to-one") %>%
  left_join(scope, by = "project_id", relationship = "one-to-one") %>%
  left_join(manual, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    # Accept a disputed count when both Assessor reports agree on the same
    # components and the selected report follows an issued residential permit.
    stable_assessor_after_permit =
      is.na(manual_action) &
      decision_reason == "permit_unit_counts_do_not_include_assessor_count" &
      stable_component_membership & !(student_housing %in% TRUE) &
      is.finite(units_2021) & is.finite(units_2024) &
      units_2021 == units_2024 & units_2024 > 0 &
      later_assessor_report_after_permit,
    recommended_units = if_else(stable_assessor_after_permit, units_2024, NA_real_),
    recommended_units_source = if_else(stable_assessor_after_permit,
      "stable_commercial_2021_2024_reports", NA_character_),

    # Parcel coverage identifies the property. The ledger below must obtain
    # its density denominator from a reported land value, never polygon area.
    land_ratio = if_else(is.finite(land_sqft) & land_sqft > 0 &
                          is.finite(project_land_area_sqft) & project_land_area_sqft > 0,
                        land_sqft / project_land_area_sqft, NA_real_),
    land_review_reason = case_when(
      geography_status != "complete_construction_year_geometry" ~ "unresolved_geometry",
      geography_status == "complete_construction_year_geometry" & collapsed_components == 0 ~
        "complete_components",
      !is.finite(project_land_area_sqft) | project_land_area_sqft <= 0 ~ "missing_area",
      is.finite(land_ratio) & between(land_ratio, 0.80, 1.25) ~ "supported_predecessor",
      TRUE ~ "unresolved_predecessor"
    ),
    exact_land_recovery =
      is.na(manual_action) &
      (decision_reason == "missing_or_nonpositive_land_area" |
         (is.finite(land_sqft) & land_sqft <= 1)) &
      geography_status == "complete_construction_year_geometry" &
      is.finite(project_land_area_sqft) & project_land_area_sqft > 0 &
      land_review_reason %in% c("complete_components", "supported_predecessor"),
    resolution_status = case_when(
      !is.na(manual_action) ~ "manual_decision_complete",
      candidate_status == "exclude_outside_period" ~ "outside_study_period",
      stable_assessor_after_permit | exact_land_recovery ~ "evidence_rule_complete",
      candidate_status == "retain_mechanical" ~ "mechanical_candidate",
      review_scope == "review_within_1500ft" ~ "manual_review_required",
      review_scope == "review_geography_unresolved" ~ "geography_review_required",
      TRUE ~ "outside_manual_review_scope"
    ),
    resolution_reason = case_when(
      !is.na(manual_action) ~ paste0("manual_", manual_action),
      candidate_status == "exclude_outside_period" ~ decision_reason,
      stable_assessor_after_permit ~
        "stable_two_vintage_assessor_count_after_issued_new_construction_permit",
      exact_land_recovery ~ "complete_construction_year_parcel_union_recovers_land",
      TRUE ~ decision_reason
    )
  ) %>%
  select(all_of(names(candidates)), stable_assessor_after_permit, exact_land_recovery,
         recommended_units, recommended_units_source, resolution_status, resolution_reason)

stopifnot(nrow(resolution) == nrow(candidates), !anyNA(resolution$resolution_status))
SaveData(resolution, c("project_id"), "../output/commercial_post_evidence_resolution.csv")
