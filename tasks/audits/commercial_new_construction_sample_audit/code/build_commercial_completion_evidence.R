# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

minimum_date <- function(x) {
  if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
}

maximum_date <- function(x) {
  if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
}

residential_construction_pattern <- regex(
  paste(
    "DWELLING|RESIDENTIAL|APARTMENT|MULTI[- ]?FAMILY|",
    "HOUSING|DORMITOR|SENIOR LIVING|",
    "\\b[0-9]{1,4}\\s*[- ]?\\s*(?:TOTAL\\s+)?(?:UNITS?|D\\.?U\\.?)\\b",
    sep = ""
  ),
  ignore_case = TRUE
)

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(between(construction_year, 2006L, 2022L))

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Commercial candidates are not unique by project.", call. = FALSE)
}

new_construction_permits <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial") %>%
  transmute(
    project_id,
    permit_id,
    permit_number,
    permit_type = "PERMIT - NEW CONSTRUCTION",
    permit_status,
    application_date = as.Date(application_date),
    issue_date = as.Date(issue_date),
    permit_address,
    evidence_link = if_else(
      directly_matched,
      paste0("component_pin:", direct_match_method),
      "permit_revision_chain"
    ),
    permit_link_strength = "linked",
    polygon_distance_ft = NA_real_,
    work_description = str_squish(coalesce(work_description, ""))
  )

address_permits <- readr::read_csv(
  "../output/commercial_address_permit_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    project_id,
    permit_id,
    permit_number,
    permit_type,
    permit_status,
    application_date = as.Date(application_date),
    issue_date = as.Date(issue_date),
    permit_address,
    evidence_link = "source_address",
    permit_link_strength = "linked",
    polygon_distance_ft = NA_real_,
    work_description = str_squish(coalesce(work_description, ""))
  )

spatial_permits <- readr::read_csv(
  "../output/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "commercial", polygon_distance_ft <= 100) %>%
  transmute(
    project_id,
    permit_id,
    permit_number,
    permit_type = "PERMIT - NEW CONSTRUCTION",
    permit_status,
    application_date = as.Date(application_date),
    issue_date = as.Date(issue_date),
    permit_address,
    evidence_link = if_else(
      polygon_distance_ft == 0,
      "inside_construction_year_project_polygon",
      "near_construction_year_project_polygon"
    ),
    permit_link_strength = if_else(
      polygon_distance_ft == 0,
      "linked",
      "nearby_review"
    ),
    polygon_distance_ft,
    work_description = str_squish(coalesce(work_description, ""))
  )

permit_evidence <- bind_rows(
  new_construction_permits,
  address_permits,
  spatial_permits
) %>%
  filter(project_id %in% candidates$project_id) %>%
  group_by(project_id, permit_id, permit_number) %>%
  summarise(
    permit_type = paste(sort(unique(permit_type)), collapse = "/"),
    permit_status = paste(sort(unique(permit_status[!is.na(permit_status)])), collapse = "/"),
    application_date = min(application_date, na.rm = TRUE),
    issue_date = min(issue_date, na.rm = TRUE),
    permit_address = paste(sort(unique(permit_address)), collapse = " / "),
    evidence_link = paste(sort(unique(evidence_link)), collapse = "/"),
    permit_link_strength = if_else(
      any(permit_link_strength == "linked"),
      "linked",
      "nearby_review"
    ),
    polygon_distance_ft = if (all(is.na(polygon_distance_ft))) {
      NA_real_
    } else {
      min(polygon_distance_ft, na.rm = TRUE)
    },
    work_description = paste(unique(work_description), collapse = " || "),
    .groups = "drop"
  ) %>%
  mutate(
    permit_status = na_if(permit_status, ""),
    permit_category = case_when(
      str_detect(permit_type, fixed("PERMIT - NEW CONSTRUCTION")) &
        str_detect(work_description, residential_construction_pattern) ~
        "residential_new_construction",
      str_detect(permit_type, fixed("PERMIT - NEW CONSTRUCTION")) ~
        "other_new_construction",
      str_detect(permit_type, fixed("PERMIT - WRECKING/DEMOLITION")) ~ "demolition",
      str_detect(
        work_description,
        regex(
          "ADAPTIVE REUSE|CHANGE OF (USE|OCCUPANCY)|CONVERT|CONVERSION|ADDITION|EXISTING BUILDING",
          ignore_case = TRUE
        )
      ) ~ "existing_building_or_addition",
      TRUE ~ "other"
    )
  ) %>%
  arrange(project_id, application_date, permit_number)

if (anyDuplicated(permit_evidence[c("project_id", "permit_id", "permit_number")]) > 0) {
  stop("Commercial completion evidence has duplicate project-permit keys.", call. = FALSE)
}

permit_summary <- permit_evidence %>%
  group_by(project_id) %>%
  summarise(
    issued_new_construction_permits = n_distinct(
      permit_number[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked" &
          !is.na(issue_date)
      ]
    ),
    completed_new_construction_permits = n_distinct(
      permit_number[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked" &
          coalesce(permit_status == "COMPLETE", FALSE)
      ]
    ),
    expired_new_construction_permits = n_distinct(
      permit_number[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked" &
          coalesce(permit_status == "EXPIRED", FALSE)
      ]
    ),
    nearby_residential_new_construction_permits = n_distinct(
      permit_number[
        permit_category == "residential_new_construction" &
          permit_link_strength == "nearby_review"
      ]
    ),
    earliest_new_construction_application = minimum_date(
      application_date[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked"
      ]
    ),
    earliest_new_construction_issue = minimum_date(
      issue_date[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked"
      ]
    ),
    latest_new_construction_issue = maximum_date(
      issue_date[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked"
      ]
    ),
    new_construction_permit_numbers = paste(
      sort(unique(permit_number[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked"
      ])),
      collapse = "/"
    ),
    new_construction_permit_statuses = paste(
      sort(unique(permit_status[
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked" &
          !is.na(permit_status)
      ])),
      collapse = "/"
    ),
    new_construction_evidence = if (
      any(
        permit_category == "residential_new_construction" &
          permit_link_strength == "linked"
      )
    ) {
      paste(
        unique(paste0(
          permit_number[
            permit_category == "residential_new_construction" &
              permit_link_strength == "linked"
          ],
          " [", permit_status[
            permit_category == "residential_new_construction" &
              permit_link_strength == "linked"
          ], "] ",
          work_description[
            permit_category == "residential_new_construction" &
              permit_link_strength == "linked"
          ]
        )),
        collapse = " || "
      )
    } else {
      NA_character_
    },
    nearby_new_construction_evidence = if (
      any(
        permit_category == "residential_new_construction" &
          permit_link_strength == "nearby_review"
      )
    ) {
      paste(
        unique(paste0(
          permit_number[
            permit_category == "residential_new_construction" &
              permit_link_strength == "nearby_review"
          ],
          " [", round(polygon_distance_ft[
            permit_category == "residential_new_construction" &
              permit_link_strength == "nearby_review"
          ], 1), "ft] ",
          permit_address[
            permit_category == "residential_new_construction" &
              permit_link_strength == "nearby_review"
          ], ": ",
          work_description[
            permit_category == "residential_new_construction" &
              permit_link_strength == "nearby_review"
          ]
        )),
        collapse = " || "
      )
    } else {
      NA_character_
    },
    demolition_permits = n_distinct(permit_number[permit_category == "demolition"]),
    demolition_evidence = if (any(permit_category == "demolition")) {
      paste(
        unique(paste0(
          permit_number[permit_category == "demolition"],
          " [", issue_date[permit_category == "demolition"], "] ",
          work_description[permit_category == "demolition"]
        )),
        collapse = " || "
      )
    } else {
      NA_character_
    },
    existing_building_or_addition_permits = n_distinct(
      permit_number[permit_category == "existing_building_or_addition"]
    ),
    existing_building_evidence = if (any(permit_category == "existing_building_or_addition")) {
      paste(
        unique(paste0(
          permit_number[permit_category == "existing_building_or_addition"],
          " [", issue_date[permit_category == "existing_building_or_addition"], "] ",
          work_description[permit_category == "existing_building_or_addition"]
        )),
        collapse = " || "
      )
    } else {
      NA_character_
    },
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        new_construction_permit_numbers,
        new_construction_permit_statuses,
        new_construction_evidence,
        nearby_new_construction_evidence,
        demolition_evidence,
        existing_building_evidence
      ),
      ~ na_if(.x, "")
    )
  )

residential_history <- readr::read_csv(
  "../output/commercial_residential_history_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    residential_history_components,
    residential_first_tax_year,
    residential_last_tax_year,
    cards_observed_before,
    cards_observed_after,
    cards_with_old_structure_persisting,
    transition_evidence,
    residential_history_screen
  )

ground_up <- readr::read_csv(
  "../output/commercial_ground_up_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    matched_city_footprints,
    city_year_built_values,
    city_year_coverage_share,
    city_near_target_share,
    city_old_building_share
  )

completion_evidence <- candidates %>%
  left_join(permit_summary, by = "project_id", relationship = "one-to-one") %>%
  left_join(residential_history, by = "project_id", relationship = "one-to-one") %>%
  left_join(ground_up, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    across(
      c(
        issued_new_construction_permits,
        completed_new_construction_permits,
        expired_new_construction_permits,
        nearby_residential_new_construction_permits,
        demolition_permits,
        existing_building_or_addition_permits,
        residential_history_components,
        cards_observed_before,
        cards_observed_after,
        cards_with_old_structure_persisting,
        matched_city_footprints
      ),
      ~ coalesce(.x, 0L)
    ),
    later_assessor_report_after_permit = issued_new_construction_permits > 0L &
      is.finite(selected_vintage) &
      selected_vintage >= lubridate::year(latest_new_construction_issue),
    completion_evidence_status = case_when(
      issued_new_construction_permits > 0L & later_assessor_report_after_permit ~
        "issued_new_permit_and_later_assessor",
      issued_new_construction_permits > 0L ~ "issued_new_permit_only",
      cards_with_old_structure_persisting > 0L & existing_building_or_addition_permits > 0L ~
        "old_structure_with_existing_building_work",
      cards_with_old_structure_persisting > 0L ~ "old_structure_requires_review",
      city_near_target_share >= 0.8 ~ "city_building_year_support",
      observed_2021 & observed_2024 & stable_component_membership &
        !substantive_building_change & !substantive_land_change ~
        "stable_two_vintage_assessor",
      TRUE ~ "assessor_only_requires_review"
    )
  ) %>%
  arrange(desc(current_within_1500ft), completion_evidence_status, project_id)

if (anyDuplicated(completion_evidence$project_id) > 0) {
  stop("Commercial completion evidence is not unique by project.", call. = FALSE)
}

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(completion_evidence), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial completion evidence contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  completion_evidence %>%
    count(completion_evidence_status, name = "value") %>%
    transmute(section = "all", metric = completion_evidence_status, value),
  completion_evidence %>%
    filter(current_within_1500ft) %>%
    count(completion_evidence_status, name = "value") %>%
    transmute(section = "within_1500ft", metric = completion_evidence_status, value)
)

readr::write_csv(
  permit_evidence,
  "../output/commercial_completion_permit_evidence.csv"
)
readr::write_csv(
  completion_evidence,
  "../output/commercial_completion_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_completion_evidence_summary.csv"
)
