# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    permit_chain_ids = readr::col_character(),
    permit_numbers = readr::col_character(),
    .default = readr::col_guess()
  )
)

vintages <- readr::read_csv(
  "../output/commercial_family_vintage_summary.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    component_pin_list = readr::col_character(),
    source_units = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  select(project_family_id, valuation_year, reported_units) %>%
  tidyr::pivot_wider(
    names_from = valuation_year,
    values_from = reported_units,
    names_prefix = "units_"
  )

permit_projects <- readr::read_csv(
  "../output/commercial_permit_project_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(
    project_id,
    permit_addresses,
    addresses_with_unit_counts,
    addresses_with_resolved_units,
    resolved_address_unit_sum
  )

permit_addresses <- readr::read_csv(
  "../output/commercial_permit_address_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    address_key = readr::col_character(),
    unit_counts = readr::col_character(),
    unit_contexts = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  group_by(project_id) %>%
  summarise(
    resolved_permit_unit_values = paste(
      sort(unique(resolved_address_units[is.finite(resolved_address_units)])),
      collapse = "/"
    ),
    resolved_permit_address_evidence = paste(
      paste0(
        address_key[is.finite(resolved_address_units)],
        "=", resolved_address_units[is.finite(resolved_address_units)]
      ),
      collapse = " | "
    ),
    .groups = "drop"
  )

permit_chains <- readr::read_csv(
  "../output/commercial_permit_chain_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    permit_numbers = readr::col_character(),
    permit_addresses = readr::col_character(),
    work_descriptions = readr::col_character(),
    unit_counts = readr::col_character(),
    unit_contexts = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(distinct_unit_counts > 0) %>%
  group_by(project_id) %>%
  summarise(
    unit_permit_chain_ids = paste(permit_chain_id, collapse = " / "),
    unit_permit_numbers = paste(permit_numbers, collapse = " / "),
    unit_permit_addresses = paste(permit_addresses, collapse = " / "),
    unit_permit_counts = paste(unit_counts, collapse = " / "),
    unit_permit_contexts = paste(unit_contexts, collapse = " || "),
    unit_permit_descriptions = paste(work_descriptions, collapse = " || "),
    .groups = "drop"
  )

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Commercial candidates are not unique by project.", call. = FALSE)
}
if (anyDuplicated(vintages$project_family_id) > 0) {
  stop("Commercial vintage counts are not unique by project.", call. = FALSE)
}
if (anyDuplicated(permit_projects$project_id) > 0) {
  stop("Commercial permit summaries are not unique by project.", call. = FALSE)
}

unit_evidence <- candidates %>%
  left_join(
    vintages,
    by = c("project_id" = "project_family_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(permit_projects, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_addresses, by = "project_id", relationship = "one-to-one") %>%
  left_join(permit_chains, by = "project_id", relationship = "one-to-one") %>%
  mutate(
    permit_addresses = coalesce(permit_addresses, 0L),
    addresses_with_unit_counts = coalesce(addresses_with_unit_counts, 0L),
    addresses_with_resolved_units = coalesce(addresses_with_resolved_units, 0L),
    stable_two_vintages =
      is.finite(units_2021) & is.finite(units_2024) & units_2021 == units_2024,
    changed_two_vintages =
      is.finite(units_2021) & is.finite(units_2024) & units_2021 != units_2024,
    only_2021 = is.finite(units_2021) & !is.finite(units_2024),
    only_2024 = !is.finite(units_2021) & is.finite(units_2024),
    permit_values = str_split(coalesce(resolved_permit_unit_values, ""), "/"),
    permit_supports_2021 = map2_lgl(
      units_2021,
      permit_values,
      ~ is.finite(.x) && any(suppressWarnings(as.numeric(.y)) == .x, na.rm = TRUE)
    ) | coalesce(resolved_address_unit_sum == units_2021, FALSE),
    permit_supports_2024 = map2_lgl(
      units_2024,
      permit_values,
      ~ is.finite(.x) && any(suppressWarnings(as.numeric(.y)) == .x, na.rm = TRUE)
    ) | coalesce(resolved_address_unit_sum == units_2024, FALSE),
    source_count_pattern = case_when(
      stable_two_vintages ~ "stable_2021_2024",
      changed_two_vintages ~ "changed_2021_2024",
      only_2024 ~ "2024_only",
      only_2021 ~ "2021_only",
      TRUE ~ "source_units_missing"
    ),
    recommended_units = case_when(
      student_housing ~ NA_real_,
      stable_two_vintages ~ units_2024,
      changed_two_vintages & permit_supports_2024 & !permit_supports_2021 ~ units_2024,
      changed_two_vintages & !permit_supports_2021 ~ units_2024,
      only_2024 ~ units_2024,
      only_2021 ~ units_2021,
      TRUE ~ NA_real_
    ),
    recommended_units_source = case_when(
      student_housing ~ NA_character_,
      stable_two_vintages ~ "stable_commercial_2021_2024_reports",
      changed_two_vintages & permit_supports_2024 & !permit_supports_2021 ~
        "commercial_2024_report_supported_by_issued_permit",
      changed_two_vintages & !permit_supports_2021 ~
        "commercial_2024_primary_report",
      only_2024 ~ "commercial_2024_only_report",
      only_2021 ~ "commercial_2021_only_report",
      TRUE ~ NA_character_
    ),
    unit_review_required = !is.finite(recommended_units) | recommended_units <= 0,
    unit_review_reason = case_when(
      !unit_review_required ~ "unit_count_resolved",
      student_housing ~ "student_housing_beds_and_dwelling_units_require_reconciliation",
      changed_two_vintages & permit_supports_2021 & permit_supports_2024 ~
        "permits_support_both_conflicting_vintages",
      changed_two_vintages & permit_supports_2021 & !permit_supports_2024 ~
        "permit_supports_earlier_count_against_2024_report",
      changed_two_vintages ~ "conflicting_vintage_counts_require_review",
      TRUE ~ "source_unit_count_missing"
    )
  ) %>%
  select(-permit_values) %>%
  arrange(project_id)

unit_review <- unit_evidence %>%
  filter(
    unit_review_required,
    between(construction_year, 2006L, 2022L)
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(unit_review), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Commercial unit review contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  unit_evidence %>%
    count(source_count_pattern, unit_review_required, unit_review_reason, name = "value") %>%
    transmute(
      metric = paste(
        "all", source_count_pattern, unit_review_required, unit_review_reason, sep = ":"
      ),
      value
    ),
  unit_evidence %>%
    filter(current_within_1500ft, between(construction_year, 2006L, 2022L)) %>%
    count(source_count_pattern, unit_review_required, unit_review_reason, name = "value") %>%
    transmute(
      metric = paste(
        "within_1500ft", source_count_pattern, unit_review_required,
        unit_review_reason, sep = ":"
      ),
      value
    )
)

readr::write_csv(
  unit_evidence,
  "../output/commercial_unit_adjudication_evidence.csv"
)
readr::write_csv(
  unit_review,
  "../output/commercial_unit_adjudication_queue.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_unit_adjudication_summary.csv"
)
