# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

entities <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    raw_row = readr::col_integer(),
    keypin = readr::col_character(),
    pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

components <- readr::read_csv(
  "../output/commercial_entity_component_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    raw_row = readr::col_integer(),
    keypin = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

if (anyDuplicated(entities$raw_row) > 0) {
  stop("Commercial entity source rows are not unique.", call. = FALSE)
}
if (anyDuplicated(components[c("raw_row", "component_pin")]) > 0) {
  stop("Commercial source-row component links are not unique.", call. = FALSE)
}
if (any(!components$raw_row %in% entities$raw_row)) {
  stop("A commercial component refers to an unknown source row.", call. = FALSE)
}

entity_components <- components %>%
  group_by(project_family_id, raw_row) %>%
  summarise(
    component_count = n_distinct(component_pin),
    component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    .groups = "drop"
  )

entity_versions <- entities %>%
  left_join(
    entity_components,
    by = c("project_family_id", "raw_row"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    entity_version_id = paste0("commercial_source_row_", raw_row),
    complete_units = is.finite(reported_units) & reported_units > 0,
    complete_building = is.finite(bldgsf) & bldgsf > 0,
    complete_land = is.finite(landsf) & landsf > 0,
    complete_year = is.finite(yearbuilt) & between(yearbuilt, 1800, 2025),
    normalized_address = str_squish(str_to_upper(coalesce(address, "")))
  ) %>%
  arrange(project_family_id, valuation_year, raw_row)

within_vintage_component_use <- components %>%
  inner_join(
    entities %>% select(project_family_id, raw_row, valuation_year),
    by = c("project_family_id", "raw_row"),
    relationship = "many-to-one"
  ) %>%
  count(project_family_id, valuation_year, component_pin, name = "entity_rows")

vintage_summary <- entity_versions %>%
  group_by(project_family_id, valuation_year) %>%
  summarise(
    entity_rows = n(),
    keypins = paste(sort(unique(keypin)), collapse = "/"),
    addresses = paste(sort(unique(normalized_address[normalized_address != ""])), collapse = " / "),
    years = paste(sort(unique(yearbuilt[is.finite(yearbuilt)])), collapse = "/"),
    unit_values = paste(sort(unique(reported_units[is.finite(reported_units)])), collapse = "/"),
    building_values = paste(sort(unique(bldgsf[is.finite(bldgsf)])), collapse = "/"),
    land_values = paste(sort(unique(landsf[is.finite(landsf)])), collapse = "/"),
    units_sum = sum(reported_units, na.rm = TRUE),
    building_sum = sum(bldgsf, na.rm = TRUE),
    land_sum = sum(landsf, na.rm = TRUE),
    all_rows_have_units = all(complete_units),
    all_rows_have_building = all(complete_building),
    all_rows_have_land = all(complete_land),
    all_rows_have_year = all(complete_year),
    .groups = "drop"
  ) %>%
  left_join(
    components %>%
      inner_join(
        entities %>% select(project_family_id, raw_row, valuation_year),
        by = c("project_family_id", "raw_row"),
        relationship = "many-to-one"
      ) %>%
      group_by(project_family_id, valuation_year) %>%
      summarise(
        component_count = n_distinct(component_pin),
        component_pins = paste(sort(unique(component_pin)), collapse = "/"),
        .groups = "drop"
      ),
    by = c("project_family_id", "valuation_year"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    within_vintage_component_use %>%
      group_by(project_family_id, valuation_year) %>%
      summarise(
        component_sets_disjoint = all(entity_rows == 1L),
        maximum_component_reuse = max(entity_rows),
        .groups = "drop"
      ),
    by = c("project_family_id", "valuation_year"),
    relationship = "one-to-one"
  )

vintage_2021 <- vintage_summary %>%
  filter(valuation_year == 2021) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2021"), -project_family_id)

vintage_2024 <- vintage_summary %>%
  filter(valuation_year == 2024) %>%
  select(-valuation_year) %>%
  rename_with(~ paste0(.x, "_2024"), -project_family_id)

partition_evidence <- full_join(
  vintage_2021,
  vintage_2024,
  by = "project_family_id",
  relationship = "one-to-one"
) %>%
  left_join(
    candidates %>%
      select(
        project_id,
        construction_year,
        current_within_1500ft,
        candidate_status,
        decision_reason
      ),
    by = c("project_family_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    observed_2021 = !is.na(entity_rows_2021),
    observed_2024 = !is.na(entity_rows_2024),
    component_union_equal =
      observed_2021 & observed_2024 & component_pins_2021 == component_pins_2024,
    split_2021_complete =
      entity_rows_2021 > 1 & component_sets_disjoint_2021 &
      all_rows_have_units_2021 & all_rows_have_building_2021 &
      all_rows_have_land_2021 & all_rows_have_year_2021,
    split_2024_complete =
      entity_rows_2024 > 1 & component_sets_disjoint_2024 &
      all_rows_have_units_2024 & all_rows_have_building_2024 &
      all_rows_have_land_2024 & all_rows_have_year_2024,
    preferred_structure = case_when(
      split_2024_complete & entity_rows_2024 > entity_rows_2021 ~ "split_2024_entity_rows",
      split_2021_complete & entity_rows_2021 > entity_rows_2024 ~ "split_2021_entity_rows",
      entity_rows_2024 == 1 & entity_rows_2021 == 1 ~ "single_physical_entity",
      observed_2024 & !observed_2021 & entity_rows_2024 == 1 ~ "single_2024_entity",
      observed_2021 & !observed_2024 & entity_rows_2021 == 1 ~ "single_2021_entity",
      TRUE ~ "manual_structure_review"
    ),
    structure_reason = case_when(
      preferred_structure == "split_2024_entity_rows" ~
        "2024 has a complete disjoint building-level partition",
      preferred_structure == "split_2021_entity_rows" ~
        "2021 has a complete disjoint building-level partition later rolled up",
      preferred_structure == "single_physical_entity" & component_union_equal ~
        "one entity row in each vintage with the same component union",
      preferred_structure == "single_physical_entity" ~
        "one entity row in each vintage; parcel lineage changed",
      preferred_structure == "single_2024_entity" ~ "only one 2024 entity row",
      preferred_structure == "single_2021_entity" ~ "only one 2021 entity row",
      TRUE ~ "vintage rows do not form a complete disjoint partition"
    )
  ) %>%
  arrange(desc(current_within_1500ft), project_family_id)

if (anyDuplicated(partition_evidence$project_family_id) > 0) {
  stop("Commercial partition evidence is not unique by family.", call. = FALSE)
}

readr::write_csv(
  entity_versions,
  "../output/commercial_entity_version_evidence.csv"
)
readr::write_csv(
  partition_evidence,
  "../output/commercial_entity_partition_evidence.csv"
)
readr::write_csv(
  bind_rows(
    partition_evidence %>%
      count(preferred_structure, name = "value") %>%
      transmute(section = "all", metric = preferred_structure, value),
    partition_evidence %>%
      filter(current_within_1500ft) %>%
      count(preferred_structure, name = "value") %>%
      transmute(section = "within_1500ft", metric = preferred_structure, value)
  ),
  "../output/commercial_entity_partition_summary.csv"
)
