# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

resolution <- readr::read_csv(
  "../output/commercial_post_evidence_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

entity_versions <- readr::read_csv(
  "../output/commercial_entity_version_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_family_id = readr::col_character(),
    keypin = readr::col_character(),
    pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

component_decisions <- readr::read_csv(
  "../adjudication/commercial_component_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_ids = readr::col_character(),
    source_rows = readr::col_character(),
    final_project_id = readr::col_character(),
    retained_component_pins = readr::col_character(),
    excluded_component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

field_decisions <- readr::read_csv(
  "../adjudication/commercial_field_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

semantic_decisions <- readr::read_csv(
  "../adjudication/commercial_semantic_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

manual_decisions <- readr::read_csv(
  "../adjudication/commercial_manual_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_ids = readr::col_character(),
    preferred_source_rows = readr::col_character(),
    final_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  mutate(manual_decision_id = row_number())

cross_family_decisions <- readr::read_csv(
  "../adjudication/commercial_cross_family_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_project_ids = readr::col_character(),
    final_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(candidates$project_id) > 0) {
  stop("Commercial candidates are not unique by source project.", call. = FALSE)
}
if (anyDuplicated(resolution$project_id) > 0) {
  stop("Commercial resolution rows are not unique by source project.", call. = FALSE)
}
if (!setequal(candidates$project_id, resolution$project_id)) {
  stop("Commercial candidates and resolution rows do not cover the same projects.", call. = FALSE)
}
if (anyDuplicated(entity_versions$raw_row) > 0) {
  stop("Commercial source rows are not unique by raw row number.", call. = FALSE)
}
if (anyDuplicated(component_decisions$decision_group_id) > 0) {
  stop("Component decision IDs are not unique.", call. = FALSE)
}
if (anyDuplicated(field_decisions$project_id) > 0) {
  stop("Field decisions are not unique by source project.", call. = FALSE)
}
if (anyDuplicated(semantic_decisions$project_id) > 0) {
  stop("Semantic decisions are not unique by source project.", call. = FALSE)
}
if (any(component_decisions$status != "reviewed_evidence") ||
    any(field_decisions$status != "reviewed_evidence") ||
    any(semantic_decisions$status != "reviewed_evidence") ||
    any(manual_decisions$status != "reviewed_evidence") ||
    any(cross_family_decisions$status != "reviewed_evidence")) {
  stop("Every adjudication row must have reviewed_evidence status.", call. = FALSE)
}

component_coverage <- component_decisions %>%
  select(decision_group_id, source_project_ids, action) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = "/") %>%
  mutate(source_project_ids = str_trim(source_project_ids)) %>%
  group_by(project_id = source_project_ids) %>%
  summarise(
    component_action_count = n_distinct(action),
    component_action = first(action),
    component_decision_ids = paste(sort(unique(decision_group_id)), collapse = "/"),
    .groups = "drop"
  )

manual_coverage <- manual_decisions %>%
  select(source_project_ids, action) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = ";") %>%
  mutate(source_project_ids = str_trim(source_project_ids)) %>%
  group_by(project_id = source_project_ids) %>%
  summarise(
    manual_action_count = n_distinct(action),
    manual_action = first(action),
    .groups = "drop"
  )

if (any(component_coverage$component_action_count != 1)) {
  stop("A source project has conflicting component decisions.", call. = FALSE)
}
if (any(manual_coverage$manual_action_count != 1)) {
  stop("A source project has conflicting manual decisions.", call. = FALSE)
}

decision_ids <- unique(c(
  component_coverage$project_id,
  field_decisions$project_id,
  semantic_decisions$project_id,
  manual_coverage$project_id
))
if (!all(decision_ids %in% candidates$project_id)) {
  stop("An adjudication row references a project outside the candidate universe.", call. = FALSE)
}

decision_map <- resolution %>%
  select(
    project_id,
    construction_year,
    candidate_status,
    resolution_status,
    resolution_reason,
    stable_assessor_after_permit,
    exact_land_recovery
  ) %>%
  left_join(
    component_coverage %>% select(-component_action_count),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    field_decisions %>% select(project_id, field_action = action),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    semantic_decisions %>% select(project_id, semantic_action = action),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    manual_coverage %>% select(-manual_action_count),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    decision_source = case_when(
      !is.na(component_action) ~ "component_ledger",
      !is.na(field_action) ~ "field_ledger",
      !is.na(semantic_action) ~ "semantic_ledger",
      !is.na(manual_action) ~ "manual_ledger",
      resolution_status == "evidence_rule_complete" ~ "evidence_rule",
      candidate_status == "retain_mechanical" ~ "mechanical_rule",
      !between(construction_year, 2006, 2022) ~ "period_rule",
      TRUE ~ "unresolved"
    ),
    selected_action = case_when(
      decision_source == "component_ledger" ~ component_action,
      decision_source == "field_ledger" ~ field_action,
      decision_source == "semantic_ledger" ~ semantic_action,
      decision_source == "manual_ledger" ~ manual_action,
      decision_source == "evidence_rule" ~ "retain_evidence_rule",
      decision_source == "mechanical_rule" ~ "retain_mechanical",
      decision_source == "period_rule" ~ "exclude_outside_period",
      TRUE ~ "unresolved"
    )
  )

if (any(decision_map$decision_source == "unresolved")) {
  stop("At least one commercial source project remains unresolved.", call. = FALSE)
}
if (any(
  decision_map$semantic_action == "resolved_by_component_ledger" &
    decision_map$decision_source != "component_ledger",
  na.rm = TRUE
)) {
  stop("A semantic row defers to a missing component decision.", call. = FALSE)
}

component_addresses <- component_decisions %>%
  select(decision_group_id, source_project_ids) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = "/") %>%
  mutate(source_project_ids = str_trim(source_project_ids)) %>%
  left_join(
    candidates %>% select(project_id, selected_source_addresses),
    by = c("source_project_ids" = "project_id"),
    relationship = "many-to-one"
  ) %>%
  group_by(decision_group_id) %>%
  summarise(
    selected_source_addresses = paste(
      sort(unique(selected_source_addresses)),
      collapse = " / "
    ),
    .groups = "drop"
  )

component_rows <- component_decisions %>%
  filter(str_detect(action, "^(retain|merge)")) %>%
  filter(action != "split_to_source_rows") %>%
  left_join(component_addresses, by = "decision_group_id", relationship = "one-to-one") %>%
  transmute(
    project_id = final_project_id,
    source_project_ids,
    source_row_ids = as.character(source_rows),
    selected_source_addresses,
    component_pins = retained_component_pins,
    construction_year = as.integer(final_year),
    dwelling_units = as.numeric(final_units),
    building_sqft = as.numeric(final_building_sqft),
    land_sqft = as.numeric(final_land_sqft),
    allow_far,
    allow_dupac,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "component_ledger",
    decision_action = action,
    decision_id = decision_group_id,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  )

split_rows <- component_decisions %>%
  filter(action == "split_to_source_rows") %>%
  select(
    decision_group_id,
    source_project_ids,
    source_rows,
    action,
    allow_far,
    allow_dupac,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  ) %>%
  tidyr::separate_longer_delim(source_rows, delim = "/") %>%
  mutate(source_rows = as.numeric(source_rows)) %>%
  left_join(
    entity_versions,
    by = c("source_rows" = "raw_row"),
    relationship = "many-to-one"
  )

if (any(is.na(split_rows$keypin))) {
  stop("A selected split source row is missing from the commercial source inventory.", call. = FALSE)
}
if (any(split_rows$project_family_id != split_rows$source_project_ids)) {
  stop("A selected split source row belongs to a different project family.", call. = FALSE)
}

split_rows <- split_rows %>%
  mutate(
    component_pins = vapply(
      strsplit(pins, ",\\s*"),
      function(x) paste(sort(unique(str_remove_all(x, "[^0-9]"))), collapse = "/"),
      character(1)
    )
  ) %>%
  transmute(
    project_id = paste0("commercial_", keypin),
    source_project_ids,
    source_row_ids = as.character(source_rows),
    selected_source_addresses = address,
    component_pins,
    construction_year = as.integer(yearbuilt),
    dwelling_units = as.numeric(reported_units),
    building_sqft = as.numeric(bldgsf),
    land_sqft = as.numeric(landsf),
    allow_far,
    allow_dupac,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "component_ledger",
    decision_action = action,
    decision_id = decision_group_id,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  )

candidate_fields <- candidates %>%
  select(
    project_id,
    selected_source_addresses,
    component_pins,
    source_row_ids,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft
  )

field_rows <- field_decisions %>%
  semi_join(
    decision_map %>% filter(decision_source == "field_ledger"),
    by = "project_id"
  ) %>%
  left_join(candidate_fields, by = "project_id", relationship = "one-to-one") %>%
  transmute(
    project_id,
    source_project_ids = project_id,
    source_row_ids,
    selected_source_addresses,
    component_pins,
    construction_year = as.integer(final_year),
    dwelling_units = as.numeric(final_units),
    building_sqft = as.numeric(final_building_sqft),
    land_sqft = as.numeric(final_land_sqft),
    allow_far,
    allow_dupac,
    membership_source = "preferred_candidate_membership",
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "field_ledger",
    decision_action = action,
    decision_id = project_id,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  )

semantic_rows <- semantic_decisions %>%
  semi_join(
    decision_map %>% filter(decision_source == "semantic_ledger"),
    by = "project_id"
  ) %>%
  filter(str_detect(action, "^retain")) %>%
  left_join(candidate_fields, by = "project_id", relationship = "one-to-one") %>%
  transmute(
    project_id,
    source_project_ids = project_id,
    source_row_ids,
    selected_source_addresses,
    component_pins,
    construction_year = as.integer(final_year),
    dwelling_units = as.numeric(final_units),
    building_sqft = as.numeric(final_building_sqft),
    land_sqft = as.numeric(final_land_sqft),
    allow_far,
    allow_dupac,
    membership_source = "preferred_candidate_membership",
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "semantic_ledger",
    decision_action = action,
    decision_id = project_id,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  )

selected_manual_decisions <- manual_decisions %>%
  select(manual_decision_id, source_project_ids) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = ";") %>%
  mutate(source_project_ids = str_trim(source_project_ids)) %>%
  left_join(
    decision_map %>% select(project_id, decision_source),
    by = c("source_project_ids" = "project_id"),
    relationship = "many-to-one"
  ) %>%
  group_by(manual_decision_id) %>%
  summarise(
    all_sources_select_manual = all(decision_source == "manual_ledger"),
    .groups = "drop"
  ) %>%
  filter(all_sources_select_manual)

manual_rows <- manual_decisions %>%
  semi_join(selected_manual_decisions, by = "manual_decision_id") %>%
  filter(str_detect(action, "^(retain|merge)")) %>%
  left_join(
    candidate_fields,
    by = c("final_project_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  transmute(
    project_id = final_project_id,
    source_project_ids = str_replace_all(source_project_ids, ";", "/"),
    source_row_ids = as.character(preferred_source_rows),
    selected_source_addresses,
    component_pins,
    construction_year = as.integer(final_year),
    dwelling_units = as.numeric(final_units),
    building_sqft = as.numeric(final_building_sqft),
    land_sqft = as.numeric(final_land_sqft),
    allow_far,
    allow_dupac,
    membership_source = "preferred_candidate_membership",
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "manual_ledger",
    decision_action = action,
    decision_id = source_project_ids,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  )

evidence_rows <- resolution %>%
  semi_join(
    decision_map %>% filter(decision_source == "evidence_rule"),
    by = "project_id"
  ) %>%
  transmute(
    project_id,
    source_project_ids = project_id,
    source_row_ids,
    selected_source_addresses,
    component_pins,
    construction_year = as.integer(construction_year),
    dwelling_units = if_else(
      stable_assessor_after_permit,
      as.numeric(recommended_units),
      as.numeric(dwelling_units)
    ),
    building_sqft = as.numeric(building_sqft),
    land_sqft = if_else(
      exact_land_recovery,
      as.numeric(project_land_area_sqft),
      as.numeric(land_sqft)
    ),
    allow_far = is.finite(building_sqft) & building_sqft > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    allow_dupac = is.finite(dwelling_units) & dwelling_units > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    membership_source = "preferred_candidate_membership",
    year_source,
    units_source = if_else(
      stable_assessor_after_permit,
      recommended_units_source,
      units_source
    ),
    building_source,
    land_source = if_else(
      exact_land_recovery,
      "construction_year_parcel_union",
      land_source
    ),
    decision_source = "evidence_rule",
    decision_action = "retain_evidence_rule",
    decision_id = project_id,
    confidence = "high",
    evidence_ids = resolution_reason,
    evidence_urls = NA_character_,
    decision_reason = resolution_reason,
    unresolved_caveat = NA_character_
  )

mechanical_rows <- candidates %>%
  semi_join(
    decision_map %>% filter(decision_source == "mechanical_rule"),
    by = "project_id"
  ) %>%
  transmute(
    project_id,
    source_project_ids = project_id,
    source_row_ids,
    selected_source_addresses,
    component_pins,
    construction_year = as.integer(construction_year),
    dwelling_units = as.numeric(dwelling_units),
    building_sqft = as.numeric(building_sqft),
    land_sqft = as.numeric(land_sqft),
    allow_far = is.finite(building_sqft) & building_sqft > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    allow_dupac = is.finite(dwelling_units) & dwelling_units > 0 &
      is.finite(land_sqft) & land_sqft > 0,
    membership_source = "commercial_2024_primary_or_stable_2021_fallback",
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source = "mechanical_rule",
    decision_action = "retain_mechanical",
    decision_id = project_id,
    confidence = "high",
    evidence_ids = NA_character_,
    evidence_urls = NA_character_,
    decision_reason,
    unresolved_caveat = NA_character_
  )

preferred_projects <- bind_rows(
  component_rows,
  split_rows,
  field_rows,
  semantic_rows,
  manual_rows,
  evidence_rows,
  mechanical_rows
) %>%
  mutate(
    source_project_ids = str_replace_all(source_project_ids, ";", "/"),
    component_count = if_else(
      is.na(component_pins) | component_pins == "",
      0L,
      str_count(component_pins, fixed("/")) + 1L
    )
  ) %>%
  select(
    project_id,
    source_project_ids,
    source_row_ids,
    selected_source_addresses,
    component_pins,
    component_count,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    allow_far,
    allow_dupac,
    membership_source,
    year_source,
    units_source,
    building_source,
    land_source,
    decision_source,
    decision_action,
    decision_id,
    confidence,
    evidence_ids,
    evidence_urls,
    decision_reason,
    unresolved_caveat
  ) %>%
  arrange(project_id)

if (anyDuplicated(preferred_projects$project_id) > 0) {
  stop("Preferred commercial project IDs are not unique.", call. = FALSE)
}
if (any(!between(preferred_projects$construction_year, 2006L, 2022L))) {
  stop("A retained commercial project falls outside 2006 through 2022.", call. = FALSE)
}
if (any(preferred_projects$component_count < 1)) {
  stop("A retained commercial project has no component PIN.", call. = FALSE)
}
invalid_far <- preferred_projects %>%
  filter(
    allow_far,
    !is.finite(building_sqft) | building_sqft <= 0 |
      !is.finite(land_sqft) | land_sqft <= 0
  )
if (nrow(invalid_far) > 0) {
  stop(
    "FAR-eligible projects lack positive building or land area: ",
    paste(invalid_far$project_id, collapse = ", "),
    call. = FALSE
  )
}
invalid_dupac <- preferred_projects %>%
  filter(
    allow_dupac,
    !is.finite(dwelling_units) | dwelling_units <= 0 |
      !is.finite(land_sqft) | land_sqft <= 0
  )
if (nrow(invalid_dupac) > 0) {
  stop(
    "DUPAC-eligible projects lack positive units or land area: ",
    paste(invalid_dupac$project_id, collapse = ", "),
    call. = FALSE
  )
}
if (any(preferred_projects$allow_far &
    (is.na(preferred_projects$building_source) |
      is.na(preferred_projects$land_source)))) {
  stop("A FAR-eligible project lacks field provenance.", call. = FALSE)
}
if (any(preferred_projects$allow_dupac &
    (is.na(preferred_projects$units_source) |
      is.na(preferred_projects$land_source)))) {
  stop("A DUPAC-eligible project lacks field provenance.", call. = FALSE)
}

project_source_links <- preferred_projects %>%
  select(project_id, source_project_ids) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = "/") %>%
  mutate(source_project_ids = str_trim(source_project_ids))

if (any(!project_source_links$source_project_ids %in% candidates$project_id)) {
  stop("A retained project links to an unknown source project.", call. = FALSE)
}

source_retention <- project_source_links %>%
  group_by(source_project_id = source_project_ids) %>%
  summarise(
    retained_project_count = n_distinct(project_id),
    retained_project_ids = paste(sort(unique(project_id)), collapse = "/"),
    .groups = "drop"
  )

source_disposition <- decision_map %>%
  left_join(
    source_retention,
    by = c("project_id" = "source_project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    retained_project_count = coalesce(retained_project_count, 0L),
    disposition = case_when(
      retained_project_count > 1 ~ "retained_as_multiple_projects",
      retained_project_count == 1 ~ "retained",
      str_detect(selected_action, "unresolved") ~ "excluded_unresolved",
      str_detect(selected_action, "outside_period") ~ "excluded_outside_period",
      str_detect(selected_action, "defer_residential") ~ "excluded_replaced_by_residential",
      str_detect(selected_action, "exclude") ~ "excluded",
      TRUE ~ "invalid_no_retained_project"
    )
  ) %>%
  select(
    source_project_id = project_id,
    candidate_status,
    resolution_status,
    decision_source,
    selected_action,
    component_decision_ids,
    retained_project_count,
    retained_project_ids,
    disposition
  ) %>%
  arrange(source_project_id)

if (nrow(source_disposition) != nrow(candidates) ||
    anyDuplicated(source_disposition$source_project_id) > 0) {
  stop("Source disposition does not contain exactly one row per candidate.", call. = FALSE)
}
invalid_disposition <- source_disposition %>%
  filter(disposition == "invalid_no_retained_project")
if (nrow(invalid_disposition) > 0) {
  stop(
    "Source projects have neither a retained output nor an exclusion: ",
    paste(invalid_disposition$source_project_id, collapse = ", "),
    call. = FALSE
  )
}

component_links <- preferred_projects %>%
  select(project_id, component_pins) %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  mutate(component_pins = str_trim(component_pins))

component_conflicts <- component_links %>%
  distinct(project_id, component_pins) %>%
  count(component_pins, name = "retained_projects") %>%
  filter(retained_projects > 1)

if (nrow(component_conflicts) > 0) {
  stop("A component PIN belongs to more than one retained commercial project.", call. = FALSE)
}

cross_family_members <- cross_family_decisions %>%
  select(duplicate_review_group_id, action, source_project_ids) %>%
  tidyr::separate_longer_delim(source_project_ids, delim = "/") %>%
  mutate(source_project_ids = str_trim(source_project_ids)) %>%
  left_join(
    source_disposition %>% select(
      source_project_id,
      retained_project_count,
      retained_project_ids,
      disposition
    ),
    by = c("source_project_ids" = "source_project_id"),
    relationship = "many-to-one"
  )

cross_family_validation <- cross_family_members %>%
  group_by(duplicate_review_group_id, action) %>%
  summarise(
    source_projects = n_distinct(source_project_ids),
    retained_source_projects = n_distinct(
      source_project_ids[retained_project_count > 0]
    ),
    retained_id_count = n_distinct(
      unlist(strsplit(retained_project_ids[retained_project_count > 0], "/"))
    ),
    .groups = "drop"
  )

invalid_cross_family_merges <- cross_family_validation %>%
  filter(
    action == "merge_same_project",
    retained_source_projects != source_projects | retained_id_count != 1
  )
if (nrow(invalid_cross_family_merges) > 0) {
  stop(
    "Cross-family merges do not resolve every source to one project: ",
    paste(invalid_cross_family_merges$duplicate_review_group_id, collapse = ", "),
    call. = FALSE
  )
}

invalid_cross_family_separations <- cross_family_validation %>%
  filter(
    action == "keep_separate_projects",
    retained_id_count < retained_source_projects
  )
if (nrow(invalid_cross_family_separations) > 0) {
  stop(
    "Cross-family keep-separate decisions collapse retained sources together: ",
    paste(invalid_cross_family_separations$duplicate_review_group_id, collapse = ", "),
    call. = FALSE
  )
}

prohibited_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
analysis_columns <- setdiff(names(preferred_projects), c("allow_far", "allow_dupac"))
if (any(str_detect(
  analysis_columns,
  regex(paste(prohibited_columns, collapse = "|"), ignore_case = TRUE)
))) {
  stop("Preferred commercial ledger contains a prohibited analysis field.", call. = FALSE)
}

unresolved <- source_disposition %>%
  filter(disposition == "excluded_unresolved") %>%
  left_join(
    candidates %>% select(
      source_project_id = project_id,
      construction_year,
      selected_source_addresses,
      decision_reason
    ),
    by = "source_project_id",
    relationship = "one-to-one"
  )

validation_summary <- bind_rows(
  source_disposition %>%
    count(section = "source_disposition", metric = disposition, name = "value"),
  preferred_projects %>%
    count(section = "retained_decision_source", metric = decision_source, name = "value"),
  preferred_projects %>%
    summarise(
      section = "retained_projects",
      retained_projects = n(),
      far_eligible = sum(allow_far),
      dupac_eligible = sum(allow_dupac),
      retained_units = sum(dwelling_units[allow_dupac]),
      unresolved_exclusions = nrow(unresolved)
    ) %>%
    tidyr::pivot_longer(
      -section,
      names_to = "metric",
      values_to = "value"
    ),
  tibble::tibble(
    section = "validation",
    metric = c(
      "candidate_source_projects",
      "source_disposition_rows",
      "duplicate_retained_project_ids",
      "duplicate_retained_component_pins",
      "unresolved_decision_sources",
      "cross_family_decisions",
      "cross_family_validation_failures"
    ),
    value = c(
      nrow(candidates),
      nrow(source_disposition),
      anyDuplicated(preferred_projects$project_id),
      nrow(component_conflicts),
      sum(decision_map$decision_source == "unresolved"),
      nrow(cross_family_validation),
      0L
    )
  )
) %>%
  arrange(section, metric)

readr::write_csv(
  preferred_projects,
  "../output/preferred_commercial_projects.csv"
)
readr::write_csv(
  source_disposition,
  "../output/preferred_commercial_source_disposition.csv"
)
readr::write_csv(
  unresolved,
  "../output/preferred_commercial_unresolved.csv"
)
readr::write_csv(
  validation_summary,
  "../output/preferred_commercial_validation_summary.csv"
)
