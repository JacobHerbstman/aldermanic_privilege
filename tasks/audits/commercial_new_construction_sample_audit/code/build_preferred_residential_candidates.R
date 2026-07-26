# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

single_finite_value <- function(x) {
  values <- sort(unique(x[is.finite(x)]))
  if (length(values) == 1) values else NA_real_
}

inventory <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    tieback_lineage_id = readr::col_character(),
    historical_tieback_groups = readr::col_character(),
    source_years = readr::col_character(),
    source_building_areas = readr::col_character(),
    source_land_areas = readr::col_character(),
    source_unit_counts = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

tieback_temporal <- readr::read_csv(
  "../output/residential_tieback_temporal_lineage_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    tieback_lineage_id = readr::col_character(),
    selected_component_pins = readr::col_character(),
    all_lineage_pins = readr::col_character(),
    selected_source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

multicard_cards <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

permit_links <- readr::read_csv(
  "../output/project_permit_chain_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    application_date = readr::col_date(),
    issue_date = readr::col_date(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential")

permit_units <- readr::read_csv(
  "../output/project_permit_chain_unit_mentions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_chain_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(source_family == "residential")

if (anyDuplicated(inventory$pin) > 0) {
  stop("Residential candidate inventory is not unique by PIN.", call. = FALSE)
}
if (anyDuplicated(tieback_temporal$tieback_lineage_id) > 0) {
  stop("Tieback lineage input is not unique by lineage.", call. = FALSE)
}
if (anyDuplicated(multicard_cards[c("pin", "card_num")]) > 0) {
  stop("Multicard evidence is not unique by PIN-card.", call. = FALSE)
}

permit_chain_evidence <- permit_links %>%
  group_by(project_id, permit_chain_id) %>%
  summarise(
    directly_matched_exact_pin = any(
      directly_matched & direct_match_method == "exact_pin",
      na.rm = TRUE
    ),
    directly_matched_inside_polygon = any(
      directly_matched & direct_match_method == "inside_project_polygon",
      na.rm = TRUE
    ),
    earliest_application_date = min(application_date, na.rm = TRUE),
    earliest_issue_date = min(issue_date, na.rm = TRUE),
    permit_numbers = paste(sort(unique(permit_number)), collapse = "/"),
    .groups = "drop"
  ) %>%
  mutate(
    earliest_application_date = if_else(
      is.infinite(as.numeric(earliest_application_date)),
      as.Date(NA),
      earliest_application_date
    ),
    earliest_issue_date = if_else(
      is.infinite(as.numeric(earliest_issue_date)),
      as.Date(NA),
      earliest_issue_date
    )
  )

permit_unit_evidence <- permit_units %>%
  group_by(project_id, permit_chain_id) %>%
  summarise(
    distinct_unit_counts = n_distinct(unit_count, na.rm = TRUE),
    permit_unit_count = single_finite_value(unit_count),
    permit_unit_values = paste(sort(unique(unit_count)), collapse = "/"),
    .groups = "drop"
  )

permit_chain_evidence <- permit_chain_evidence %>%
  left_join(
    permit_unit_evidence,
    by = c("project_id", "permit_chain_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    distinct_unit_counts = coalesce(distinct_unit_counts, 0L),
    permit_unit_values = coalesce(permit_unit_values, "")
  )

exact_permit_year <- permit_chain_evidence %>%
  filter(directly_matched_exact_pin) %>%
  group_by(project_id) %>%
  summarise(
    exact_permit_chains = n_distinct(permit_chain_id),
    exact_permit_chain_id = if_else(
      exact_permit_chains == 1,
      first(permit_chain_id),
      NA_character_
    ),
    exact_permit_application_date = if_else(
      exact_permit_chains == 1,
      first(earliest_application_date),
      as.Date(NA)
    ),
    exact_permit_numbers = if_else(
      exact_permit_chains == 1,
      first(permit_numbers),
      NA_character_
    ),
    .groups = "drop"
  )

inventory <- inventory %>%
  mutate(
    source_project_id = paste0("residential_", pin),
    assessor_single_family =
      str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE)) |
      type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      ),
    assessor_units = case_when(
      explicit_multifamily_one_unit_conflict ~ NA_real_,
      assessor_single_family & (is.na(num_apartments) | num_apartments == 0) ~ 1,
      TRUE ~ num_apartments
    )
  ) %>%
  left_join(
    exact_permit_year,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    exact_permit_chains = coalesce(exact_permit_chains, 0L),
    permit_year_correction =
      exact_permit_chains == 1 &
      !is.na(exact_permit_application_date) &
      year_built == lubridate::year(exact_permit_application_date) - 1L,
    preferred_year = if_else(
      permit_year_correction,
      lubridate::year(exact_permit_application_date),
      year_built
    ),
    year_source = if_else(
      permit_year_correction,
      paste0("issued_permit_chain:", exact_permit_chain_id),
      paste0("assessor_row:", row_id)
    )
  )

tieback_pin_lineage <- tieback_temporal %>%
  select(tieback_lineage_id, pin = all_lineage_pins) %>%
  tidyr::separate_longer_delim(pin, delim = "/") %>%
  filter(!is.na(pin), pin != "") %>%
  distinct(pin, tieback_lineage_id)

if (anyDuplicated(tieback_pin_lineage$pin) > 0) {
  stop("A residential PIN maps to multiple corrected tieback lineages.", call. = FALSE)
}

ordinary_candidates <- inventory %>%
  filter(
    !pin %in% tieback_pin_lineage$pin,
    review_category == "ordinary"
  ) %>%
  transmute(
    project_id = source_project_id,
    source_family = "residential",
    project_kind = "single_pin_single_card",
    component_pins = pin,
    component_count = 1L,
    construction_year = preferred_year,
    dwelling_units = assessor_units,
    building_sqft,
    land_sqft,
    class_values = class,
    source_row_ids = row_id,
    permit_chain_ids = exact_permit_chain_id,
    permit_numbers = exact_permit_numbers,
    year_source,
    units_source = paste0("assessor_row:", row_id),
    building_source = paste0("assessor_row:", row_id),
    land_source = paste0("assessor_row:", row_id),
    current_distance_m = dist_to_boundary_m,
    current_within_1500ft = within_1500ft,
    candidate_status = case_when(
      !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "missing_or_nonpositive_units",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      permit_year_correction ~ "single_exact_permit_chain_one_year_after_assessor_year",
      TRUE ~ "latest_single_card_assessor_report"
    )
  )

tieback_selected_flags <- inventory %>%
  select(-tieback_lineage_id) %>%
  inner_join(
    tieback_pin_lineage,
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    has_commercial_overlap = any(in_commercial_source),
    has_class_297 = any(class == "297", na.rm = TRUE),
    has_multicard = any(pin_is_multicard | maximum_concurrent_cards > 1, na.rm = TRUE),
    selected_assessor_unit_values = n_distinct(assessor_units, na.rm = TRUE),
    assessor_units = single_finite_value(assessor_units),
    selected_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    .groups = "drop"
  )

tieback_candidates <- tieback_temporal %>%
  mutate(
    fallback_construction_year = purrr::map_dbl(
      candidate_construction_years,
      function(x) {
        values <- suppressWarnings(as.numeric(str_split_1(coalesce(x, ""), "/")))
        single_finite_value(values)
      }
    ),
    component_pins = coalesce(selected_component_pins, all_lineage_pins),
    component_count = if_else(
      is.na(component_pins) | component_pins == "",
      NA_integer_,
      str_count(component_pins, fixed("/")) + 1L
    ),
    construction_year = coalesce(selected_construction_year, fallback_construction_year),
    dwelling_units = selected_dwelling_units,
    building_sqft = selected_building_sqft,
    land_sqft = selected_land_sqft,
    source_row_ids = selected_source_row_ids,
    class_values = coalesce(candidate_classes, ""),
    current_within_1500ft = any_within_1500ft,
    current_distance_m = minimum_boundary_distance_m
  ) %>%
  left_join(
    tieback_selected_flags,
    by = "tieback_lineage_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    has_commercial_overlap = coalesce(has_commercial_overlap, FALSE),
    has_class_297 = coalesce(has_class_297, FALSE),
    has_multicard = coalesce(has_multicard, FALSE),
    project_id = tieback_lineage_id,
    source_family = "residential",
    project_kind = "tieback_building",
    permit_chain_ids = NA_character_,
    permit_numbers = NA_character_,
    year_source = if_else(
      temporal_status == "temporally_resolved",
      paste0("contemporaneous_tieback_snapshot:", selected_tax_year),
      NA_character_
    ),
    units_source = if_else(
      temporal_status == "temporally_resolved",
      paste0("contemporaneous_tieback_snapshot:", selected_tax_year),
      NA_character_
    ),
    building_source = if_else(
      temporal_status == "temporally_resolved",
      paste0("contemporaneous_tieback_snapshot:", selected_tax_year),
      NA_character_
    ),
    land_source = if_else(
      temporal_status == "temporally_resolved",
      paste0("sum_distinct_snapshot_component_pins:", selected_tax_year),
      NA_character_
    ),
    candidate_status = case_when(
      is.finite(construction_year) & !between(construction_year, 2006L, 2022L) ~
        "exclude_outside_period",
      has_commercial_overlap ~ "defer_to_commercial_reconciliation",
      temporal_status != "temporally_resolved" | has_class_297 | has_multicard ~
        "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      !is.finite(construction_year) ~ "tieback_construction_year_unresolved",
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      has_commercial_overlap ~ "tieback_contains_commercial_source_pin",
      has_class_297 ~ "tieback_contains_class_297",
      has_multicard ~ "tieback_contains_multicard_pin",
      temporal_status != "temporally_resolved" ~ temporal_reason,
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "missing_or_nonpositive_units",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      TRUE ~ temporal_reason
    )
  ) %>%
  select(all_of(names(ordinary_candidates)))

multicard_candidates <- multicard_cards %>%
  anti_join(tieback_pin_lineage %>% select(pin), by = "pin") %>%
  filter(!pin %in% inventory$pin[inventory$review_category == "class_297"]) %>%
  mutate(
    study_period_card = between(year_built, 2006L, 2022L),
    card_units = case_when(
      class %in% c("211", "212") ~ num_apartments,
      study_period_card ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(pin) %>%
  summarise(
    project_id = paste0("residential_multicard_", first(pin)),
    source_family = "residential",
    project_kind = "same_pin_multiple_cards",
    component_pins = first(pin),
    component_count = 1L,
    study_cards = sum(study_period_card),
    study_year_values = n_distinct(year_built[study_period_card], na.rm = TRUE),
    construction_year = single_finite_value(year_built[study_period_card]),
    dwelling_units = sum(card_units[study_period_card], na.rm = TRUE),
    building_sqft = sum(building_sqft[study_period_card], na.rm = TRUE),
    land_values = n_distinct(land_sqft[study_period_card], na.rm = TRUE),
    land_sqft = single_finite_value(land_sqft[study_period_card]),
    class_values = paste(sort(unique(class[study_period_card])), collapse = "/"),
    source_row_ids = paste(sort(unique(row_id[study_period_card])), collapse = "/"),
    source_year_values = paste(sort(unique(year_built[study_period_card])), collapse = "/"),
    permit_chain_ids = NA_character_,
    permit_numbers = NA_character_,
    year_source = paste0("multicard_assessor_rows:", source_row_ids),
    units_source = paste0("card_level_rule:", source_row_ids),
    building_source = paste0("sum_study_year_cards:", source_row_ids),
    land_source = paste0("one_pin_land_once:", source_row_ids),
    current_distance_m = first(dist_to_boundary_m),
    current_within_1500ft = first(within_1500ft),
    candidate_status = case_when(
      study_cards == 0 ~ "exclude_outside_period",
      study_year_values != 1 ~ "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      study_cards == 0 ~ "no_card_built_from_2006_through_2022",
      study_year_values > 1 ~ "cards_report_multiple_study_period_construction_years",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "missing_or_nonpositive_units",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      TRUE ~ "same_year_cards_aggregated_land_counted_once"
    ),
    .groups = "drop"
  ) %>%
  select(all_of(names(ordinary_candidates)))

class_297_rows <- inventory %>%
  filter(
    class == "297",
    !pin %in% tieback_pin_lineage$pin,
    !in_commercial_source
  ) %>%
  transmute(
    source_project_id,
    pin,
    row_id,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    dist_to_boundary_m,
    within_1500ft
  )

class_297_direct_chains <- permit_links %>%
  filter(
    project_id %in% class_297_rows$source_project_id,
    directly_matched
  ) %>%
  distinct(project_id, permit_chain_id)

if (nrow(class_297_direct_chains) > 0) {
  class_297_graph <- igraph::graph_from_data_frame(
    class_297_direct_chains %>%
      transmute(from = paste0("project:", project_id), to = paste0("chain:", permit_chain_id)),
    directed = FALSE
  )
  class_297_membership <- igraph::components(class_297_graph)$membership
  class_297_nodes <- tibble::tibble(
    node = names(class_297_membership),
    graph_component = as.integer(class_297_membership)
  )
  class_297_project_groups <- class_297_nodes %>%
    filter(str_starts(node, "project:")) %>%
    transmute(
      source_project_id = str_remove(node, "^project:"),
      graph_component
    ) %>%
    group_by(graph_component) %>%
    mutate(
      minimum_pin = min(str_remove(source_project_id, "^residential_")),
      class_297_group_id = paste0("residential_297_group_", minimum_pin)
    ) %>%
    ungroup() %>%
    select(-minimum_pin)
} else {
  class_297_project_groups <- tibble::tibble(
    source_project_id = character(),
    graph_component = integer(),
    class_297_group_id = character()
  )
}

class_297_rows <- class_297_rows %>%
  left_join(
    class_297_project_groups,
    by = "source_project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    class_297_group_id = coalesce(
      class_297_group_id,
      paste0("residential_297_group_", pin)
    )
  )

class_297_group_chains <- class_297_rows %>%
  select(class_297_group_id, source_project_id) %>%
  left_join(
    class_297_direct_chains,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-many"
  ) %>%
  filter(!is.na(permit_chain_id)) %>%
  group_by(class_297_group_id) %>%
  summarise(
    permit_chains = n_distinct(permit_chain_id),
    permit_chain_ids = paste(sort(unique(permit_chain_id)), collapse = "/"),
    .groups = "drop"
  )

class_297_group_units <- class_297_rows %>%
  select(class_297_group_id, source_project_id) %>%
  left_join(
    permit_unit_evidence,
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-many"
  ) %>%
  filter(!is.na(permit_chain_id)) %>%
  group_by(class_297_group_id) %>%
  summarise(
    permit_unit_values = paste(
      sort(unique(permit_unit_count[is.finite(permit_unit_count)])),
      collapse = "/"
    ),
    distinct_permit_unit_values = n_distinct(permit_unit_count, na.rm = TRUE),
    permit_unit_count = single_finite_value(permit_unit_count),
    .groups = "drop"
  )

class_297_candidates <- class_297_rows %>%
  group_by(class_297_group_id) %>%
  summarise(
    project_id = first(class_297_group_id),
    source_family = "residential",
    project_kind = "class_297",
    component_pins = paste(sort(unique(pin)), collapse = "/"),
    component_count = n_distinct(pin),
    year_values = n_distinct(year_built, na.rm = TRUE),
    construction_year = single_finite_value(year_built),
    assessor_unit_values = n_distinct(num_apartments, na.rm = TRUE),
    assessor_units = single_finite_value(num_apartments),
    building_values = n_distinct(building_sqft, na.rm = TRUE),
    building_sqft = if_else(
      component_count == 1,
      first(building_sqft),
      NA_real_
    ),
    land_values = n_distinct(land_sqft, na.rm = TRUE),
    land_sqft = if_else(
      component_count == 1,
      first(land_sqft),
      NA_real_
    ),
    class_values = "297",
    source_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    current_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    current_within_1500ft = any(within_1500ft %in% TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    current_distance_m = if_else(is.infinite(current_distance_m), NA_real_, current_distance_m)
  ) %>%
  left_join(
    class_297_group_chains,
    by = "class_297_group_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    class_297_group_units,
    by = "class_297_group_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    permit_chains = coalesce(permit_chains, 0L),
    distinct_permit_unit_values = coalesce(distinct_permit_unit_values, 0L),
    dwelling_units = case_when(
      component_count == 1 & is.finite(assessor_units) & assessor_units > 0 ~ assessor_units,
      component_count == 1 & permit_chains == 1 &
        distinct_permit_unit_values == 1 ~ permit_unit_count,
      TRUE ~ NA_real_
    ),
    permit_numbers = NA_character_,
    year_source = paste0("class_297_assessor_rows:", source_row_ids),
    units_source = case_when(
      component_count == 1 & is.finite(assessor_units) & assessor_units > 0 ~
        paste0("class_297_assessor_row:", source_row_ids),
      component_count == 1 & permit_chains == 1 & distinct_permit_unit_values == 1 ~
        paste0("issued_permit_chain:", permit_chain_ids),
      TRUE ~ NA_character_
    ),
    building_source = if_else(
      component_count == 1,
      paste0("class_297_assessor_row:", source_row_ids),
      NA_character_
    ),
    land_source = if_else(
      component_count == 1,
      paste0("class_297_assessor_row:", source_row_ids),
      NA_character_
    ),
    candidate_status = case_when(
      !between(construction_year, 2006L, 2022L) ~ "exclude_outside_period",
      component_count > 1 ~ "review_required",
      distinct_permit_unit_values > 1 ~ "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      component_count > 1 ~ "class_297_pins_share_permit_chain",
      distinct_permit_unit_values > 1 ~ "conflicting_permit_unit_mentions",
      !is.finite(dwelling_units) | dwelling_units <= 0 ~ "class_297_units_unresolved",
      !is.finite(building_sqft) | building_sqft <= 0 ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      is.finite(assessor_units) & assessor_units > 0 ~ "class_297_assessor_units_available",
      TRUE ~ "class_297_single_permit_unit_count"
    )
  ) %>%
  select(all_of(names(ordinary_candidates)))

commercial_overlap_candidates <- inventory %>%
  filter(
    !pin %in% tieback_pin_lineage$pin,
    in_commercial_source
  ) %>%
  transmute(
    project_id = paste0("residential_overlap_", pin),
    source_family = "residential",
    project_kind = "residential_commercial_overlap",
    component_pins = pin,
    component_count = 1L,
    construction_year = preferred_year,
    dwelling_units = assessor_units,
    building_sqft,
    land_sqft,
    class_values = class,
    source_row_ids = row_id,
    permit_chain_ids = exact_permit_chain_id,
    permit_numbers = exact_permit_numbers,
    year_source,
    units_source = paste0("assessor_row:", row_id),
    building_source = paste0("assessor_row:", row_id),
    land_source = paste0("assessor_row:", row_id),
    current_distance_m = dist_to_boundary_m,
    current_within_1500ft = within_1500ft,
    candidate_status = "defer_to_commercial_reconciliation",
    decision_reason = "pin_also_appears_in_commercial_source"
  )

residential_candidates <- bind_rows(
  ordinary_candidates,
  tieback_candidates,
  multicard_candidates,
  class_297_candidates,
  commercial_overlap_candidates
) %>%
  arrange(project_kind, project_id)

if (anyDuplicated(residential_candidates$project_id) > 0) {
  stop("Preferred residential candidate IDs are not unique.", call. = FALSE)
}

component_rows <- bind_rows(
  ordinary_candidates %>%
    select(project_id, source_family, project_kind, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/"),
  tieback_candidates %>%
    select(project_id, source_family, project_kind, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/"),
  multicard_candidates %>%
    select(project_id, source_family, project_kind, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/"),
  class_297_candidates %>%
    select(project_id, source_family, project_kind, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/"),
  commercial_overlap_candidates %>%
    select(project_id, source_family, project_kind, component_pins) %>%
    tidyr::separate_longer_delim(component_pins, delim = "/")
) %>%
  rename(component_pin = component_pins) %>%
  distinct(project_id, component_pin, .keep_all = TRUE) %>%
  arrange(project_id, component_pin)

component_conflicts <- component_rows %>%
  group_by(component_pin) %>%
  summarise(projects = n_distinct(project_id), .groups = "drop") %>%
  filter(projects > 1)

if (nrow(component_conflicts) > 0) {
  stop(
    paste0(
      "Residential component PINs belong to multiple candidate projects: ",
      paste(head(component_conflicts$component_pin, 10), collapse = ", ")
    ),
    call. = FALSE
  )
}

adjudication_queue <- residential_candidates %>%
  filter(
    candidate_status %in% c(
      "review_required",
      "defer_to_commercial_reconciliation"
    ),
    is.na(construction_year) | between(construction_year, 2006L, 2022L)
  ) %>%
  select(
    project_id,
    project_kind,
    component_pins,
    component_count,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    class_values,
    source_row_ids,
    permit_chain_ids,
    permit_numbers,
    year_source,
    units_source,
    building_source,
    land_source,
    current_distance_m,
    current_within_1500ft,
    candidate_status,
    decision_reason
  )

prohibited_review_columns <- c(
  "score", "stringency", "strictness", "treatment", "more_stringent",
  "far", "dupac", "coefficient", "influence", "ward_pair"
)
if (any(str_detect(names(adjudication_queue), regex(
  paste(prohibited_review_columns, collapse = "|"),
  ignore_case = TRUE
)))) {
  stop("Residential adjudication queue contains a prohibited analysis field.", call. = FALSE)
}

summary <- bind_rows(
  residential_candidates %>%
    count(project_kind, candidate_status, decision_reason, name = "value") %>%
    transmute(
      section = "candidate_decisions",
      metric = paste(project_kind, candidate_status, decision_reason, sep = ":"),
      value
    ),
  tibble::tibble(
    section = "validation",
    metric = c(
      "candidate_projects",
      "candidate_components",
      "retained_mechanical_projects",
      "study_period_projects_requiring_review",
      "duplicate_project_ids",
      "component_pins_in_multiple_projects"
    ),
    value = c(
      nrow(residential_candidates),
      nrow(component_rows),
      sum(residential_candidates$candidate_status == "retain_mechanical"),
      nrow(adjudication_queue),
      anyDuplicated(residential_candidates$project_id),
      nrow(component_conflicts)
    )
  )
)

readr::write_csv(
  residential_candidates,
  "../output/preferred_residential_project_candidates.csv"
)
readr::write_csv(
  component_rows,
  "../output/preferred_residential_project_components.csv"
)
readr::write_csv(
  adjudication_queue,
  "../output/residential_adjudication_queue.csv"
)
readr::write_csv(
  summary,
  "../output/preferred_residential_candidate_summary.csv"
)
