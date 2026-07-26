# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidate_pins <- readr::read_csv(
  "../output/residential_project_candidate_inventory.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    year_built = readr::col_integer(),
    class = readr::col_character(),
    review_category = readr::col_character(),
    dist_to_boundary_m = readr::col_double(),
    within_1500ft = readr::col_logical(),
    .default = readr::col_skip()
  )
) %>%
  select(pin, year_built, class, review_category, dist_to_boundary_m, within_1500ft)

fractional_base_groups <- readr::read_csv(
  "../output/residential_fractional_base_groups.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    base_pin = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(link_fractional_siblings) %>%
  select(base_pin, component_pins)

if (anyDuplicated(candidate_pins$pin) > 0) {
  stop("Residential candidate scope is not unique by PIN.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

tieback_history <- DBI::dbGetQuery(con, "
WITH source AS (
  SELECT
    regexp_replace(trim(pin), '[^0-9]', '', 'g') AS pin,
    try_cast(numeric_text(year) AS INTEGER) AS tax_year,
    try_cast(numeric_text(card) AS INTEGER) AS card_num,
    trim(class) AS class,
    regexp_replace(trim(tieback_key_pin), '[^0-9]', '', 'g') AS tieback_group,
    try_cast(numeric_text(tieback_proration_rate) AS DOUBLE) AS pin_proration_rate,
    try_cast(numeric_text(card_proration_rate) AS DOUBLE) AS card_proration_rate,
    try_cast(numeric_text(char_yrblt) AS INTEGER) AS year_built,
    try_cast(numeric_text(char_bldg_sf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(char_land_sf) AS DOUBLE) AS land_sqft,
    trim(char_apts) AS apartments_text,
    trim(char_type_resd) AS type_of_residence,
    trim(char_use) AS single_v_multi_family,
    trim(row_id) AS row_id
  FROM read_csv(
    '../input/residential_improvement_characteristics_full.csv',
    all_varchar = true,
    header = true,
    ignore_errors = false,
    max_line_size = 10000000
  )
  WHERE try_cast(numeric_text(township_code) AS INTEGER)
        IN (70, 71, 72, 73, 74, 75, 76, 77)
)
SELECT *
FROM source
WHERE pin IS NOT NULL
  AND pin != ''
  AND tieback_group IS NOT NULL
  AND tieback_group != ''
  AND tax_year IS NOT NULL
  AND card_num IS NOT NULL
  AND row_id IS NOT NULL
  AND row_id != ''
") %>%
  mutate(
    apartments_text = str_to_lower(str_squish(apartments_text)),
    num_apartments = case_when(
      is.na(apartments_text) | apartments_text == "" ~ NA_real_,
      apartments_text %in% c("none", "zero") ~ 0,
      apartments_text == "one" ~ 1,
      apartments_text == "two" ~ 2,
      apartments_text == "three" ~ 3,
      apartments_text == "four" ~ 4,
      apartments_text == "five" ~ 5,
      apartments_text == "six" ~ 6,
      TRUE ~ suppressWarnings(as.numeric(str_replace_all(apartments_text, "[^0-9.-]", "")))
    ),
    assessor_single_family =
      str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE)) |
      type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      ),
    dwelling_units = case_when(
      is.finite(num_apartments) & num_apartments > 0 ~ num_apartments,
      assessor_single_family ~ 1,
      TRUE ~ NA_real_
    )
  )

fractional_base_edges <- fractional_base_groups %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  group_by(base_pin) %>%
  mutate(anchor_pin = min(component_pins)) %>%
  ungroup() %>%
  filter(component_pins != anchor_pin) %>%
  transmute(pin = component_pins, tieback_group = anchor_pin)

tieback_edges <- tieback_history %>%
  distinct(pin, tieback_group) %>%
  filter(pin != tieback_group) %>%
  bind_rows(fractional_base_edges) %>%
  distinct()

tieback_nodes <- sort(unique(c(tieback_history$pin, tieback_history$tieback_group)))
tieback_graph <- igraph::graph_from_data_frame(
  tieback_edges,
  directed = FALSE,
  vertices = tibble::tibble(name = tieback_nodes)
)
graph_membership <- igraph::components(tieback_graph)$membership

lineage_nodes <- tibble::tibble(
  node = names(graph_membership),
  graph_component = as.integer(graph_membership)
) %>%
  group_by(graph_component) %>%
  mutate(
    tieback_lineage_id = paste0("residential_tieback_", min(node))
  ) %>%
  ungroup()

tieback_history <- tieback_history %>%
  left_join(
    lineage_nodes %>% transmute(pin = node, tieback_lineage_id),
    by = "pin",
    relationship = "many-to-one"
  )

relevant_lineages <- tieback_history %>%
  filter(pin %in% candidate_pins$pin) %>%
  distinct(tieback_lineage_id)

tieback_history <- tieback_history %>%
  semi_join(relevant_lineages, by = "tieback_lineage_id")

lineage_scope <- tieback_history %>%
  distinct(tieback_lineage_id, pin) %>%
  inner_join(candidate_pins, by = "pin", relationship = "many-to-one") %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    candidate_member_pins = n_distinct(pin),
    candidate_study_period_pins = n_distinct(pin[between(year_built, 2006L, 2022L)]),
    candidate_construction_years = paste(
      sort(unique(year_built[is.finite(year_built)])), collapse = "/"
    ),
    candidate_classes = paste(sort(unique(class)), collapse = "/"),
    candidate_review_categories = paste(sort(unique(review_category)), collapse = "/"),
    any_within_1500ft = any(within_1500ft %in% TRUE),
    minimum_boundary_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    minimum_boundary_distance_m = if_else(
      is.infinite(minimum_boundary_distance_m), NA_real_, minimum_boundary_distance_m
    )
  )

lineage_membership <- lineage_nodes %>%
  semi_join(relevant_lineages, by = "tieback_lineage_id") %>%
  left_join(
    tieback_history %>% distinct(tieback_lineage_id, source_pin = pin) %>% mutate(has_source_row = TRUE),
    by = c("tieback_lineage_id", "node" = "source_pin"),
    relationship = "one-to-one"
  ) %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    lineage_nodes = n_distinct(node),
    source_member_pins = n_distinct(node[has_source_row %in% TRUE]),
    key_only_nodes = sum(!coalesce(has_source_row, FALSE)),
    all_lineage_pins = paste(sort(unique(node)), collapse = "/"),
    .groups = "drop"
  )

lineage_member_crosswalk <- lineage_nodes %>%
  semi_join(relevant_lineages, by = "tieback_lineage_id") %>%
  transmute(tieback_lineage_id, lineage_pin = node) %>%
  left_join(
    tieback_history %>%
      distinct(tieback_lineage_id, source_pin = pin) %>%
      mutate(has_source_row = TRUE),
    by = c("tieback_lineage_id", "lineage_pin" = "source_pin"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    candidate_pins %>%
      transmute(
        lineage_pin = pin,
        candidate_construction_year = year_built,
        candidate_class = class,
        candidate_review_category = review_category,
        candidate_distance_m = dist_to_boundary_m,
        candidate_within_1500ft = within_1500ft
      ),
    by = "lineage_pin",
    relationship = "many-to-one"
  ) %>%
  mutate(
    has_source_row = coalesce(has_source_row, FALSE),
    in_candidate_inventory = !is.na(candidate_construction_year) |
      !is.na(candidate_review_category)
  ) %>%
  arrange(tieback_lineage_id, lineage_pin)

duplicate_evidence <- tieback_history %>%
  group_by(tieback_lineage_id, pin, card_num, tax_year) %>%
  summarise(
    source_rows_for_key = n(),
    duplicate_group_values = n_distinct(tieback_group, na.rm = TRUE),
    duplicate_year_values = n_distinct(year_built, na.rm = TRUE),
    duplicate_building_values = n_distinct(building_sqft, na.rm = TRUE),
    duplicate_land_values = n_distinct(land_sqft, na.rm = TRUE),
    duplicate_unit_values = n_distinct(dwelling_units, na.rm = TRUE),
    duplicate_proration_values = n_distinct(pin_proration_rate, na.rm = TRUE),
    duplicate_key_conflict =
      duplicate_group_values > 1 |
      duplicate_year_values > 1 |
      duplicate_building_values > 1 |
      duplicate_land_values > 1 |
      duplicate_unit_values > 1 |
      duplicate_proration_values > 1,
    .groups = "drop"
  )

tieback_history <- tieback_history %>%
  arrange(tieback_lineage_id, pin, card_num, tax_year, desc(row_id)) %>%
  group_by(tieback_lineage_id, pin, card_num, tax_year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  left_join(
    duplicate_evidence,
    by = c("tieback_lineage_id", "pin", "card_num", "tax_year"),
    relationship = "one-to-one"
  )

pin_year <- tieback_history %>%
  group_by(tieback_lineage_id, tax_year, pin) %>%
  summarise(
    cards = n_distinct(card_num),
    pin_proration_values = n_distinct(pin_proration_rate, na.rm = TRUE),
    pin_proration_rate = if (pin_proration_values == 1) {
      min(pin_proration_rate[is.finite(pin_proration_rate)])
    } else {
      NA_real_
    },
    pin_land_values = n_distinct(land_sqft, na.rm = TRUE),
    pin_land_sqft = if (pin_land_values == 1) {
      min(land_sqft[is.finite(land_sqft)])
    } else {
      NA_real_
    },
    .groups = "drop"
  )

snapshot_values <- tieback_history %>%
  group_by(tieback_lineage_id, tax_year) %>%
  summarise(
    tieback_groups = paste(sort(unique(tieback_group)), collapse = "/"),
    tieback_group_count = n_distinct(tieback_group),
    member_pins = paste(sort(unique(pin)), collapse = "/"),
    member_pin_count = n_distinct(pin),
    cards = n_distinct(paste(pin, card_num, sep = ":")),
    construction_year_values = n_distinct(year_built, na.rm = TRUE),
    construction_year = if (construction_year_values == 1) {
      min(year_built[is.finite(year_built)])
    } else {
      NA_integer_
    },
    building_area_values = n_distinct(building_sqft, na.rm = TRUE),
    building_sqft = if (building_area_values == 1) {
      min(building_sqft[is.finite(building_sqft)])
    } else {
      NA_real_
    },
    unit_values = n_distinct(dwelling_units, na.rm = TRUE),
    dwelling_units = if (unit_values == 1) {
      min(dwelling_units[is.finite(dwelling_units)])
    } else {
      NA_real_
    },
    source_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    duplicate_source_keys = sum(source_rows_for_key > 1),
    duplicate_key_conflicts = sum(duplicate_key_conflict),
    .groups = "drop"
  )

snapshots <- pin_year %>%
  group_by(tieback_lineage_id, tax_year) %>%
  summarise(
    pin_proration_complete = all(
      pin_proration_values == 1 & is.finite(pin_proration_rate)
    ),
    pin_proration_sum = sum(pin_proration_rate, na.rm = TRUE),
    one_card_per_pin = all(cards == 1),
    pin_land_complete = all(
      pin_land_values == 1 & is.finite(pin_land_sqft) & pin_land_sqft > 0
    ),
    land_sqft = sum(pin_land_sqft, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    snapshot_values,
    by = c("tieback_lineage_id", "tax_year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    snapshot_mechanical =
      pin_proration_complete &
      abs(pin_proration_sum - 1) < 0.001 &
      one_card_per_pin &
      pin_land_complete &
      construction_year_values == 1 &
      is.finite(construction_year) &
      building_area_values == 1 &
      is.finite(building_sqft) & building_sqft > 0 &
      unit_values == 1 &
      is.finite(dwelling_units) & dwelling_units > 0 &
      duplicate_key_conflicts == 0,
    snapshot_review_reason = case_when(
      snapshot_mechanical ~ "complete_contemporaneous_lineage_snapshot",
      duplicate_key_conflicts > 0 ~ "conflicting_duplicate_source_rows",
      !pin_proration_complete | abs(pin_proration_sum - 1) >= 0.001 ~
        "incomplete_contemporaneous_lineage_proration",
      !one_card_per_pin ~ "multiple_cards_in_contemporaneous_lineage",
      construction_year_values != 1 | !is.finite(construction_year) ~
        "conflicting_or_missing_construction_year",
      building_area_values != 1 | !is.finite(building_sqft) | building_sqft <= 0 ~
        "conflicting_or_missing_building_area",
      unit_values != 1 | !is.finite(dwelling_units) | dwelling_units <= 0 ~
        "conflicting_or_missing_units",
      !pin_land_complete ~ "conflicting_or_missing_component_land",
      TRUE ~ "manual_review"
    )
  ) %>%
  arrange(tieback_lineage_id, tax_year)

complete_snapshots <- snapshots %>%
  filter(snapshot_mechanical) %>%
  mutate(
    selection_tier = case_when(
      tax_year <= 2022 ~ 1L,
      tax_year <= 2025 ~ 2L,
      TRUE ~ 3L
    )
  )

selected_snapshots <- complete_snapshots %>%
  group_by(tieback_lineage_id) %>%
  filter(selection_tier == min(selection_tier)) %>%
  arrange(desc(tax_year), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    tieback_lineage_id,
    selected_tax_year = tax_year,
    selected_tieback_groups = tieback_groups,
    selected_component_pins = member_pins,
    selected_component_count = member_pin_count,
    selected_construction_year = construction_year,
    selected_dwelling_units = dwelling_units,
    selected_building_sqft = building_sqft,
    selected_land_sqft = land_sqft,
    selected_source_row_ids = source_row_ids,
    selected_report_tier = selection_tier
  )

lineage_evidence <- snapshots %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    reported_tax_years = n_distinct(tax_year),
    reported_tieback_groups = n_distinct(tieback_groups),
    complete_snapshots = sum(snapshot_mechanical),
    complete_member_sets = n_distinct(member_pins[snapshot_mechanical]),
    complete_construction_year_values = n_distinct(
      construction_year[snapshot_mechanical], na.rm = TRUE
    ),
    complete_building_area_values = n_distinct(
      building_sqft[snapshot_mechanical], na.rm = TRUE
    ),
    complete_unit_values = n_distinct(
      dwelling_units[snapshot_mechanical], na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(lineage_membership, by = "tieback_lineage_id", relationship = "one-to-one") %>%
  left_join(selected_snapshots, by = "tieback_lineage_id", relationship = "one-to-one") %>%
  left_join(lineage_scope, by = "tieback_lineage_id", relationship = "one-to-one") %>%
  mutate(
    temporal_status = if_else(
      complete_snapshots > 0, "temporally_resolved", "review_required"
    ),
    temporal_reason = case_when(
      complete_snapshots == 0 ~ "no_complete_contemporaneous_lineage_snapshot",
      selected_report_tier == 3 ~ "resolved_from_first_report_after_2025",
      complete_member_sets > 1 ~ "resolved_with_membership_revision",
      complete_construction_year_values > 1 |
        complete_building_area_values > 1 |
        complete_unit_values > 1 ~ "resolved_with_assessor_field_revision",
      TRUE ~ "stable_complete_contemporaneous_lineage_snapshots"
    )
  ) %>%
  arrange(temporal_status, tieback_lineage_id)

if (anyDuplicated(lineage_evidence$tieback_lineage_id) > 0) {
  stop("Temporal tieback evidence is not unique by corrected lineage.", call. = FALSE)
}
if (any(is.na(lineage_evidence$any_within_1500ft))) {
  stop("A corrected tieback lineage has no candidate-sample scope.", call. = FALSE)
}

summary <- bind_rows(
  lineage_evidence %>%
    count(temporal_status, temporal_reason, name = "value") %>%
    transmute(
      metric = paste("lineage", temporal_status, temporal_reason, sep = ":"),
      value
    ),
  lineage_evidence %>%
    filter(any_within_1500ft) %>%
    count(temporal_status, temporal_reason, name = "value") %>%
    transmute(
      metric = paste("within_1500ft", temporal_status, temporal_reason, sep = ":"),
      value
    ),
  snapshots %>%
    count(snapshot_review_reason, name = "value") %>%
    transmute(metric = paste("snapshot", snapshot_review_reason, sep = ":"), value)
)

readr::write_csv(
  tieback_history,
  "../output/residential_tieback_temporal_rows.csv"
)
readr::write_csv(
  snapshots,
  "../output/residential_tieback_temporal_snapshots.csv"
)
readr::write_csv(
  lineage_evidence,
  "../output/residential_tieback_temporal_lineage_evidence.csv"
)
readr::write_csv(
  lineage_member_crosswalk,
  "../output/residential_tieback_corrected_lineage_members.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_temporal_summary.csv"
)
