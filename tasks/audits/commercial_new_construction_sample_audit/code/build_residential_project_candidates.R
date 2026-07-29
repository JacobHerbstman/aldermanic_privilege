# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  value <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(value == "", NA_character_, value)
}

parse_apartments <- function(x) {
  value <- str_to_lower(str_squish(as.character(x)))
  case_when(
    is.na(value) | value == "" ~ NA_real_,
    value %in% c("none", "zero") ~ 0,
    value == "one" ~ 1,
    value == "two" ~ 2,
    value == "three" ~ 3,
    value == "four" ~ 4,
    value == "five" ~ 5,
    value == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(str_replace_all(value, "[^0-9.-]", "")))
  )
}

selected <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    tax_year = readr::col_integer(),
    card_num = readr::col_integer(),
    class = readr::col_character(),
    proration_key_pin = readr::col_character(),
    pin_proration_rate = readr::col_double(),
    card_proration_rate = readr::col_double(),
    pin_is_multicard = readr::col_logical(),
    pin_num_cards = readr::col_integer(),
    pin_is_multiland = readr::col_logical(),
    pin_num_landlines = readr::col_integer(),
    year_built = readr::col_integer(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    num_apartments = readr::col_double(),
    type_of_residence = readr::col_character(),
    single_v_multi_family = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  mutate(
    tieback_group = normalize_pin(proration_key_pin),
    explicit_multifamily = coalesce(
      str_to_lower(str_squish(single_v_multi_family)) == "multi-family",
      FALSE
    ),
    residence_form_looks_single_family = coalesce(
      type_of_residence %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"),
      FALSE
    ),
    explicit_multifamily_one_unit_conflict = explicit_multifamily &
      residence_form_looks_single_family & is.na(num_apartments)
  )

if (nrow(readr::problems(selected)) > 0) {
  stop("Residential cross-section has parsing failures under explicit column types.", call. = FALSE)
}
if (anyDuplicated(selected$pin) > 0) {
  stop("Residential cross-section is not unique by PIN.", call. = FALSE)
}

coordinates <- sf::st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  sf::st_transform(3435)

coordinate_matrix <- sf::st_coordinates(coordinates)
coordinates <- coordinates %>%
  sf::st_drop_geometry() %>%
  transmute(
    pin,
    coordinate_x_3435 = coordinate_matrix[, "X"],
    coordinate_y_3435 = coordinate_matrix[, "Y"],
    coordinate_source
  )

if (anyDuplicated(coordinates$pin) > 0) {
  stop("Geocoded residential input is not unique by PIN.", call. = FALSE)
}

boundary_distance <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    dist_to_boundary_m = readr::col_double(),
    .default = readr::col_skip()
  )
)

if (anyDuplicated(boundary_distance$pin) > 0) {
  stop("Boundary-distance input is not unique by PIN.", call. = FALSE)
}

commercial_pins <- readr::read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_skip())
) %>%
  distinct(pin) %>%
  mutate(in_commercial_source = TRUE)

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(con, "selected_pins", selected %>% select(pin), overwrite = TRUE)

invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

source_query <- sprintf("
WITH source AS (
  SELECT
    regexp_replace(trim(pin), '[^0-9]', '', 'g') AS pin,
    try_cast(numeric_text(year) AS INTEGER) AS tax_year,
    try_cast(numeric_text(card) AS INTEGER) AS card_num,
    trim(class) AS class,
    regexp_replace(trim(tieback_key_pin), '[^0-9]', '', 'g') AS tieback_group,
    try_cast(numeric_text(tieback_proration_rate) AS DOUBLE) AS pin_proration_rate,
    try_cast(numeric_text(card_proration_rate) AS DOUBLE) AS card_proration_rate,
    lower(trim(pin_is_multicard)) = 'true' AS pin_is_multicard,
    try_cast(numeric_text(pin_num_cards) AS INTEGER) AS pin_num_cards,
    lower(trim(pin_is_multiland)) = 'true' AS pin_is_multiland,
    try_cast(numeric_text(pin_num_landlines) AS INTEGER) AS pin_num_landlines,
    try_cast(numeric_text(char_yrblt) AS INTEGER) AS year_built,
    try_cast(numeric_text(char_bldg_sf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(char_land_sf) AS DOUBLE) AS land_sqft,
    trim(char_apts) AS apartments_text,
    trim(char_type_resd) AS type_of_residence,
    trim(char_use) AS single_v_multi_family,
    trim(row_id) AS row_id
  FROM read_csv(%s,
                all_varchar = true,
                header = true,
                ignore_errors = false,
                max_line_size = 10000000)
  WHERE try_cast(numeric_text(township_code) AS INTEGER)
        IN (70, 71, 72, 73, 74, 75, 76, 77)
    AND trim(pin) IS NOT NULL
    AND trim(pin) != ''
    AND trim(card) IS NOT NULL
    AND trim(card) != ''
)
SELECT * FROM source
", DBI::dbQuoteString(con, "../input/residential_improvement_characteristics_full.csv"))

invisible(DBI::dbExecute(con, paste0("
CREATE TEMP TABLE tieback_edges AS
SELECT DISTINCT
  'pin:' || pin AS pin_node,
  'group:' || tieback_group AS group_node
FROM (", source_query, ")
WHERE tieback_group IS NOT NULL
  AND tieback_group != ''
UNION
SELECT DISTINCT
  'pin:' || pin AS pin_node,
  'base:' || substr(pin, 1, 10) AS group_node
FROM (", source_query, ")
WHERE length(pin) = 14
  AND right(pin, 4) != '0000';
")))

invisible(DBI::dbExecute(con, "
CREATE TEMP TABLE relevant_tieback_nodes AS
WITH RECURSIVE reached(node) AS (
  SELECT 'pin:' || pin
  FROM selected_pins
  UNION
  SELECT
    CASE
      WHEN tieback_edges.pin_node = reached.node THEN tieback_edges.group_node
      ELSE tieback_edges.pin_node
    END
  FROM reached
  INNER JOIN tieback_edges
    ON tieback_edges.pin_node = reached.node
    OR tieback_edges.group_node = reached.node
)
SELECT node FROM reached;
"))

invisible(DBI::dbExecute(con, paste0("
CREATE TEMP TABLE relevant_history AS
SELECT source.*
FROM (", source_query, ") AS source
WHERE source.pin IN (SELECT pin FROM selected_pins)
   OR 'pin:' || source.pin IN (
     SELECT node FROM relevant_tieback_nodes WHERE starts_with(node, 'pin:')
   );
")))

relevant_tieback_groups <- DBI::dbGetQuery(con, "
SELECT replace(node, 'group:', '') AS tieback_group
FROM relevant_tieback_nodes
WHERE starts_with(node, 'group:')
")$tieback_group

history <- DBI::dbGetQuery(con, "SELECT * FROM relevant_history") %>%
  mutate(
    tieback_group = if_else(tieback_group == "", NA_character_, tieback_group),
    num_apartments = parse_apartments(apartments_text)
  ) %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup()

latest_card_report_2022 <- history %>%
  filter(!is.na(tax_year), tax_year <= 2022) %>%
  arrange(pin, card_num, desc(tax_year), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_card_report_2025 <- history %>%
  filter(!is.na(tax_year), tax_year <= 2025) %>%
  anti_join(
    latest_card_report_2022 %>% select(pin, card_num),
    by = c("pin", "card_num")
  ) %>%
  arrange(pin, card_num, desc(tax_year), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_card_report_later <- history %>%
  anti_join(
    bind_rows(latest_card_report_2022, latest_card_report_2025) %>%
      select(pin, card_num),
    by = c("pin", "card_num")
  ) %>%
  arrange(pin, card_num, desc(tax_year), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_card_report <- bind_rows(
  latest_card_report_2022,
  latest_card_report_2025,
  latest_card_report_later
)

fractional_pin_reports <- latest_card_report %>%
  filter(str_length(pin) == 14, str_sub(pin, -4) != "0000") %>%
  group_by(pin) %>%
  summarise(
    base_pin = str_sub(first(pin), 1, 10),
    cards = n_distinct(card_num),
    proration_values = n_distinct(pin_proration_rate, na.rm = TRUE),
    pin_proration_rate = if (proration_values == 1) {
      min(pin_proration_rate[is.finite(pin_proration_rate)])
    } else {
      NA_real_
    },
    construction_year_values = n_distinct(year_built, na.rm = TRUE),
    construction_year = if (construction_year_values == 1) {
      min(year_built[is.finite(year_built)])
    } else {
      NA_integer_
    },
    .groups = "drop"
  )

fractional_base_groups <- fractional_pin_reports %>%
  group_by(base_pin) %>%
  summarise(
    member_pins = n_distinct(pin),
    component_pins = paste(sort(pin), collapse = "/"),
    one_card_per_pin = all(cards == 1),
    complete_fractional_proration = all(
      proration_values == 1 &
        is.finite(pin_proration_rate) &
        pin_proration_rate > 0 &
        pin_proration_rate < 1
    ),
    pin_proration_sum = sum(pin_proration_rate, na.rm = TRUE),
    construction_year_values = n_distinct(construction_year, na.rm = TRUE),
    construction_year = if (construction_year_values == 1) {
      min(construction_year[is.finite(construction_year)])
    } else {
      NA_integer_
    },
    .groups = "drop"
  ) %>%
  mutate(
    link_fractional_siblings =
      member_pins > 1 &
      one_card_per_pin &
      complete_fractional_proration &
      abs(pin_proration_sum - 1) < 0.001 &
      construction_year_values == 1 &
      is.finite(construction_year),
    linkage_reason = case_when(
      member_pins <= 1 ~ "single_unit_pin",
      !one_card_per_pin ~ "multiple_cards_for_unit_pin",
      !complete_fractional_proration ~ "missing_or_invalid_fractional_proration",
      abs(pin_proration_sum - 1) >= 0.001 ~ "fractional_proration_does_not_sum_to_one",
      construction_year_values != 1 | !is.finite(construction_year) ~
        "fractional_siblings_disagree_on_construction_year",
      TRUE ~ "complete_fractional_sibling_group"
    )
  ) %>%
  arrange(base_pin)

fractional_base_edges <- fractional_pin_reports %>%
  semi_join(
    fractional_base_groups %>% filter(link_fractional_siblings),
    by = "base_pin"
  ) %>%
  group_by(base_pin) %>%
  mutate(anchor_pin = min(pin)) %>%
  ungroup() %>%
  filter(pin != anchor_pin) %>%
  transmute(
    from = paste0("pin:", pin),
    to = paste0("pin:", anchor_pin)
  )

tieback_membership_history <- history %>%
  filter(!is.na(tieback_group), tieback_group %in% relevant_tieback_groups) %>%
  group_by(pin, tieback_group) %>%
  summarise(
    membership_first_tax_year = min(tax_year, na.rm = TRUE),
    membership_last_tax_year = max(tax_year, na.rm = TRUE),
    membership_reports = n(),
    membership_cards = n_distinct(card_num),
    .groups = "drop"
  )

latest_pin_tieback_2022 <- history %>%
  filter(!is.na(tieback_group), tieback_group %in% relevant_tieback_groups) %>%
  filter(!is.na(tax_year), tax_year <= 2022) %>%
  arrange(pin, tieback_group, desc(tax_year), card_num, desc(row_id)) %>%
  group_by(pin, tieback_group) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_pin_tieback_2025 <- history %>%
  filter(!is.na(tieback_group), tieback_group %in% relevant_tieback_groups) %>%
  filter(!is.na(tax_year), tax_year <= 2025) %>%
  anti_join(
    latest_pin_tieback_2022 %>% select(pin, tieback_group),
    by = c("pin", "tieback_group")
  ) %>%
  arrange(pin, tieback_group, desc(tax_year), card_num, desc(row_id)) %>%
  group_by(pin, tieback_group) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_pin_tieback_later <- history %>%
  filter(!is.na(tieback_group), tieback_group %in% relevant_tieback_groups) %>%
  anti_join(
    bind_rows(latest_pin_tieback_2022, latest_pin_tieback_2025) %>%
      select(pin, tieback_group),
    by = c("pin", "tieback_group")
  ) %>%
  arrange(pin, tieback_group, desc(tax_year), card_num, desc(row_id)) %>%
  group_by(pin, tieback_group) %>%
  slice_head(n = 1) %>%
  ungroup()

latest_pin_tieback <- bind_rows(
  latest_pin_tieback_2022,
  latest_pin_tieback_2025,
  latest_pin_tieback_later
) %>%
  left_join(
    tieback_membership_history,
    by = c("pin", "tieback_group"),
    relationship = "one-to-one"
  ) %>%
  group_by(pin) %>%
  mutate(historical_tieback_groups_for_pin = n_distinct(tieback_group)) %>%
  ungroup()

historical_tieback_flags <- history %>%
  filter(pin %in% selected$pin, !is.na(tieback_group)) %>%
  group_by(pin) %>%
  summarise(
    historical_tieback_membership = TRUE,
    historical_tieback_groups = paste(sort(unique(tieback_group)), collapse = "/"),
    .groups = "drop"
  )

tieback_edges <- latest_pin_tieback %>%
  distinct(pin, tieback_group) %>%
  transmute(
    from = paste0("pin:", pin),
    to = paste0("group:", tieback_group)
  ) %>%
  bind_rows(fractional_base_edges) %>%
  distinct()

tieback_graph <- igraph::graph_from_data_frame(tieback_edges, directed = FALSE)
tieback_graph_membership <- igraph::components(tieback_graph)$membership
tieback_lineage_nodes <- tibble::tibble(
  node = names(tieback_graph_membership),
  graph_component = as.integer(tieback_graph_membership)
) %>%
  group_by(graph_component) %>%
  mutate(
    minimum_group = min(str_remove(node[str_starts(node, "group:")], "^group:")),
    tieback_lineage_id = paste0("residential_tieback_", minimum_group)
  ) %>%
  ungroup()

tieback_pin_lineage <- tieback_lineage_nodes %>%
  filter(str_starts(node, "pin:")) %>%
  transmute(
    pin = str_remove(node, "^pin:"),
    tieback_lineage_id
  )

tieback_group_lineage <- tieback_lineage_nodes %>%
  filter(str_starts(node, "group:")) %>%
  transmute(
    tieback_group = str_remove(node, "^group:"),
    tieback_lineage_id
  )

concurrent_card_history <- history %>%
  filter(pin %in% selected$pin) %>%
  group_by(pin, tax_year) %>%
  summarise(concurrent_cards = n_distinct(card_num), .groups = "drop") %>%
  group_by(pin) %>%
  summarise(
    maximum_concurrent_cards = max(concurrent_cards),
    years_with_multiple_cards = sum(concurrent_cards > 1),
    .groups = "drop"
  )

residential_history_summary <- history %>%
  filter(pin %in% selected$pin) %>%
  group_by(pin) %>%
  summarise(
    history_reports = n(),
    historical_cards = n_distinct(card_num),
    history_year_values = n_distinct(year_built, na.rm = TRUE),
    history_building_area_values = n_distinct(building_sqft, na.rm = TRUE),
    history_land_area_values = n_distinct(land_sqft, na.rm = TRUE),
    history_unit_values = n_distinct(num_apartments, na.rm = TRUE),
    source_years = paste(sort(unique(year_built[is.finite(year_built)])), collapse = "/"),
    source_building_areas = paste(sort(unique(building_sqft[is.finite(building_sqft)])), collapse = "/"),
    source_land_areas = paste(sort(unique(land_sqft[is.finite(land_sqft)])), collapse = "/"),
    source_unit_counts = paste(sort(unique(num_apartments[is.finite(num_apartments)])), collapse = "/"),
    .groups = "drop"
  ) %>%
  left_join(concurrent_card_history, by = "pin", relationship = "one-to-one")

selected_tieback_groups <- sort(unique(na.omit(selected$tieback_group)))
historical_selected_pin_groups <- sort(unique(na.omit(
  history$tieback_group[history$pin %in% selected$pin]
)))
historical_groups_missing_from_selected_cross_section <- setdiff(
  historical_selected_pin_groups,
  selected_tieback_groups
)

candidate_inventory <- selected %>%
  left_join(historical_tieback_flags, by = "pin", relationship = "one-to-one") %>%
  left_join(tieback_pin_lineage, by = "pin", relationship = "one-to-one") %>%
  left_join(residential_history_summary, by = "pin", relationship = "one-to-one") %>%
  left_join(coordinates, by = "pin", relationship = "one-to-one") %>%
  left_join(boundary_distance, by = "pin", relationship = "one-to-one") %>%
  left_join(commercial_pins, by = "pin", relationship = "one-to-one") %>%
  mutate(
    in_commercial_source = coalesce(in_commercial_source, FALSE),
    historical_tieback_membership = coalesce(historical_tieback_membership, FALSE),
    within_1500ft = !is.na(dist_to_boundary_m) & dist_to_boundary_m <= 457.2,
    review_category = case_when(
      in_commercial_source ~ "residential_commercial_overlap",
      class == "297" ~ "class_297",
      pin_is_multicard | maximum_concurrent_cards > 1 ~ "multicard",
      !is.na(tieback_group) | historical_tieback_membership ~ "tieback",
      TRUE ~ "ordinary"
    ),
    mechanical_status = case_when(
      review_category == "ordinary" &
        !explicit_multifamily_one_unit_conflict &
        historical_cards == 1 &
        history_year_values <= 1 &
        history_building_area_values <= 1 &
        history_land_area_values <= 1 &
        history_unit_values <= 1 ~ "ordinary_candidate",
      review_category == "ordinary" ~ "ordinary_history_review",
      TRUE ~ "requires_rule_or_review"
    )
  ) %>%
  select(
    pin,
    tax_year,
    card_num,
    class,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    single_v_multi_family,
    type_of_residence,
    tieback_group,
    tieback_lineage_id,
    historical_tieback_membership,
    historical_tieback_groups,
    history_reports,
    historical_cards,
    maximum_concurrent_cards,
    years_with_multiple_cards,
    history_year_values,
    history_building_area_values,
    history_land_area_values,
    history_unit_values,
    source_years,
    source_building_areas,
    source_land_areas,
    source_unit_counts,
    pin_proration_rate,
    card_proration_rate,
    pin_is_multicard,
    pin_num_cards,
    pin_is_multiland,
    pin_num_landlines,
    explicit_multifamily_one_unit_conflict,
    in_commercial_source,
    coordinate_x_3435,
    coordinate_y_3435,
    coordinate_source,
    dist_to_boundary_m,
    within_1500ft,
    review_category,
    mechanical_status,
    row_id
  )

tieback_members <- latest_pin_tieback %>%
  filter(tieback_group %in% relevant_tieback_groups) %>%
  left_join(
    candidate_inventory %>%
      select(pin, selected_year = year_built, dist_to_boundary_m, within_1500ft),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  mutate(selected_for_construction = !is.na(selected_year)) %>%
  select(
    tieback_group,
    pin,
    card_num,
    tax_year,
    class,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    pin_proration_rate,
    card_proration_rate,
    pin_is_multicard,
    pin_is_multiland,
    membership_first_tax_year,
    membership_last_tax_year,
    membership_reports,
    membership_cards,
    historical_tieback_groups_for_pin,
    selected_for_construction,
    selected_year,
    dist_to_boundary_m,
    within_1500ft,
    row_id
  )

tieback_groups <- tieback_members %>%
  group_by(tieback_group) %>%
  summarise(
    source_member_pins = n_distinct(pin),
    selected_member_pins = n_distinct(pin[selected_for_construction]),
    component_pins = paste(sort(unique(pin)), collapse = "/"),
    proration_complete = all(is.finite(pin_proration_rate)),
    proration_sum = sum(pin_proration_rate, na.rm = TRUE),
    cards = sum(membership_cards),
    pins_with_multiple_historical_groups = sum(historical_tieback_groups_for_pin > 1),
    year_values = n_distinct(year_built, na.rm = TRUE),
    building_area_values = n_distinct(building_sqft, na.rm = TRUE),
    unit_values = n_distinct(num_apartments, na.rm = TRUE),
    source_land_sum = sum(land_sqft, na.rm = TRUE),
    any_within_1500ft = any(within_1500ft %in% TRUE),
    minimum_boundary_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(tieback_group_lineage, by = "tieback_group", relationship = "one-to-one") %>%
  mutate(
    minimum_boundary_distance_m = if_else(
      is.infinite(minimum_boundary_distance_m),
      NA_real_,
      minimum_boundary_distance_m
    ),
    mechanical_group_candidate =
      proration_complete &
      abs(proration_sum - 1) < 0.001 &
      cards == source_member_pins &
      pins_with_multiple_historical_groups == 0 &
      year_values <= 1 &
      building_area_values <= 1 &
      unit_values <= 1,
    review_reason = case_when(
      mechanical_group_candidate ~ "complete_stable_tieback",
      pins_with_multiple_historical_groups > 0 ~ "changing_tieback_membership",
      !proration_complete | abs(proration_sum - 1) >= 0.001 ~ "incomplete_proration",
      cards != source_member_pins ~ "multiple_cards_in_tieback",
      year_values > 1 ~ "conflicting_years",
      building_area_values > 1 ~ "conflicting_building_area",
      unit_values > 1 ~ "conflicting_units",
      TRUE ~ "manual_review"
    )
  )

tieback_lineage_members <- tieback_members %>%
  left_join(tieback_group_lineage, by = "tieback_group", relationship = "many-to-one") %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    source_member_pins = n_distinct(pin),
    selected_member_pins = n_distinct(pin[selected_for_construction]),
    component_pins = paste(sort(unique(pin)), collapse = "/"),
    any_within_1500ft = any(within_1500ft %in% TRUE),
    minimum_boundary_distance_m = suppressWarnings(min(dist_to_boundary_m, na.rm = TRUE)),
    .groups = "drop"
  )

tieback_lineage_status <- tieback_groups %>%
  group_by(tieback_lineage_id) %>%
  summarise(
    tieback_groups = n(),
    all_groups_mechanical = all(mechanical_group_candidate),
    .groups = "drop"
  )

tieback_lineages <- tieback_lineage_members %>%
  left_join(tieback_lineage_status, by = "tieback_lineage_id", relationship = "one-to-one") %>%
  mutate(
    minimum_boundary_distance_m = if_else(
      is.infinite(minimum_boundary_distance_m),
      NA_real_,
      minimum_boundary_distance_m
    ),
    lineage_review_reason = case_when(
      tieback_groups > 1 ~ "changing_tieback_membership",
      !all_groups_mechanical ~ "group_requires_review",
      TRUE ~ "complete_stable_tieback"
    )
  )

multicard_pins <- candidate_inventory %>%
  filter(review_category == "multicard") %>%
  select(
    pin,
    selected_card = card_num,
    selected_year = year_built,
    selected_class = class,
    dist_to_boundary_m,
    within_1500ft
  )

multicard_cards <- latest_card_report %>%
  inner_join(multicard_pins, by = "pin", relationship = "many-to-one") %>%
  group_by(pin) %>%
  mutate(
    card_count = n(),
    card_proration_complete = all(is.finite(card_proration_rate)),
    card_proration_sum = sum(card_proration_rate, na.rm = TRUE),
    post_1998_cards = sum(year_built >= 1999, na.rm = TRUE),
    study_period_cards = sum(between(year_built, 2006, 2022), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    pin,
    card_num,
    tax_year,
    class,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    pin_proration_rate,
    card_proration_rate,
    card_count,
    card_proration_complete,
    card_proration_sum,
    post_1998_cards,
    study_period_cards,
    selected_card,
    selected_year,
    selected_class,
    dist_to_boundary_m,
    within_1500ft,
    row_id
  )

class_297 <- candidate_inventory %>%
  filter(class == "297") %>%
  select(
    pin,
    card_num,
    tax_year,
    year_built,
    building_sqft,
    land_sqft,
    num_apartments,
    single_v_multi_family,
    type_of_residence,
    explicit_multifamily_one_unit_conflict,
    tieback_group,
    pin_is_multicard,
    dist_to_boundary_m,
    within_1500ft,
    coordinate_x_3435,
    coordinate_y_3435,
    coordinate_source,
    row_id
  )

summary <- bind_rows(
  tibble::tibble(metric = "selected_residential_pins", value = nrow(candidate_inventory)),
  tibble::tibble(metric = "ordinary_candidates", value = sum(candidate_inventory$review_category == "ordinary")),
  tibble::tibble(metric = "mechanical_ordinary_candidates", value = sum(candidate_inventory$mechanical_status == "ordinary_candidate")),
  tibble::tibble(metric = "ordinary_history_review_candidates", value = sum(candidate_inventory$mechanical_status == "ordinary_history_review")),
  tibble::tibble(metric = "tieback_pins", value = sum(candidate_inventory$review_category == "tieback")),
  tibble::tibble(metric = "multicard_pins", value = sum(candidate_inventory$review_category == "multicard")),
  tibble::tibble(metric = "class_297_pins", value = sum(candidate_inventory$review_category == "class_297")),
  tibble::tibble(metric = "residential_commercial_overlap_pins", value = sum(candidate_inventory$review_category == "residential_commercial_overlap")),
  tibble::tibble(metric = "explicit_multifamily_one_unit_conflicts", value = sum(candidate_inventory$explicit_multifamily_one_unit_conflict)),
  tibble::tibble(metric = "tieback_groups", value = nrow(tieback_groups)),
  tibble::tibble(metric = "tieback_lineages", value = nrow(tieback_lineages)),
  tibble::tibble(metric = "historical_tieback_groups_missing_from_selected_cross_section", value = length(historical_groups_missing_from_selected_cross_section)),
  tibble::tibble(metric = "mechanical_tieback_group_candidates", value = sum(tieback_groups$mechanical_group_candidate)),
  tibble::tibble(metric = "tieback_groups_with_changing_membership", value = sum(tieback_groups$pins_with_multiple_historical_groups > 0)),
  tibble::tibble(metric = "tieback_groups_within_1500ft", value = sum(tieback_groups$any_within_1500ft)),
  tibble::tibble(metric = "multicard_pins_within_1500ft", value = n_distinct(multicard_cards$pin[multicard_cards$within_1500ft %in% TRUE])),
  tibble::tibble(metric = "class_297_pins_within_1500ft", value = sum(class_297$within_1500ft %in% TRUE)),
  tibble::tibble(metric = "strict_raw_csv_parse_completed", value = 1)
)

readr::write_csv(summary, "../output/residential_project_candidate_summary.csv")
readr::write_csv(candidate_inventory, "../output/residential_project_candidate_inventory.csv")
readr::write_csv(residential_history_summary, "../output/residential_project_history_summary.csv")
readr::write_csv(fractional_base_groups, "../output/residential_fractional_base_groups.csv")
readr::write_csv(tieback_groups, "../output/residential_tieback_groups_full.csv")
readr::write_csv(tieback_members, "../output/residential_tieback_members_full.csv")
readr::write_csv(tieback_lineages, "../output/residential_tieback_lineages_full.csv")
readr::write_csv(multicard_cards, "../output/residential_multicard_cards.csv")
readr::write_csv(class_297, "../output/residential_class297_project_queue.csv")
