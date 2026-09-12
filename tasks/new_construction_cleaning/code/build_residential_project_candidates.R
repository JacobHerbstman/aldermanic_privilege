# setwd("tasks/new_construction_cleaning/code")
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(preferred_assessment_year, fallback_assessment_year)
if (length(args) != 2L) stop("Expected preferred and fallback assessment years.")
preferred_assessment_year <- as.integer(args[1])
fallback_assessment_year <- as.integer(args[2])
if (anyNA(c(preferred_assessment_year, fallback_assessment_year)) ||
    preferred_assessment_year > fallback_assessment_year) stop("Invalid assessment-year priority.")

normalize_pin <- function(x) {
  value <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(value == "", NA_character_, value)
}

selected <- readr::read_csv(
  "../output/residential_cross_section.csv",
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

coordinates <- sf::st_read("../output/geocoded_residential_data.gpkg", quiet = TRUE) %>%
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
  "../output/construction_parcel_boundary_distances.csv",
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
  "../output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_skip())
) %>%
  distinct(pin) %>%
  mutate(in_commercial_source = TRUE)

con <- DBI::dbConnect(duckdb::duckdb())

DBI::dbWriteTable(con, "selected_pins", selected %>% select(pin), overwrite = TRUE)

invisible(DBI::dbExecute(con, "
CREATE TEMP VIEW source AS
SELECT pin, tax_year, card_num, class, proration_key_pin AS tieback_group,
  pin_proration_rate, card_proration_rate, pin_is_multicard, pin_num_cards,
  pin_is_multiland, pin_num_landlines, year_built, building_sqft, land_sqft,
  apartments_text, type_of_residence, single_v_multi_family, row_id, num_apartments
FROM read_parquet('../input/residential_assessor_history.parquet');
"))

invisible(DBI::dbExecute(con, "
CREATE TEMP TABLE tieback_edges AS
SELECT DISTINCT
  'pin:' || pin AS pin_node,
  'group:' || tieback_group AS group_node
FROM source
WHERE tieback_group IS NOT NULL
  AND tieback_group != ''
UNION
SELECT DISTINCT
  'pin:' || pin AS pin_node,
  'base:' || substr(pin, 1, 10) AS group_node
FROM source
WHERE length(pin) = 14
  AND right(pin, 4) != '0000';
"))

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

invisible(DBI::dbExecute(con, "
CREATE TEMP TABLE relevant_history AS
SELECT source.*
FROM source
WHERE source.pin IN (SELECT pin FROM selected_pins)
   OR 'pin:' || source.pin IN (
     SELECT node FROM relevant_tieback_nodes WHERE starts_with(node, 'pin:')
   );
"))

relevant_tieback_groups <- DBI::dbGetQuery(con, "
SELECT replace(node, 'group:', '') AS tieback_group
FROM relevant_tieback_nodes
WHERE starts_with(node, 'group:')
")$tieback_group

history <- DBI::dbGetQuery(con, "SELECT * FROM relevant_history") %>%
  mutate(
    tieback_group = if_else(tieback_group == "", NA_character_, tieback_group)
  ) %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Rank reports once within each PIN-card, using the same assessment windows
# as the cross-section. The discovery selection has a different purpose.
latest_card_report <- history %>%
  # Keep empty records in the history, but never select them as buildings.
  filter(!is.na(building_sqft) | !is.na(num_apartments)) %>%
  mutate(report_priority = case_when(
    tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L,
    TRUE ~ 3L
  )) %>%
  arrange(pin, card_num, report_priority, desc(tax_year), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-report_priority)

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

# Choose one complete assessment for each proposed construction-year episode.
# Card-by-card priorities can combine measurements that never existed together.
selected_episode_cards <- latest_card_report %>%
  semi_join(multicard_pins, by = "pin") %>%
  filter(between(year_built, 2006L, 2022L)) %>%
  select(pin, card_num, selected_construction_year = year_built)
episode_sizes <- selected_episode_cards %>%
  count(pin, selected_construction_year, name = "episode_cards")

complete_snapshots <- history %>%
  filter(!is.na(building_sqft) | !is.na(num_apartments)) %>%
  inner_join(
    episode_sizes, by = c("pin", "year_built" = "selected_construction_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(selected_episode_cards, by = c("pin", "card_num"), relationship = "many-to-one") %>%
  mutate(card_units = if_else(class %in% c("211", "212"), num_apartments, 1)) %>%
  group_by(pin, year_built, tax_year) %>%
  summarise(
    complete_membership = n() == first(episode_cards) &
      all(!is.na(selected_construction_year) & selected_construction_year == year_built),
    complete_measurements =
      all(is.finite(card_units) & card_units > 0) &
      all(is.finite(building_sqft) & building_sqft > 0) &
      all(is.finite(land_sqft) & land_sqft > 0) & n_distinct(land_sqft) == 1,
    .groups = "drop"
  ) %>%
  filter(complete_membership, complete_measurements) %>%
  mutate(report_priority = case_when(
    tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L,
    TRUE ~ 3L
  )) %>%
  arrange(pin, year_built, report_priority, desc(tax_year)) %>%
  group_by(pin, year_built) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(pin, year_built, tax_year)

snapshot_cards <- history %>%
  filter(!is.na(building_sqft) | !is.na(num_apartments)) %>%
  inner_join(complete_snapshots, by = c("pin", "year_built", "tax_year"), relationship = "many-to-one")
stopifnot(!anyDuplicated(snapshot_cards[c("pin", "card_num")]))

multicard_reports <- bind_rows(
  latest_card_report %>%
    semi_join(multicard_pins, by = "pin") %>%
    anti_join(snapshot_cards %>% select(pin, card_num), by = c("pin", "card_num")) %>%
    mutate(complete_episode_snapshot = FALSE),
  snapshot_cards %>% mutate(complete_episode_snapshot = TRUE)
) %>% arrange(pin, card_num)

# Reviewed physical identities select source rows before units and areas are added.
# The ledger records why other cards for the same PIN are not separate buildings.
reviewed_cards <- readr::read_csv("../input/residential_reviewed_card_selections.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(reviewed_cards$row_id),
  all(reviewed_cards$pin %in% multicard_pins$pin))
reviewed_reports <- history %>%
  semi_join(reviewed_cards, by = c("pin", "row_id")) %>%
  mutate(complete_episode_snapshot = TRUE)
stopifnot(nrow(reviewed_reports) == nrow(reviewed_cards),
  !anyDuplicated(reviewed_reports[c("pin", "card_num")]),
  all(is.finite(reviewed_reports$building_sqft) & reviewed_reports$building_sqft > 0),
  all(is.finite(reviewed_reports$land_sqft) & reviewed_reports$land_sqft > 0))
stopifnot(all(reviewed_reports %>% count(pin, tax_year) %>% count(pin) %>% pull(n) == 1L))
multicard_reports <- bind_rows(
  multicard_reports %>% anti_join(reviewed_cards %>% distinct(pin), by = "pin"),
  reviewed_reports
) %>% arrange(pin, card_num)

multicard_cards <- multicard_reports %>%
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
    row_id,
    complete_episode_snapshot
  )

SaveData(candidate_inventory, c("pin"), "../output/residential_project_candidate_inventory.csv")
SaveData(residential_history_summary, c("pin"), "../output/residential_project_history_summary.csv")
SaveData(fractional_base_groups, c("base_pin"), "../output/residential_fractional_base_groups.csv")
SaveData(tieback_groups, c("tieback_group"), "../output/residential_tieback_groups_full.csv")
SaveData(tieback_members, c("tieback_group", "pin"), "../output/residential_tieback_members_full.csv")
SaveData(multicard_cards, c("pin", "card_num"), "../output/residential_multicard_cards.csv")

DBI::dbDisconnect(con, shutdown = TRUE)
