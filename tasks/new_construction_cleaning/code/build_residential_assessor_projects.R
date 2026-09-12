# setwd("tasks/new_construction_cleaning/code")
# maximum_building_gap <- 0.02
# maximum_year_gap <- 2
# successor_point_tolerance_ft <- 1
# successor_land_tolerance <- 0.005

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(maximum_building_gap, maximum_year_gap,
  successor_point_tolerance_ft, successor_land_tolerance)
stopifnot(length(args) == 4L)
maximum_building_gap <- as.numeric(args[1])
maximum_year_gap <- as.integer(args[2])
successor_point_tolerance_ft <- as.numeric(args[3])
successor_land_tolerance <- as.numeric(args[4])
stopifnot(is.finite(maximum_building_gap), maximum_building_gap >= 0,
  !is.na(maximum_year_gap), maximum_year_gap >= 0,
  is.finite(successor_point_tolerance_ft), successor_point_tolerance_ft >= 0,
  is.finite(successor_land_tolerance), successor_land_tolerance >= 0)

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

if (anyDuplicated(inventory$pin) > 0) {
  stop("Residential candidate inventory is not unique by PIN.", call. = FALSE)
}
if (anyDuplicated(tieback_temporal$tieback_lineage_id) > 0) {
  stop("Tieback lineage input is not unique by lineage.", call. = FALSE)
}
if (anyDuplicated(multicard_cards[c("pin", "card_num")]) > 0) {
  stop("Multicard evidence is not unique by PIN-card.", call. = FALSE)
}

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
  )

# Fill an absent floor area only from the same building on the same lot.
# Keep every card when checking that a later assessment contains one record.
missing_floor <- inventory %>% filter(review_category == "ordinary",
  !is.finite(building_sqft) | building_sqft <= 0) %>% select(pin)
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "missing_floor", missing_floor)
later_measurements <- DBI::dbGetQuery(con, "SELECT h.*
  FROM read_parquet('../input/residential_assessor_history.parquet') h
  INNER JOIN missing_floor m ON h.pin = m.pin") %>% as_tibble() %>%
  group_by(pin, tax_year) %>% filter(n() == 1L) %>% ungroup() %>%
  mutate(comparable_units = if_else(class %in% single_family_assessor_classes, 1, num_apartments))
DBI::dbDisconnect(con, shutdown = TRUE)
for (i in which(inventory$pin %in% missing_floor$pin)) {
  selected_units <- if (inventory$class[i] %in% single_family_assessor_classes) 1 else inventory$assessor_units[i]
  matches <- later_measurements %>% filter(pin == inventory$pin[i],
    tax_year > inventory$tax_year[i], year_built == inventory$year_built[i],
    comparable_units == selected_units, land_sqft == inventory$land_sqft[i],
    is.finite(building_sqft), building_sqft > 0) %>%
    arrange(desc(tax_year == 2025), tax_year)
  if (nrow(matches) == 0L) next
  inventory$building_sqft[i] <- matches$building_sqft[1]
  # The complete later row also reports the unchanged year, units, and land.
  inventory$row_id[i] <- matches$row_id[1]
}

tieback_pin_lineage <- tieback_temporal %>%
  select(tieback_lineage_id, pin = all_lineage_pins) %>%
  tidyr::separate_longer_delim(pin, delim = "/") %>%
  filter(!is.na(pin), pin != "") %>%
  distinct(pin, tieback_lineage_id)

if (anyDuplicated(tieback_pin_lineage$pin) > 0) {
  stop("A residential PIN maps to multiple corrected tieback lineages.", call. = FALSE)
}

# A historical self-reference can disappear in a later complete assessment.
# Use that whole assessment only for the same single-card building: same PIN,
# class, construction year, floor area and residential count. Its reported lot
# replaces the old shared-site value; no tax share or polygon area is used.
self_references <- tieback_pin_lineage %>% group_by(tieback_lineage_id) %>%
  filter(n() == 1L) %>% ungroup() %>% inner_join(inventory, by = "pin", relationship = "one-to-one") %>%
  filter(review_category == "tieback", maximum_concurrent_cards == 1,
    !in_commercial_source, class != "297", between(year_built, 2006L, 2022L)) %>% select(pin)
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "self_references", self_references)
independent_reports <- DBI::dbGetQuery(con, "SELECT h.*
  FROM read_parquet('../input/residential_assessor_history.parquet') h
  INNER JOIN self_references s ON h.pin = s.pin") %>% as_tibble() %>%
  group_by(pin, tax_year) %>% filter(n() == 1L) %>% ungroup()
DBI::dbDisconnect(con, shutdown = TRUE)
for (i in which(inventory$pin %in% self_references$pin)) {
  matches <- independent_reports %>% filter(pin == inventory$pin[i],
    tax_year > inventory$tax_year[i], is.na(proration_key_pin), pin_proration_rate == 1,
    class == inventory$class[i], year_built == inventory$year_built[i],
    building_sqft == inventory$building_sqft[i], is.finite(building_sqft), building_sqft > 1,
    coalesce(num_apartments, -1) == coalesce(inventory$num_apartments[i], -1),
    is.finite(land_sqft), land_sqft > 1) %>%
    arrange(desc(tax_year == 2025), tax_year)
  if (nrow(matches) == 0L) next
  for (field in c("tax_year", "card_num", "land_sqft", "pin_proration_rate", "card_proration_rate", "row_id"))
    inventory[[field]][i] <- matches[[field]][1]
  inventory$tieback_group[i] <- NA_character_
}

# A tie that ended before the new construction does not combine the new buildings.
# Require independent, complete, single-card reports for every current member.
# A historical self-reference alone does not tie a parcel to another property.
con <- DBI::dbConnect(duckdb::duckdb())
last_ties <- DBI::dbGetQuery(con, "SELECT pin, max(tax_year) AS last_tied_year
  FROM read_parquet('../input/residential_assessor_history.parquet')
  WHERE proration_key_pin IS NOT NULL AND proration_key_pin != '' GROUP BY pin")
DBI::dbDisconnect(con, shutdown = TRUE)
lineage_last_ties <- tieback_pin_lineage %>% left_join(last_ties, by = "pin", relationship = "many-to-one") %>%
  group_by(tieback_lineage_id) %>% summarise(last_tied_year = max(last_tied_year, na.rm = TRUE),
    lineage_pin_count = n_distinct(pin), .groups = "drop")
independent_members <- inventory %>% inner_join(tieback_pin_lineage, by = "pin", relationship = "one-to-one",
  suffix = c("", "_historical")) %>%
  left_join(lineage_last_ties, by = c("tieback_lineage_id_historical" = "tieback_lineage_id"), relationship = "many-to-one") %>%
  group_by(tieback_lineage_id_historical) %>%
  filter(all(is.finite(last_tied_year) & (year_built > last_tied_year | lineage_pin_count == 1) & between(year_built, 2006L, 2022L) &
    is.na(tieback_group) & pin_proration_rate == 1 & maximum_concurrent_cards == 1 &
    !in_commercial_source & class != "297" & is.finite(building_sqft) & building_sqft > 1 &
    is.finite(land_sqft) & land_sqft > 1 &
    (class %in% single_family_assessor_classes | (is.finite(assessor_units) & assessor_units > 0)))) %>% ungroup()
inventory$review_category[inventory$pin %in% independent_members$pin] <- "ordinary"
tieback_pin_lineage <- tieback_pin_lineage %>%
  filter(!tieback_lineage_id %in% independent_members$tieback_lineage_id_historical)

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
    construction_year = year_built,
    # A single card in a single-family class represents one dwelling. Apply this
    # before grouping projects; later classification uses the same definition.
    dwelling_units = if_else(class %in% single_family_assessor_classes, 1, assessor_units),
    building_sqft,
    land_sqft,
    class_values = class,
    source_row_ids = row_id,
    year_source = paste0("assessor_row:", row_id),
    units_source = if_else(
      class %in% single_family_assessor_classes &
        (!is.finite(assessor_units) | assessor_units != 1),
      paste0("single_family_class:", class, "; assessor_row:", row_id),
      paste0("assessor_row:", row_id)
    ),
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
    .groups = "drop"
  )

# A complete snapshot already requires one card per parcel. Historical extra
# cards do not invalidate that selected, complete assessment.
tieback_candidates <- tieback_temporal %>%
  mutate(
    all_candidate_years_outside_period =
      candidate_year_count > 0L & candidate_in_period_year_count == 0L,
    component_pins = coalesce(selected_component_pins, all_lineage_pins),
    component_count = if_else(
      is.na(component_pins) | component_pins == "",
      NA_integer_,
      str_count(component_pins, fixed("/")) + 1L
    ),
    construction_year = coalesce(selected_construction_year, unique_candidate_construction_year),
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
      (is.finite(construction_year) & !between(construction_year, 2006L, 2022L)) |
        (!is.finite(construction_year) & all_candidate_years_outside_period) ~
        "exclude_outside_period",
      has_commercial_overlap ~ "defer_to_commercial_reconciliation",
      temporal_status != "temporally_resolved" | has_class_297 ~
        "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      !is.finite(construction_year) & all_candidate_years_outside_period ~
        "all_candidate_construction_years_outside_2006_2022",
      !is.finite(construction_year) ~ "tieback_construction_year_unresolved",
      !between(construction_year, 2006L, 2022L) ~ "construction_year_outside_2006_2022",
      has_commercial_overlap ~ "tieback_contains_commercial_source_pin",
      has_class_297 ~ "tieback_contains_class_297",
      has_multicard & temporal_status != "temporally_resolved" ~ "tieback_contains_multicard_pin",
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
    complete_units = all(is.finite(card_units[study_period_card]) & card_units[study_period_card] > 0),
    complete_building = all(is.finite(building_sqft[study_period_card]) & building_sqft[study_period_card] > 0),
    measurements_observed_together = study_cards == 0L ||
      (all(complete_episode_snapshot[study_period_card]) &&
       n_distinct(tax_year[study_period_card]) == 1L),
    dwelling_units = if (complete_units && measurements_observed_together) sum(card_units[study_period_card]) else NA_real_,
    building_sqft = if (complete_building && measurements_observed_together) sum(building_sqft[study_period_card]) else NA_real_,
    land_values = n_distinct(land_sqft[study_period_card], na.rm = TRUE),
    land_sqft = single_finite_value(land_sqft[study_period_card]),
    class_values = paste(sort(unique(class[study_period_card])), collapse = "/"),
    source_row_ids = paste(sort(unique(row_id[study_period_card])), collapse = "/"),
    source_year_values = paste(sort(unique(year_built[study_period_card])), collapse = "/"),
    year_source = paste0("multicard_assessor_rows:", source_row_ids),
    units_source = paste0("card_level_rule:", source_row_ids),
    building_source = paste0("sum_study_year_cards:", source_row_ids),
    land_source = paste0("one_pin_land_once:", source_row_ids),
    current_distance_m = first(dist_to_boundary_m),
    current_within_1500ft = first(within_1500ft),
    candidate_status = case_when(
      study_cards == 0 ~ "exclude_outside_period",
      study_year_values != 1 | !measurements_observed_together ~ "review_required",
      !is.finite(dwelling_units) | dwelling_units <= 0 |
        !is.finite(building_sqft) | building_sqft <= 0 |
        !is.finite(land_sqft) | land_sqft <= 0 ~ "review_required",
      TRUE ~ "retain_mechanical"
    ),
    decision_reason = case_when(
      study_cards == 0 ~ "no_card_built_from_2006_through_2022",
      study_year_values > 1 ~ "cards_report_multiple_study_period_construction_years",
      !complete_units ~ "missing_or_nonpositive_units",
      !complete_building ~ "missing_or_nonpositive_building_area",
      !is.finite(land_sqft) | land_sqft <= 0 ~ "missing_or_nonpositive_land_area",
      !measurements_observed_together ~ "no_complete_assessment_for_selected_components",
      TRUE ~ "same_year_cards_aggregated_land_counted_once"
    ),
    .groups = "drop"
  ) %>%
  select(all_of(names(ordinary_candidates)))

# Recorded completion-year decisions apply before matching old and new records.
reviewed_years <- readr::read_csv("../input/construction_modifications.csv",
  col_types = readr::cols(construction_year = "i", reported_construction_year = "i",
    .default = readr::col_character())) %>%
  filter(source_family == "residential", application_stage == "project_identity") %>%
  transmute(project_id = source_project_id, reported_year = reported_construction_year,
    construction_year)
stopifnot(!anyDuplicated(reviewed_years$project_id),
  all(reviewed_years$project_id %in% c(ordinary_candidates$project_id,
    tieback_candidates$project_id, multicard_candidates$project_id)),
  all(!is.na(reviewed_years$construction_year) & reviewed_years$construction_year > 0 &
    reviewed_years$construction_year <= 2022L))
candidates <- bind_rows(ordinary_candidates, tieback_candidates, multicard_candidates)
stopifnot(!anyDuplicated(candidates$project_id))
i <- match(reviewed_years$project_id, candidates$project_id)
stopifnot(all(candidates$construction_year[i] == reviewed_years$reported_year),
  all(candidates$dwelling_units[i] > 0), all(candidates$building_sqft[i] > 0),
  all(candidates$land_sqft[i] > 0))
candidates$construction_year[i] <- reviewed_years$construction_year
candidates$year_source[i] <- paste0("reviewed_construction_year:", reviewed_years$project_id)
candidates$candidate_status[i] <- ifelse(reviewed_years$construction_year < 2006L,
  "exclude_outside_period", "retain_mechanical")
candidates$decision_reason[i] <- "reviewed_construction_year_with_recorded_assessor_measurements"

ordinary_candidates <- candidates %>% filter(project_kind == "single_pin_single_card")
tieback_candidates <- candidates %>% filter(project_kind == "tieback_building")
multicard_candidates <- candidates %>% filter(project_kind == "same_pin_multiple_cards")

# An old parcel may describe homes that now have individual property numbers.
# Suppress it only when all of its homes have separately accepted replacements.
old_parcels <- bind_rows(
  sf::st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE) %>%
    transmute(pin = pin14, map_year = target_year),
  sf::st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) %>%
    transmute(pin = predecessor_pin14, map_year = target_year)
) %>% sf::st_transform(3435)
current_parcels <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv", show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), longitude = readr::col_double(),
    latitude = readr::col_double(), .default = readr::col_skip())
)
stopifnot(!anyDuplicated(current_parcels$pin))

# Recognize a home whose parcel number changes between consecutive assessments.
# The successor's accepted construction year predates the old assessment, so this is
# not permission to merge a later replacement building at the same address.
con <- DBI::dbConnect(duckdb::duckdb())
assessment_periods <- DBI::dbGetQuery(con, "SELECT pin, min(tax_year) AS first_assessment,
  max(tax_year) AS last_assessment FROM read_parquet('../input/residential_assessor_history.parquet')
  WHERE building_sqft IS NOT NULL OR num_apartments IS NOT NULL GROUP BY pin")
DBI::dbDisconnect(con, shutdown = TRUE)
historical_points <- bind_rows(
  readr::read_csv("../input/geocoding_parcel_history.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), lon = readr::col_double(),
    lat = readr::col_double(), .default = readr::col_skip())),
  readr::read_csv("../input/predecessor_parcel_history.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), lon = readr::col_double(),
    lat = readr::col_double(), .default = readr::col_skip())),
  readr::read_csv("../input/density_historical_parcel_records.csv", col_types = readr::cols(
    pin = readr::col_character(), year = readr::col_integer(), longitude = readr::col_double(),
    latitude = readr::col_double(), .default = readr::col_skip())) %>% rename(lon = longitude, lat = latitude)
) %>% filter(is.finite(lon), is.finite(lat)) %>% distinct()
old_homes <- ordinary_candidates %>%
  filter(candidate_status == "retain_mechanical", dwelling_units == 1,
    !component_pins %in% current_parcels$pin) %>%
  left_join(assessment_periods, by = c("component_pins" = "pin"), relationship = "one-to-one")
historical_points <- historical_points %>%
  inner_join(old_homes %>% select(component_pins, last_assessment),
    by = c("pin" = "component_pins"), relationship = "many-to-one") %>%
  filter(year <= last_assessment) %>% group_by(pin) %>%
  filter(year == max(year)) %>% filter(n() == 1L) %>% ungroup()
old_homes <- old_homes %>% inner_join(historical_points %>% select(pin, lon, lat),
  by = c("component_pins" = "pin"), relationship = "one-to-one")
new_homes <- ordinary_candidates %>%
  filter(candidate_status == "retain_mechanical", dwelling_units == 1) %>%
  inner_join(current_parcels, by = c("component_pins" = "pin"), relationship = "one-to-one") %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  left_join(assessment_periods, by = c("component_pins" = "pin"), relationship = "one-to-one")
old_addresses <- readr::read_csv("../input/density_historical_address_records.csv",
  col_types = readr::cols(pin = readr::col_character(), year = readr::col_integer(),
    property_address = readr::col_character(), .default = readr::col_skip()))
new_addresses <- readr::read_csv("../input/parcel_addresses_2025_chicago.csv",
  col_types = readr::cols(pin = readr::col_character(), prop_address_full = readr::col_character(),
    .default = readr::col_skip()))
stopifnot(!anyDuplicated(new_addresses$pin))
hits <- sf::st_is_within_distance(
  sf::st_transform(sf::st_as_sf(old_homes, coords = c("lon", "lat"), crs = 4326), 3435),
  sf::st_transform(sf::st_as_sf(new_homes, coords = c("longitude", "latitude"), crs = 4326), 3435),
  dist = successor_point_tolerance_ft)
ordinary_candidates$replacement_project_ids <- NA_character_
ordinary_candidates$replacement_check <- NA_character_
for (i in which(lengths(hits) == 1L)) {
  old <- old_homes[i, ]; new <- new_homes[hits[[i]], ]
  addresses <- old_addresses %>% filter(pin == old$component_pins, year <= old$last_assessment)
  if (!nrow(addresses)) next
  addresses <- addresses %>% filter(year == max(year))
  # A missing trailing unit letter does not change the street address; the
  # unique parcel point and land match still distinguish individual homes.
  old_address <- unique(str_remove(str_to_upper(str_squish(addresses$property_address)), " [A-Z]$"))
  new_address <- new_addresses$prop_address_full[match(new$component_pins, new_addresses$pin)]
  new_address <- str_remove(str_to_upper(str_squish(new_address)), " [A-Z]$")
  if (length(old_address) != 1L || is.na(old_address) || is.na(new_address) ||
      old_address == "" || old_address != new_address ||
      new$first_assessment != old$last_assessment + 1L ||
      new$construction_year > old$first_assessment ||
      abs(new$land_sqft / old$land_sqft - 1) > successor_land_tolerance) next
  j <- match(old$project_id, ordinary_candidates$project_id)
  ordinary_candidates$replacement_project_ids[j] <- new$project_id
  ordinary_candidates$replacement_check[j] <- "same_home_consecutive_parcel_numbers"
}
# Ambiguous reuse is never converted into automatic suppression.
reused <- ordinary_candidates$replacement_project_ids[
  duplicated(ordinary_candidates$replacement_project_ids) & !is.na(ordinary_candidates$replacement_project_ids)]
for (j in which(!is.na(ordinary_candidates$replacement_project_ids))) {
  if (ordinary_candidates$replacement_project_ids[j] %in% reused) {
    ordinary_candidates$replacement_project_ids[j] <- NA_character_
    ordinary_candidates$replacement_check[j] <- "successor_claimed_by_multiple_old_homes"
  } else {
    ordinary_candidates$candidate_status[j] <- "exclude_source_duplicate_keep_successors"
    ordinary_candidates$decision_reason[j] <- ordinary_candidates$replacement_check[j]
  }
}
individuals <- ordinary_candidates %>%
  inner_join(current_parcels, by = c("component_pins" = "pin"), relationship = "one-to-one") %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_transform(3435)
multicard_candidates$replacement_project_ids <- NA_character_
multicard_candidates$replacement_check <- "not_checked"
for (i in seq_len(nrow(multicard_candidates))) {
  parent_pin <- multicard_candidates$component_pins[i]
  cards <- multicard_cards %>%
    filter(pin == parent_pin, between(year_built, 2006L, 2022L))
  # A surviving current parcel is not automatically replaced by nearby homes.
  if (parent_pin %in% current_parcels$pin) {
    multicard_candidates$replacement_check[i] <- "parent_still_in_current_parcel_source"
    next
  }
  if (nrow(cards) < 2L || anyNA(cards$year_built) ||
      any(!between(cards$year_built, 2006L, 2022L)) ||
      any(!cards$class %in% single_family_assessor_classes) ||
      any(!is.finite(cards$building_sqft) | cards$building_sqft <= 0) ||
      n_distinct(cards$tax_year) != 1L) {
    multicard_candidates$replacement_check[i] <- "incomplete_or_mixed_parent_cards"
    next
  }
  shapes <- old_parcels %>% filter(pin == parent_pin, map_year <= max(cards$tax_year))
  if (nrow(shapes) == 0L) {
    multicard_candidates$replacement_check[i] <- "missing_exact_parent_parcel"
    next
  }
  shapes <- shapes %>% filter(map_year == max(map_year))
  if (any(!sf::st_is_valid(shapes)) ||
      !all(lengths(sf::st_equals(shapes)) == nrow(shapes))) {
    multicard_candidates$replacement_check[i] <- "conflicting_parent_parcels"
    next
  }
  homes <- individuals[lengths(sf::st_within(individuals, shapes[1, ])) == 1L, ] %>%
    filter(between(construction_year, min(cards$year_built) - maximum_year_gap,
      max(cards$year_built) + maximum_year_gap))
  if (nrow(homes) != nrow(cards) ||
      any(homes$candidate_status != "retain_mechanical") ||
      any(homes$dwelling_units != 1) || anyNA(homes$dwelling_units)) {
    multicard_candidates$replacement_check[i] <- "individual_count_or_eligibility_conflict"
    next
  }
  # Compare every home, not just total floor area; identical sizes may repeat.
  homes <- homes %>% arrange(building_sqft, construction_year, project_id)
  cards <- cards %>% arrange(building_sqft, year_built, card_num)
  if (any(abs(homes$building_sqft / cards$building_sqft - 1) > maximum_building_gap) ||
      any(abs(homes$construction_year - cards$year_built) > maximum_year_gap)) {
    multicard_candidates$replacement_check[i] <- "individual_measurement_or_year_conflict"
    next
  }
  multicard_candidates$replacement_project_ids[i] <- paste(sort(homes$project_id), collapse = "/")
  multicard_candidates$replacement_check[i] <- "complete_individual_coverage"
}
# Approved exceptions document identity where strict measurement/point tests fail.
reviewed_replacements <- readr::read_csv(
  "../input/residential_reviewed_home_replacements.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(reviewed_replacements$project_id),
  all(reviewed_replacements$project_id %in% multicard_candidates$project_id))
for (j in seq_len(nrow(reviewed_replacements))) {
  i <- match(reviewed_replacements$project_id[j], multicard_candidates$project_id)
  ids <- strsplit(reviewed_replacements$replacement_project_ids[j], "/", fixed = TRUE)[[1]]
  homes <- ordinary_candidates %>% filter(project_id %in% ids)
  stopifnot(!anyDuplicated(ids), nrow(homes) == length(ids),
    all(homes$candidate_status == "retain_mechanical"), all(homes$dwelling_units == 1))
  multicard_candidates$replacement_project_ids[i] <- paste(sort(ids), collapse = "/")
  multicard_candidates$replacement_check[i] <- "reviewed_individual_coverage"
}
# One individual cannot be used to clear two old parcels automatically.
replacement_ids <- strsplit(na.omit(multicard_candidates$replacement_project_ids), "/", fixed = TRUE)
reused_ids <- names(which(table(unlist(replacement_ids)) > 1L))
for (i in which(!is.na(multicard_candidates$replacement_project_ids))) {
  ids <- strsplit(multicard_candidates$replacement_project_ids[i], "/", fixed = TRUE)[[1]]
  if (any(ids %in% reused_ids)) {
    multicard_candidates$replacement_project_ids[i] <- NA_character_
    multicard_candidates$replacement_check[i] <- "individuals_claimed_by_multiple_parents"
  } else {
    multicard_candidates$candidate_status[i] <- "exclude_source_duplicate_keep_successors"
    multicard_candidates$decision_reason[i] <- multicard_candidates$replacement_check[i]
  }
}

# Membership and measurements are built together from the same source records.
assessor_projects <- bind_rows(ordinary_candidates, tieback_candidates, multicard_candidates) %>%
  arrange(project_kind, project_id)
stopifnot(!anyNA(assessor_projects$project_id), !anyDuplicated(assessor_projects$project_id))
for (id in unique(independent_members$tieback_lineage_id_historical)) {
  members <- independent_members %>% filter(tieback_lineage_id_historical == id)
  replacements <- paste0("residential_", members$pin)
  j <- match(replacements, assessor_projects$project_id)
  stopifnot(!anyNA(j), all(assessor_projects$candidate_status[j] == "retain_mechanical"))
  old <- match(id, assessor_projects$project_id)
  stopifnot(!is.na(old))
  assessor_projects$candidate_status[old] <- "exclude_source_duplicate_keep_successors"
  assessor_projects$decision_reason[old] <- if (all(members$year_built > members$last_tied_year))
    "historical_tie_ended_before_independently_reported_new_buildings" else
    "historical_self_reference_only_current_complete_independent_property"
  assessor_projects$replacement_project_ids[old] <- paste(sort(replacements), collapse = "/")
  assessor_projects$replacement_check[old] <- "complete_independent_assessor_reports_after_old_tie_ended_or_self_reference_only"
}

# Reviewed sites combine identified buildings from one assessment across parcels.
reviewed_components <- readr::read_csv(
  "../input/residential_reviewed_building_components.csv",
  col_types = readr::cols(construction_year = readr::col_integer(), .default = readr::col_character()))
stopifnot(!anyDuplicated(reviewed_components$row_id),
  all(reviewed_components$source_project_id %in% assessor_projects$project_id))
reviewed_membership <- assessor_projects %>%
  select(source_project_id = project_id, pin = component_pins) %>%
  tidyr::separate_longer_delim(pin, delim = "/")
stopifnot(nrow(anti_join(reviewed_components, reviewed_membership,
  by = c("source_project_id", "pin"))) == 0L)
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "reviewed_components", reviewed_components)
reviewed_measurements <- DBI::dbGetQuery(con, "
  SELECT c.project_id, c.construction_year, h.*
  FROM read_parquet('../input/residential_assessor_history.parquet') h
  INNER JOIN reviewed_components c ON h.row_id = c.row_id AND h.pin = c.pin")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(nrow(reviewed_measurements) == nrow(reviewed_components))
for (id in unique(reviewed_components$project_id)) {
  rows <- reviewed_measurements %>% filter(project_id == id)
  stopifnot(n_distinct(rows$tax_year) == 1L, n_distinct(rows$construction_year) == 1L,
    !anyDuplicated(rows[c("pin", "card_num")]),
    all(is.finite(rows$building_sqft) & rows$building_sqft > 0))
  land <- rows %>% distinct(pin, land_sqft)
  stopifnot(!anyDuplicated(land$pin), all(is.finite(land$land_sqft) & land$land_sqft > 0))
  units <- ifelse(rows$class %in% single_family_assessor_classes, 1, rows$num_apartments)
  stopifnot(all(is.finite(units) & units > 0))
  source_ids <- paste(sort(rows$row_id), collapse = "/")
  site <- tibble::tibble(project_id = id, source_family = "residential",
    project_kind = "reviewed_multi_parcel_building",
    component_pins = paste(sort(unique(rows$pin)), collapse = "/"), component_count = nrow(land),
    construction_year = first(rows$construction_year), dwelling_units = sum(units),
    building_sqft = sum(rows$building_sqft), land_sqft = sum(land$land_sqft),
    class_values = paste(sort(unique(rows$class)), collapse = "/"), source_row_ids = source_ids,
    year_source = paste0("reviewed_building_year:", id),
    units_source = paste0("reviewed_assessor_components:", source_ids),
    building_source = paste0("reviewed_assessor_components:", source_ids),
    land_source = paste0("distinct_component_parcels_in_assessment:", first(rows$tax_year)),
    current_distance_m = NA_real_, current_within_1500ft = FALSE,
    candidate_status = "retain_mechanical", decision_reason = "reviewed_complete_site_and_construction_year",
    replacement_project_ids = NA_character_, replacement_check = NA_character_)
  assessor_projects <- bind_rows(assessor_projects, site)
}
# Keep the superseded source records visible, but count only their reviewed buildings.
reviewed_replacements <- reviewed_components %>%
  group_by(source_project_id) %>%
  summarise(replacements = paste(sort(unique(project_id)), collapse = "/"), .groups = "drop")
i <- match(reviewed_replacements$source_project_id, assessor_projects$project_id)
assessor_projects$candidate_status[i] <- "exclude_source_duplicate_keep_successors"
assessor_projects$decision_reason[i] <- "source_replaced_by_reviewed_assessor_buildings"
assessor_projects$replacement_project_ids[i] <- reviewed_replacements$replacements
assessor_projects$replacement_check[i] <- "reviewed_same_building_identity"
reviewed_exclusions <- readr::read_csv("../input/residential_reviewed_source_exclusions.csv",
  col_types = readr::cols(.default = readr::col_character()))
stopifnot(!anyDuplicated(reviewed_exclusions$project_id),
  all(reviewed_exclusions$project_id %in% assessor_projects$project_id))
i <- match(reviewed_exclusions$project_id, assessor_projects$project_id)
assessor_projects$candidate_status[i] <- "exclude_unreliable_combined_record"
assessor_projects$decision_reason[i] <- reviewed_exclusions$reason
# Reviewed identities handle proven duplicates that fail the strict automatic crosswalk.
reviewed_duplicates <- readr::read_csv("../input/residential_reviewed_source_duplicates.csv",
  col_types = readr::cols(.default = readr::col_character()))
duplicate_replacements <- reviewed_duplicates %>%
  select(project_id, replacement_project_id) %>%
  tidyr::separate_longer_delim(replacement_project_id, delim = "/")
stopifnot(!anyDuplicated(reviewed_duplicates$project_id),
  all(reviewed_duplicates$project_id %in% assessor_projects$project_id),
  all(duplicate_replacements$replacement_project_id %in% assessor_projects$project_id),
  !any(duplicate_replacements$replacement_project_id %in% reviewed_duplicates$project_id))
i <- match(reviewed_duplicates$project_id, assessor_projects$project_id)
j <- match(duplicate_replacements$replacement_project_id, assessor_projects$project_id)
stopifnot(all(assessor_projects$candidate_status[j] == "retain_mechanical"))
assessor_projects$candidate_status[i] <- "exclude_source_duplicate_keep_successors"
assessor_projects$decision_reason[i] <- reviewed_duplicates$reason
assessor_projects$replacement_project_ids[i] <- reviewed_duplicates$replacement_project_id
assessor_projects$replacement_check[i] <- "reviewed_same_building_identity"
assessor_projects <- assessor_projects %>% arrange(project_kind, project_id)
stopifnot(!anyDuplicated(assessor_projects$project_id))
SaveData(assessor_projects, c("project_id"), "../output/residential_assessor_project_candidates.csv")
