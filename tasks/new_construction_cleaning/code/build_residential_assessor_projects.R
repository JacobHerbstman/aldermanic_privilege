# setwd("tasks/new_construction_cleaning/code")
# maximum_building_gap <- 0.02
# maximum_year_gap <- 2

source("../../setup_environment/code/packages.R")
source("../../shared/code/assessor_classification.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(maximum_building_gap, maximum_year_gap)
stopifnot(length(args) == 2L)
maximum_building_gap <- as.numeric(args[1])
maximum_year_gap <- as.integer(args[2])
stopifnot(is.finite(maximum_building_gap), maximum_building_gap >= 0,
  !is.na(maximum_year_gap), maximum_year_gap >= 0)

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
      temporal_status != "temporally_resolved" | has_class_297 | has_multicard ~
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
    multicard_candidates$decision_reason[i] <- "complete_individual_coverage"
  }
}

# Membership and measurements are built together from the same source records.
assessor_projects <- bind_rows(ordinary_candidates, tieback_candidates, multicard_candidates) %>%
  arrange(project_kind, project_id)
stopifnot(!anyNA(assessor_projects$project_id), !anyDuplicated(assessor_projects$project_id))
readr::write_csv(assessor_projects, "../output/residential_assessor_project_candidates.csv")
