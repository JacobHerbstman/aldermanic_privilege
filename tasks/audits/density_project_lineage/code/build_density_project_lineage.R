# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")
# exact_match_ft <- 10
# local_match_ft <- 150
# max_building_gap <- 0.10

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop(
    "Expected three arguments: exact_match_ft, local_match_ft, max_building_gap.",
    call. = FALSE
  )
}

exact_match_ft <- as.numeric(args[1])
local_match_ft <- as.numeric(args[2])
max_building_gap <- as.numeric(args[3])

if (!is.finite(exact_match_ft) || !is.finite(local_match_ft) ||
    !is.finite(max_building_gap) || exact_match_ft < 0 ||
    local_match_ft <= exact_match_ft || max_building_gap < 0) {
  stop("Invalid lineage threshold.", call. = FALSE)
}

commercial_source <- read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), pins = col_character(), .default = col_guess())
) %>%
  mutate(
    commercial_project_key = paste0(
      "commercial_",
      str_replace_all(coalesce(pins, pin), "[^0-9]", "")
    )
  )

if (anyDuplicated(commercial_source$pin) > 0) {
  stop("Commercial construction source is not unique by representative PIN.", call. = FALSE)
}

cases <- read_csv(
  "../input/density_historical_coordinate_cases.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), proration_key_pin = col_character(),
    .default = col_guess()
  )
) %>%
  left_join(
    commercial_source %>%
      transmute(
        pin,
        commercial_project_key,
        commercial_group_pins = pins,
        commercial_pin_group_count = as.integer(pin_group_count),
        commercial_address = address
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    source_card = as.integer(source_card),
    project_key = case_when(
      source == "residential_improvements" ~ paste0(
        "residential_",
        coalesce(na_if(proration_key_pin, ""), pin)
      ),
      source == "commercial_valuation" ~ commercial_project_key,
      TRUE ~ paste0(source, "_", pin)
    )
  )

if (anyDuplicated(cases$pin) > 0) {
  stop("Historical coordinate cases are not unique by PIN.", call. = FALSE)
}

historical <- read_csv(
  "../input/density_historical_parcel_records.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pin10 = col_character(), parcel_class = col_character(),
    subdivision_id = col_character(), .default = col_guess()
  )
)

if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical parcel records are not unique by PIN-year.", call. = FALSE)
}

exact_coordinates <- cases %>%
  select(pin, yearbuilt) %>%
  inner_join(
    historical,
    by = c("pin", "yearbuilt" = "year"),
    relationship = "one-to-one"
  ) %>%
  select(
    pin,
    exact_x = centroid_x_crs_3435,
    exact_y = centroid_y_crs_3435,
    historical_subdivision_id = subdivision_id,
    class_at_reported_year = parcel_class
  )

recovered_500ft_pins <- read_csv(
  "../input/density_historical_recovery_membership.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(
    analysis_sample == "existing_plus_exact_year",
    construction_sample == "all",
    outcome == "density_far",
    treatment == "continuous",
    sample_source == "recovered_historical_coordinate"
  ) %>%
  distinct(pin) %>%
  mutate(recovered_500ft = TRUE)

year_revisions <- read_csv(
  "../input/density_year_built_revision_cases.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    source_card = as.integer(card_num),
    year_revised = TRUE,
    minimum_report_construction_year,
    latest_report_construction_year,
    modal_report_construction_year,
    distinct_reported_years
  )

if (anyDuplicated(year_revisions[c("pin", "source_card")]) > 0) {
  stop("Year-revision cases are not unique by PIN-card.", call. = FALSE)
}

case_history <- cases %>%
  select(pin, project_key, yearbuilt) %>%
  left_join(historical, by = "pin", relationship = "one-to-many") %>%
  arrange(pin, year) %>%
  group_by(pin, project_key, yearbuilt) %>%
  summarise(
    years_observed_before = sum(year < yearbuilt),
    improved_years_through_two_years_before = sum(
      year <= yearbuilt - 2 & !parcel_class %in% c("100", "EX"),
      na.rm = TRUE
    ),
    class_change_near_reported_year = any(
      year >= yearbuilt - 1 & year <= yearbuilt + 2 &
        parcel_class != lag(parcel_class, order_by = year),
      na.rm = TRUE
    ),
    unimproved_to_improved_near_reported_year = any(
      year >= yearbuilt - 1 & year <= yearbuilt + 2 &
        lag(parcel_class, order_by = year) %in% c("100", "EX") &
        !parcel_class %in% c("100", "EX"),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    construction_year_evidence = case_when(
      unimproved_to_improved_near_reported_year ~ "unimproved_to_improved_near_reported_year",
      improved_years_through_two_years_before >= 3 & class_change_near_reported_year ~
        "preexisting_improved_class_change_near_reported_year",
      improved_years_through_two_years_before >= 3 ~
        "preexisting_improved_without_nearby_class_change",
      years_observed_before == 0 ~ "no_preconstruction_parcel_history",
      TRUE ~ "inconclusive_parcel_class_history"
    )
  )

case_rows <- cases %>%
  left_join(exact_coordinates, by = "pin", relationship = "one-to-one") %>%
  left_join(recovered_500ft_pins, by = "pin", relationship = "one-to-one") %>%
  left_join(
    year_revisions,
    by = c("pin", "source_card"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    case_history,
    by = c("pin", "project_key", "yearbuilt"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    recovered_500ft = coalesce(recovered_500ft, FALSE),
    year_revised = coalesce(year_revised, FALSE),
    coordinate_weight = case_when(
      is.finite(pin_proration_rate) & pin_proration_rate > 0 ~ pin_proration_rate,
      TRUE ~ 1
    )
  )

historical_projects <- case_rows %>%
  arrange(project_key, pin) %>%
  group_by(project_key) %>%
  summarise(
    source = first(source),
    source_class = paste(sort(unique(na.omit(source_class))), collapse = ";"),
    commercial_group_pins = first(commercial_group_pins),
    commercial_pin_group_count = first(commercial_pin_group_count),
    commercial_address = first(commercial_address),
    source_cards = paste(sort(unique(na.omit(source_card))), collapse = ";"),
    source_record_count = n(),
    member_pins = paste(sort(unique(pin)), collapse = ";"),
    member_pin_count = n_distinct(pin),
    exact_coordinate_pin_count = sum(is.finite(exact_x) & is.finite(exact_y)),
    recovered_500ft_pin_count = sum(recovered_500ft),
    recovered_500ft = any(recovered_500ft),
    multifamily = max(unitscount, na.rm = TRUE) > 1,
    reported_construction_year = min(yearbuilt, na.rm = TRUE),
    distinct_construction_years = n_distinct(yearbuilt),
    unitscount = max(unitscount, na.rm = TRUE),
    distinct_unit_counts = n_distinct(unitscount),
    areabuilding = max(areabuilding, na.rm = TRUE),
    distinct_building_areas = n_distinct(areabuilding),
    maximum_lot_area = max(arealotsf, na.rm = TRUE),
    summed_member_lot_area = sum(arealotsf, na.rm = TRUE),
    historical_x = if_else(
      any(is.finite(exact_x) & is.finite(exact_y)),
      weighted.mean(
        exact_x[is.finite(exact_x) & is.finite(exact_y)],
        coordinate_weight[is.finite(exact_x) & is.finite(exact_y)]
      ),
      NA_real_
    ),
    historical_y = if_else(
      any(is.finite(exact_x) & is.finite(exact_y)),
      weighted.mean(
        exact_y[is.finite(exact_x) & is.finite(exact_y)],
        coordinate_weight[is.finite(exact_x) & is.finite(exact_y)]
      ),
      NA_real_
    ),
    historical_subdivision_id = {
      values <- sort(unique(na.omit(historical_subdivision_id)))
      if (length(values) == 1) values else NA_character_
    },
    class_at_reported_year = paste(
      sort(unique(na.omit(class_at_reported_year))), collapse = ";"
    ),
    year_revised = any(year_revised),
    construction_year_evidence = paste(
      sort(unique(construction_year_evidence)), collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    historical_pin_prefix_8 = substr(member_pins, 1, 8),
    source_group_conflict =
      distinct_construction_years > 1 |
      distinct_unit_counts > 1 |
      distinct_building_areas > 1
  )

residential <- read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), proration_key_pin = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    residential_single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    unitscount = if_else(
      residential_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) %>%
  transmute(
    pin,
    construction_year = as.integer(year_built),
    arealotsf = as.numeric(land_sqft),
    areabuilding = as.numeric(building_sqft),
    unitscount,
    source = "residential_improvements",
    project_key = paste0(
      "residential_",
      coalesce(na_if(proration_key_pin, ""), pin)
    ),
    coordinate_weight = if_else(
      is.finite(pin_proration_rate) & pin_proration_rate > 0,
      as.numeric(pin_proration_rate),
      1
    )
  )

commercial <- commercial_source %>%
  transmute(
    pin,
    construction_year = as.integer(yearbuilt),
    arealotsf = as.numeric(landsf),
    areabuilding = as.numeric(bldgsf),
    unitscount = as.numeric(tot_units),
    source = "commercial_valuation",
    project_key = commercial_project_key,
    coordinate_weight = 1
  )

current_source <- bind_rows(residential, commercial) %>%
  group_by(pin) %>%
  arrange(desc(unitscount), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    construction_year >= 2006,
    construction_year <= 2022,
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0
  )

current_geometry <- st_read(
  "../input/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin))

if (anyDuplicated(current_geometry$pin) > 0) {
  stop("Current production geometry is not unique by PIN.", call. = FALSE)
}

current_coordinates <- st_coordinates(current_geometry)
current_geometry <- current_geometry %>%
  st_drop_geometry() %>%
  mutate(current_x = current_coordinates[, 1], current_y = current_coordinates[, 2]) %>%
  select(pin, current_x, current_y)

current_parcels <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pin10 = col_character(), class = col_character(),
    centroid_x_crs_3435 = col_double(), centroid_y_crs_3435 = col_double(),
    subdivision_id = col_character(), .default = col_skip()
  )
) %>%
  transmute(
    pin,
    pin10,
    current_parcel_class = class,
    current_subdivision_id = subdivision_id,
    current_x = as.numeric(centroid_x_crs_3435),
    current_y = as.numeric(centroid_y_crs_3435)
  ) %>%
  filter(is.finite(current_x), is.finite(current_y))

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel universe is not unique by PIN.", call. = FALSE)
}

current_source <- current_source %>%
  inner_join(current_geometry, by = "pin", relationship = "one-to-one") %>%
  left_join(
    current_parcels %>% select(pin, current_subdivision_id),
    by = "pin",
    relationship = "one-to-one"
  )

current_projects <- current_source %>%
  arrange(project_key, pin) %>%
  group_by(project_key) %>%
  summarise(
    current_member_pins = paste(sort(unique(pin)), collapse = ";"),
    current_member_pin_count = n_distinct(pin),
    current_source_record_count = n(),
    current_source = first(source),
    current_construction_year = min(construction_year),
    current_distinct_construction_years = n_distinct(construction_year),
    current_unitscount = max(unitscount),
    current_distinct_unit_counts = n_distinct(unitscount),
    current_areabuilding = max(areabuilding),
    current_distinct_building_areas = n_distinct(areabuilding),
    current_x = weighted.mean(current_x, coordinate_weight),
    current_y = weighted.mean(current_y, coordinate_weight),
    current_subdivision_id = {
      values <- sort(unique(na.omit(current_subdivision_id)))
      if (length(values) == 1) values else NA_character_
    },
    .groups = "drop"
  ) %>%
  rename(current_project_key = project_key) %>%
  mutate(
    current_pin_prefix_8 = substr(current_member_pins, 1, 8),
    current_source_group_conflict =
      current_distinct_construction_years > 1 |
      current_distinct_unit_counts > 1 |
      current_distinct_building_areas > 1
  )

lineage <- historical_projects
coordinate_rows <- which(is.finite(lineage$historical_x) & is.finite(lineage$historical_y))

nearest_parcel <- nabor::knn(
  data = as.matrix(current_parcels[c("current_x", "current_y")]),
  query = as.matrix(lineage[coordinate_rows, c("historical_x", "historical_y")]),
  k = 1
)

parcel_matches <- current_parcels[nearest_parcel$nn.idx[, 1], ] %>%
  transmute(
    nearest_current_pin = pin,
    nearest_current_pin10 = pin10,
    nearest_current_parcel_class = current_parcel_class,
    nearest_current_subdivision_id = current_subdivision_id,
    nearest_current_parcel_distance_ft = nearest_parcel$nn.dists[, 1]
  ) %>%
  mutate(lineage_row = coordinate_rows)

lineage <- lineage %>%
  mutate(lineage_row = row_number()) %>%
  left_join(parcel_matches, by = "lineage_row", relationship = "one-to-one") %>%
  mutate(
    nearest_current_parcel_same_prefix_8 =
      historical_pin_prefix_8 == substr(nearest_current_pin, 1, 8),
    nearest_current_parcel_same_subdivision =
      !is.na(historical_subdivision_id) & historical_subdivision_id != "" &
      historical_subdivision_id == nearest_current_subdivision_id,
    coordinate_lineage_status = case_when(
      exact_coordinate_pin_count == 0 ~ "no_exact_historical_coordinate",
      nearest_current_parcel_distance_ft <= 1 ~ "exact_current_parcel_location",
      nearest_current_parcel_distance_ft <= 100 &
        (nearest_current_parcel_same_prefix_8 |
          nearest_current_parcel_same_subdivision) ~ "linked_current_parcel",
      nearest_current_parcel_distance_ft <= 100 ~ "local_current_parcel",
      TRUE ~ "no_local_current_parcel"
    )
  )

same_year_matches <- list()
for (construction_year_i in sort(unique(lineage$reported_construction_year))) {
  historical_rows <- which(
    lineage$reported_construction_year == construction_year_i &
      is.finite(lineage$historical_x) & is.finite(lineage$historical_y)
  )
  current_rows <- which(current_projects$current_construction_year == construction_year_i)
  if (length(historical_rows) == 0 || length(current_rows) == 0) {
    next
  }

  nearest <- nabor::knn(
    data = as.matrix(current_projects[current_rows, c("current_x", "current_y")]),
    query = as.matrix(lineage[historical_rows, c("historical_x", "historical_y")]),
    k = 1
  )

  same_year_matches[[as.character(construction_year_i)]] <-
    current_projects[current_rows[nearest$nn.idx[, 1]], ] %>%
    mutate(
      lineage_row = historical_rows,
      same_year_project_distance_ft = nearest$nn.dists[, 1]
    )
}

same_year_matches <- bind_rows(same_year_matches) %>%
  select(
    lineage_row,
    same_year_current_project_key = current_project_key,
    same_year_current_member_pins = current_member_pins,
    same_year_current_member_pin_count = current_member_pin_count,
    same_year_current_source_record_count = current_source_record_count,
    same_year_current_source = current_source,
    same_year_current_construction_year = current_construction_year,
    same_year_current_unitscount = current_unitscount,
    same_year_current_areabuilding = current_areabuilding,
    same_year_current_source_group_conflict = current_source_group_conflict,
    same_year_current_subdivision_id = current_subdivision_id,
    same_year_current_pin_prefix_8 = current_pin_prefix_8,
    same_year_project_distance_ft
  )

lineage <- lineage %>%
  left_join(same_year_matches, by = "lineage_row", relationship = "one-to-one")

nearest_project <- nabor::knn(
  data = as.matrix(current_projects[c("current_x", "current_y")]),
  query = as.matrix(lineage[coordinate_rows, c("historical_x", "historical_y")]),
  k = 1
)

nearest_project_matches <- current_projects[nearest_project$nn.idx[, 1], ] %>%
  transmute(
    nearest_current_project_key = current_project_key,
    nearest_current_project_pins = current_member_pins,
    nearest_current_project_year = current_construction_year,
    nearest_current_project_units = current_unitscount,
    nearest_current_project_building_area = current_areabuilding,
    nearest_current_project_distance_ft = nearest_project$nn.dists[, 1]
  ) %>%
  mutate(lineage_row = coordinate_rows)

lineage <- lineage %>%
  left_join(nearest_project_matches, by = "lineage_row", relationship = "one-to-one") %>%
  mutate(
    same_year_unit_gap = abs(unitscount - same_year_current_unitscount),
    same_year_building_gap = abs(areabuilding - same_year_current_areabuilding) /
      pmax(areabuilding, same_year_current_areabuilding),
    same_year_same_prefix_8 =
      historical_pin_prefix_8 == same_year_current_pin_prefix_8,
    same_year_same_subdivision =
      !is.na(historical_subdivision_id) & historical_subdivision_id != "" &
      historical_subdivision_id == same_year_current_subdivision_id,
    successor_match_reason = case_when(
      same_year_current_source_group_conflict ~ NA_character_,
      same_year_project_distance_ft <= 1 ~ "same_year_exact_centroid",
      same_year_project_distance_ft <= exact_match_ft &
        same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap ~
        "same_year_exact_location_and_attributes",
      same_year_project_distance_ft <= 50 &
        same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~
        "same_year_local_identity_and_attributes",
      same_year_project_distance_ft <= local_match_ft &
        same_year_building_gap <= 0.02 &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~
        "same_year_local_identity_and_building_area",
      TRUE ~ NA_character_
    ),
    high_confidence_successor = !is.na(successor_match_reason),
    probable_successor =
      !high_confidence_successor &
      !same_year_current_source_group_conflict &
      same_year_project_distance_ft <= local_match_ft &
      same_year_building_gap <= max_building_gap &
      (
        same_year_unit_gap == 0 |
        same_year_same_prefix_8 |
        same_year_same_subdivision
      ),
    nearby_different_year_project =
      nearest_current_project_distance_ft <= 50 &
      abs(reported_construction_year - nearest_current_project_year) <= 2,
    construction_year_review = case_when(
      year_revised ~ "reported_year_revised_across_assessor_files",
      str_detect(
        construction_year_evidence,
        "preexisting_improved_without_nearby_class_change"
      ) ~ "preexisting_improved_parcel_without_nearby_class_change",
      str_detect(
        construction_year_evidence,
        "preexisting_improved_class_change_near_reported_year"
      ) ~ "possible_adaptive_reuse_or_rebuild",
      TRUE ~ "no_lineage_warning"
    ),
    lineage_status = case_when(
      exact_coordinate_pin_count == 0 ~ "unresolved_no_exact_historical_coordinate",
      source_group_conflict ~ "unresolved_source_group_conflict",
      high_confidence_successor ~ "duplicate_high_confidence",
      probable_successor ~ "duplicate_probable",
      same_year_project_distance_ft <= local_match_ft ~ "unresolved_nearby_same_year_project",
      nearby_different_year_project ~ "unresolved_nearby_different_year_project",
      TRUE ~ "candidate_unique_historical_project"
    ),
    recommended_action = case_when(
      lineage_status == "duplicate_high_confidence" ~ "exclude_as_already_represented",
      lineage_status == "candidate_unique_historical_project" ~ "candidate_for_recovery",
      TRUE ~ "hold_out_pending_resolution"
    ),
    exact_match_threshold_ft = exact_match_ft,
    local_match_threshold_ft = local_match_ft,
    maximum_building_area_gap = max_building_gap
  ) %>%
  select(-lineage_row) %>%
  arrange(desc(recovered_500ft), lineage_status, project_key)

write_csv(lineage, "../output/density_project_lineage.csv")

write_csv(
  lineage %>%
    group_by(lineage_status, recommended_action) %>%
    summarise(
      historical_projects = n(),
      historical_pins = sum(member_pin_count),
      exact_coordinate_projects = sum(exact_coordinate_pin_count > 0),
      projects_in_500ft_model_sample = sum(recovered_500ft),
      pins_in_500ft_model_sample = sum(recovered_500ft_pin_count),
      multifamily_projects_in_500ft_model_sample = sum(recovered_500ft & multifamily),
      .groups = "drop"
    ) %>%
    arrange(desc(projects_in_500ft_model_sample), lineage_status),
  "../output/density_project_lineage_summary.csv"
)

write_csv(
  lineage %>%
    filter(
      recovered_500ft,
      recommended_action == "hold_out_pending_resolution"
    ),
  "../output/density_project_lineage_unresolved_500ft.csv"
)
