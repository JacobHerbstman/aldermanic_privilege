# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[.,#]", " ") %>%
    str_replace_all("\\bNORTH\\b", "N") %>%
    str_replace_all("\\bSOUTH\\b", "S") %>%
    str_replace_all("\\bEAST\\b", "E") %>%
    str_replace_all("\\bWEST\\b", "W") %>%
    str_replace_all("\\bAVENUE\\b", "AVE") %>%
    str_replace_all("\\bSTREET\\b", "ST") %>%
    str_replace_all("\\bROAD\\b", "RD") %>%
    str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
    str_replace_all("\\bDRIVE\\b", "DR") %>%
    str_replace_all("\\bPLACE\\b", "PL") %>%
    str_replace_all("\\bCOURT\\b", "CT") %>%
    str_replace_all("\\bPARKWAY\\b", "PKWY") %>%
    str_replace_all("\\bTERRACE\\b", "TER") %>%
    str_replace_all("\\bHIGHWAY\\b", "HWY") %>%
    str_replace_all("\\bLANE\\b", "LN") %>%
    str_squish() %>%
    na_if("")
}

targets <- read_csv(
  "../output/density_parcel_address_targets.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
historical_addresses <- read_csv(
  "../output/density_historical_address_records.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), pin10 = col_character(), .default = col_guess())
) %>%
  inner_join(
    targets %>% select(pin, construction_year),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  mutate(
    address_normalized = normalize_address(property_address),
    year_gap = year - construction_year,
    absolute_year_gap = abs(year_gap)
  ) %>%
  filter(!is.na(address_normalized))

nearest_address_rows <- historical_addresses %>%
  group_by(pin) %>%
  filter(absolute_year_gap == min(absolute_year_gap)) %>%
  arrange(desc(year <= construction_year), desc(year), .by_group = TRUE) %>%
  mutate(nearest_address_count = n_distinct(address_normalized)) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    pin,
    selected_address = property_address,
    selected_address_normalized = address_normalized,
    selected_address_year = year,
    selected_address_year_gap = year_gap,
    nearest_address_count
  )

address_history_summary <- historical_addresses %>%
  group_by(pin) %>%
  summarise(
    address_history_rows = n(),
    distinct_historical_addresses = n_distinct(address_normalized),
    .groups = "drop"
  )

selected_history <- targets %>%
  left_join(nearest_address_rows, by = "pin", relationship = "one-to-one") %>%
  left_join(address_history_summary, by = "pin", relationship = "one-to-one") %>%
  mutate(
    address_selection_status = case_when(
      is.na(selected_address_normalized) ~ "no_historical_property_address",
      nearest_address_count > 1 ~ "ambiguous_nearest_year_address",
      TRUE ~ "selected_nearest_year_address"
    )
  )

current_addresses <- fread(
  "../input/parcel_addresses_2025_chicago.csv",
  select = c("pin", "prop_address_full")
) %>%
  as_tibble() %>%
  transmute(
    pin = as.character(pin),
    current_property_address = as.character(prop_address_full),
    address_normalized = normalize_address(prop_address_full)
  )

if (anyDuplicated(current_addresses$pin) > 0) {
  stop("Current parcel addresses are not unique by PIN.", call. = FALSE)
}

current_parcels <- fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "class", "longitude", "latitude",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  )
) %>%
  as_tibble() %>%
  transmute(
    pin = as.character(pin),
    current_parcel_class = as.character(class),
    current_longitude = as.numeric(longitude),
    current_latitude = as.numeric(latitude),
    current_x_crs_3435 = as.numeric(centroid_x_crs_3435),
    current_y_crs_3435 = as.numeric(centroid_y_crs_3435)
  )

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel coordinates are not unique by PIN.", call. = FALSE)
}

address_candidates <- current_addresses %>%
  filter(!is.na(address_normalized)) %>%
  left_join(current_parcels, by = "pin", relationship = "one-to-one") %>%
  mutate(
    coordinate_complete =
      is.finite(current_x_crs_3435) & is.finite(current_y_crs_3435)
  ) %>%
  group_by(address_normalized) %>%
  mutate(
    address_x_crs_3435 = mean(current_x_crs_3435[coordinate_complete]),
    address_y_crs_3435 = mean(current_y_crs_3435[coordinate_complete]),
    distance_from_address_center_ft = sqrt(
      (current_x_crs_3435 - address_x_crs_3435)^2 +
        (current_y_crs_3435 - address_y_crs_3435)^2
    )
  ) %>%
  summarise(
    current_address_pin_count = n(),
    current_address_coordinate_count = sum(coordinate_complete),
    current_address_class_299_count = sum(current_parcel_class == "299", na.rm = TRUE),
    address_longitude = mean(current_longitude[coordinate_complete]),
    address_latitude = mean(current_latitude[coordinate_complete]),
    address_x_crs_3435 = first(address_x_crs_3435),
    address_y_crs_3435 = first(address_y_crs_3435),
    maximum_coordinate_spread_ft = {
      values <- distance_from_address_center_ft[
        coordinate_complete & is.finite(distance_from_address_center_ft)
      ]
      if (length(values) > 0) max(values) else NA_real_
    },
    current_address_pins = paste(sort(unique(pin)), collapse = ";"),
    .groups = "drop"
  )

building_universe <- read_csv(
  "../output/density_historical_building_universe.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), source_class = col_character(), current_parcel_class = col_character(),
    .default = col_guess()
  )
)
if (anyDuplicated(building_universe$pin) > 0) {
  stop("Building universe is not unique by original PIN.", call. = FALSE)
}

unlocated <- read_csv(
  "../output/density_historical_parcel_unmatched.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), source_class = col_character(), parcel_class = col_character(),
    .default = col_guess()
  )
) %>%
  filter(!current_coordinates_complete) %>%
  left_join(
    building_universe %>% select(pin, arealotsf, areabuilding),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(multifamily = unitscount > 1)

historical_parcels <- read_csv(
  "../input/density_historical_parcel_records.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), parcel_class = col_character(), subdivision_id = col_character(),
    .default = col_guess()
  )
)
if (anyDuplicated(historical_parcels[c("pin", "year")]) > 0) {
  stop("Historical parcel coordinates are not unique by PIN-year.", call. = FALSE)
}

nearest_parcel_coordinates <- unlocated %>%
  select(pin, construction_year) %>%
  inner_join(historical_parcels, by = "pin", relationship = "one-to-many") %>%
  mutate(
    parcel_year_gap = year - construction_year,
    absolute_parcel_year_gap = abs(parcel_year_gap)
  ) %>%
  group_by(pin) %>%
  arrange(
    absolute_parcel_year_gap,
    desc(year <= construction_year),
    desc(year),
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    pin,
    nearest_parcel_year = year,
    nearest_parcel_year_gap = parcel_year_gap,
    nearest_parcel_x_crs_3435 = centroid_x_crs_3435,
    nearest_parcel_y_crs_3435 = centroid_y_crs_3435
  )

unlocated_recovery <- unlocated %>%
  left_join(selected_history, by = c("pin", "construction_year"), relationship = "one-to-one") %>%
  left_join(
    address_candidates,
    by = c("selected_address_normalized" = "address_normalized"),
    relationship = "many-to-one"
  ) %>%
  left_join(nearest_parcel_coordinates, by = "pin", relationship = "one-to-one") %>%
  mutate(
    address_nearest_parcel_distance_ft = sqrt(
      (address_x_crs_3435 - nearest_parcel_x_crs_3435)^2 +
        (address_y_crs_3435 - nearest_parcel_y_crs_3435)^2
    ),
    address_recovery_status = case_when(
      coverage_status == "excluded_historical_class_299" ~
        "exclude_historical_class_299",
      address_selection_status != "selected_nearest_year_address" ~
        address_selection_status,
      is.na(current_address_pin_count) ~ "no_current_parcel_at_historical_address",
      current_address_coordinate_count == 0 ~ "current_address_match_missing_coordinates",
      maximum_coordinate_spread_ft > 50 ~ "multiple_current_locations_at_address",
      is.finite(address_nearest_parcel_distance_ft) &
        address_nearest_parcel_distance_ft > 100 ~
        "address_conflicts_with_nearest_year_parcel",
      is.finite(address_nearest_parcel_distance_ft) ~
        "recover_unique_address_confirmed_by_parcel_history",
      TRUE ~ "recover_unique_address_without_parcel_confirmation"
    )
  )

source_buildings <- building_universe %>%
  select(
    source_candidate_pin = pin,
    source_candidate_year = construction_year,
    source_candidate_units = unitscount,
    source_candidate_building_area = areabuilding
  )

address_source_evidence <- unlocated_recovery %>%
  select(
    original_pin = pin,
    construction_year,
    unitscount,
    areabuilding,
    current_address_pins
  ) %>%
  separate_longer_delim(current_address_pins, delim = ";") %>%
  rename(source_candidate_pin = current_address_pins) %>%
  inner_join(source_buildings, by = "source_candidate_pin", relationship = "many-to-one") %>%
  mutate(
    source_candidate_year_gap = abs(source_candidate_year - construction_year),
    source_candidate_unit_gap = abs(source_candidate_units - unitscount),
    source_candidate_building_gap = abs(
      source_candidate_building_area - areabuilding
    ) / pmax(source_candidate_building_area, areabuilding),
    source_candidate_duplicate =
      source_candidate_year_gap == 0 &
      source_candidate_unit_gap == 0 &
      source_candidate_building_gap <= 0.10
  ) %>%
  arrange(
    original_pin,
    desc(source_candidate_duplicate),
    source_candidate_year_gap,
    source_candidate_unit_gap,
    source_candidate_building_gap
  ) %>%
  group_by(original_pin) %>%
  summarise(
    current_address_source_candidate_count = n_distinct(source_candidate_pin),
    source_successor_duplicate = any(source_candidate_duplicate),
    closest_source_candidate_pin = first(source_candidate_pin),
    closest_source_candidate_year = first(source_candidate_year),
    closest_source_candidate_units = first(source_candidate_units),
    closest_source_candidate_building_area = first(source_candidate_building_area),
    closest_source_candidate_year_gap = first(source_candidate_year_gap),
    closest_source_candidate_unit_gap = first(source_candidate_unit_gap),
    closest_source_candidate_building_gap = first(source_candidate_building_gap),
    .groups = "drop"
  )

unlocated_recovery <- unlocated_recovery %>%
  left_join(
    address_source_evidence,
    by = c("pin" = "original_pin"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    current_address_source_candidate_count = coalesce(
      current_address_source_candidate_count,
      0L
    ),
    source_successor_duplicate = coalesce(source_successor_duplicate, FALSE),
    address_recovery_status = case_when(
      str_starts(address_recovery_status, "recover_") & source_successor_duplicate ~
        "exclude_address_successor_already_in_source",
      str_starts(address_recovery_status, "recover_") &
        current_address_source_candidate_count > 0 ~
        "hold_address_successor_source_conflict",
      TRUE ~ address_recovery_status
    )
  )

lineage_all <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(
    member_pins = col_character(), nearest_current_pin = col_character(),
    same_year_current_member_pins = col_character(),
    nearest_current_project_pins = col_character(),
    .default = col_guess()
  )
)

lineage_pin_lookup <- lineage_all %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  transmute(project_key, pin = member_pins)

recovered_project_counts <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(dist_to_boundary_m <= 457.2) %>%
  select(pin) %>%
  left_join(lineage_pin_lookup, by = "pin", relationship = "one-to-one") %>%
  count(project_key, name = "recovered_audit_pin_count")

if (any(is.na(recovered_project_counts$project_key))) {
  stop("A recovered 1,500-foot PIN is missing from project lineage.", call. = FALSE)
}

lineage <- lineage_all %>%
  inner_join(recovered_project_counts, by = "project_key", relationship = "one-to-one")

project_target_addresses <- lineage %>%
  select(project_key, member_pins) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins) %>%
  left_join(
    selected_history %>%
      select(pin, selected_address_normalized, address_selection_status),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    target_address_count = n_distinct(
      selected_address_normalized[address_selection_status == "selected_nearest_year_address"],
      na.rm = TRUE
    ),
    target_addresses = paste(
      sort(unique(na.omit(selected_address_normalized[
        address_selection_status == "selected_nearest_year_address"
      ]))),
      collapse = ";"
    ),
    .groups = "drop"
  )

candidate_addresses <- lineage %>%
  select(
    project_key,
    same_year_current_member_pins,
    nearest_current_project_pins
  ) %>%
  pivot_longer(
    cols = -project_key,
    names_to = "candidate_source",
    values_to = "candidate_pin_text"
  ) %>%
  mutate(candidate_pin = str_extract_all(candidate_pin_text, "[0-9]{14}")) %>%
  unnest_longer(candidate_pin) %>%
  filter(!is.na(candidate_pin)) %>%
  distinct(project_key, candidate_pin) %>%
  left_join(
    current_addresses %>% select(candidate_pin = pin, candidate_address = address_normalized),
    by = "candidate_pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    candidate_pin_count = n_distinct(candidate_pin),
    candidate_address_count = n_distinct(candidate_address, na.rm = TRUE),
    candidate_addresses = paste(sort(unique(na.omit(candidate_address))), collapse = ";"),
    .groups = "drop"
  )

nearest_project_addresses <- lineage %>%
  select(project_key, nearest_current_project_pins) %>%
  mutate(candidate_pin = str_extract_all(nearest_current_project_pins, "[0-9]{14}")) %>%
  unnest_longer(candidate_pin) %>%
  filter(!is.na(candidate_pin)) %>%
  distinct(project_key, candidate_pin) %>%
  left_join(
    current_addresses %>% select(candidate_pin = pin, nearest_project_address = address_normalized),
    by = "candidate_pin",
    relationship = "many-to-one"
  ) %>%
  group_by(project_key) %>%
  summarise(
    nearest_project_addresses = paste(
      sort(unique(na.omit(nearest_project_address))),
      collapse = ";"
    ),
    .groups = "drop"
  )

lineage_evidence <- lineage %>%
  left_join(project_target_addresses, by = "project_key", relationship = "one-to-one") %>%
  left_join(candidate_addresses, by = "project_key", relationship = "one-to-one") %>%
  left_join(nearest_project_addresses, by = "project_key", relationship = "one-to-one") %>%
  mutate(
    across(
      c(target_address_count, candidate_pin_count, candidate_address_count),
      ~ coalesce(.x, 0L)
    ),
    target_addresses = coalesce(target_addresses, ""),
    candidate_addresses = coalesce(candidate_addresses, ""),
    nearest_project_addresses = coalesce(nearest_project_addresses, ""),
    exact_address_match = map2_lgl(
      str_split(target_addresses, fixed(";")),
      str_split(candidate_addresses, fixed(";")),
      ~ length(intersect(.x[.x != ""], .y[.y != ""])) > 0
    ),
    exact_nearest_project_address_match = map2_lgl(
      str_split(target_addresses, fixed(";")),
      str_split(nearest_project_addresses, fixed(";")),
      ~ length(intersect(.x[.x != ""], .y[.y != ""])) > 0
    ),
    address_lineage_evidence = case_when(
      exact_address_match ~ "exact_address_supports_duplicate",
      target_address_count > 0 & candidate_address_count > 0 ~
        "addresses_differ_not_dispositive",
      target_address_count == 0 ~ "missing_historical_project_address",
      TRUE ~ "missing_candidate_project_address"
    ),
    nearest_project_building_gap = abs(
      nearest_current_project_building_area - areabuilding
    ) / pmax(nearest_current_project_building_area, areabuilding),
    nearest_project_unit_gap = abs(nearest_current_project_units - unitscount),
    address_duplicate_confirmation =
      exact_nearest_project_address_match &
      nearest_project_building_gap <= 0.10 &
      nearest_project_unit_gap == 0,
    address_audit_recommendation = case_when(
      address_duplicate_confirmation ~
        "exclude_address_confirmed_duplicate",
      recommended_action == "hold_out_pending_resolution" ~
        "continue_hold_out_pending_resolution",
      TRUE ~ recommended_action
    )
  )

lineage_summary <- lineage_evidence %>%
  group_by(recommended_action, address_lineage_evidence, address_audit_recommendation) %>%
  summarise(
    projects = n(),
    recovered_audit_pin_rows = sum(recovered_audit_pin_count),
    multifamily_pin_rows = sum(recovered_audit_pin_count * multifamily),
    .groups = "drop"
  )
recovery_summary <- unlocated_recovery %>%
  group_by(multifamily, address_recovery_status) %>%
  summarise(buildings = n(), .groups = "drop") %>%
  mutate(summary_section = "unlocated_coordinate_recovery") %>%
  rename(category = address_recovery_status)

write_csv(
  bind_rows(
    recovery_summary %>%
      transmute(
        summary_section,
        sample = if_else(multifamily, "multifamily", "non_multifamily"),
        category,
        buildings
      ),
    lineage_summary %>%
      transmute(
        summary_section = "lineage_address_evidence",
        sample = recommended_action,
        category = paste(address_lineage_evidence, address_audit_recommendation, sep = " | "),
        buildings = projects
      )
  ),
  "../output/density_parcel_address_audit_summary.csv"
)

write_csv(
  selected_history,
  "../output/density_parcel_address_selected_history.csv"
)
write_csv(
  unlocated_recovery,
  "../output/density_parcel_address_unlocated_recovery.csv"
)
write_csv(
  lineage_evidence,
  "../output/density_parcel_address_lineage_evidence.csv"
)
write_csv(
  lineage_summary,
  "../output/density_parcel_address_lineage_summary.csv"
)
