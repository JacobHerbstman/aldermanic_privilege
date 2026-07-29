# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  digits <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
  dplyr::if_else(stringr::str_length(digits) == 14, digits, NA_character_)
}

same_number <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & dplyr::near(x, y))
}

expand_entity_pins <- function(entity_pin, pins_text) {
  tokens <- stringr::str_split(dplyr::coalesce(pins_text, ""), ",")[[1]] %>%
    stringr::str_trim()
  tokens <- tokens[tokens != ""]

  expanded <- character()
  base_prefix <- NA_character_
  base_suffix <- "0000"
  for (token in tokens) {
    digits <- stringr::str_replace_all(token, "[^0-9]", "")
    if (nchar(digits) == 14) {
      expanded <- c(expanded, digits)
      base_prefix <- substr(digits, 1, 7)
      base_suffix <- substr(digits, 11, 14)
    } else if (nchar(digits) == 3 && !is.na(base_prefix)) {
      expanded <- c(expanded, paste0(base_prefix, digits, base_suffix))
    }
  }

  tibble::tibble(
    pin = entity_pin,
    component_pin = unique(c(entity_pin, expanded))
  )
}

chicago_townships <- c(
  "West Chicago", "South Chicago", "Jefferson", "North Chicago",
  "Lake View", "Rogers Park", "Hyde Park", "Lake"
)
apartment_unit_cols <- c("studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits")

cleaned <- readr::read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
)

raw <- readr::read_csv(
  "../input/commercial_value_raw.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = "c")
) %>%
  janitor::clean_names()

if (!"modelgroup" %in% names(raw) && "sheet" %in% names(raw)) {
  raw <- raw %>% dplyr::rename(modelgroup = sheet)
}

raw <- raw %>%
  mutate(
    raw_row = row_number(),
    pin = normalize_pin(keypin),
    across(
      any_of(c("year", apartment_unit_cols, "tot_units", "bldgsf", "landsf", "yearbuilt", "aprx_comm_sf")),
      ~ suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", .x)))
    ),
    apartment_unit_sum = rowSums(pick(all_of(apartment_unit_cols)), na.rm = TRUE),
    preliminary_units = case_when(
      apartment_unit_sum > 0 ~ apartment_unit_sum,
      !is.na(tot_units) & tot_units > 0 ~ tot_units,
      TRUE ~ NA_real_
    ),
    has_land = landsf > 0,
    multifamily_model = str_detect(modelgroup, regex("Multifamily|Class3|Class9|Condos", ignore_case = TRUE)),
    chicago = township %in% chicago_townships
  ) %>%
  left_join(
    cleaned %>%
      select(pin, manual_residential_units) %>%
      distinct(),
    by = "pin",
    relationship = "many-to-one"
  ) %>%
  mutate(selected_units = coalesce(manual_residential_units, preliminary_units))

selected <- raw %>%
  filter(
    chicago,
    yearbuilt >= 1999,
    multifamily_model,
    !is.na(preliminary_units)
  ) %>%
  group_by(pin) %>%
  arrange(desc(has_land), desc(selected_units), desc(year), modelgroup, address, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

if (anyDuplicated(selected$pin) > 0) {
  stop("Reproduced commercial selection is not unique by key PIN.", call. = FALSE)
}
if (!setequal(selected$pin, cleaned$pin)) {
  stop("Audit does not reproduce the set of cleaned commercial key PINs.", call. = FALSE)
}

selection_check <- selected %>%
  transmute(
    pin,
    selected_valuation_year = as.integer(year),
    selected_yearbuilt = as.integer(yearbuilt),
    selected_address = address,
    selected_modelgroup = modelgroup,
    selected_source_tot_units = tot_units,
    selected_apartment_unit_sum = apartment_unit_sum,
    selected_unit_rule = if_else(
      apartment_unit_sum > 0,
      "apartment_unit_sum",
      "tot_units_fallback"
    ),
    selected_units_before_override = preliminary_units,
    selected_units_after_override = selected_units,
    selected_bldgsf = bldgsf,
    selected_source_landsf = landsf,
    selected_pins = pins
  ) %>%
  left_join(
    cleaned %>%
      transmute(
        pin,
        production_yearbuilt = as.integer(yearbuilt),
        production_address = address,
        production_modelgroup = modelgroup,
        production_units = as.numeric(tot_units),
        production_bldgsf = as.numeric(bldgsf),
        production_landsf = as.numeric(landsf),
        production_source_landsf = as.numeric(source_landsf),
        production_pin_group_count = as.integer(pin_group_count),
        production_land_correction = as.logical(apply_land_correction)
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    exact_selected_row =
      selected_yearbuilt == production_yearbuilt &
      selected_modelgroup == production_modelgroup &
      selected_units_after_override == production_units &
      same_number(selected_bldgsf, production_bldgsf) &
      same_number(selected_source_landsf, production_source_landsf)
  )

if (any(!selection_check$exact_selected_row)) {
  stop("Audit does not reproduce one or more production commercial source rows.", call. = FALSE)
}

vintage_rows <- raw %>%
  filter(
    pin %in% selected$pin,
    chicago,
    multifamily_model,
    !is.na(preliminary_units)
  ) %>%
  group_by(pin, year) %>%
  arrange(desc(has_land), desc(selected_units), modelgroup, address, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    pin,
    valuation_year = as.integer(year),
    source_yearbuilt = as.integer(yearbuilt),
    source_units = selected_units,
    source_bldgsf = bldgsf,
    source_landsf = landsf,
    source_address = address,
    source_modelgroup = modelgroup,
    source_pins = pins
  )

vintage_comparison <- vintage_rows %>%
  filter(valuation_year %in% c(2021L, 2024L)) %>%
  pivot_wider(
    names_from = valuation_year,
    values_from = c(
      source_yearbuilt, source_units, source_bldgsf, source_landsf,
      source_address, source_modelgroup, source_pins
    ),
    names_sep = "_"
  ) %>%
  mutate(
    yearbuilt_conflict =
      !is.na(source_yearbuilt_2021) &
      !is.na(source_yearbuilt_2024) &
      source_yearbuilt_2021 != source_yearbuilt_2024,
    building_change = abs(log(source_bldgsf_2024 / source_bldgsf_2021)),
    land_change = abs(log(source_landsf_2024 / source_landsf_2021)),
    unit_change = abs(log(source_units_2024 / source_units_2021)),
    stable_physical_fields =
      coalesce(building_change <= log(1.1), FALSE) &
      coalesce(land_change <= log(1.1), FALSE) &
      coalesce(unit_change <= log(1.1), FALSE),
    large_physical_change =
      coalesce(building_change >= log(1.25), FALSE) |
      coalesce(land_change >= log(1.25), FALSE) |
      coalesce(unit_change >= log(1.25), FALSE)
  )

vintage_components <- purrr::pmap_dfr(
  vintage_rows %>% select(pin, valuation_year, source_pins),
  function(pin, valuation_year, source_pins) {
    expand_entity_pins(pin, source_pins) %>%
      transmute(
        valuation_year,
        entity_pin = pin,
        component_pin
      )
  }
) %>%
  distinct(valuation_year, entity_pin, component_pin) %>%
  group_by(valuation_year, component_pin) %>%
  mutate(entities_using_component_in_vintage = n_distinct(entity_pin)) %>%
  ungroup()

vintage_2021_components <- vintage_components %>%
  filter(valuation_year == 2021L, entities_using_component_in_vintage == 1) %>%
  transmute(entity_pin_2021 = entity_pin, component_pin)

vintage_2024_components <- vintage_components %>%
  filter(valuation_year == 2024L, entities_using_component_in_vintage == 1) %>%
  transmute(entity_pin_2024 = entity_pin, component_pin)

entity_crosswalk <- vintage_2021_components %>%
  inner_join(
    vintage_2024_components,
    by = "component_pin",
    relationship = "one-to-one"
  ) %>%
  group_by(entity_pin_2021, entity_pin_2024) %>%
  summarise(shared_components = n_distinct(component_pin), .groups = "drop") %>%
  mutate(keypin_changed = entity_pin_2021 != entity_pin_2024)

component_pins <- purrr::map2_dfr(
  selection_check$pin,
  selection_check$selected_pins,
  expand_entity_pins
) %>%
  filter(!is.na(component_pin)) %>%
  distinct(pin, component_pin) %>%
  group_by(pin) %>%
  mutate(
    component_count_parsed = n(),
    keypin_component = component_pin == pin
  ) %>%
  ungroup()

parcel_coordinates <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    centroid_x_crs_3435 = readr::col_double(),
    centroid_y_crs_3435 = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  select(pin, centroid_x_crs_3435, centroid_y_crs_3435)

if (anyDuplicated(parcel_coordinates$pin) > 0) {
  stop("Parcel coordinate input is not unique by PIN.", call. = FALSE)
}

component_locations <- component_pins %>%
  left_join(
    parcel_coordinates,
    by = c("component_pin" = "pin"),
    relationship = "many-to-one"
  ) %>%
  group_by(pin) %>%
  mutate(
    components_with_coordinates = sum(
      is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
    ),
    group_centroid_x = mean(centroid_x_crs_3435, na.rm = TRUE),
    group_centroid_y = mean(centroid_y_crs_3435, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    component_to_group_centroid_ft = sqrt(
      (centroid_x_crs_3435 - group_centroid_x)^2 +
        (centroid_y_crs_3435 - group_centroid_y)^2
    )
  )

entity_locations <- component_locations %>%
  group_by(pin, component_count_parsed, components_with_coordinates) %>%
  summarise(
    keypin_x = centroid_x_crs_3435[keypin_component][1],
    keypin_y = centroid_y_crs_3435[keypin_component][1],
    group_centroid_x = first(group_centroid_x),
    group_centroid_y = first(group_centroid_y),
    max_component_radius_ft = if (all(is.na(component_to_group_centroid_ft))) {
      NA_real_
    } else {
      max(component_to_group_centroid_ft, na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(
    keypin_to_group_centroid_ft = sqrt(
      (keypin_x - group_centroid_x)^2 +
        (keypin_y - group_centroid_y)^2
    ),
    complete_component_coordinates = components_with_coordinates == component_count_parsed,
    across(
      c(max_component_radius_ft, keypin_to_group_centroid_ft),
      ~ if_else(is.infinite(.x), NA_real_, .x)
    )
  )

component_entity_conflicts <- component_pins %>%
  group_by(component_pin) %>%
  filter(n_distinct(pin) > 1) %>%
  mutate(entities_sharing_component = n_distinct(pin)) %>%
  ungroup() %>%
  arrange(component_pin, pin)

entity_component_conflicts <- component_entity_conflicts %>%
  group_by(pin) %>%
  summarise(
    shared_components = n_distinct(component_pin),
    .groups = "drop"
  )

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  transmute(
    permit_id = as.character(id),
    permit_pin = as.character(pin),
    permit_type,
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_issued = as.integer(permit_issued),
    permit_status,
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    work_description
  ) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_issued == 1,
    permit_status == "COMPLETE",
    !is.na(permit_pin),
    !is.na(application_date)
  ) %>%
  separate_rows(permit_pin, sep = "\\s*\\|\\s*") %>%
  mutate(permit_pin10 = str_replace_all(permit_pin, "[^0-9]", "")) %>%
  filter(str_detect(permit_pin10, "^[0-9]{10}$")) %>%
  distinct(permit_id, permit_pin10, .keep_all = TRUE) %>%
  mutate(permit_application_year = lubridate::year(application_date))

entity_pin10 <- component_pins %>%
  mutate(component_pin10 = str_sub(component_pin, 1, 10)) %>%
  group_by(pin, component_pin10) %>%
  summarise(component_pin = first(component_pin), .groups = "drop")

entity_pin10 <- entity_pin10 %>%
  add_count(component_pin10, name = "commercial_entities_under_pin10")

permit_matches <- entity_pin10 %>%
  filter(commercial_entities_under_pin10 == 1) %>%
  inner_join(
    permits,
    by = c("component_pin10" = "permit_pin10"),
    relationship = "one-to-many"
  ) %>%
  distinct(pin, permit_id, .keep_all = TRUE)

permit_summary <- permit_matches %>%
  group_by(pin) %>%
  summarise(
    new_construction_permits = n_distinct(permit_id),
    first_permit_application_year = min(permit_application_year),
    last_permit_application_year = max(permit_application_year),
    permit_application_years = paste(sort(unique(permit_application_year)), collapse = "/"),
    permit_ids = paste(sort(unique(permit_id)), collapse = "/"),
    .groups = "drop"
  )

residential <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    year_built = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    num_apartments = readr::col_double(),
    class = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  transmute(
    component_pin = pin,
    residential_yearbuilt = as.integer(year_built),
    residential_units = as.numeric(num_apartments),
    residential_bldgsf = as.numeric(building_sqft),
    residential_landsf = as.numeric(land_sqft),
    residential_class = as.character(class)
  )

component_overlap <- component_locations %>%
  left_join(residential, by = "component_pin", relationship = "many-to-one") %>%
  mutate(residential_source_overlap = !is.na(residential_yearbuilt))

entity_overlap <- component_overlap %>%
  group_by(pin) %>%
  summarise(
    residential_component_overlaps = sum(residential_source_overlap),
    residential_nonkey_component_overlaps = sum(residential_source_overlap & !keypin_component),
    .groups = "drop"
  )

analysis <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
) %>%
  mutate(
    in_500ft_source_sample =
      arealotsf > 1 &
      areabuilding > 1 &
      unitscount > 0 &
      construction_year >= 2006 &
      construction_year <= 2022 &
      dist_to_boundary_m <= 152.4,
    in_main_model_inputs =
      in_500ft_source_sample &
      !is.na(ward_pair) &
      is.finite(signed_distance_m) &
      !is.na(construction_zone_group) &
      !is.na(segment_id) &
      segment_id != "" &
      is.finite(density_far) & density_far > 0 &
      is.finite(density_dupac) & density_dupac > 0 &
      is.finite(strictness_own) &
      is.finite(strictness_neighbor) &
      if_all(
        c(
          share_white_own, share_black_own, median_hh_income_own,
          share_bach_plus_own, homeownership_rate_own
        ),
        is.finite
      )
  ) %>%
  select(
    pin, construction_year, ward_pair, segment_id, signed_distance_m,
    dist_to_boundary_m, unitscount, areabuilding, arealotsf,
    density_far, density_dupac, strictness_own, strictness_neighbor,
    construction_zone_group, in_500ft_source_sample, in_main_model_inputs
  )

component_analysis_overlap <- component_pins %>%
  filter(component_pin != pin) %>%
  inner_join(
    analysis %>%
      filter(in_500ft_source_sample) %>%
      select(component_pin = pin, component_construction_year = construction_year),
    by = "component_pin",
    relationship = "many-to-one"
  ) %>%
  distinct(pin, component_pin, .keep_all = TRUE)

entity_analysis_overlap <- component_analysis_overlap %>%
  count(pin, name = "nonkey_components_also_in_500ft_sample")

entity_crosswalk <- entity_crosswalk %>%
  mutate(
    entity_2021_selected = entity_pin_2021 %in% selected$pin,
    entity_2024_selected = entity_pin_2024 %in% selected$pin,
    both_entities_selected = entity_2021_selected & entity_2024_selected,
    entity_2021_in_main_model = entity_pin_2021 %in% analysis$pin[analysis$in_main_model_inputs],
    entity_2024_in_main_model = entity_pin_2024 %in% analysis$pin[analysis$in_main_model_inputs],
    both_entities_in_main_model = entity_2021_in_main_model & entity_2024_in_main_model
  )

entity_history <- selection_check %>%
  left_join(vintage_comparison, by = "pin", relationship = "one-to-one") %>%
  left_join(permit_summary, by = "pin", relationship = "one-to-one") %>%
  left_join(entity_locations, by = "pin", relationship = "one-to-one") %>%
  left_join(entity_component_conflicts, by = "pin", relationship = "one-to-one") %>%
  left_join(entity_overlap, by = "pin", relationship = "one-to-one") %>%
  left_join(entity_analysis_overlap, by = "pin", relationship = "one-to-one") %>%
  left_join(analysis, by = "pin", relationship = "one-to-one") %>%
  mutate(
    new_construction_permits = replace_na(new_construction_permits, 0L),
    shared_components = replace_na(shared_components, 0L),
    residential_component_overlaps = replace_na(residential_component_overlaps, 0L),
    residential_nonkey_component_overlaps = replace_na(residential_nonkey_component_overlaps, 0L),
    nonkey_components_also_in_500ft_sample = replace_na(nonkey_components_also_in_500ft_sample, 0L),
    in_500ft_source_sample = replace_na(in_500ft_source_sample, FALSE),
    in_main_model_inputs = replace_na(in_main_model_inputs, FALSE),
    selected_year_permit_supported = purrr::map2_lgl(
      production_yearbuilt,
      permit_application_years,
      ~ {
        if (is.na(.y)) return(FALSE)
        years <- suppressWarnings(as.integer(str_split(.y, "/", simplify = TRUE)))
        any(.x - years >= 0 & .x - years <= 3, na.rm = TRUE)
      }
    ),
    counterpart_pre1999 = case_when(
      selected_valuation_year == 2021L ~ source_yearbuilt_2024 < 1999,
      selected_valuation_year == 2024L ~ source_yearbuilt_2021 < 1999,
      TRUE ~ FALSE
    ),
    review_stratum = case_when(
      is.na(source_yearbuilt_2021) | is.na(source_yearbuilt_2024) ~ "single_valuation_vintage",
      !yearbuilt_conflict ~ "same_yearbuilt_both_vintages",
      selected_year_permit_supported ~ "selected_year_supported_by_new_construction_permit",
      counterpart_pre1999 & stable_physical_fields ~ "pre1999_counterpart_stable_fields_no_permit_support",
      counterpart_pre1999 & large_physical_change ~ "pre1999_counterpart_large_physical_change_no_permit_support",
      source_yearbuilt_2021 >= 1999 & source_yearbuilt_2024 >= 1999 ~ "conflicting_post1999_years_no_permit_support",
      TRUE ~ "other_yearbuilt_conflict_no_permit_support"
    )
  )

summary <- bind_rows(
  tibble(metric = "cleaned_commercial_entities", value = nrow(cleaned)),
  tibble(metric = "entities_observed_in_2021_and_2024", value = sum(!is.na(entity_history$source_yearbuilt_2021) & !is.na(entity_history$source_yearbuilt_2024))),
  tibble(metric = "entities_with_2021_2024_yearbuilt_conflict", value = sum(entity_history$yearbuilt_conflict, na.rm = TRUE)),
  tibble(metric = "components_assigned_to_multiple_entities_within_a_vintage", value = n_distinct(vintage_components$component_pin[vintage_components$entities_using_component_in_vintage > 1])),
  tibble(metric = "changed_keypin_links_between_2021_and_2024", value = sum(entity_crosswalk$keypin_changed)),
  tibble(metric = "changed_keypin_links_with_both_entities_selected", value = sum(entity_crosswalk$keypin_changed & entity_crosswalk$both_entities_selected)),
  tibble(metric = "changed_keypin_links_with_both_entities_in_main_model", value = sum(entity_crosswalk$keypin_changed & entity_crosswalk$both_entities_in_main_model)),
  tibble(metric = "entities_with_multiple_component_pins", value = sum(entity_history$component_count_parsed > 1, na.rm = TRUE)),
  tibble(metric = "entities_with_residential_component_overlap", value = sum(entity_history$residential_component_overlaps > 0)),
  tibble(metric = "entities_with_nonkey_residential_component_overlap", value = sum(entity_history$residential_nonkey_component_overlaps > 0)),
  tibble(metric = "entities_sharing_a_component_with_another_commercial_entity", value = sum(entity_history$shared_components > 0)),
  tibble(metric = "entities_with_new_construction_permit_match", value = sum(entity_history$new_construction_permits > 0)),
  tibble(metric = "ambiguous_commercial_permit_pin10_links", value = n_distinct(entity_pin10$component_pin10[entity_pin10$commercial_entities_under_pin10 > 1])),
  tibble(metric = "entities_with_production_land_correction", value = sum(entity_history$production_land_correction)),
  tibble(metric = "commercial_entities_in_500ft_source_sample", value = sum(entity_history$in_500ft_source_sample, na.rm = TRUE)),
  tibble(metric = "commercial_entities_in_main_model_inputs", value = sum(entity_history$in_main_model_inputs, na.rm = TRUE)),
  tibble(metric = "main_sample_entities_with_yearbuilt_conflict", value = sum(entity_history$in_main_model_inputs & entity_history$yearbuilt_conflict, na.rm = TRUE)),
  tibble(metric = "main_sample_entities_with_selected_year_permit_support", value = sum(entity_history$in_main_model_inputs & entity_history$selected_year_permit_supported, na.rm = TRUE)),
  tibble(metric = "main_sample_entities_with_nonkey_component_also_in_sample", value = sum(entity_history$in_main_model_inputs & entity_history$nonkey_components_also_in_500ft_sample > 0, na.rm = TRUE)),
  tibble(metric = "main_sample_entities_sharing_a_component_with_another_commercial_entity", value = sum(entity_history$in_main_model_inputs & entity_history$shared_components > 0, na.rm = TRUE)),
  tibble(metric = "main_sample_rows_using_apartment_unit_sum", value = sum(entity_history$in_main_model_inputs & entity_history$selected_unit_rule == "apartment_unit_sum", na.rm = TRUE)),
  tibble(metric = "main_sample_rows_with_tot_unit_and_apartment_sum_disagreement", value = sum(
    entity_history$in_main_model_inputs &
      entity_history$selected_source_tot_units > 0 &
      entity_history$selected_apartment_unit_sum > 0 &
      entity_history$selected_source_tot_units != entity_history$selected_apartment_unit_sum,
    na.rm = TRUE
  )),
  tibble(metric = "main_sample_rows_with_tot_unit_and_apartment_sum_gap_over_10pct", value = sum(
    entity_history$in_main_model_inputs &
      entity_history$selected_source_tot_units > 0 &
      entity_history$selected_apartment_unit_sum > 0 &
      abs(entity_history$selected_source_tot_units - entity_history$selected_apartment_unit_sum) /
        pmax(entity_history$selected_source_tot_units, entity_history$selected_apartment_unit_sum) > 0.1,
    na.rm = TRUE
  ))
)

readr::write_csv(summary, "../output/commercial_sample_audit_summary.csv")
readr::write_csv(entity_history %>% arrange(pin), "../output/commercial_entity_history.csv")
readr::write_csv(
  entity_history %>% filter(in_500ft_source_sample) %>% arrange(pin),
  "../output/commercial_500ft_sample_review.csv"
)
readr::write_csv(component_overlap %>% arrange(pin, component_pin), "../output/commercial_component_pin_overlap.csv")
readr::write_csv(component_entity_conflicts, "../output/commercial_overlapping_entity_components.csv")
readr::write_csv(entity_crosswalk %>% arrange(entity_pin_2021, entity_pin_2024), "../output/commercial_2021_2024_entity_crosswalk.csv")
readr::write_csv(permit_matches %>% arrange(pin, application_date, permit_id), "../output/commercial_new_construction_permit_matches.csv")
readr::write_csv(
  component_analysis_overlap %>% arrange(pin, component_pin),
  "../output/commercial_component_analysis_overlap.csv"
)
