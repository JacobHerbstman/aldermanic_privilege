# Clean multifamily commercial valuation rows for buildings built since 1999 in Chicago townships.

# setwd("tasks/commercial_value_data_cleaning/code")
source("../../setup_environment/code/packages.R")

data <- readr::read_csv("../input/commercial_value_raw.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) %>%
  janitor::clean_names()

if (!"modelgroup" %in% names(data) && "sheet" %in% names(data)) {
  data <- data %>% dplyr::rename(modelgroup = sheet)
}

numeric_cols <- c(
  "year", "studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits",
  "tot_units", "bldgsf", "landsf", "yearbuilt", "aprx_comm_sf"
)
apartment_unit_cols <- c("studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits")
month_codes <- c(
  jan = "1", feb = "2", mar = "3", apr = "4", may = "5", jun = "6",
  jul = "7", aug = "8", sep = "9", oct = "10", nov = "11", dec = "12"
)
manual_unit_overrides <- tibble::tribble(
  ~pin, ~manual_residential_units, ~unit_override_reason,
  "14303140460000", 34,
  "Permit/public records describe 34 dwelling units; source bedroom-count detail reports only 30 two-bedroom units.",
  "17151100370000", 524,
  "University Center student housing: CCAO apartment-unit fields appear to count beds rather than dwelling units."
)

data <- data %>%
  dplyr::mutate(dplyr::across(
    dplyr::any_of(numeric_cols),
    ~ suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", .x)))
  )) %>%
  dplyr::mutate(
    pin_group_count = dplyr::if_else(
      is.na(pins) | stringr::str_trim(pins) == "",
      1L,
      stringr::str_count(pins, ",") + 1L
    )
  ) %>%
  dplyr::mutate(class_es = dplyr::case_when(
    stringr::str_detect(class_es, "^[A-Za-z]{3}-[0-9]{2}$") ~ paste0(
      month_codes[stringr::str_to_lower(stringr::str_extract(class_es, "^[A-Za-z]{3}"))],
      "-",
      stringr::str_extract(class_es, "[0-9]{2}$")
    ),
    stringr::str_detect(class_es, "^[0-9]{1,2}-[A-Za-z]{3}$") ~ paste0(
      month_codes[stringr::str_to_lower(stringr::str_extract(class_es, "[A-Za-z]{3}$"))],
      "-",
      stringr::str_extract(class_es, "^[0-9]{1,2}")
    ),
    TRUE ~ class_es
  ))

raw_data <- data

data <- raw_data %>%
  dplyr::filter(township %in% c("West Chicago","South Chicago","Jefferson","North Chicago","Lake View","Rogers Park","Hyde Park","Lake"))

data <- data %>%
  filter(yearbuilt >= 1999)


multifamily_data <- data %>%
  filter(str_detect(modelgroup, "(?i)Multifamily|Class3|Class9|Condos")) %>%
  mutate(
    source_tot_units = tot_units,
    apartment_unit_sum = rowSums(select(., all_of(apartment_unit_cols)), na.rm = TRUE),
    preliminary_unit_source = case_when(
      apartment_unit_sum > 0 ~ "apartment_unit_sum",
      !is.na(source_tot_units) & source_tot_units > 0 ~ "tot_units_fallback",
      TRUE ~ "missing"
    ),
    preliminary_tot_units = case_when(
      apartment_unit_sum > 0 ~ apartment_unit_sum,
      !is.na(source_tot_units) & source_tot_units > 0 ~ source_tot_units,
      TRUE ~ NA_real_
    ),
    tot_units_apartment_sum_gap = apartment_unit_sum > 0 &
      !is.na(source_tot_units) &
      abs(source_tot_units - apartment_unit_sum) > 0,
    tot_units_apartment_sum_large_gap = tot_units_apartment_sum_gap &
      abs(source_tot_units - apartment_unit_sum) / pmax(source_tot_units, apartment_unit_sum) > 0.1
  ) %>%
  filter(!is.na(preliminary_tot_units)) %>%
  mutate(source_landsf = landsf) %>%
  select(
    keypin,
    address,
    year,
    yearbuilt,
    preliminary_tot_units,
    preliminary_unit_source,
    source_tot_units,
    apartment_unit_sum,
    tot_units_apartment_sum_gap,
    tot_units_apartment_sum_large_gap,
    bldgsf,
    landsf,
    source_landsf,
    aprx_comm_sf,
    pin_group_count,
    pins,
    modelgroup,
    class_es,
    property_type_use,
    property_name_description,
    studiounits,
    x1brunits,
    x2brunits,
    x3brunits,
    x4brunits
  ) %>% 
  mutate(keypin = str_remove_all(keypin, "-")) %>% 
  rename(pin = keypin) %>%
  left_join(manual_unit_overrides, by = "pin", relationship = "many-to-one") %>%
  mutate(
    unit_override_applied = !is.na(manual_residential_units),
    unit_source = if_else(unit_override_applied, "manual_override", preliminary_unit_source),
    tot_units = coalesce(manual_residential_units, preliminary_tot_units)
  ) %>%
  select(
    pin,
    address,
    year,
    yearbuilt,
    tot_units,
    source_tot_units,
    apartment_unit_sum,
    unit_source,
    unit_override_applied,
    manual_residential_units,
    unit_override_reason,
    tot_units_apartment_sum_gap,
    tot_units_apartment_sum_large_gap,
    bldgsf,
    landsf,
    source_landsf,
    aprx_comm_sf,
    pin_group_count,
    pins,
    modelgroup,
    class_es,
    property_type_use,
    property_name_description,
    studiounits,
    x1brunits,
    x2brunits,
    x3brunits,
    x4brunits
  )


multifamily_data_selected <- multifamily_data %>%
  mutate(has_land = landsf > 0) %>%
  group_by(pin) %>% 
  arrange(desc(has_land), desc(tot_units), desc(year), modelgroup, address) %>%
  slice(1) %>%
  ungroup()

land_correction_candidates <- multifamily_data_selected %>%
  filter(
    pin_group_count > 1,
    !is.na(landsf),
    landsf > 0,
    !is.na(bldgsf),
    bldgsf > 0
  ) %>%
  select(
    pin,
    selected_year = year,
    selected_address = address,
    selected_units = tot_units,
    selected_source_tot_units = source_tot_units,
    selected_apartment_unit_sum = apartment_unit_sum,
    selected_unit_source = unit_source,
    selected_unit_override_applied = unit_override_applied,
    selected_manual_residential_units = manual_residential_units,
    selected_unit_override_reason = unit_override_reason,
    selected_bldgsf = bldgsf,
    selected_landsf = landsf,
    selected_pin_group_count = pin_group_count
  ) %>%
  inner_join(
    multifamily_data %>%
      filter(
        !is.na(landsf),
        landsf > 0,
        !is.na(bldgsf),
        bldgsf > 0
      ) %>%
      select(
        pin,
        candidate_year = year,
        candidate_address = address,
        candidate_modelgroup = modelgroup,
        candidate_landsf = landsf,
        candidate_bldgsf = bldgsf
      ),
    by = "pin",
    relationship = "one-to-many"
  ) %>%
  filter(
    selected_landsf > candidate_landsf,
    abs(selected_bldgsf - candidate_bldgsf) / pmax(selected_bldgsf, candidate_bldgsf) <= 0.05
  ) %>%
  mutate(
    building_sf_relative_gap = abs(selected_bldgsf - candidate_bldgsf) / pmax(selected_bldgsf, candidate_bldgsf),
    land_ratio = selected_landsf / candidate_landsf,
    pin_multiple_gap = abs(land_ratio - selected_pin_group_count),
    near_pin_multiple = pin_multiple_gap < 0.05,
    selected_far = selected_bldgsf / selected_landsf,
    selected_dupac = 43560 * selected_units / selected_landsf,
    candidate_far = selected_bldgsf / candidate_landsf,
    candidate_dupac = 43560 * selected_units / candidate_landsf
  ) %>%
  filter(near_pin_multiple) %>%
  group_by(pin) %>%
  arrange(pin_multiple_gap, desc(candidate_year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    apply_land_correction = selected_dupac < 25 & selected_far < 1,
    land_correction_reason = dplyr::if_else(
      apply_land_correction,
      "selected_land_equals_pin_count_multiple_of_same_building_land_and_density_implausibly_low",
      "pin_count_multiple_candidate_review_only"
    )
  ) %>%
  select(
    pin,
    candidate_year,
    candidate_address,
    candidate_modelgroup,
    candidate_bldgsf,
    building_sf_relative_gap,
    corrected_landsf = candidate_landsf,
    land_ratio,
    selected_far,
    selected_dupac,
    candidate_far,
    candidate_dupac,
    apply_land_correction,
    land_correction_reason
  )

multifamily_data_deduped <- multifamily_data_selected %>%
  left_join(
    land_correction_candidates %>%
      select(
        pin,
        corrected_landsf,
        candidate_bldgsf,
        building_sf_relative_gap,
        land_ratio,
        selected_far,
        selected_dupac,
        candidate_far,
        candidate_dupac,
        apply_land_correction,
        land_correction_reason
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    apply_land_correction = coalesce(apply_land_correction, FALSE),
    land_correction_reason = coalesce(land_correction_reason, "none"),
    landsf = if_else(apply_land_correction, corrected_landsf, landsf)
  ) %>%
  select(
    -has_land,
    -year,
    -corrected_landsf,
    -candidate_bldgsf,
    -building_sf_relative_gap,
    -land_ratio,
    -selected_far,
    -selected_dupac,
    -candidate_far,
    -candidate_dupac
  )

write_csv(multifamily_data_deduped, "../output/multifamily_data_cleaned.csv")
