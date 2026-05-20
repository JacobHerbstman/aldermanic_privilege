# Clean multifamily commercial valuation rows for buildings built since 1999 in Chicago townships.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/commercial_value_data_cleaning/code")
source("../../setup_environment/code/packages.R")

data <- readr::read_csv("../input/commercial_value_raw.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) %>%
  janitor::clean_names()

if (!"modelgroup" %in% names(data) && "sheet" %in% names(data)) {
  data <- data %>% dplyr::rename(modelgroup = sheet)
}

numeric_cols <- c(
  "year", "studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits",
  "tot_units", "bldgsf", "landsf", "yearbuilt"
)
month_codes <- c(
  jan = "1", feb = "2", mar = "3", apr = "4", may = "5", jun = "6",
  jul = "7", aug = "8", sep = "9", oct = "10", nov = "11", dec = "12"
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
  mutate(calculated_sum = rowSums(select(., studiounits, x1brunits, x2brunits, x3brunits, x4brunits), na.rm = TRUE)) %>% 
  mutate(tot_units = coalesce(tot_units, if_else(calculated_sum > 0, calculated_sum, NA_real_))) %>% 
  filter(!is.na(tot_units)) %>% 
  select(-calculated_sum) %>%
  mutate(source_landsf = landsf) %>%
  select(
    keypin,
    address,
    year,
    yearbuilt,
    tot_units,
    bldgsf,
    landsf,
    source_landsf,
    pin_group_count,
    pins,
    modelgroup,
    class_es, # usage class
    studiounits,
    x1brunits,
    x2brunits,
    x3brunits,
    x4brunits
  ) %>% 
  mutate(keypin = str_remove_all(keypin, "-")) %>% 
  rename(pin = keypin)


multifamily_data_selected <- multifamily_data %>%
  # Create a helper flag for "Has Land"
  mutate(has_land = landsf > 0) %>%
  group_by(pin) %>% 
  arrange(desc(has_land), desc(tot_units), desc(year), modelgroup, address) %>%
  slice(1) %>%
  ungroup()

multifamily_data_latest <- multifamily_data %>%
  mutate(has_land = landsf > 0) %>%
  group_by(pin) %>%
  arrange(desc(has_land), desc(year), desc(tot_units), modelgroup, address) %>%
  slice(1) %>%
  ungroup()

selected_vs_latest_audit <- multifamily_data_selected %>%
  select(
    pin,
    selected_year = year,
    selected_address = address,
    selected_units = tot_units,
    selected_bldgsf = bldgsf,
    selected_landsf = landsf,
    selected_pin_group_count = pin_group_count,
    selected_modelgroup = modelgroup,
    selected_class_es = class_es
  ) %>%
  left_join(
    multifamily_data_latest %>%
      select(
        pin,
        latest_year = year,
        latest_address = address,
        latest_units = tot_units,
        latest_bldgsf = bldgsf,
        latest_landsf = landsf,
        latest_modelgroup = modelgroup,
        latest_class_es = class_es
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    selected_not_latest = selected_year != latest_year,
    unit_relative_gap = abs(selected_units - latest_units) / pmax(selected_units, latest_units),
    building_sf_relative_gap = abs(selected_bldgsf - latest_bldgsf) / pmax(selected_bldgsf, latest_bldgsf),
    land_sf_relative_gap = abs(selected_landsf - latest_landsf) / pmax(selected_landsf, latest_landsf),
    material_selected_latest_gap = coalesce(selected_not_latest &
      (
        unit_relative_gap > 0.1 |
          building_sf_relative_gap > 0.2 |
          land_sf_relative_gap > 0.2
      ), FALSE)
  ) %>%
  filter(selected_not_latest) %>%
  arrange(
    desc(material_selected_latest_gap),
    desc(building_sf_relative_gap),
    desc(land_sf_relative_gap),
    desc(unit_relative_gap),
    pin
  )

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

multi_pin_land_audit <- multifamily_data_selected %>%
  filter(pin_group_count > 1) %>%
  left_join(
    land_correction_candidates,
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    selected_far = bldgsf / source_landsf,
    selected_dupac = 43560 * tot_units / source_landsf,
    corrected_landsf = if_else(apply_land_correction %in% TRUE, corrected_landsf, source_landsf),
    corrected_far = bldgsf / corrected_landsf,
    corrected_dupac = 43560 * tot_units / corrected_landsf,
    apply_land_correction = coalesce(apply_land_correction, FALSE),
    land_correction_reason = coalesce(land_correction_reason, "no_same_building_pin_multiple_candidate")
  ) %>%
  select(
    pin,
    address,
    selected_year = year,
    yearbuilt,
    tot_units,
    bldgsf,
    source_landsf,
    corrected_landsf,
    pin_group_count,
    selected_far,
    corrected_far,
    selected_dupac,
    corrected_dupac,
    candidate_year,
    candidate_address,
    candidate_modelgroup,
    candidate_bldgsf,
    building_sf_relative_gap,
    land_ratio,
    apply_land_correction,
    land_correction_reason,
    pins,
    modelgroup,
    class_es
  ) %>%
  arrange(desc(apply_land_correction), desc(pin_group_count), desc(tot_units), pin)

density_review_flags <- multifamily_data_deduped %>%
  mutate(
    dupac = 43560 * tot_units / landsf,
    far = bldgsf / landsf,
    missing_or_nonpositive_landsf = is.na(landsf) | landsf <= 0,
    missing_or_nonpositive_bldgsf = is.na(bldgsf) | bldgsf <= 0,
    high_unit_low_density = tot_units >= 50 &
      !missing_or_nonpositive_landsf &
      (dupac < 25 | far < 0.5 | is.na(far)),
    grouped_pin_low_density = pin_group_count > 1 &
      tot_units >= 50 &
      !missing_or_nonpositive_landsf &
      (dupac < 25 | far < 1 | is.na(far))
  ) %>%
  filter(
    missing_or_nonpositive_landsf |
      missing_or_nonpositive_bldgsf |
      high_unit_low_density |
      grouped_pin_low_density
  ) %>%
  select(
    pin,
    address,
    yearbuilt,
    tot_units,
    bldgsf,
    landsf,
    source_landsf,
    pin_group_count,
    dupac,
    far,
    apply_land_correction,
    land_correction_reason,
    missing_or_nonpositive_landsf,
    missing_or_nonpositive_bldgsf,
    high_unit_low_density,
    grouped_pin_low_density,
    pins,
    modelgroup,
    class_es
  ) %>%
  arrange(
    desc(high_unit_low_density),
    desc(grouped_pin_low_density),
    desc(missing_or_nonpositive_landsf),
    desc(missing_or_nonpositive_bldgsf),
    dupac,
    far,
    pin
  )

multi_pin_land_audit_summary <- tibble::tibble(
  metric = c(
    "raw_rows",
    "raw_multi_pin_rows",
    "raw_multi_pin_share",
    "task_township_yearbuilt_rows",
    "task_township_yearbuilt_multi_pin_rows",
    "task_township_yearbuilt_multi_pin_share",
    "multifamily_with_units_rows",
    "multifamily_with_units_multi_pin_rows",
    "multifamily_with_units_multi_pin_share",
    "selected_rows",
    "selected_multi_pin_rows",
    "selected_multi_pin_share",
    "selected_not_latest_rows",
    "material_selected_latest_gap_rows",
    "near_pin_multiple_candidates",
    "land_corrections_applied",
    "density_review_flag_rows"
  ),
  value = c(
    nrow(raw_data),
    sum(raw_data$pin_group_count > 1, na.rm = TRUE),
    mean(raw_data$pin_group_count > 1, na.rm = TRUE),
    nrow(data),
    sum(data$pin_group_count > 1, na.rm = TRUE),
    mean(data$pin_group_count > 1, na.rm = TRUE),
    nrow(multifamily_data),
    sum(multifamily_data$pin_group_count > 1, na.rm = TRUE),
    mean(multifamily_data$pin_group_count > 1, na.rm = TRUE),
    nrow(multifamily_data_selected),
    sum(multifamily_data_selected$pin_group_count > 1, na.rm = TRUE),
    mean(multifamily_data_selected$pin_group_count > 1, na.rm = TRUE),
    nrow(selected_vs_latest_audit),
    sum(selected_vs_latest_audit$material_selected_latest_gap, na.rm = TRUE),
    nrow(land_correction_candidates),
    sum(land_correction_candidates$apply_land_correction, na.rm = TRUE),
    nrow(density_review_flags)
  )
)

write_csv(multifamily_data_deduped, "../output/multifamily_data_cleaned.csv")
write_csv(multi_pin_land_audit, "../output/commercial_multi_pin_land_audit.csv")
write_csv(multi_pin_land_audit_summary, "../output/commercial_multi_pin_land_audit_summary.csv")
write_csv(
  multi_pin_land_audit %>% filter(apply_land_correction),
  "../output/commercial_land_corrections.csv"
)
write_csv(density_review_flags, "../output/commercial_density_review_flags.csv")
write_csv(selected_vs_latest_audit, "../output/commercial_selected_vs_latest_audit.csv")
