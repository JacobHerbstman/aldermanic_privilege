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
    class_es, # usage class
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
    selected_source_tot_units = source_tot_units,
    selected_apartment_unit_sum = apartment_unit_sum,
    selected_unit_source = unit_source,
    selected_unit_override_applied = unit_override_applied,
    selected_manual_residential_units = manual_residential_units,
    selected_unit_override_reason = unit_override_reason,
    selected_tot_units_apartment_sum_gap = tot_units_apartment_sum_gap,
    selected_tot_units_apartment_sum_large_gap = tot_units_apartment_sum_large_gap,
    selected_bldgsf = bldgsf,
    selected_landsf = landsf,
    selected_aprx_comm_sf = aprx_comm_sf,
    selected_pin_group_count = pin_group_count,
    selected_modelgroup = modelgroup,
    selected_class_es = class_es,
    selected_property_type_use = property_type_use,
    selected_property_name_description = property_name_description
  ) %>%
  left_join(
    multifamily_data_latest %>%
      select(
        pin,
        latest_year = year,
        latest_address = address,
        latest_units = tot_units,
        latest_source_tot_units = source_tot_units,
        latest_apartment_unit_sum = apartment_unit_sum,
        latest_unit_source = unit_source,
        latest_unit_override_applied = unit_override_applied,
        latest_manual_residential_units = manual_residential_units,
        latest_unit_override_reason = unit_override_reason,
        latest_tot_units_apartment_sum_gap = tot_units_apartment_sum_gap,
        latest_tot_units_apartment_sum_large_gap = tot_units_apartment_sum_large_gap,
        latest_bldgsf = bldgsf,
        latest_landsf = landsf,
        latest_aprx_comm_sf = aprx_comm_sf,
        latest_modelgroup = modelgroup,
        latest_class_es = class_es,
        latest_property_type_use = property_type_use,
        latest_property_name_description = property_name_description
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
    source_tot_units,
    apartment_unit_sum,
    unit_source,
    unit_override_applied,
    manual_residential_units,
    unit_override_reason,
    tot_units_apartment_sum_gap,
    tot_units_apartment_sum_large_gap,
    bldgsf,
    source_landsf,
    corrected_landsf,
    aprx_comm_sf,
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
    class_es,
    property_type_use,
    property_name_description
  ) %>%
  arrange(desc(apply_land_correction), desc(pin_group_count), desc(tot_units), pin)

unit_definition_audit <- multifamily_data %>%
  mutate(
    unit_gap = source_tot_units - apartment_unit_sum,
    unit_gap_relative = abs(unit_gap) / pmax(source_tot_units, apartment_unit_sum),
    has_commercial_area = !is.na(aprx_comm_sf) & aprx_comm_sf > 0
  ) %>%
  filter(
    unit_override_applied |
    unit_source == "tot_units_fallback" |
      tot_units_apartment_sum_gap |
      has_commercial_area
  ) %>%
  select(
    pin,
    address,
    year,
    yearbuilt,
    residential_units = tot_units,
    source_tot_units,
    apartment_unit_sum,
    unit_source,
    unit_override_applied,
    manual_residential_units,
    unit_override_reason,
    unit_gap,
    unit_gap_relative,
    tot_units_apartment_sum_gap,
    tot_units_apartment_sum_large_gap,
    aprx_comm_sf,
    bldgsf,
    landsf,
    source_landsf,
    pin_group_count,
    pins,
    modelgroup,
    class_es,
    property_type_use,
    property_name_description
  ) %>%
  arrange(
    desc(tot_units_apartment_sum_large_gap),
    desc(tot_units_apartment_sum_gap),
    desc(unit_source == "tot_units_fallback"),
    desc(residential_units),
    pin,
    desc(year)
  )

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
    class_es,
    property_type_use,
    property_name_description
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

questionable_apartment_review <- tibble::tribble(
  ~pin, ~audit_reason, ~review_decision, ~reviewed_residential_units, ~evidence_summary, ~action_taken,
  "17151100370000",
  "Student-housing row where CCAO unit detail appears to count beds rather than apartments.",
  "Use externally reviewed apartment count.",
  524,
  "525 S State is University Center. Public apartment listings describe about 524 apartments, while the CCAO student-housing unit fields report much larger bed-like counts.",
  "Manual override applied.",
  "17172310020000",
  "Student-housing label, high unit count.",
  "Keep apartment-unit sum.",
  300,
  "1035 W Van Buren has stable CCAO apartment-unit detail across available commercial valuation rows and no total/detail disagreement.",
  "No override.",
  "17164040190000",
  "Student-housing label, high unit count.",
  "Keep apartment-unit sum.",
  178,
  "642 S Clark has stable CCAO apartment-unit detail across available commercial valuation rows and no total/detail disagreement.",
  "No override.",
  "14321060130000",
  "Student-housing label, high unit count.",
  "Keep apartment-unit sum.",
  160,
  "1237 W Fullerton has stable CCAO apartment-unit detail across available commercial valuation rows and no total/detail disagreement.",
  "No override.",
  "11323310340000",
  "Student-housing label, high unit count.",
  "Keep apartment-unit sum.",
  152,
  "1209 W Arthur has stable CCAO apartment-unit detail across available commercial valuation rows and no total/detail disagreement.",
  "No override.",
  "17172270010000",
  "Student-housing label, high unit count.",
  "Keep apartment-unit sum.",
  135,
  "847 W Jackson / Tailor Lofts public descriptions separate roughly 135 apartments from larger bed counts; the selected CCAO row reports 135 apartment units.",
  "No override.",
  "17194110010000",
  "Student-housing label on a small mixed-use row.",
  "Keep apartment-unit sum after review.",
  15,
  "1659 W 18th has stable CCAO apartment-unit detail at 15. A larger public aggregation appears to combine multiple valuation records rather than identify residential apartments on this selected row.",
  "No override; keep flagged as reviewed.",
  "14303140460000",
  "Bedroom-detail sum is below the source total on a reviewed mixed-use apartment building.",
  "Use externally reviewed dwelling-unit count.",
  34,
  "2439/2443 N Western public and permit descriptions identify 34 dwelling units, while the CCAO bedroom detail reports only 30 two-bedroom units.",
  "Manual override applied.",
  "16122180170000",
  "Mixed-use row where source total appears to include nonresidential units.",
  "Keep apartment-unit sum.",
  10,
  "520 N Western has 10 apartments in the CCAO bedroom detail; the broader source total is 13 and appears to include commercial or ancillary units.",
  "No override.",
  "17053280180000",
  "Mixed-use row where source total appears to include nonresidential units.",
  "Keep apartment-unit sum.",
  8,
  "880 N Milwaukee has 8 apartments in the CCAO bedroom detail and commercial square footage; the source total of 9 is consistent with adding one commercial unit.",
  "No override.",
  "14204200420000",
  "Mixed-use row where source total appears to include retail.",
  "Keep apartment-unit sum.",
  7,
  "3322 N Halsted permit language describes ground-floor retail and 7 dwelling units; the source total is 8.",
  "No override.",
  "13253150460000",
  "Mixed-use row where source total appears to include nonresidential units.",
  "Keep apartment-unit sum.",
  6,
  "2487 N Milwaukee public descriptions are consistent with 6 residential units plus commercial space; the source total is 7.",
  "No override.",
  "14203180020000",
  "Mixed-use row where source total appears to include retail.",
  "Keep apartment-unit sum.",
  6,
  "3355 N Southport permit language describes 6 dwelling units plus one retail tenant space; the source total is 7.",
  "No override.",
  "17301040090000",
  "Mixed-use row where source total appears to include retail.",
  "Keep apartment-unit sum.",
  6,
  "2135 W Cermak permit language describes 6 dwelling units with ground-floor retail; the source total is 7.",
  "No override."
)

commercial_questionable_apartment_audit <- questionable_apartment_review %>%
  left_join(
    multifamily_data_deduped %>%
      mutate(
        cleaned_far = bldgsf / landsf,
        cleaned_dupac = 43560 * tot_units / landsf,
        in_main_2006_2022_density_period = yearbuilt >= 2006 & yearbuilt <= 2022
      ) %>%
      select(
        pin,
        address,
        yearbuilt,
        cleaned_residential_units = tot_units,
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
        cleaned_far,
        cleaned_dupac,
        pin_group_count,
        modelgroup,
        class_es,
        property_type_use,
        in_main_2006_2022_density_period
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    cleaned_units_match_review = cleaned_residential_units == reviewed_residential_units,
    needs_followup = is.na(cleaned_residential_units) | !cleaned_units_match_review
  ) %>%
  select(
    pin,
    address,
    yearbuilt,
    audit_reason,
    review_decision,
    reviewed_residential_units,
    cleaned_residential_units,
    cleaned_units_match_review,
    needs_followup,
    action_taken,
    evidence_summary,
    source_tot_units,
    apartment_unit_sum,
    unit_source,
    unit_override_applied,
    manual_residential_units,
    unit_override_reason,
    tot_units_apartment_sum_gap,
    tot_units_apartment_sum_large_gap,
    aprx_comm_sf,
    bldgsf,
    landsf,
    source_landsf,
    cleaned_far,
    cleaned_dupac,
    pin_group_count,
    modelgroup,
    class_es,
    property_type_use,
    in_main_2006_2022_density_period
  ) %>%
  arrange(desc(cleaned_residential_units), pin)

if (any(commercial_questionable_apartment_audit$needs_followup)) {
  stop("Questionable apartment audit has unmatched or mismatched cleaned unit counts.")
}

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
    "unit_rows_using_apartment_sum",
    "unit_rows_using_tot_units_fallback",
    "unit_rows_using_manual_override",
    "unit_rows_with_tot_apartment_gap",
    "unit_rows_with_large_tot_apartment_gap",
    "selected_rows_using_tot_units_fallback",
    "selected_rows_using_manual_override",
    "selected_rows_with_tot_apartment_gap",
    "selected_rows_with_large_tot_apartment_gap",
    "near_pin_multiple_candidates",
    "land_corrections_applied",
    "density_review_flag_rows",
    "questionable_apartment_review_rows"
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
    sum(multifamily_data$unit_source == "apartment_unit_sum", na.rm = TRUE),
    sum(multifamily_data$unit_source == "tot_units_fallback", na.rm = TRUE),
    sum(multifamily_data$unit_source == "manual_override", na.rm = TRUE),
    sum(multifamily_data$tot_units_apartment_sum_gap, na.rm = TRUE),
    sum(multifamily_data$tot_units_apartment_sum_large_gap, na.rm = TRUE),
    sum(multifamily_data_selected$unit_source == "tot_units_fallback", na.rm = TRUE),
    sum(multifamily_data_selected$unit_source == "manual_override", na.rm = TRUE),
    sum(multifamily_data_selected$tot_units_apartment_sum_gap, na.rm = TRUE),
    sum(multifamily_data_selected$tot_units_apartment_sum_large_gap, na.rm = TRUE),
    nrow(land_correction_candidates),
    sum(land_correction_candidates$apply_land_correction, na.rm = TRUE),
    nrow(density_review_flags),
    nrow(commercial_questionable_apartment_audit)
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
write_csv(unit_definition_audit, "../output/commercial_unit_definition_audit.csv")
write_csv(commercial_questionable_apartment_audit, "../output/commercial_questionable_apartment_audit.csv")
