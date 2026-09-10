# Clean multifamily commercial valuation rows for buildings built since 1999 in Chicago townships.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_cleaning/code")
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
  mutate(
    unit_source = preliminary_unit_source,
    tot_units = preliminary_tot_units
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

# This file discovers candidate parcels. Preserve the source measurements here;
# building-specific land selection belongs to the final commercial cleaning.
multifamily_data_deduped <- multifamily_data_selected %>% select(-has_land, -year)

if (anyNA(multifamily_data_deduped$pin) || anyDuplicated(multifamily_data_deduped$pin)) {
  stop("Commercial cross-section must have unique nonmissing PINs.")
}

write_csv(multifamily_data_deduped, "../output/multifamily_data_cleaned.csv")
