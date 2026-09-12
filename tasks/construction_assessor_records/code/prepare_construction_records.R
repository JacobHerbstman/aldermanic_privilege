# setwd("tasks/construction_assessor_records/code")
# minimum_construction_year <- 1999
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(minimum_construction_year, preferred_assessment_year, fallback_assessment_year)
if (length(args) != 3L) stop("Expected 3 specification arguments from Makefile.")
minimum_construction_year <- as.integer(args[1])
preferred_assessment_year <- as.integer(args[2])
fallback_assessment_year <- as.integer(args[3])

# Build residential cross section

if (anyNA(c(minimum_construction_year, preferred_assessment_year, fallback_assessment_year)) ||
    minimum_construction_year > preferred_assessment_year ||
    preferred_assessment_year > fallback_assessment_year) {
  stop("Construction and assessment-year cutoffs must be ordered integers.")
}

con <- DBI::dbConnect(duckdb::duckdb())

# Read the Assessor history once. Both selections retain the original source order
# for ties; the first supplies historical parcel searches, the second measurements.
history <- DBI::dbGetQuery(con, sprintf("
WITH candidate_cards AS (
  SELECT pin, card_num FROM read_parquet('../input/residential_assessor_history.parquet')
  GROUP BY pin, card_num HAVING max(year_built) >= %d
)
SELECT r.* EXCLUDE (apartments_text, source_row_order)
FROM read_parquet('../input/residential_assessor_history.parquet') r
INNER JOIN candidate_cards USING (pin, card_num)
ORDER BY source_row_order
", minimum_construction_year))

# Historical parcel searches retain the earliest reported building year.
discovery <- history %>%
  filter(year_built >= minimum_construction_year) %>%
  group_by(pin) %>%
  slice_min(year_built, with_ties = TRUE) %>%
  slice_min(tax_year, with_ties = TRUE) %>%
  slice_max(building_sqft, with_ties = FALSE) %>%
  ungroup()
stopifnot(!anyDuplicated(discovery$pin))

# Empty cards cannot establish the building used for measurement selection.
data <- history %>%
  filter(!is.na(building_sqft) | !is.na(num_apartments)) %>%
  group_by(pin, card_num) %>%
  filter(any(year_built >= minimum_construction_year, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  group_by(pin) %>%
  mutate(cards_in_history = n_distinct(card_num)) %>%
  ungroup()

# Prefer the latest report through 2022; use the 2025 window, then later
# reports, only for PINs absent from the earlier window.
single_card <- data %>%
  filter(cards_in_history == 1, year_built >= minimum_construction_year) %>%
  mutate(report_priority = case_when(
    tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L,
    TRUE ~ 3L
  )) %>%
  arrange(pin, report_priority, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-report_priority)

multicard <- data %>%
  filter(cards_in_history > 1, year_built >= minimum_construction_year) %>%
  group_by(pin) %>%
  slice_min(order_by = year_built, with_ties = TRUE) %>%
  slice_min(order_by = tax_year, with_ties = TRUE) %>%
  slice_max(order_by = building_sqft, with_ties = FALSE) %>%
  ungroup()

cross_section_buildings <- bind_rows(
  single_card,
  multicard
) %>%
  select(-cards_in_history) %>%
  arrange(pin)

if (any(cross_section_buildings$year_built < minimum_construction_year, na.rm = TRUE)) {
  stop("New-construction residential cross-section contains buildings before the minimum construction year.", call. = FALSE)
}

if (anyDuplicated(cross_section_buildings$pin) > 0) {
  stop("Residential new-construction cross-section is not unique by PIN.", call. = FALSE)
}

dbDisconnect(con, shutdown = TRUE)

SaveData(discovery, c("pin"), "../output/residential_discovery_cross_section.csv")
SaveData(cross_section_buildings, c("pin"), "../output/residential_cross_section.csv")

# Build commercial cross section

# Clean multifamily commercial valuation rows for buildings built since 1999 in Chicago townships.

data <- readr::read_csv("../input/commercial_value_raw.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::rename(modelgroup = sheet)

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

SaveData(multifamily_data_deduped, c("pin"), "../output/multifamily_data_cleaned.csv")
