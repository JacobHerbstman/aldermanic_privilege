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

data <- data %>%
  dplyr::filter(township %in% c("West Chicago","South Chicago","Jefferson","North Chicago","Lake View","Rogers Park","Hyde Park","Lake"))

data <- data %>%
  filter(yearbuilt >= 1999)


multifamily_data <- data %>%
  filter(str_detect(modelgroup, "(?i)Multifamily|Class3|Class9|Condos")) %>%
  mutate(calculated_sum = rowSums(select(., studiounits, x1brunits, x2brunits, x3brunits, x4brunits), na.rm = TRUE)) %>% 
  mutate(tot_units = coalesce(tot_units, if_else(calculated_sum > 0, calculated_sum, NA_real_))) %>% 
  filter(!is.na(tot_units)) %>% 
  select(-calculated_sum) %>% 
  select(
    keypin,
    address,
    year,
    yearbuilt,
    tot_units,
    bldgsf,
    landsf,
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


multifamily_data_deduped <- multifamily_data %>%
  # Create a helper flag for "Has Land"
  mutate(has_land = landsf > 0) %>%
  group_by(pin) %>% 
  arrange(desc(has_land), desc(tot_units), desc(year), modelgroup, address) %>%
  slice(1) %>%
  ungroup() %>%
  select(-has_land, -year)

write_csv(multifamily_data_deduped, "../output/multifamily_data_cleaned.csv")
