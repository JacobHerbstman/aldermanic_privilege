# Clean Cook County commercial valuation rows for non-condo residential multifamily.
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/commercial_value_data_cleaning/code")
# commercial_value_raw_path <- "../input/commercial_value_raw.csv"
# approved_assessor_corrections_path <- "../input/approved_assessor_corrections.csv"
# approved_commercial_density_blocker_corrections_path <- "../input/approved_commercial_density_blocker_corrections.csv"
# multifamily_data_cleaned_path <- "../output/multifamily_data_cleaned.csv"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) > 0) {
  if (length(cli_args) != 4) {
    stop("Expected arguments: commercial_value_raw_path approved_assessor_corrections_path approved_commercial_density_blocker_corrections_path multifamily_data_cleaned_path.", call. = FALSE)
  }
  commercial_value_raw_path <- cli_args[1]
  approved_assessor_corrections_path <- cli_args[2]
  approved_commercial_density_blocker_corrections_path <- cli_args[3]
  multifamily_data_cleaned_path <- cli_args[4]
} else if (!interactive()) {
  stop("Expected arguments: commercial_value_raw_path approved_assessor_corrections_path approved_commercial_density_blocker_corrections_path multifamily_data_cleaned_path.", call. = FALSE)
}

min_construction_year <- 2006L
max_construction_year <- 2022L

normalize_pin <- function(x) {
  out <- stringr::str_sub(gsub("[^0-9]", "", as.character(x)), 1, 14)
  bad_pin <- is.na(out) | out == "" | out == "NA" | nchar(out) < 14
  out[bad_pin] <- NA_character_
  out
}

as_num_clean <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

normalize_class_codes <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  pieces <- strsplit(x, ",")
  vapply(pieces, function(vals) {
    vals <- trimws(vals)
    vals <- vals[vals != ""]
    vals <- gsub("[.]0$", "", vals)

    idx <- grepl("^[0-9]{3}$", vals)
    vals[idx] <- paste0(substr(vals[idx], 1, 1), "-", substr(vals[idx], 2, 3))

    idx <- grepl("^[0-9]{2}-May$", vals)
    vals[idx] <- paste0("5-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^May-[0-9]{2}$", vals)
    vals[idx] <- paste0("5-", substr(vals[idx], 5, 6))

    idx <- grepl("^[0-9]{2}-Mar$", vals)
    vals[idx] <- paste0("3-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^Mar-[0-9]{2}$", vals)
    vals[idx] <- paste0("3-", substr(vals[idx], 5, 6))

    idx <- grepl("^[0-9]{2}-Sep$", vals)
    vals[idx] <- paste0("9-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^Sep-[0-9]{2}$", vals)
    vals[idx] <- paste0("9-", substr(vals[idx], 5, 6))

    vals <- vals[vals != ""]
    if (length(vals) == 0) {
      NA_character_
    } else {
      paste(unique(vals), collapse = ", ")
    }
  }, character(1))
}

has_class_code <- function(class_codes, codes) {
  class_codes <- paste0(", ", as.character(class_codes), ", ")
  Reduce(`|`, lapply(codes, function(code) grepl(paste0("(^|, )", code, "(,|$)"), class_codes)))
}

approved_corrections <- dplyr::bind_rows(
  readr::read_csv(approved_assessor_corrections_path, show_col_types = FALSE, col_types = readr::cols(.default = "c")),
  readr::read_csv(approved_commercial_density_blocker_corrections_path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
) %>%
  janitor::clean_names() %>%
  dplyr::filter(status == "approved", source == "commercial_value_raw") %>%
  dplyr::mutate(
    pin = normalize_pin(pin),
    corrected_year_built = as.integer(as_num_clean(corrected_year_built)),
    corrected_units = as.numeric(as_num_clean(corrected_units)),
    corrected_building_sqft = as.numeric(as_num_clean(corrected_building_sqft)),
    corrected_land_sqft = as.numeric(as_num_clean(corrected_land_sqft))
  ) %>%
  dplyr::select(
    source_row_id,
    correction_pin = pin,
    corrected_year_built,
    corrected_units,
    corrected_building_sqft,
    corrected_land_sqft
  )

missing_source_row_id_corrections <- approved_corrections %>%
  dplyr::filter(is.na(source_row_id) | trimws(source_row_id) == "")

if (nrow(missing_source_row_id_corrections) > 0) {
  stop("Approved commercial assessor corrections must include source_row_id.", call. = FALSE)
}

duplicate_approved_corrections <- approved_corrections %>%
  dplyr::count(source_row_id, name = "n") %>%
  dplyr::filter(n > 1)

if (nrow(duplicate_approved_corrections) > 0) {
  stop("Approved commercial assessor corrections contain duplicate source_row_id values.", call. = FALSE)
}

raw_data <- readr::read_csv(
  commercial_value_raw_path,
  show_col_types = FALSE,
  col_types = readr::cols(
    keypin = readr::col_character(),
    pins = readr::col_character(),
    township = readr::col_character(),
    modelgroup = readr::col_character(),
    `class(es)` = readr::col_character(),
    address = readr::col_character(),
    category = readr::col_character(),
    owner = readr::col_character(),
    `property_name/description` = readr::col_character(),
    `property_type/use` = readr::col_character(),
    subclass2 = readr::col_character(),
    taxpayer = readr::col_character(),
    .default = readr::col_character()
  )
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    raw_row_n = dplyr::row_number(),
    source_row_id = paste0("commercial_raw:", raw_row_n),
    pin = normalize_pin(keypin),
    yearbuilt = as.integer(as_num_clean(yearbuilt)),
    tot_units = as.numeric(as_num_clean(tot_units)),
    studiounits = as.numeric(as_num_clean(studiounits)),
    x1brunits = as.numeric(as_num_clean(x1brunits)),
    x2brunits = as.numeric(as_num_clean(x2brunits)),
    x3brunits = as.numeric(as_num_clean(x3brunits)),
    x4brunits = as.numeric(as_num_clean(x4brunits)),
    bldgsf = as.numeric(as_num_clean(bldgsf)),
    landsf = as.numeric(as_num_clean(landsf))
  )

unmatched_approved_corrections <- approved_corrections %>%
  dplyr::anti_join(raw_data %>% dplyr::select(source_row_id), by = "source_row_id")

if (nrow(unmatched_approved_corrections) > 0) {
  stop("Approved commercial assessor corrections reference source_row_id values not found in raw data.", call. = FALSE)
}

data <- raw_data %>%
  dplyr::left_join(approved_corrections, by = "source_row_id") %>%
  dplyr::mutate(
    manual_correction_applied = (!is.na(correction_pin) & correction_pin != pin) |
      !is.na(corrected_year_built) |
      !is.na(corrected_units) |
      !is.na(corrected_building_sqft) |
      !is.na(corrected_land_sqft),
    pin = dplyr::coalesce(correction_pin, pin),
    yearbuilt = dplyr::coalesce(corrected_year_built, yearbuilt),
    unit_mix_sum = rowSums(dplyr::pick(studiounits, x1brunits, x2brunits, x3brunits, x4brunits), na.rm = TRUE),
    tot_units = dplyr::if_else(!is.na(corrected_units), corrected_units,
      dplyr::if_else(!is.na(tot_units) & tot_units > 0, tot_units,
        dplyr::if_else(unit_mix_sum > 0, unit_mix_sum, NA_real_)
      )
    ),
    bldgsf = dplyr::coalesce(corrected_building_sqft, bldgsf),
    landsf = dplyr::coalesce(corrected_land_sqft, landsf),
    class_codes = normalize_class_codes(class_es),
    text_property_fields = paste(modelgroup, class_es, property_type_use, property_name_description, category, subclass2),
    current_model_rule = stringr::str_detect(modelgroup, stringr::regex("Multifamily|Class3|Class9|Condos", ignore_case = TRUE)),
    known_condo_signal = stringr::str_detect(text_property_fields, stringr::regex("\\b(condo|condominium)\\b", ignore_case = TRUE)) |
      has_class_code(class_codes, c("2-99", "3-99", "4-99", "5-99", "7-99", "8-99", "9-59")),
    hotel_motel_signal = stringr::str_detect(
      text_property_fields,
      stringr::regex("\\b(hotel|motel|lodging|inn|hostel|rooming|boarding|dorm|hospital|nursing|assisted living|shelter|hyatt|aloft|waldorf)\\b", ignore_case = TRUE)
    ) | has_class_code(class_codes, c("5-16", "5-29", "7-16", "7-29", "8-16", "8-29")),
    commercial_exclusion_flag = dplyr::case_when(
      known_condo_signal ~ "known_condo",
      hotel_motel_signal ~ "hotel_motel_or_institution",
      TRUE ~ "included_noncondo_nonhotel"
    ),
    in_paper_window = yearbuilt >= min_construction_year & yearbuilt <= max_construction_year,
    has_land = landsf > 0
  ) %>%
  dplyr::filter(
    township %in% c("West Chicago", "South Chicago", "Jefferson", "North Chicago", "Lake View", "Rogers Park", "Hyde Park", "Lake"),
    !is.na(pin),
    yearbuilt >= 1999,
    current_model_rule,
    tot_units > 0,
    commercial_exclusion_flag == "included_noncondo_nonhotel"
  )

equivalent_unit_fills <- data %>%
  dplyr::group_by(pin, yearbuilt, bldgsf) %>%
  dplyr::summarise(equivalent_row_units = max(tot_units, na.rm = TRUE), .groups = "drop")

multifamily_data_cleaned <- data %>%
  dplyr::group_by(pin) %>%
  dplyr::mutate(
    pin_land_fill = ifelse(all(is.na(landsf) | landsf <= 0), NA_real_, max(landsf, na.rm = TRUE)),
    pin_building_fill = ifelse(all(is.na(bldgsf) | bldgsf <= 0), NA_real_, max(bldgsf, na.rm = TRUE))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(pin, dplyr::desc(in_paper_window), dplyr::desc(has_land), dplyr::desc(tot_units), yearbuilt, dplyr::desc(bldgsf), raw_row_n) %>%
  dplyr::group_by(pin) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(equivalent_unit_fills, by = c("pin", "yearbuilt", "bldgsf")) %>%
  dplyr::mutate(
    tot_units = dplyr::if_else(equivalent_row_units > tot_units, equivalent_row_units, tot_units),
    landsf = dplyr::if_else(is.na(landsf) | landsf <= 0, pin_land_fill, landsf),
    bldgsf = dplyr::if_else(is.na(bldgsf) | bldgsf <= 0, pin_building_fill, bldgsf)
  ) %>%
  dplyr::select(
    pin,
    address,
    yearbuilt,
    tot_units,
    bldgsf,
    landsf,
    modelgroup,
    class_es,
    property_type_use,
    commercial_exclusion_flag,
    source_row_id,
    manual_correction_applied,
    studiounits,
    x1brunits,
    x2brunits,
    x3brunits,
    x4brunits
  )

if (anyDuplicated(multifamily_data_cleaned$pin)) {
  stop("Commercial multifamily output still contains duplicate PINs after deduplication.", call. = FALSE)
}

readr::write_csv(multifamily_data_cleaned, multifamily_data_cleaned_path)
