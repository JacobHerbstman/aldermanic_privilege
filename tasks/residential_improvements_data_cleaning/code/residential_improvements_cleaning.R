# Build a parcel-level residential assessor source from improvement cards.
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/residential_improvements_data_cleaning/code")
# residential_improvement_characteristics_path <- "../input/residential_improvement_characteristics.csv"
# approved_assessor_corrections_path <- "../input/approved_assessor_corrections.csv"
# residential_cross_section_path <- "../output/residential_cross_section.csv"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) > 0) {
  if (length(cli_args) != 3) {
    stop("Expected arguments: residential_improvement_characteristics_path approved_assessor_corrections_path residential_cross_section_path.", call. = FALSE)
  }
  residential_improvement_characteristics_path <- cli_args[1]
  approved_assessor_corrections_path <- cli_args[2]
  residential_cross_section_path <- cli_args[3]
} else if (!interactive()) {
  stop("Expected arguments: residential_improvement_characteristics_path approved_assessor_corrections_path residential_cross_section_path.", call. = FALSE)
}

min_construction_year <- 2006L
max_construction_year <- 2022L

normalize_pin <- function(x) {
  out <- stringr::str_sub(gsub("[^0-9]", "", as.character(x)), 1, 14)
  out[nchar(out) < 14] <- NA_character_
  out[out == "" | out == "NA"] <- NA_character_
  out
}

as_num_clean <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

parse_unit_words <- function(x) {
  x_clean <- tolower(trimws(as.character(x)))
  out <- suppressWarnings(as.integer(as_num_clean(x_clean)))
  out[x_clean %in% c("none", "zero", "no", "na", "n/a", "")] <- 0L
  out[x_clean == "one"] <- 1L
  out[x_clean == "two"] <- 2L
  out[x_clean == "three"] <- 3L
  out[x_clean == "four"] <- 4L
  out[x_clean == "five"] <- 5L
  out[x_clean == "six"] <- 6L
  out[is.na(x)] <- NA_integer_
  out
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

    vals <- vals[vals != ""]
    if (length(vals) == 0) {
      NA_character_
    } else {
      paste(unique(vals), collapse = ", ")
    }
  }, character(1))
}

combine_unique <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    NA_character_
  } else {
    paste(unique(x), collapse = ", ")
  }
}

sum_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}

max_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

min_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    min(x, na.rm = TRUE)
  }
}

has_class_code <- function(class_codes, codes) {
  class_codes <- paste0(", ", as.character(class_codes), ", ")
  Reduce(`|`, lapply(codes, function(code) grepl(paste0("(^|, )", code, "(,|$)"), class_codes)))
}

approved_corrections <- readr::read_csv(approved_assessor_corrections_path, show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::filter(status == "approved", source == "residential_improvement_characteristics") %>%
  dplyr::mutate(
    pin = normalize_pin(pin),
    corrected_year_built = as.integer(as_num_clean(corrected_year_built)),
    corrected_units = as.integer(as_num_clean(corrected_units)),
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

data <- readr::read_csv(residential_improvement_characteristics_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    raw_row_n = dplyr::row_number(),
    source_row_id = paste0("residential_raw:", raw_row_n),
    pin = normalize_pin(pin),
    tax_year = as.integer(as_num_clean(tax_year)),
    card_num = as.integer(as_num_clean(card_num)),
    year_built = as.integer(as_num_clean(year_built)),
    building_sqft = as.numeric(as_num_clean(building_sqft)),
    land_sqft = as.numeric(as_num_clean(land_sqft)),
    num_rooms = as.numeric(as_num_clean(num_rooms)),
    num_bedrooms = as.numeric(as_num_clean(num_bedrooms)),
    num_full_baths = as.numeric(as_num_clean(num_full_baths)),
    num_half_baths = as.numeric(as_num_clean(num_half_baths)),
    num_fireplaces = as.numeric(as_num_clean(num_fireplaces)),
    garage_size = as.numeric(as_num_clean(garage_size)),
    num_apartments = parse_unit_words(num_apartments)
  ) %>%
  dplyr::filter(
    township_code %in% c("70", "71", "72", "73", "74", "75", "76", "77"),
    !is.na(pin),
    !is.na(card_num)
  ) %>%
  dplyr::left_join(approved_corrections, by = "source_row_id") %>%
  dplyr::mutate(
    pin = dplyr::coalesce(correction_pin, pin),
    year_built = dplyr::coalesce(corrected_year_built, year_built),
    num_apartments = dplyr::coalesce(corrected_units, num_apartments),
    building_sqft = dplyr::coalesce(corrected_building_sqft, building_sqft),
    land_sqft = dplyr::coalesce(corrected_land_sqft, land_sqft),
    class_codes = normalize_class_codes(class),
    source_class_group = stringr::str_replace(class_codes, ",.*$", ""),
    known_condo_signal = has_class_code(class_codes, c("2-99")) |
      stringr::str_detect(paste(class, type_of_residence, single_v_multi_family), stringr::regex("condo|condominium", ignore_case = TRUE)),
    possible_condo_signal = has_class_code(class_codes, c("2-97")),
    residential_exclusion_flag = dplyr::case_when(
      known_condo_signal ~ "known_condo",
      possible_condo_signal ~ "possible_condo_2_97_review",
      TRUE ~ "included_noncondo"
    ),
    is_single_family_card = stringr::str_detect(single_v_multi_family, stringr::regex("^single", ignore_case = TRUE)) |
      type_of_residence %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"),
    num_apartments = dplyr::if_else(is_single_family_card & (is.na(num_apartments) | num_apartments == 0L), 1L, num_apartments)
  )

card_selected <- data %>%
  dplyr::arrange(pin, card_num, dplyr::coalesce(year_built, Inf), tax_year, dplyr::desc(building_sqft), raw_row_n) %>%
  dplyr::group_by(pin, card_num) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(year_built))

residential_cross_section <- card_selected %>%
  dplyr::group_by(pin) %>%
  dplyr::summarise(
    tax_year = suppressWarnings(as.integer(min_na(tax_year))),
    card_num = suppressWarnings(as.integer(min_na(card_num))),
    selected_card_nums = combine_unique(card_num),
    year_built = suppressWarnings(as.integer(min_na(year_built))),
    max_year_built = suppressWarnings(as.integer(max_na(year_built))),
    card_year_window_flag = dplyr::case_when(
      year_built >= min_construction_year & year_built <= max_construction_year &
        max_year_built >= min_construction_year & max_year_built <= max_construction_year ~ "all_cards_inside_2006_2022",
      !(year_built >= min_construction_year & year_built <= max_construction_year) &
        max_year_built >= min_construction_year & max_year_built <= max_construction_year ~ "min_outside_max_inside_review",
      year_built >= min_construction_year & year_built <= max_construction_year &
        !(max_year_built >= min_construction_year & max_year_built <= max_construction_year) ~ "min_inside_max_outside_review",
      TRUE ~ "outside_2006_2022"
    ),
    n_cards = dplyr::n(),
    n_distinct_card_years = dplyr::n_distinct(year_built, na.rm = TRUE),
    building_sqft = sum_na(building_sqft),
    land_sqft = max_na(land_sqft),
    num_rooms = sum_na(num_rooms),
    num_bedrooms = sum_na(num_bedrooms),
    num_full_baths = sum_na(num_full_baths),
    num_half_baths = sum_na(num_half_baths),
    num_fireplaces = sum_na(num_fireplaces),
    garage_size = sum_na(garage_size),
    num_apartments = suppressWarnings(as.integer(sum_na(num_apartments))),
    single_v_multi_family = dplyr::if_else(all(is_single_family_card, na.rm = TRUE), "Single-Family", "Multi-Family"),
    type_of_residence = {
      vals <- unique(stats::na.omit(type_of_residence))
      if (length(vals) == 1L) vals else NA_character_
    },
    class = combine_unique(class_codes),
    source_class_group = combine_unique(source_class_group),
    known_condo_signal = any(known_condo_signal, na.rm = TRUE),
    possible_condo_signal = any(possible_condo_signal, na.rm = TRUE),
    residential_exclusion_flag = dplyr::case_when(
      any(residential_exclusion_flag == "known_condo", na.rm = TRUE) ~ "known_condo",
      any(residential_exclusion_flag == "possible_condo_2_97_review", na.rm = TRUE) ~ "possible_condo_2_97_review",
      TRUE ~ "included_noncondo"
    ),
    row_id = combine_unique(row_id),
    source_row_ids = combine_unique(source_row_id),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    residential_exclusion_flag == "included_noncondo",
    num_apartments > 0
  )

if (anyDuplicated(residential_cross_section$pin)) {
  stop("Residential cross-section still contains duplicate PINs after card aggregation.", call. = FALSE)
}

readr::write_csv(residential_cross_section, residential_cross_section_path)
