# this code creates a dataset of unique properties in Chicago based on their build year. it is queried from:
# https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds
# to include only buildings built on or after the year 1999 and in townships 70-77 (Chicago)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---------- 1) Load and keep Chicago ----------
data <- readr::read_csv("../input/commercial_value_raw.csv")

data <- data %>%
  dplyr::filter(township %in% c("West Chicago","South Chicago","Jefferson","North Chicago","Lake View","Rogers Park","Hyde Park","Lake"))

# - Convert spelled counts (None/Two/…) to integers for num_apartments
# - Pull the numeric from "garage_size" strings like "2.5 cars"
# - Coerce key numeric columns
data <- data %>%
  dplyr::mutate(
    tax_year      = as.integer(tax_year),
    card_num      = as.integer(card_num),
    year_built    = as.integer(year_built),
    building_sqft = as.numeric(building_sqft),
    land_sqft     = as.numeric(land_sqft),
    num_rooms         = as.numeric(num_rooms),
    num_bedrooms      = as.numeric(num_bedrooms),
    num_full_baths    = as.numeric(num_full_baths),
    num_half_baths    = as.numeric(num_half_baths),
    num_fireplaces    = as.numeric(num_fireplaces),
    num_apartments = dplyr::case_when(
      is.na(num_apartments) ~ NA_integer_,
      tolower(trimws(num_apartments)) %in% c("none","zero") ~ 0L,
      tolower(trimws(num_apartments)) == "one"   ~ 1L,
      tolower(trimws(num_apartments)) == "two"   ~ 2L,
      tolower(trimws(num_apartments)) == "three" ~ 3L,
      tolower(trimws(num_apartments)) == "four"  ~ 4L,
      tolower(trimws(num_apartments)) == "five"  ~ 5L,
      tolower(trimws(num_apartments)) == "six"   ~ 6L,
      TRUE ~ suppressWarnings(as.integer(num_apartments)) # if already numeric text
    ),
    garage_size = readr::parse_number(garage_size)  # e.g., "2.5 cars" -> 2.5
  )

# ---------- 3) Cross-section: one row per building (pin, card_num) at build year ----------
# Tie-breakers: earliest tax year, then largest building_sqft
cross_section_buildings <- data %>%
  dplyr::group_by(pin) %>%
  dplyr::slice_min(order_by = dplyr::if_else(is.na(year_built), Inf, year_built), with_ties = TRUE) %>%  # earliest build year
  dplyr::slice_min(order_by = tax_year, with_ties = TRUE) %>%                                            # then earliest tax year
  dplyr::slice_max(order_by = building_sqft, with_ties = FALSE) %>%                                      # then largest sqft
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(year_built)) 

# (Optional) sanity check uniqueness
stopifnot(nrow(cross_section_buildings) == dplyr::n_distinct(cross_section_buildings$pin, cross_section_buildings$card_num))

# ---------- 4) Save ----------
write_csv(cross_section_buildings, "../output/residential_cross_section.csv")
