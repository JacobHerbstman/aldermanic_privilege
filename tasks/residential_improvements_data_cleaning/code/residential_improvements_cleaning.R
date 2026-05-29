# this code creates a dataset of unique properties in Chicago based on their build year. it is queried from:
# https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds
# to include only buildings built on or after the year 1999 and in townships 70-77 (Chicago)

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/residential_improvements_data_cleaning/code")
source("../../setup_environment/code/packages.R")

parse_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

invisible(dbExecute(con, sprintf("
CREATE TABLE residential_improvements AS
SELECT
  trim(pin) AS pin,
  trim(year) AS tax_year,
  trim(card) AS card_num,
  trim(class) AS class,
  trim(township_code) AS township_code,
  trim(tieback_key_pin) AS proration_key_pin,
  trim(tieback_proration_rate) AS pin_proration_rate,
  trim(card_proration_rate) AS card_proration_rate,
  trim(cdu) AS cdu,
  trim(pin_is_multicard) AS pin_is_multicard,
  trim(pin_num_cards) AS pin_num_cards,
  trim(pin_is_multiland) AS pin_is_multiland,
  trim(pin_num_landlines) AS pin_num_landlines,
  trim(char_yrblt) AS year_built,
  trim(char_bldg_sf) AS building_sqft,
  trim(char_land_sf) AS land_sqft,
  trim(char_beds) AS num_bedrooms,
  trim(char_rooms) AS num_rooms,
  trim(char_fbath) AS num_full_baths,
  trim(char_hbath) AS num_half_baths,
  trim(char_frpl) AS num_fireplaces,
  trim(char_type_resd) AS type_of_residence,
  trim(char_cnst_qlty) AS construction_quality,
  trim(char_apts) AS num_apartments,
  trim(char_attic_fnsh) AS attic_finish,
  trim(char_gar1_att) AS garage_attached,
  trim(char_gar1_area) AS garage_area_included,
  trim(char_gar1_size) AS garage_size,
  trim(char_gar1_cnst) AS garage_ext_wall_material,
  trim(char_attic_type) AS attic_type,
  trim(char_bsmt) AS basement_type,
  trim(char_ext_wall) AS ext_wall_material,
  trim(char_heat) AS central_heating,
  trim(char_repair_cnd) AS repair_condition,
  trim(char_bsmt_fin) AS basement_finish,
  trim(char_roof_cnst) AS roof_material,
  trim(char_use) AS single_v_multi_family,
  trim(char_site) AS site_desirability,
  trim(char_ncu) AS num_commercial_units,
  trim(char_renovation) AS renovation,
  trim(char_porch) AS porch,
  trim(char_air) AS central_air,
  trim(char_tp_plan) AS design_plan,
  trim(row_id) AS row_id
FROM read_csv(%s,
              all_varchar = true,
              header = true,
              ignore_errors = true,
              max_line_size = 10000000)
WHERE try_cast(numeric_text(township_code) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
  AND try_cast(numeric_text(char_yrblt) AS INTEGER) >= 1999;
", dbQuoteString(con, "../input/residential_improvement_characteristics_full.csv"))))

data <- dbGetQuery(con, "SELECT * FROM residential_improvements")

# - Convert spelled counts (None/Two/…) to integers for num_apartments
# - Pull the numeric from "garage_size" strings like "2.5 cars"
# - Coerce key numeric columns
data <- data %>%
  dplyr::mutate(
    tax_year      = as.integer(parse_numeric(tax_year)),
    card_num      = as.integer(parse_numeric(card_num)),
    pin_is_multicard = tolower(trimws(pin_is_multicard)) == "true",
    pin_is_multiland = tolower(trimws(pin_is_multiland)) == "true",
    year_built    = as.integer(parse_numeric(year_built)),
    building_sqft = parse_numeric(building_sqft),
    land_sqft     = parse_numeric(land_sqft),
    pin_proration_rate = parse_numeric(pin_proration_rate),
    card_proration_rate = parse_numeric(card_proration_rate),
    pin_num_cards = as.integer(parse_numeric(pin_num_cards)),
    pin_num_landlines = as.integer(parse_numeric(pin_num_landlines)),
    num_rooms         = parse_numeric(num_rooms),
    num_bedrooms      = parse_numeric(num_bedrooms),
    num_full_baths    = parse_numeric(num_full_baths),
    num_half_baths    = parse_numeric(num_half_baths),
    num_fireplaces    = parse_numeric(num_fireplaces),
    num_apartments = dplyr::case_when(
      is.na(num_apartments) ~ NA_integer_,
      tolower(trimws(num_apartments)) %in% c("none","zero") ~ 0L,
      tolower(trimws(num_apartments)) == "one"   ~ 1L,
      tolower(trimws(num_apartments)) == "two"   ~ 2L,
      tolower(trimws(num_apartments)) == "three" ~ 3L,
      tolower(trimws(num_apartments)) == "four"  ~ 4L,
      tolower(trimws(num_apartments)) == "five"  ~ 5L,
      tolower(trimws(num_apartments)) == "six"   ~ 6L,
      TRUE ~ as.integer(parse_numeric(num_apartments))
    ),
    garage_size = parse_numeric(garage_size),
    num_commercial_units = as.integer(parse_numeric(num_commercial_units))
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

if (any(cross_section_buildings$year_built < 1999, na.rm = TRUE)) {
  stop("New-construction residential cross-section contains pre-1999 buildings.", call. = FALSE)
}

# (Optional) sanity check uniqueness
stopifnot(nrow(cross_section_buildings) == dplyr::n_distinct(cross_section_buildings$pin, cross_section_buildings$card_num))

write_csv(cross_section_buildings, "../output/residential_cross_section.csv")
