# this code creates a dataset of unique properties in Chicago based on their build year. it is queried from:
# https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds
# to include only buildings built on or after the year 2000 for memory reasons.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---------- 1) Load and keep Chicago ----------
data <- readr::read_csv("../input/condos_characteristics.csv")

data <- data %>%
  filter(year_built >= 2000) %>% 
 filter(township_code %in% c("70","71","72","73","74","75","76","77"))

## 1) Chicago + typing + keep only living units (same as before)
condo <- data %>%
  mutate(
    tax_year      = as.integer(tax_year),
    year_built    = as.integer(year_built),
    building_sqft = as.numeric(building_sqft),
    land_sqft     = as.numeric(land_sqft),
    unit_sf       = as.numeric(unit_sf),
    is_parking_space = tolower(trimws(as.character(is_parking_space))) %in% c("true","t","y","1"),
    is_common_area   = tolower(trimws(as.character(is_common_area)))   %in% c("true","t","y","1")
  ) %>%
  filter(!is_parking_space, !is_common_area)

## 2) Per-building (pin10) metadata + cohort tax year = earliest tax_year >= built_year
bldg_meta <- condo %>%
  group_by(pin10) %>%
  summarise(
    built_year     = if (all(is.na(year_built))) NA_integer_ else min(year_built, na.rm = TRUE),
    first_tax_year = min(tax_year, na.rm = TRUE),
    .groups = "drop"
  )

cohort_tax <- condo %>%
  inner_join(bldg_meta, by = "pin10") %>%
  group_by(pin10) %>%
  summarise(
    cohort_tax_year = {
      by <- first(built_year)
      if (is.na(by)) {
        first(first_tax_year)
      } else {
        m <- min(tax_year[tax_year >= by], na.rm = TRUE)
        if (is.infinite(m)) first(first_tax_year) else m
      }
    },
    .groups = "drop"
  )

keys <- bldg_meta %>% left_join(cohort_tax, by = "pin10")

## 3) Keep only the cohort tax year rows (so no 2015+ rows for a 2003 build)
condo_at_build <- condo %>%
  inner_join(keys, by = "pin10") %>%
  filter(tax_year == cohort_tax_year)

## 4) Collapse to one row per condo building (pin10)
condo_buildings <- condo_at_build %>%
  group_by(pin10) %>%
  summarise(
    built_year        = first(built_year),
    tax_year          = first(cohort_tax_year),
    units             = n_distinct(pin),                                 # unit count at cohort
    building_sqft     = if (all(is.na(building_sqft))) NA_real_ else max(building_sqft, na.rm = TRUE),
    land_sqft         = if (all(is.na(land_sqft)))     NA_real_ else max(land_sqft,   na.rm = TRUE),
    total_unit_sf     = sum(unit_sf, na.rm = TRUE),
    bldg_is_mixed_use = any(tolower(trimws(as.character(bldg_is_mixed_use))) %in% c("true","t","y","1"), na.rm = TRUE),
    avg_bedrooms      = mean(as.numeric(num_bedrooms), na.rm = TRUE),
    avg_bathrooms     = mean(as.numeric(num_full_baths) + 0.5 * as.numeric(num_half_baths), na.rm = TRUE),
    .groups = "drop"
  )

# ---------- 5) Save ----------
write_csv(condo_buildings, "../output/condos_cross_section.csv")
