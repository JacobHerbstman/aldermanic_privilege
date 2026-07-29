# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

sample <- readr::read_csv(
  "../output/commercial_500ft_sample_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    pin,
    address = production_address,
    original_year = production_yearbuilt,
    original_units = production_units,
    original_building_sqft = production_bldgsf,
    original_land_sqft = production_landsf
  )

review <- tibble::tribble(
  ~pin, ~audit_action, ~revised_units, ~revised_building_sqft, ~revised_land_sqft, ~evidence, ~confidence, ~remaining_caveat,
  "14303140460000", "retain_verified_unit_override", 34, NA_real_, NA_real_, "New-construction permit 100647438 at 2443 N. Western states 34 dwelling units and uses this PIN10.", "high", "The commercial source address is 2439 N. Western, within the same development parcel.",
  "17153000140000", "exclude_as_preperiod", NA_real_, NA_real_, NA_real_, "Public housing records identify the building as completed in 2005; no new-construction permit supports the selected 2015 year.", "high", "The two CCAO vintages report 2007 and 2015 rather than 2005.",
  "17153000220000", "correct_dwelling_units", 134, NA_real_, NA_real_, "New-construction permits 2739711 and 2750391 each state 134 total units.", "high", "None.",
  "14313080700000", "correct_dwelling_units", 44, NA_real_, NA_real_, "New-construction permits 2758981 and 2781834 each state 44 units.", "high", "None.",
  "17164010220000", "restore_disaggregated_2021_building", 173, 175540, 21366, "The 2021 CCAO row contains this building's area and land; permits at 207 W Harrison state 173 units.", "high", "The CCAO row reports 176 rather than 173 units.",
  "17164010230000", "retain_disaggregated_2021_building", 173, 185094, 21337, "The 2021 CCAO row contains this building's area and land; permits at 221 W Harrison state 173 units.", "high", "The CCAO row reports 176 rather than 173 units.",
  "20151060290000", "correct_dwelling_units", 27, NA_real_, NA_real_, "New-construction permit 1767244 states 27 units; the companion permit 1767243 states 27 units for the adjacent building.", "high", "None.",
  "14202300160000", "retain_single_2024_entity", 79, 79309, 163152, "The 2024 CCAO record groups the two 2021 key PINs into one 79-unit entity, matching the public building count.", "moderate", "The grouped land denominator is not independently verified.",
  "14202300200000", "remove_duplicate_entity", NA_real_, NA_real_, NA_real_, "The two 2021 rows have the same address, units, land, and nearly identical building area; the 2024 file groups them as one entity.", "high", "Paired with the retained 2024 entity above.",
  "17074290130000", "restore_disaggregated_2021_building", 12, 13203, 11235, "The 2021 CCAO file reports separate 12- and 16-unit buildings; the 2024 file rolls both into this 27-unit row while production also retains the 16-unit component.", "moderate", "The source vintages disagree whether this building dates to 2006 or 2008."
)

readr::write_csv(
  review %>%
    left_join(sample, by = "pin", relationship = "many-to-one") %>%
    relocate(pin, address, original_year, original_units, original_building_sqft, original_land_sqft),
  "../output/commercial_verified_case_review.csv"
)
