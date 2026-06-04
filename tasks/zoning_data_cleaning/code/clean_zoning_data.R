# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/zoning_data_cleaning/code")

source("../../setup_environment/code/packages.R")

norm_code <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_squish() %>%
    str_replace_all("\\s+", "") %>%
    str_replace("^([A-Z]+)(\\d+)-\\.(\\d+)$", "\\1-\\2.\\3") %>%
    str_replace("^([A-Z]+\\d?)(\\d)", "\\1-\\2") %>%
    str_replace_all("-{2,}", "-")
}

none_pattern <- regex("^\\s*(none|n/?a|not\\s+applicable)\\.?$", ignore_case = TRUE)
leading_number_pattern <- "^\\s*([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)"
height_feet_pattern <- regex(
  "([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)\\s*(?=ft\\.?\\b|feet\\b|\\u2032|')",
  ignore_case = TRUE
)

zones_sf <- st_read("../input/raw_zoning_data.geojson", quiet = TRUE) %>%
  transmute(zone_code = norm_code(zone_class), geometry) %>%
  st_make_valid() %>%
  st_transform(3435)

regs <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) %>%
  transmute(
    zone_code = norm_code(district_type_code),
    floor_area_ratio_text = as.character(floor_area_ratio),
    lot_area_per_unit_text = as.character(lot_area_per_unit),
    maximum_building_height_text = as.character(maximum_building_height)
  ) %>%
  distinct(zone_code, .keep_all = TRUE) %>%
  mutate(
    floor_area_ratio_text = if_else(
      str_detect(coalesce(floor_area_ratio_text, ""), none_pattern),
      NA_character_,
      floor_area_ratio_text
    ),
    lot_area_per_unit_text = if_else(
      str_detect(coalesce(lot_area_per_unit_text, ""), none_pattern),
      NA_character_,
      lot_area_per_unit_text
    ),
    maximum_building_height_text = if_else(
      str_detect(coalesce(maximum_building_height_text, ""), none_pattern),
      NA_character_,
      maximum_building_height_text
    ),
    floor_area_ratio = as.numeric(str_replace_all(
      str_extract(coalesce(floor_area_ratio_text, ""), leading_number_pattern),
      ",",
      ""
    )),
    lot_area_per_unit = as.numeric(str_replace_all(
      str_extract(coalesce(lot_area_per_unit_text, ""), leading_number_pattern),
      ",",
      ""
    )),
    maximum_building_height = as.numeric(str_replace_all(
      str_extract(coalesce(maximum_building_height_text, ""), height_feet_pattern),
      ",",
      ""
    ))
  )

zones_with_regs <- zones_sf %>%
  left_join(regs, by = "zone_code", relationship = "many-to-one") %>%
  select(zone_code, geometry, floor_area_ratio, lot_area_per_unit, maximum_building_height)

st_write(zones_with_regs, "../output/zoning_data_clean.gpkg", delete_layer = TRUE, quiet = TRUE)
