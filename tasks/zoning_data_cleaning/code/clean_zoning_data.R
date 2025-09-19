# This script creates a shapefile of zoning codes around the city and their regulations such as FAR and LAPU

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

`%||%` <- function(a,b) if (is.null(a)) b else a

# --- helpers (compact) ---
norm_code <- function(x){
  x %>% toupper() %>% str_replace_all("[\u2010-\u2015]","-") %>% str_squish() %>%
    str_replace_all("\\s+","") %>%                                  # RM 4.5 -> RM4.5
    str_replace("^([A-Z]+)(\\d+)-\\.(\\d+)$","\\1-\\2.\\3") %>%      # RM4-.5 -> RM-4.5
    str_replace("^([A-Z]+\\d?)(\\d)","\\1-\\2") %>%                  # RM4.5 -> RM-4.5 ; DX10 -> DX-10
    str_replace_all("-{2,}","-")
}
na_none   <- function(x) ifelse(str_detect(x %||% "", "(?i)^\\s*(none|n/?a|not\\s+applicable)\\.?$"), NA_character_, x)
num_start <- function(x){ raw <- str_extract(x %||% "", "^\\s*([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)"); as.numeric(str_replace_all(raw, ",", "")) }
num_ft    <- function(x){ raw <- str_extract(x %||% "", regex("([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)\\s*(?=ft\\.?\\b|feet\\b|\\u2032|')", TRUE)); as.numeric(str_replace_all(raw, ",","")) }

# --- read + normalize/drop in one go ---
zones_sf <- st_read("../input/raw_zoning_data.geojson") %>%
  transmute(zone_code = norm_code(zone_class), geometry) %>%
  filter(!str_detect(zone_code, "^(?:PD|PMD|POS)"))                   # catch PMD13, PD 123, etc.

regs <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE) %>%
  transmute(
    zone_code                    = norm_code(district_type_code),
    floor_area_ratio_raw         = floor_area_ratio,
    lot_area_per_unit_raw        = lot_area_per_unit,
    minimum_lot_area,                                   # keep as-is (often text)
    maximum_building_height_raw  = maximum_building_height
  ) %>%
  filter(!str_detect(zone_code, "^(?:PD|PMD|POS)")) %>%
  distinct(zone_code, .keep_all = TRUE) %>%
  mutate(
    floor_area_ratio             = na_none(floor_area_ratio_raw),
    lot_area_per_unit            = na_none(lot_area_per_unit_raw),
    maximum_building_height      = na_none(maximum_building_height_raw),
    floor_area_ratio_num         = num_start(floor_area_ratio),
    lot_area_per_unit_sqft       = num_start(lot_area_per_unit),
    max_building_height_ft       = num_ft(maximum_building_height)
  )

# --- join + quick QA ---
zones_with_regs <- zones_sf %>% left_join(regs, by = "zone_code")
unmatched <- anti_join(zones_sf, regs, by = "zone_code")
message("Unmatched zone codes: ", nrow(unmatched))

## keep just columns I want 
zones_with_regs <- zones_with_regs %>%
  select(zone_code, geometry, 
         floor_area_ratio = floor_area_ratio_num, 
         lot_area_per_unit = lot_area_per_unit_sqft, 
         maximum_building_height = max_building_height_ft)

st_write(zones_with_regs, "../output/zoning_data_clean.gpkg", delete_layer = TRUE)
