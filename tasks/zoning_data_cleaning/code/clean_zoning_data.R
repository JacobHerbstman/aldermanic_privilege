# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/zoning_data_cleaning/code")

source("../../setup_environment/code/packages.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

norm_code <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_squish() %>%
    str_replace_all("\\s+", "") %>%
    str_replace("^([BCM]\\d)-\\.(\\d+)$", "\\1-\\2") %>%
    str_replace("^([BCM]\\d)\\.(\\d+)$", "\\1-\\2") %>%
    str_replace("^([A-Z]{2,})(\\d+(?:\\.\\d+)?)$", "\\1-\\2") %>%
    str_replace("^([BCM])-([1-7])-(\\d+(?:\\.\\d+)?)$", "\\1\\2-\\3") %>%
    str_replace("^R-(\\d)$", "R\\1") %>%
    str_replace("^C-4$", "C4") %>%
    str_replace_all("-{2,}", "-")
}

norm_old_code <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("[\u2010-\u2015]", "-") %>%
    str_squish() %>%
    str_replace_all("\\s+", "") %>%
    str_replace("^([BCM]\\d)\\.(\\d)$", "\\1-\\2") %>%
    str_replace("^([BCM]\\d)-\\.(\\d)$", "\\1-\\2") %>%
    str_replace("^([BCM]\\d)(\\d)$", "\\1-\\2") %>%
    str_replace("^R-(\\d)$", "R\\1") %>%
    str_replace("^C-4$", "C4") %>%
    str_replace_all("-{2,}", "-")
}

na_none <- function(x) {
  ifelse(str_detect(x %||% "", "(?i)^\\s*(none|n/?a|not\\s+applicable)\\.?$"), NA_character_, x)
}

num_start <- function(x) {
  raw <- str_extract(x %||% "", "^\\s*([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)")
  as.numeric(str_replace_all(raw, ",", ""))
}

num_ft <- function(x) {
  raw <- str_extract(x %||% "", regex("([0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?)\\s*(?=ft\\.?\\b|feet\\b|\\u2032|')", TRUE))
  as.numeric(str_replace_all(raw, ",", ""))
}

pick_col <- function(df, candidates, contains = NULL) {
  nms <- names(df)
  lower <- tolower(nms)
  for (cand in candidates) {
    idx <- which(lower == tolower(cand))
    if (length(idx) > 0) {
      return(nms[idx[[1]]])
    }
  }
  if (!is.null(contains)) {
    for (nm in nms) {
      low <- tolower(nm)
      if (all(vapply(contains, function(x) str_detect(low, x), logical(1)))) {
        return(nm)
      }
    }
  }
  return(NULL)
}

zones_raw <- st_read("../input/raw_zoning_data.geojson", quiet = TRUE)
zone_col <- pick_col(zones_raw, c("zone_class", "zoneclass", "zoning", "district", "zone"), contains = c("zone"))
if (is.null(zone_col)) {
  stop("Could not identify zoning code column in zoning geojson")
}

zones_sf <- zones_raw %>%
  transmute(zone_code = norm_code(as.character(.data[[zone_col]])), geometry) %>%
  filter(!is.na(zone_code))

regs_raw <- read_csv("../input/zoning-code-summary-district-types.csv", show_col_types = FALSE)
code_col <- pick_col(regs_raw, c("district_type_code", "district", "district_type", "zoning_code", "code"), contains = c("district"))
far_col <- pick_col(regs_raw, c("floor_area_ratio", "max_far", "maximum_far", "far"), contains = c("far"))
lapu_col <- pick_col(regs_raw, c("lot_area_per_unit"), contains = c("lot", "unit"))
height_col <- pick_col(regs_raw, c("maximum_building_height"), contains = c("height"))

if (is.null(code_col) || is.null(far_col)) {
  stop("Could not identify zoning code/FAR columns in lookup csv")
}

regs <- regs_raw %>%
  transmute(
    zone_code = norm_code(as.character(.data[[code_col]])),
    floor_area_ratio_raw = as.character(.data[[far_col]]),
    lot_area_per_unit_raw = if (!is.null(lapu_col)) as.character(.data[[lapu_col]]) else NA_character_,
    maximum_building_height_raw = if (!is.null(height_col)) as.character(.data[[height_col]]) else NA_character_
  ) %>%
  filter(!is.na(zone_code)) %>%
  distinct(zone_code, .keep_all = TRUE) %>%
  mutate(
    floor_area_ratio = na_none(floor_area_ratio_raw),
    lot_area_per_unit = na_none(lot_area_per_unit_raw),
    maximum_building_height = na_none(maximum_building_height_raw),
    floor_area_ratio_num = num_start(floor_area_ratio),
    lot_area_per_unit_sqft = num_start(lot_area_per_unit),
    max_building_height_ft = num_ft(maximum_building_height)
  )

manual_far <- tribble(
  ~zone_code, ~floor_area_ratio_num,
  "BL-1", 1.2,
  "BL-1.5", 1.5,
  "BL-2", 2.2,
  "BL-3", 3.0,
  "BL-5", 5.0,
  "CI-1", 1.2,
  "CI-1.5", 1.5,
  "CI-2", 2.2,
  "CI-3", 3.0,
  "CI-5", 5.0,
  "CL-1", 1.2,
  "CL-1.5", 1.5,
  "CL-2", 2.2,
  "CL-3", 3.0,
  "CL-5", 5.0,
  "ML-1", 1.2,
  "ML-1.5", 1.5,
  "ML-2", 2.2,
  "ML-3", 3.0,
  "ML-5", 5.0,
  "RT-4A", 1.2,
  "RM-4.5", 1.7,
  "T", 1.5
)

modern_lookup <- regs %>%
  transmute(
    zone_code,
    floor_area_ratio = floor_area_ratio_num,
    effective_start_date = "2004-11-01",
    effective_end_date = NA_character_,
    zoning_code_version = "post_2004"
  ) %>%
  filter(!is.na(zone_code), !is.na(floor_area_ratio)) %>%
  bind_rows(
    manual_far %>%
      rename(floor_area_ratio = floor_area_ratio_num) %>%
      mutate(
        effective_start_date = "2004-11-01",
        effective_end_date = NA_character_,
        zoning_code_version = "post_2004"
      )
  ) %>%
  group_by(zone_code, effective_start_date, effective_end_date, zoning_code_version) %>%
  summarise(floor_area_ratio = first(floor_area_ratio), .groups = "drop")

old_raw <- read_csv("../input/old_zoning_bulk_density_1957_2004.csv", show_col_types = FALSE)
old_code_col <- pick_col(old_raw, c("district_code", "zone_code", "code"), contains = c("district"))
old_far_col <- pick_col(old_raw, c("basic_far", "floor_area_ratio", "max_far", "far"), contains = c("far"))
if (is.null(old_code_col) || is.null(old_far_col)) {
  stop("Could not identify zoning code/FAR columns in the historical lookup.", call. = FALSE)
}

old_lookup <- old_raw %>%
  transmute(
    zone_code = norm_old_code(as.character(.data[[old_code_col]])),
    floor_area_ratio = as.numeric(.data[[old_far_col]]),
    effective_start_date = "1957-05-29",
    effective_end_date = "2004-10-31",
    zoning_code_version = "pre_2004"
  ) %>%
  filter(!is.na(zone_code), !is.na(floor_area_ratio))

far_lookup <- bind_rows(old_lookup, modern_lookup) %>%
  distinct(zone_code, effective_start_date, effective_end_date, zoning_code_version, .keep_all = TRUE) %>%
  arrange(zone_code, effective_start_date, effective_end_date)

zones_with_regs <- zones_sf %>%
  left_join(regs, by = "zone_code") %>%
  select(
    zone_code,
    geometry,
    floor_area_ratio = floor_area_ratio_num,
    lot_area_per_unit = lot_area_per_unit_sqft,
    maximum_building_height = max_building_height_ft
  )

st_write(zones_with_regs, "../output/zoning_data_clean.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(far_lookup, "../output/zoning_far_lookup_clean.csv")

cat(sprintf("Wrote %d FAR lookup rows\n", nrow(far_lookup)))
