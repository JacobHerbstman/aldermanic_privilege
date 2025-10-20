#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load ----
land_values   <- sfarrow::st_read_parquet("../input/land_values_aug.parquet", show_col_types = FALSE) 
ward_panel <- st_read("../input/ward_panel.gpkg")


land_values_aug <- land_values %>%
  filter(tax_year >= 2003) %>% 
  mutate(
    ward_map_year = case_when(
      # tax_year < 2003 ~ 1998,
      tax_year >= 2003 & tax_year < 2015 ~ 2003,
      tax_year >= 2015 ~ 2015
    )) 

EVENT_YEARS <- c(2015)

MAP_YEAR_LOOKUP <- list(
  `2015` = list(before = 2003, after = 2015)
)

# 1. Get unique parcel geometries.
pins_latest_geom <- land_values_aug %>%
  distinct(pin10, .keep_all = TRUE) %>%
  select(pin10, geometry) %>%
  filter(!st_is_empty(geometry))

# 2. Create a crosswalk of ward assignments for the relevant map years.
map_years_to_check <- unique(unlist(MAP_YEAR_LOOKUP))
ward_crosswalk <- pins_latest_geom

for(map_year in map_years_to_check) {
  message(paste("...spatially joining pins to", map_year, "ward map"))
  ward_polygons_year <- ward_panel %>% filter(year == map_year)
  joined <- st_join(pins_latest_geom, ward_polygons_year, join = st_within) %>%
    st_drop_geometry() %>%
    select(pin10, ward)
  names(joined)[names(joined) == "ward"] <- paste0("ward_", map_year)
  ward_crosswalk <- ward_crosswalk %>%
    left_join(distinct(joined, pin10, .keep_all = TRUE), by = "pin10")
}
ward_crosswalk <- ward_crosswalk %>% st_drop_geometry()


# 3. Define treatment status and event-time variables for ALL parcels.
event_study_df <- land_values_aug %>%
  # Join the crosswalk to get pre/post wards for defining treatment.
  left_join(ward_crosswalk %>% select(pin10, ward_2003, ward_2015), by = "pin10") %>%
  # Redefine is_treated based on actual ward change.
  mutate(
    is_treated = if_else(
      !is.na(ward_2003) & !is.na(ward_2015) & ward_2003 != ward_2015, 
      1, 
      0
    )
  ) %>%
  # Assign an event year to ALL parcels for time_to_event calculation.
  mutate(event_year = EVENT_YEARS) %>%
  # Now, calculate time_to_event for ALL parcels.
  mutate(
    land_share_pin10 = land_sum / (land_sum + bldg_sum + 1),
    time_to_event = tax_year - event_year, # No longer NA for controls
    block_id = substr(pin10, 1, 7)
  ) %>%
  # Filter to analysis window and remove missing values.
  filter(dist_to_boundary_ft <= 1056) %>%
  filter(!is.na(ward_pair)) %>%
  filter(tax_year >= (EVENT_YEARS - 5) & tax_year <= (EVENT_YEARS + 5)) %>% 
  st_drop_geometry()

# Save the prepared dataset for event study analysis.
write_csv(event_study_df, "../output/land_event_study_data.csv")
