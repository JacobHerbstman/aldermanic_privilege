# setwd("tasks/new_construction_cleaning/code")

# start_year <- 2006
# end_year <- 2022
# minimum_area_sqft <- 1

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(start_year, end_year, minimum_area_sqft)
if (length(args) != 3L) stop("Expected start year, end year, and minimum initial area.")
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])
minimum_area_sqft <- as.numeric(args[3])
if (anyNA(c(start_year, end_year, minimum_area_sqft)) || start_year > end_year || minimum_area_sqft < 0) {
  stop("Invalid historical-coordinate request window or area threshold.")
}

residential <- read_csv(
  "../output/residential_discovery_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(), year_built = col_integer(),
    land_sqft = col_double(), building_sqft = col_double(), num_apartments = col_double(),
    single_v_multi_family = col_character(), type_of_residence = col_character(),
    proration_key_pin = col_character(), pin_proration_rate = col_double(),
    .default = col_skip()
  )
) %>%
  mutate(
    residential_single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    unitscount = if_else(
      residential_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) %>%
  transmute(
    pin,
    construction_year = as.integer(year_built),
    arealotsf = as.numeric(land_sqft),
    areabuilding = as.numeric(building_sqft),
    unitscount,
    source = "residential_improvements",
    source_class = class,
    project_key = paste0("residential_", coalesce(na_if(proration_key_pin, ""), pin)),
    coordinate_weight = if_else(is.finite(pin_proration_rate) & pin_proration_rate > 0, pin_proration_rate, 1)
  )

commercial <- read_csv(
  "../output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pins = col_character(), yearbuilt = col_integer(),
    landsf = col_double(), bldgsf = col_double(), tot_units = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    construction_year = as.integer(yearbuilt),
    arealotsf = as.numeric(landsf),
    areabuilding = as.numeric(bldgsf),
    unitscount = as.numeric(tot_units),
    source = "commercial_valuation",
    source_class = NA_character_,
    project_key = paste0("commercial_", str_replace_all(coalesce(pins, pin), "[^0-9]", "")),
    coordinate_weight = 1
  )

buildings <- bind_rows(residential, commercial) %>%
  group_by(pin) %>%
  arrange(desc(unitscount), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    construction_year >= start_year,
    construction_year <= end_year,
    arealotsf > minimum_area_sqft,
    areabuilding > minimum_area_sqft,
    unitscount > 0
  )

if (anyNA(buildings$pin) || any(!str_detect(buildings$pin, "^[0-9]{14}$")) || anyDuplicated(buildings$pin) > 0) {
  stop("Eligible construction rows are not unique by original building PIN.", call. = FALSE)
}
if (any(buildings$source_class == "299", na.rm = TRUE)) {
  stop("The original building source unexpectedly contains class-299 rows.", call. = FALSE)
}

current_parcels <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(), subdivision_id = col_character(),
    longitude = col_double(), latitude = col_double(),
    centroid_x_crs_3435 = col_double(), centroid_y_crs_3435 = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    current_parcel_class = class,
    current_subdivision_id = subdivision_id,
    current_longitude = longitude,
    current_latitude = latitude,
    current_coordinates_complete =
      is.finite(longitude) & is.finite(latitude)
  )

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel universe is not unique by PIN.", call. = FALSE)
}

buildings %>%
  left_join(current_parcels, by = "pin", relationship = "many-to-one") %>%
  mutate(
    current_pin_present = pin %in% current_parcels$pin,
    current_coordinates_complete = coalesce(current_coordinates_complete, FALSE),
    multifamily = unitscount > 1
  ) %>%
  arrange(pin) %>%
  SaveData(key = "pin", outfile = "../output/density_historical_building_universe.csv")
