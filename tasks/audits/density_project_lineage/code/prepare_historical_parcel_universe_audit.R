# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

residential <- read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), class = col_character(), .default = col_guess())
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
    source_class = class
  )

commercial <- read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    construction_year = as.integer(yearbuilt),
    arealotsf = as.numeric(landsf),
    areabuilding = as.numeric(bldgsf),
    unitscount = as.numeric(tot_units),
    source = "commercial_valuation",
    source_class = NA_character_
  )

buildings <- bind_rows(residential, commercial) %>%
  group_by(pin) %>%
  arrange(desc(unitscount), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    construction_year >= 2006,
    construction_year <= 2022,
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0
  )

if (anyDuplicated(buildings$pin) > 0) {
  stop("Eligible construction rows are not unique by original building PIN.", call. = FALSE)
}
if (any(buildings$source_class == "299", na.rm = TRUE)) {
  stop("The original building source unexpectedly contains class-299 rows.", call. = FALSE)
}

current_parcels <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(),
    longitude = col_double(), latitude = col_double(),
    centroid_x_crs_3435 = col_double(), centroid_y_crs_3435 = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    current_parcel_class = class,
    current_coordinates_complete =
      is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
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
  write_csv("../output/density_historical_building_universe.csv")
