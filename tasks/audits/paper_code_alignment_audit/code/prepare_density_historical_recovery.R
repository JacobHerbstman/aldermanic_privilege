# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

residential <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  mutate(
    residential_single_family =
      (!is.na(single_v_multi_family) & str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    unitscount = if_else(
      residential_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    ),
    storiescount = case_when(
      is.na(type_of_residence) ~ NA_real_,
      str_detect(type_of_residence, regex("^1\\s*Story$", ignore_case = TRUE)) ~ 1,
      str_detect(type_of_residence, regex("^1\\.5\\s*Story$", ignore_case = TRUE)) ~ 1.5,
      str_detect(type_of_residence, regex("^2\\s*Story$", ignore_case = TRUE)) ~ 2,
      str_detect(type_of_residence, regex("^3\\s*Story\\s*\\+$", ignore_case = TRUE)) ~ 3,
      str_detect(type_of_residence, regex("Split\\s*Level", ignore_case = TRUE)) ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  transmute(
    pin,
    yearbuilt = as.integer(year_built),
    arealotsf = as.numeric(land_sqft),
    areabuilding = as.numeric(building_sqft),
    unitscount,
    storiescount,
    bedroomscount = as.numeric(num_bedrooms),
    residential = TRUE,
    source = "residential_improvements",
    source_class = as.character(class),
    source_card = as.integer(card_num),
    proration_key_pin = as.character(proration_key_pin),
    pin_proration_rate = as.numeric(pin_proration_rate),
    card_proration_rate = as.numeric(card_proration_rate),
    pin_is_multicard = as.logical(pin_is_multicard),
    pin_num_cards = as.integer(pin_num_cards),
    pin_is_multiland = as.logical(pin_is_multiland),
    pin_num_landlines = as.integer(pin_num_landlines)
  )

commercial <- read_csv(
  "../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    yearbuilt = as.integer(yearbuilt),
    arealotsf = as.numeric(landsf),
    areabuilding = as.numeric(bldgsf),
    unitscount = as.numeric(tot_units),
    storiescount = NA_real_,
    bedroomscount =
      1 * coalesce(as.numeric(x1brunits), 0) +
      2 * coalesce(as.numeric(x2brunits), 0) +
      3 * coalesce(as.numeric(x3brunits), 0) +
      4 * coalesce(as.numeric(x4brunits), 0),
    residential = TRUE,
    source = "commercial_valuation",
    source_class = NA_character_,
    source_card = NA_integer_,
    proration_key_pin = NA_character_,
    pin_proration_rate = NA_real_,
    card_proration_rate = NA_real_,
    pin_is_multicard = NA,
    pin_num_cards = NA_integer_,
    pin_is_multiland = NA,
    pin_num_landlines = NA_integer_
  ) %>%
  mutate(
    bedroomscount = if_else(bedroomscount == 0 & unitscount > 0, NA_real_, bedroomscount)
  )

eligible <- bind_rows(residential, commercial) %>%
  group_by(pin) %>%
  arrange(desc(unitscount), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    yearbuilt >= 2006,
    yearbuilt <= 2022,
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0
  )

current_parcels <- read_csv(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    current_latitude = as.numeric(latitude),
    current_longitude = as.numeric(longitude)
  )

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel universe is not unique by full PIN.", call. = FALSE)
}

recovery_cases <- eligible %>%
  left_join(current_parcels, by = "pin", relationship = "many-to-one") %>%
  mutate(
    current_pin_present = !is.na(current_latitude) | !is.na(current_longitude),
    current_coordinates_complete = is.finite(current_latitude) & is.finite(current_longitude),
    current_coordinate_status = case_when(
      current_coordinates_complete ~ "complete_current_coordinate",
      pin %in% current_parcels$pin ~ "current_pin_missing_coordinate",
      TRUE ~ "pin_absent_from_current_universe"
    )
  ) %>%
  filter(!current_coordinates_complete) %>%
  select(-current_latitude, -current_longitude)

write_csv(recovery_cases, "../output/density_historical_coordinate_cases.csv")
write_csv(
  bind_rows(
    eligible %>%
      summarise(
        stage = "Eligible unique construction PINs before current geocoding",
        n = n(),
        multifamily = sum(unitscount > 1)
      ),
    recovery_cases %>%
      summarise(
        stage = "Missing complete coordinates in 2025 parcel universe",
        n = n(),
        multifamily = sum(unitscount > 1)
      )
  ),
  "../output/density_geocoding_attrition.csv"
)
