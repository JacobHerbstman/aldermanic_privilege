# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

residential <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    year_built = readr::col_double(),
    land_sqft = readr::col_double(),
    building_sqft = readr::col_double(),
    num_apartments = readr::col_double(),
    single_v_multi_family = readr::col_character(),
    type_of_residence = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  transmute(
    pin,
    source_yearbuilt = as.integer(year_built),
    source_arealotsf = as.numeric(land_sqft),
    source_areabuilding = as.numeric(building_sqft),
    source_unitscount = as.numeric(num_apartments),
    is_single_family =
      (!is.na(single_v_multi_family) & str_detect(str_to_lower(single_v_multi_family), "^single")) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    source_order = 1L,
    selected_source = "residential"
  ) %>%
  mutate(
    source_unitscount = if_else(
      is_single_family & (is.na(source_unitscount) | source_unitscount == 0),
      1,
      source_unitscount
    )
  ) %>%
  select(-is_single_family)

commercial <- readr::read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    yearbuilt = readr::col_double(),
    landsf = readr::col_double(),
    bldgsf = readr::col_double(),
    tot_units = readr::col_double(),
    .default = readr::col_skip()
  )
) %>%
  transmute(
    pin,
    source_yearbuilt = as.integer(yearbuilt),
    source_arealotsf = as.numeric(landsf),
    source_areabuilding = as.numeric(bldgsf),
    source_unitscount = as.numeric(tot_units),
    source_order = 2L,
    selected_source = "commercial"
  )

residential_pins <- residential %>% distinct(pin) %>% mutate(in_residential_source = TRUE)
commercial_pins <- commercial %>% distinct(pin) %>% mutate(in_commercial_source = TRUE)

selected_source_rows <- bind_rows(residential, commercial) %>%
  arrange(pin, desc(source_unitscount), source_order) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup()

source_universe <- full_join(
  residential_pins,
  commercial_pins,
  by = "pin",
  relationship = "one-to-one"
) %>%
  mutate(
    in_residential_source = replace_na(in_residential_source, FALSE),
    in_commercial_source = replace_na(in_commercial_source, FALSE),
    source_membership = case_when(
      in_residential_source & in_commercial_source ~ "both_sources",
      in_commercial_source ~ "commercial_only",
      TRUE ~ "residential_only"
  )
  ) %>%
  left_join(selected_source_rows, by = "pin", relationship = "one-to-one")

if (anyDuplicated(source_universe$pin) > 0) {
  stop("The union of cleaned residential and commercial PINs is not unique.", call. = FALSE)
}

geocoded <- sf::st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  select(
    pin,
    yearbuilt,
    arealotsf,
    areabuilding,
    unitscount,
    coordinate_source
  )

if (anyDuplicated(geocoded$pin) > 0) {
  stop("The geocoded construction file is not unique by PIN.", call. = FALSE)
}

analysis <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
)

if (anyDuplicated(analysis$pin) > 0) {
  stop("The scored construction file is not unique by PIN.", call. = FALSE)
}

sample <- source_universe %>%
  left_join(geocoded, by = "pin", relationship = "one-to-one") %>%
  left_join(
    analysis %>%
      select(
        pin,
        construction_year,
        dist_to_boundary_m,
        ward_pair,
        segment_id,
        signed_distance_m,
        construction_zone_group,
        strictness_own,
        strictness_neighbor,
        share_white_own,
        share_black_own,
        median_hh_income_own,
        share_bach_plus_own,
        homeownership_rate_own
      ),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(
    has_coordinates = !is.na(coordinate_source),
    source_year_in_study_period = coalesce(between(source_yearbuilt, 2006, 2022), FALSE),
    production_year_in_study_period = has_coordinates &
      coalesce(between(construction_year, 2006, 2022), FALSE),
    positive_density_fields =
      production_year_in_study_period &
      is.finite(arealotsf) & arealotsf > 1 &
      is.finite(areabuilding) & areabuilding > 1 &
      is.finite(unitscount) & unitscount > 0,
    within_1500ft = positive_density_fields & dist_to_boundary_m <= 457.2,
    within_500ft = positive_density_fields & dist_to_boundary_m <= 152.4,
    complete_main_model =
      within_500ft &
      !is.na(ward_pair) &
      is.finite(signed_distance_m) &
      !is.na(construction_zone_group) &
      !is.na(segment_id) &
      segment_id != "" &
      is.finite(strictness_own) &
      is.finite(strictness_neighbor) &
      if_all(
        c(
          share_white_own, share_black_own, median_hh_income_own,
          share_bach_plus_own, homeownership_rate_own
        ),
        is.finite
      )
  )

stages <- c(
  "cleaned_source_union",
  "with_coordinates",
  "production_years_2006_2022",
  "positive_density_fields",
  "within_1500ft",
  "within_500ft",
  "complete_main_model"
)

stage_flags <- list(
  rep(TRUE, nrow(sample)),
  sample$has_coordinates,
  sample$production_year_in_study_period,
  sample$positive_density_fields,
  sample$within_1500ft,
  sample$within_500ft,
  sample$complete_main_model
)

attrition <- purrr::map2_dfr(stages, stage_flags, function(stage, keep) {
  bind_rows(
    tibble::tibble(stage, source_membership = "all", observations = sum(keep, na.rm = TRUE)),
    sample %>%
      filter(keep %in% TRUE) %>%
      count(source_membership, name = "observations") %>%
      mutate(stage = stage) %>%
      select(stage, source_membership, observations)
  )
})

readr::write_csv(attrition, "../output/new_construction_sample_attrition.csv")
readr::write_csv(
  tibble::tribble(
    ~metric, ~observations,
    "source_year_in_study_period", sum(sample$source_year_in_study_period, na.rm = TRUE),
    "production_year_in_study_period", sum(sample$production_year_in_study_period, na.rm = TRUE),
    "in_both_year_definitions", sum(
      sample$source_year_in_study_period & sample$production_year_in_study_period,
      na.rm = TRUE
    ),
    "source_year_only", sum(
      sample$source_year_in_study_period & !sample$production_year_in_study_period,
      na.rm = TRUE
    ),
    "source_year_in_period_without_coordinates", sum(
      sample$source_year_in_study_period & !sample$has_coordinates,
      na.rm = TRUE
    ),
    "source_year_in_period_with_coordinates_but_no_production_year", sum(
      sample$source_year_in_study_period & sample$has_coordinates & is.na(sample$construction_year),
      na.rm = TRUE
    ),
    "source_year_in_period_but_production_year_outside_period", sum(
      sample$source_year_in_study_period & sample$has_coordinates &
        !is.na(sample$construction_year) & !between(sample$construction_year, 2006, 2022),
      na.rm = TRUE
    ),
    "production_year_only", sum(
      !sample$source_year_in_study_period & sample$production_year_in_study_period,
      na.rm = TRUE
    )
  ),
  "../output/new_construction_sample_year_reconciliation.csv"
)
readr::write_csv(
  sample %>%
    filter(!has_coordinates) %>%
    select(pin, source_membership, selected_source, source_yearbuilt),
  "../output/new_construction_ungeocoded_pins.csv"
)
