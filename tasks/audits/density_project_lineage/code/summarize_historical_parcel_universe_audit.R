# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

buildings <- read_csv(
  "../output/density_historical_building_universe.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
historical <- read_csv(
  "../output/density_historical_exact_parcel_records.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pin10 = col_character(),
    parcel_class = col_character(), row_id = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(historical_record_found = TRUE)

current_coordinates <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    longitude = col_double(), latitude = col_double(),
    centroid_x_crs_3435 = col_double(), centroid_y_crs_3435 = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    current_longitude = longitude,
    current_latitude = latitude,
    current_x_crs_3435 = centroid_x_crs_3435,
    current_y_crs_3435 = centroid_y_crs_3435
  )

if (anyDuplicated(buildings$pin) > 0) {
  stop("Building universe is not unique by original PIN.", call. = FALSE)
}
if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical Parcel Universe records are not unique by PIN-year.", call. = FALSE)
}
if (anyDuplicated(current_coordinates$pin) > 0) {
  stop("Current Parcel Universe coordinates are not unique by PIN.", call. = FALSE)
}

coverage <- buildings %>%
  left_join(current_coordinates, by = "pin", relationship = "many-to-one") %>%
  left_join(
    historical,
    by = c("pin", "construction_year" = "year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    historical_record_found = coalesce(historical_record_found, FALSE),
    historical_coordinates_complete =
      is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435),
    historical_class_299 = historical_record_found & parcel_class == "299",
    usable_historical_coordinate =
      historical_coordinates_complete & !coalesce(historical_class_299, FALSE),
    usable_current_coordinate =
      is.finite(current_x_crs_3435) & is.finite(current_y_crs_3435),
    historical_current_distance_ft = if_else(
      usable_historical_coordinate & usable_current_coordinate,
      sqrt(
        (centroid_x_crs_3435 - current_x_crs_3435)^2 +
          (centroid_y_crs_3435 - current_y_crs_3435)^2
      ),
      NA_real_
    ),
    selected_coordinate_source = case_when(
      usable_historical_coordinate ~ "exact_construction_year_history",
      usable_current_coordinate ~ "current_2025_fallback",
      TRUE ~ "unresolved"
    ),
    selected_longitude = if_else(
      usable_historical_coordinate,
      longitude,
      current_longitude
    ),
    selected_latitude = if_else(
      usable_historical_coordinate,
      latitude,
      current_latitude
    ),
    selected_x_crs_3435 = if_else(
      usable_historical_coordinate,
      centroid_x_crs_3435,
      current_x_crs_3435
    ),
    selected_y_crs_3435 = if_else(
      usable_historical_coordinate,
      centroid_y_crs_3435,
      current_y_crs_3435
    ),
    coverage_status = case_when(
      !historical_record_found ~ "no_exact_construction_year_record",
      !historical_coordinates_complete ~ "exact_record_missing_coordinates",
      historical_class_299 ~ "excluded_historical_class_299",
      TRUE ~ "usable_exact_historical_coordinate"
    ),
    current_historical_status = case_when(
      current_coordinates_complete & usable_historical_coordinate ~ "current_and_historical",
      current_coordinates_complete & !usable_historical_coordinate ~ "current_only",
      !current_coordinates_complete & usable_historical_coordinate ~ "historical_only_recovery",
      TRUE ~ "neither"
    )
  )

coverage_rows <- list()
for (construction_sample in c("all", "multifamily")) {
  sample_rows <- if (construction_sample == "all") {
    coverage
  } else {
    coverage %>% filter(multifamily)
  }

  coverage_rows[[length(coverage_rows) + 1L]] <- tibble(
    construction_sample,
    eligible_original_buildings = nrow(sample_rows),
    current_2025_coordinates = sum(sample_rows$current_coordinates_complete),
    exact_historical_records = sum(sample_rows$historical_record_found),
    exact_historical_coordinates = sum(sample_rows$historical_coordinates_complete),
    excluded_historical_class_299 = sum(sample_rows$historical_class_299, na.rm = TRUE),
    usable_exact_historical_coordinates = sum(sample_rows$usable_historical_coordinate),
    both_current_and_historical_coordinates = sum(
      sample_rows$usable_current_coordinate & sample_rows$usable_historical_coordinate
    ),
    recovered_beyond_current_2025 = sum(
      !sample_rows$current_coordinates_complete & sample_rows$usable_historical_coordinate
    ),
    current_coordinates_without_usable_exact_history = sum(
      sample_rows$current_coordinates_complete & !sample_rows$usable_historical_coordinate
    ),
    unresolved_without_either_coordinate = sum(
      !sample_rows$current_coordinates_complete & !sample_rows$usable_historical_coordinate
    ),
    selected_historical_primary_current_fallback = sum(
      sample_rows$selected_coordinate_source != "unresolved"
    ),
    selected_current_fallback = sum(
      sample_rows$selected_coordinate_source == "current_2025_fallback"
    ),
    median_historical_current_distance_ft = median(
      sample_rows$historical_current_distance_ft,
      na.rm = TRUE
    ),
    p95_historical_current_distance_ft = quantile(
      sample_rows$historical_current_distance_ft,
      0.95,
      na.rm = TRUE
    ),
    historical_current_distance_over_100ft = sum(
      sample_rows$historical_current_distance_ft > 100,
      na.rm = TRUE
    ),
    historical_current_distance_over_500ft = sum(
      sample_rows$historical_current_distance_ft > 500,
      na.rm = TRUE
    )
  )
}

write_csv(
  bind_rows(coverage_rows),
  "../output/density_historical_parcel_coverage.csv"
)

write_csv(
  coverage %>%
    count(multifamily, current_historical_status, name = "buildings") %>%
    arrange(multifamily, current_historical_status),
  "../output/density_historical_parcel_coverage_by_current_status.csv"
)

write_csv(
  coverage %>%
    group_by(construction_year) %>%
    summarise(
      eligible_buildings = n(),
      eligible_multifamily = sum(multifamily),
      usable_historical_coordinates = sum(usable_historical_coordinate),
      usable_historical_multifamily = sum(usable_historical_coordinate & multifamily),
      recovered_beyond_current = sum(
        usable_historical_coordinate & !current_coordinates_complete
      ),
      recovered_multifamily_beyond_current = sum(
        usable_historical_coordinate & !current_coordinates_complete & multifamily
      ),
      .groups = "drop"
    ),
  "../output/density_historical_parcel_coverage_by_year.csv"
)

write_csv(
  coverage %>%
    filter(usable_historical_coordinate) %>%
    select(
      pin,
      construction_year,
      arealotsf,
      areabuilding,
      unitscount,
      source,
      source_class,
      parcel_class,
      longitude,
      latitude,
      centroid_x_crs_3435,
      centroid_y_crs_3435,
      subdivision_id,
      current_coordinates_complete,
      current_historical_status
    ) %>%
    arrange(pin),
  "../output/density_historical_geocoded_buildings.csv"
)

write_csv(
  coverage %>%
    filter(selected_coordinate_source != "unresolved") %>%
    select(
      pin,
      construction_year,
      arealotsf,
      areabuilding,
      unitscount,
      source,
      source_class,
      selected_coordinate_source,
      selected_longitude,
      selected_latitude,
      selected_x_crs_3435,
      selected_y_crs_3435,
      parcel_class,
      current_parcel_class,
      historical_current_distance_ft
    ) %>%
    arrange(pin),
  "../output/density_historical_selected_geocoded_buildings.csv"
)

write_csv(
  coverage %>%
    filter(!usable_historical_coordinate) %>%
    select(
      pin,
      construction_year,
      unitscount,
      source,
      source_class,
      current_coordinates_complete,
      coverage_status,
      parcel_class
    ) %>%
    arrange(coverage_status, pin),
  "../output/density_historical_parcel_unmatched.csv"
)
