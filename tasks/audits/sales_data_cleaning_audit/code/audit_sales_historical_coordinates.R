# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_data_cleaning_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")
source("../../../_lib/amenity_distance_helpers.R")

suppressMessages(sf_use_s2(FALSE))

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    audit_id = row_number(),
    pin = gsub("[^0-9]", "", trimws(as.character(pin))),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    sale_date = as.Date(sale_date),
    current_signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    current_right = as.integer(current_signed_dist_ft >= 0)
  ) %>%
  filter(year >= 2006, year <= 2022)

if (any(nchar(sales$pin) != 14L)) {
  stop("Sales audit input contains an invalid full PIN.", call. = FALSE)
}
if (anyDuplicated(sales$audit_id) > 0) {
  stop("Sales audit identifiers must be unique.", call. = FALSE)
}

historical_coordinates <- read_csv(
  "../output/sales_model_historical_coordinates.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
) %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin),
    year = as.integer(year)
  )
if (anyDuplicated(historical_coordinates[c("pin", "year")]) > 0) {
  stop("Historical coordinates must be unique by full PIN and year.", call. = FALSE)
}

sales <- sales %>%
  left_join(historical_coordinates, by = c("pin", "year"), relationship = "many-to-one") %>%
  mutate(
    has_historical_coordinates = is.finite(historical_longitude) & is.finite(historical_latitude),
    historical_longitude_use = if_else(has_historical_coordinates, historical_longitude, longitude),
    historical_latitude_use = if_else(has_historical_coordinates, historical_latitude, latitude),
    coordinate_source = if_else(
      has_historical_coordinates,
      "historical_exact_pin_year",
      "current_coordinate_fallback"
    ),
    era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE)
  )

current_points <- st_as_sf(
  sales,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)
historical_points <- st_as_sf(
  sales,
  coords = c("historical_longitude_use", "historical_latitude_use"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

sales$coordinate_shift_ft <- as.numeric(st_distance(
  current_points,
  historical_points,
  by_element = TRUE
))

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

historical_assignments <- assign_points_to_boundaries(
  historical_points,
  sales$era,
  ward_maps,
  boundary_lines,
  chunk_n = 5000L
) %>%
  rename(
    historical_ward = ward,
    historical_neighbor_ward = neighbor_ward,
    historical_ward_pair_id = ward_pair_id,
    historical_dist_m = dist_m,
    historical_dist_ft = dist_ft
  )

sales <- bind_cols(sales, historical_assignments) %>%
  mutate(month_join = as.yearmon(sale_date))

alderman_lookup <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month)) %>%
  select(ward, month, alderman) %>%
  distinct()
if (anyDuplicated(alderman_lookup[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

scores <- read_csv(
  "../input/aldermen_uncertainty_scores_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score input must be unique by alderman.", call. = FALSE)
}

sales <- sales %>%
  left_join(
    alderman_lookup %>% rename(historical_ward = ward, historical_alderman_own = alderman),
    by = c("historical_ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    alderman_lookup %>% rename(
      historical_neighbor_ward = ward,
      historical_alderman_neighbor = alderman
    ),
    by = c("historical_neighbor_ward", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(historical_alderman_own = alderman, historical_strictness_own = score),
    by = "historical_alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(
      historical_alderman_neighbor = alderman,
      historical_strictness_neighbor = score
    ),
    by = "historical_alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    historical_sign = case_when(
      historical_strictness_own > historical_strictness_neighbor ~ 1,
      historical_strictness_own < historical_strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    historical_signed_dist_m = historical_dist_m * historical_sign,
    historical_signed_dist_ft = historical_dist_ft * historical_sign,
    historical_right = as.integer(historical_signed_dist_ft >= 0)
  )

segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")
historical_points_4326 <- st_transform(historical_points, 4326)
sales$historical_segment_id <- assign_points_to_nearest_segments(
  historical_points_4326,
  sales$era,
  sales$historical_ward_pair_id,
  segment_layers,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 50000L
)

historical_amenities <- build_unique_coordinate_amenity_table(
  sales,
  "historical_longitude_use",
  "historical_latitude_use",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  100000L
) %>%
  rename(
    historical_longitude_use = longitude,
    historical_latitude_use = latitude,
    historical_nearest_school_dist_ft = nearest_school_dist_ft,
    historical_nearest_park_dist_ft = nearest_park_dist_ft,
    historical_nearest_major_road_dist_ft = nearest_major_road_dist_ft,
    historical_lake_michigan_dist_ft = lake_michigan_dist_ft
  )

sales <- sales %>%
  left_join(
    historical_amenities,
    by = c("historical_longitude_use", "historical_latitude_use"),
    relationship = "many-to-one"
  )

cta_stops <- read_amenity_layer("../input/cta_stops.gpkg") %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )

coordinate_months <- sales %>%
  distinct(historical_longitude_use, historical_latitude_use, year_month) %>%
  mutate(
    month_start = as.Date(paste0(year_month, "-01")),
    month_end = lubridate::ceiling_date(month_start, "month") - lubridate::days(1)
  )
coordinate_month_points <- st_as_sf(
  coordinate_months,
  coords = c("historical_longitude_use", "historical_latitude_use"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435)

cta_distance_rows <- list()
coordinates_by_month <- split(coordinate_month_points, coordinate_month_points$year_month)
for (month_i in names(coordinates_by_month)) {
  month_points <- coordinates_by_month[[month_i]]
  month_start <- unique(month_points$month_start)
  month_end <- unique(month_points$month_end)
  active_cta <- cta_stops %>%
    filter(
      active_from_date <= month_end,
      is.na(active_to_date) | active_to_date >= month_start
    )
  nearest_idx <- st_nearest_feature(month_points, active_cta)
  cta_distance_rows[[length(cta_distance_rows) + 1L]] <- st_drop_geometry(month_points) %>%
    transmute(
      historical_longitude_use,
      historical_latitude_use,
      year_month,
      historical_nearest_cta_stop_dist_ft = as.numeric(st_distance(
        month_points,
        active_cta[nearest_idx, ],
        by_element = TRUE
      ))
    )
}
historical_cta_distances <- bind_rows(cta_distance_rows)
if (anyDuplicated(historical_cta_distances[
  c("historical_longitude_use", "historical_latitude_use", "year_month")
]) > 0) {
  stop("Historical CTA distances must be unique by coordinate-month.", call. = FALSE)
}

sales <- sales %>%
  left_join(
    historical_cta_distances,
    by = c("historical_longitude_use", "historical_latitude_use", "year_month"),
    relationship = "many-to-one"
  )

hedonic_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age",
  "log_bedrooms", "log_baths", "has_garage"
)
current_amenity_controls <- c(
  "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
historical_amenity_controls <- paste0("historical_", current_amenity_controls)

current_model_data <- sales %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(ward_pair_id),
    !is.na(segment_id), segment_id != "",
    is.finite(current_signed_dist_ft),
    abs(current_signed_dist_ft) <= 500,
    !is.na(strictness_own), !is.na(strictness_neighbor),
    if_all(all_of(c(hedonic_controls, current_amenity_controls)), ~ is.finite(.x))
  )

historical_model_data <- sales %>%
  filter(
    !is.na(sale_price), sale_price > 0,
    !is.na(historical_ward_pair_id),
    !is.na(historical_segment_id), historical_segment_id != "",
    is.finite(historical_signed_dist_ft),
    abs(historical_signed_dist_ft) <= 500,
    !is.na(historical_strictness_own), !is.na(historical_strictness_neighbor),
    if_all(all_of(c(hedonic_controls, historical_amenity_controls)), ~ is.finite(.x))
  )

current_model <- feols(
  log(sale_price) ~ current_right + log_sqft + log_land_sqft +
    log_building_age + log_bedrooms + log_baths + has_garage +
    nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft +
    nearest_cta_stop_dist_ft + lake_michigan_dist_ft |
    segment_id^year_quarter,
  data = current_model_data,
  cluster = ~segment_id
)

historical_model <- feols(
  log(sale_price) ~ historical_right + log_sqft + log_land_sqft +
    log_building_age + log_bedrooms + log_baths + has_garage +
    historical_nearest_school_dist_ft + historical_nearest_park_dist_ft +
    historical_nearest_major_road_dist_ft + historical_nearest_cta_stop_dist_ft +
    historical_lake_michigan_dist_ft |
    historical_segment_id^year_quarter,
  data = historical_model_data,
  cluster = ~historical_segment_id
)

current_row <- coeftable(current_model)["current_right", , drop = FALSE]
historical_row <- coeftable(historical_model)["historical_right", , drop = FALSE]

current_ids <- unique(current_model_data$audit_id)
historical_ids <- unique(historical_model_data$audit_id)

report <- bind_rows(
  tibble(
    record_type = "model",
    item = c("Current-primary coordinates", "Historical-primary coordinates"),
    estimate = c(current_row[1, "Estimate"], historical_row[1, "Estimate"]),
    std_error = c(current_row[1, "Std. Error"], historical_row[1, "Std. Error"]),
    p_value = c(current_row[1, "Pr(>|t|)"], historical_row[1, "Pr(>|t|)"]),
    n = c(nobs(current_model), nobs(historical_model)),
    value = NA_real_,
    details = NA_character_
  ),
  tribble(
    ~record_type, ~item, ~estimate, ~std_error, ~p_value, ~n, ~value, ~details,
    "metric", "Candidate sales", NA, NA, NA, NA, nrow(sales), "All scored and enriched 2006-2022 sales",
    "metric", "Exact historical coordinate coverage", NA, NA, NA, NA, sum(sales$has_historical_coordinates), "Rows",
    "metric", "Current coordinate fallbacks", NA, NA, NA, NA, sum(!sales$has_historical_coordinates), "Rows",
    "metric", "Coordinate shift greater than 1 foot", NA, NA, NA, NA, sum(sales$coordinate_shift_ft > 1), "Rows",
    "metric", "Coordinate shift greater than 50 feet", NA, NA, NA, NA, sum(sales$coordinate_shift_ft > 50), "Rows",
    "metric", "Coordinate shift greater than 500 feet", NA, NA, NA, NA, sum(sales$coordinate_shift_ft > 500), "Rows",
    "metric", "Ward assignment changes", NA, NA, NA, NA, sum(sales$ward != sales$historical_ward, na.rm = TRUE), "Rows",
    "metric", "Ward-pair assignment changes", NA, NA, NA, NA, sum(sales$ward_pair_id != sales$historical_ward_pair_id, na.rm = TRUE), "Rows",
    "metric", "Segment assignment changes", NA, NA, NA, NA, sum(sales$segment_id != sales$historical_segment_id, na.rm = TRUE), "Rows",
    "metric", "Current 500ft sample", NA, NA, NA, NA, length(current_ids), "Rows before fixest dropping",
    "metric", "Historical-primary 500ft sample", NA, NA, NA, NA, length(historical_ids), "Rows before fixest dropping",
    "metric", "Enter historical-primary sample", NA, NA, NA, NA, length(setdiff(historical_ids, current_ids)), "Rows",
    "metric", "Exit historical-primary sample", NA, NA, NA, NA, length(setdiff(current_ids, historical_ids)), "Rows"
  )
)

examples <- sales %>%
  filter(
    coordinate_shift_ft > 1 |
      ward != historical_ward |
      ward_pair_id != historical_ward_pair_id
  ) %>%
  arrange(desc(coordinate_shift_ft)) %>%
  transmute(
    audit_id, pin, sale_date, year, sale_price,
    coordinate_source, coordinate_shift_ft,
    current_longitude = longitude,
    current_latitude = latitude,
    historical_longitude = historical_longitude_use,
    historical_latitude = historical_latitude_use,
    current_ward = ward,
    historical_ward,
    current_ward_pair = ward_pair_id,
    historical_ward_pair = historical_ward_pair_id,
    current_dist_ft = abs(current_signed_dist_ft),
    historical_dist_ft = abs(historical_signed_dist_ft),
    current_segment_id = segment_id,
    historical_segment_id
  )

write_csv(report, "../output/sales_historical_coordinate_check.csv")
write_csv(examples, "../output/sales_historical_coordinate_examples.csv")
