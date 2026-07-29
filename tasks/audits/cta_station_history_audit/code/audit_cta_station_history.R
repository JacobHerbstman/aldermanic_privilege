# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/cta_station_history_audit/code")

source("../../../setup_environment/code/packages.R")

historical_stations <- tribble(
  ~station, ~longitude, ~latitude, ~active_to,
  "Washington/State", -87.627800, 41.883700, as.Date("2006-10-22"),
  "Madison/Wabash", -87.626098, 41.882023, as.Date("2015-03-15"),
  "Randolph/Wabash", -87.626149, 41.884431, as.Date("2017-09-02")
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)

add_historical_station_distances <- function(data) {
  points <- data %>%
    distinct(longitude, latitude, cta_date) %>%
    mutate(
      corrected_cta_dist_ft = Inf,
      corrected_cta_station = NA_character_
    ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3435)

  for (station_i in seq_len(nrow(historical_stations))) {
    candidate_distance <- as.numeric(st_distance(
      points,
      historical_stations[station_i, ],
      by_element = FALSE
    ))
    station_active <- points$cta_date <= historical_stations$active_to[station_i]
    use_station <- station_active & candidate_distance < points$corrected_cta_dist_ft
    points$corrected_cta_dist_ft[use_station] <- candidate_distance[use_station]
    points$corrected_cta_station[use_station] <- historical_stations$station[station_i]
  }

  data %>%
    left_join(
      st_drop_geometry(points) %>%
        select(
          longitude,
          latitude,
          cta_date,
          historical_cta_dist_ft = corrected_cta_dist_ft,
          historical_cta_station = corrected_cta_station
        ),
      by = c("longitude", "latitude", "cta_date"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      historical_cta_dist_ft = if_else(
        is.finite(historical_cta_dist_ft),
        historical_cta_dist_ft,
        NA_real_
      ),
      corrected_cta_dist_ft = pmin(
        nearest_cta_stop_dist_ft,
        historical_cta_dist_ft,
        na.rm = TRUE
      ),
      corrected_cta_station = if_else(
        !is.na(historical_cta_dist_ft) &
          historical_cta_dist_ft < nearest_cta_stop_dist_ft,
        historical_cta_station,
        NA_character_
      ),
      cta_distance_reduction_ft = nearest_cta_stop_dist_ft - corrected_cta_dist_ft
    )
}

model_result <- function(model, analysis, station_history, clusters) {
  table <- coeftable(model)
  tibble(
    analysis = analysis,
    station_history = station_history,
    estimate = unname(table["right", "Estimate"]),
    se = unname(table["right", "Std. Error"]),
    p_value = unname(table["right", "Pr(>|t|)"]),
    observations = nobs(model),
    clusters = clusters
  )
}

distance_summary <- function(data, analysis) {
  affected <- data %>%
    filter(cta_distance_reduction_ft > 1e-8)

  if (nrow(affected) == 0L) {
    return(tibble(
      analysis = analysis,
      historical_station = "Any historical station",
      observations_affected = 0L,
      share_affected = 0,
      distinct_coordinate_dates = 0L,
      mean_distance_reduction_ft = NA_real_,
      maximum_distance_reduction_ft = NA_real_
    ))
  }

  bind_rows(
    tibble(
      analysis = analysis,
      historical_station = "Any historical station",
      observations_affected = nrow(affected),
      share_affected = nrow(affected) / nrow(data),
      distinct_coordinate_dates = n_distinct(
        paste(affected$longitude, affected$latitude, affected$cta_date)
      ),
      mean_distance_reduction_ft = mean(affected$cta_distance_reduction_ft),
      maximum_distance_reduction_ft = max(affected$cta_distance_reduction_ft)
    ),
    affected %>%
      count(corrected_cta_station, name = "observations_affected") %>%
      left_join(
        affected %>%
          group_by(corrected_cta_station) %>%
          summarise(
            distinct_coordinate_dates = n_distinct(paste(longitude, latitude, cta_date)),
            mean_distance_reduction_ft = mean(cta_distance_reduction_ft),
            maximum_distance_reduction_ft = max(cta_distance_reduction_ft),
            .groups = "drop"
          ),
        by = "corrected_cta_station",
        relationship = "one-to-one"
      ) %>%
      transmute(
        analysis = analysis,
        historical_station = corrected_cta_station,
        observations_affected,
        share_affected = observations_affected / nrow(data),
        distinct_coordinate_dates,
        mean_distance_reduction_ft,
        maximum_distance_reduction_ft
      )
  )
}

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = year(file_date),
    year_month = format(file_date, "%Y-%m"),
    cta_date = as.Date(paste0(year_month, "-01")),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    beds_factor = factor(beds),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    nearest_school_dist_kft = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
    lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
  ) %>%
  filter(
    between(year, 2014, 2022),
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    flag_clean_location_sample,
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft),
    !is.na(log_baths),
    if_all(
      all_of(c(
        "nearest_school_dist_kft",
        "nearest_park_dist_kft",
        "nearest_major_road_dist_kft",
        "nearest_cta_stop_dist_ft",
        "lake_michigan_dist_kft"
      )),
      is.finite
    )
  ) %>%
  add_historical_station_distances() %>%
  mutate(
    production_cta_dist_kft = nearest_cta_stop_dist_ft / 1000,
    corrected_cta_dist_kft = corrected_cta_dist_ft / 1000
  )

rent_rhs <- paste(
  c(
    "right",
    "log_sqft",
    "beds_factor",
    "log_baths",
    "building_type_factor",
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "CTA_DISTANCE",
    "lake_michigan_dist_kft"
  ),
  collapse = " + "
)
rent_production <- feols(
  as.formula(paste0(
    "log(rent_price) ~ ",
    sub("CTA_DISTANCE", "production_cta_dist_kft", rent_rhs),
    " | segment_id^year_month"
  )),
  data = rent,
  cluster = ~segment_id
)
rent_corrected <- feols(
  as.formula(paste0(
    "log(rent_price) ~ ",
    sub("CTA_DISTANCE", "corrected_cta_dist_kft", rent_rhs),
    " | segment_id^year_month"
  )),
  data = rent,
  cluster = ~segment_id
)

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    cta_date = as.Date(sale_date),
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    is.finite(sale_price),
    sale_price > 0,
    between(year, 2006, 2022),
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(
      all_of(c(
        "log_sqft",
        "log_land_sqft",
        "log_building_age",
        "log_bedrooms",
        "log_baths",
        "has_garage",
        "nearest_school_dist_ft",
        "nearest_park_dist_ft",
        "nearest_major_road_dist_ft",
        "nearest_cta_stop_dist_ft",
        "lake_michigan_dist_ft"
      )),
      ~ !is.na(.x)
    )
  ) %>%
  add_historical_station_distances()

sales_rhs <- c(
  "right",
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "CTA_DISTANCE",
  "lake_michigan_dist_ft"
) %>%
  paste(collapse = " + ")
sales_production <- feols(
  as.formula(paste0(
    "log(sale_price) ~ ",
    sub("CTA_DISTANCE", "nearest_cta_stop_dist_ft", sales_rhs),
    " | segment_id^year_quarter"
  )),
  data = sales,
  cluster = ~segment_id
)
sales_corrected <- feols(
  as.formula(paste0(
    "log(sale_price) ~ ",
    sub("CTA_DISTANCE", "corrected_cta_dist_ft", sales_rhs),
    " | segment_id^year_quarter"
  )),
  data = sales,
  cluster = ~segment_id
)

bind_rows(
  model_result(
    rent_production,
    "rental_rd",
    "production_station_history",
    n_distinct(rent$segment_id)
  ),
  model_result(
    rent_corrected,
    "rental_rd",
    "production_plus_historical_minimum_check",
    n_distinct(rent$segment_id)
  ),
  model_result(
    sales_production,
    "sales_rd",
    "production_station_history",
    n_distinct(sales$segment_id)
  ),
  model_result(
    sales_corrected,
    "sales_rd",
    "production_plus_historical_minimum_check",
    n_distinct(sales$segment_id)
  )
) %>%
  write_csv("../output/cta_history_estimate_comparison.csv")

bind_rows(
  distance_summary(rent, "rental_rd"),
  distance_summary(sales, "sales_rd")
) %>%
  write_csv("../output/cta_history_distance_summary.csv")
