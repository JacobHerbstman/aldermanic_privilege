# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    audit_row_id = row_number(),
    pin = as.character(pin),
    year = suppressWarnings(as.integer(year)),
    sale_date = as.Date(sale_date),
    dist_ft_current = as.numeric(dist_m) / 0.3048,
    signed_dist_ft_current = as.numeric(signed_dist_m) / 0.3048,
    segment_id_current = as.character(segment_id),
    sign_current = as.numeric(sign)
  ) %>%
  filter(
    year >= 2006,
    year <= 2022,
    is.finite(dist_ft_current),
    dist_ft_current <= 1000
  )

historical <- read_parquet("../output/sales_historical_coordinates_bw1000_2006_2022.parquet") %>%
  as_tibble()
if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical coordinate audit input must be unique by PIN-year.", call. = FALSE)
}

current_pins <- fread(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  select = "pin",
  colClasses = list(character = "pin")
) %>%
  as_tibble() %>%
  transmute(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  ) %>%
  distinct(pin)

sales <- sales %>%
  left_join(historical, by = c("pin", "year"), relationship = "many-to-one") %>%
  mutate(
    current_2025_pin = pin %in% current_pins$pin,
    longitude_historical = if_else(current_2025_pin, longitude_historical, longitude),
    latitude_historical = if_else(current_2025_pin, latitude_historical, latitude),
    exact_historical_coordinate = is.finite(longitude_historical) & is.finite(latitude_historical)
  )

current_points <- sales %>%
  filter(exact_historical_coordinate) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)
current_xy <- st_coordinates(current_points)
current_points <- current_points %>%
  st_drop_geometry() %>%
  mutate(
    x_3435_current = current_xy[, 1],
    y_3435_current = current_xy[, 2],
    x_3435_historical = if_else(current_2025_pin, x_3435_historical, x_3435_current),
    y_3435_historical = if_else(current_2025_pin, y_3435_historical, y_3435_current),
    coordinate_shift_ft = sqrt(
      (x_3435_historical - x_3435_current)^2 +
        (y_3435_historical - y_3435_current)^2
    )
  )

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../../../border_segment_creation/output/ward_pair_boundaries.gpkg")

historical_points <- current_points %>%
  st_as_sf(
    coords = c("longitude_historical", "latitude_historical"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(3435) %>%
  mutate(era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE))

historical_boundary <- assign_points_to_boundaries(
  points_sf = historical_points,
  era_values = historical_points$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 5000L
) %>%
  rename(
    ward_historical = ward,
    neighbor_ward_historical = neighbor_ward,
    ward_pair_id_historical = ward_pair_id,
    dist_m_historical = dist_m,
    dist_ft_historical = dist_ft
  )

historical_points <- bind_cols(historical_points, historical_boundary)

segments_by_era <- load_segment_line_layers("../../../border_segment_creation/output/boundary_segments_1320ft.gpkg")
historical_points$segment_id_historical <- assign_points_to_nearest_segments(
  points_sf = historical_points,
  era_values = historical_points$era,
  pair_values = historical_points$ward_pair_id_historical,
  segment_layers = segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 50000L
)

alderman_lookup <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(month = as.yearmon(month)) %>%
  select(ward, month, alderman) %>%
  distinct()
if (anyDuplicated(alderman_lookup[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)
if (anyDuplicated(scores$alderman) > 0) {
  stop("Through-2022 scores must be unique by alderman.", call. = FALSE)
}

historical_data <- historical_points %>%
  st_drop_geometry() %>%
  mutate(month_join = as.yearmon(sale_date)) %>%
  left_join(
    alderman_lookup %>% rename(ward_historical = ward, alderman_own_historical = alderman),
    by = c("ward_historical", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    alderman_lookup %>% rename(neighbor_ward_historical = ward, alderman_neighbor_historical = alderman),
    by = c("neighbor_ward_historical", "month_join" = "month"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_own_historical = alderman, strictness_own_historical = score),
    by = "alderman_own_historical",
    relationship = "many-to-one"
  ) %>%
  left_join(
    scores %>% rename(alderman_neighbor_historical = alderman, strictness_neighbor_historical = score),
    by = "alderman_neighbor_historical",
    relationship = "many-to-one"
  ) %>%
  mutate(
    sign_historical = case_when(
      strictness_own_historical > strictness_neighbor_historical ~ 1,
      strictness_own_historical < strictness_neighbor_historical ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_ft_historical = dist_ft_historical * sign_historical,
    right_historical = as.integer(signed_dist_ft_historical >= 0),
    pair_changed = as.character(ward_pair_id) != as.character(ward_pair_id_historical),
    ward_changed = as.integer(ward) != ward_historical,
    neighbor_changed = as.integer(neighbor_ward) != neighbor_ward_historical,
    sign_changed = sign_current != sign_historical,
    segment_changed = segment_id_current != segment_id_historical,
    current_in_500 = abs(signed_dist_ft_current) <= 500,
    historical_in_500 = abs(signed_dist_ft_historical) <= 500,
    bandwidth_membership_changed = current_in_500 != historical_in_500
  )

summary_rows <- bind_rows(
  tibble(
    metric = c(
      "screened_sale_rows",
      "screened_unique_pins",
      "exact_historical_coordinate_rows",
      "missing_exact_historical_coordinate_rows",
      "exact_historical_coordinate_pins",
      "rows_using_new_current_pin_history",
      "rows_already_using_production_historical_fallback"
    ),
    value = c(
      nrow(sales),
      n_distinct(sales$pin),
      nrow(current_points),
      sum(!sales$exact_historical_coordinate),
      n_distinct(current_points$pin),
      sum(current_points$current_2025_pin),
      sum(!current_points$current_2025_pin)
    )
  ),
  tibble(
    metric = c(
      "coordinate_shift_ft_min",
      "coordinate_shift_ft_p50",
      "coordinate_shift_ft_p90",
      "coordinate_shift_ft_p95",
      "coordinate_shift_ft_p99",
      "coordinate_shift_ft_max",
      "coordinate_shift_gt_1ft_rows",
      "coordinate_shift_gt_10ft_rows",
      "coordinate_shift_gt_50ft_rows",
      "coordinate_shift_gt_100ft_rows"
    ),
    value = c(
      min(current_points$coordinate_shift_ft, na.rm = TRUE),
      quantile(current_points$coordinate_shift_ft, 0.50, na.rm = TRUE),
      quantile(current_points$coordinate_shift_ft, 0.90, na.rm = TRUE),
      quantile(current_points$coordinate_shift_ft, 0.95, na.rm = TRUE),
      quantile(current_points$coordinate_shift_ft, 0.99, na.rm = TRUE),
      max(current_points$coordinate_shift_ft, na.rm = TRUE),
      sum(current_points$coordinate_shift_ft > 1, na.rm = TRUE),
      sum(current_points$coordinate_shift_ft > 10, na.rm = TRUE),
      sum(current_points$coordinate_shift_ft > 50, na.rm = TRUE),
      sum(current_points$coordinate_shift_ft > 100, na.rm = TRUE)
    )
  ),
  tibble(
    metric = c(
      "ward_changed_rows",
      "neighbor_changed_rows",
      "ward_pair_changed_rows",
      "strictness_side_changed_rows",
      "segment_changed_rows",
      "current_in500_rows_with_history",
      "historical_in500_rows",
      "left_500ft_sample_rows",
      "entered_500ft_sample_rows",
      "bandwidth_membership_changed_rows"
    ),
    value = c(
      sum(historical_data$ward_changed, na.rm = TRUE),
      sum(historical_data$neighbor_changed, na.rm = TRUE),
      sum(historical_data$pair_changed, na.rm = TRUE),
      sum(historical_data$sign_changed, na.rm = TRUE),
      sum(historical_data$segment_changed, na.rm = TRUE),
      sum(historical_data$current_in_500, na.rm = TRUE),
      sum(historical_data$historical_in_500, na.rm = TRUE),
      sum(historical_data$current_in_500 & !historical_data$historical_in_500, na.rm = TRUE),
      sum(!historical_data$current_in_500 & historical_data$historical_in_500, na.rm = TRUE),
      sum(historical_data$bandwidth_membership_changed, na.rm = TRUE)
    )
  )
)
write_csv(summary_rows, "../output/sales_historical_coordinate_drift_summary.csv")

write_csv(
  historical_data %>%
    filter(
      coordinate_shift_ft > 10 |
        coalesce(ward_changed, FALSE) |
        coalesce(pair_changed, FALSE) |
        coalesce(sign_changed, FALSE) |
        coalesce(segment_changed, FALSE) |
        coalesce(bandwidth_membership_changed, FALSE)
    ) %>%
    select(
      audit_row_id, pin, year, sale_date,
      longitude, latitude, longitude_historical, latitude_historical,
      coordinate_shift_ft,
      ward, ward_historical, neighbor_ward, neighbor_ward_historical,
      ward_pair_id, ward_pair_id_historical,
      segment_id_current, segment_id_historical,
      sign_current, sign_historical,
      signed_dist_ft_current, signed_dist_ft_historical,
      current_in_500, historical_in_500,
      ward_changed, neighbor_changed, pair_changed, sign_changed,
      segment_changed, bandwidth_membership_changed
    ) %>%
    arrange(desc(coordinate_shift_ft), pin, year),
  "../output/sales_historical_coordinate_assignment_changes.csv"
)

hedonic_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage"
)
amenity_controls <- c(
  "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
control_names <- c(hedonic_controls, amenity_controls)

production_data <- sales %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    !is.na(ward_pair_id),
    !is.na(segment_id_current),
    segment_id_current != "",
    is.finite(signed_dist_ft_current),
    abs(signed_dist_ft_current) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(all_of(control_names), ~ !is.na(.x))
  ) %>%
  mutate(right_current = as.integer(signed_dist_ft_current >= 0))

historical_model_data <- historical_data %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    !is.na(ward_pair_id_historical),
    !is.na(segment_id_historical),
    segment_id_historical != "",
    is.finite(signed_dist_ft_historical),
    !is.na(strictness_own_historical),
    !is.na(strictness_neighbor_historical),
    if_all(all_of(control_names), ~ !is.na(.x))
  )

rhs_current <- paste(c("right_current", control_names), collapse = " + ")
rhs_historical <- paste(c("right_historical", control_names), collapse = " + ")

model_inputs <- list(
  production_published = list(
    data = production_data,
    formula = as.formula(paste0("log(sale_price) ~ ", rhs_current, " | segment_id_current^year_quarter")),
    cluster = "segment_id_current",
    coefficient = "right_current"
  ),
  production_exact_history_subset = list(
    data = production_data %>% filter(exact_historical_coordinate),
    formula = as.formula(paste0("log(sale_price) ~ ", rhs_current, " | segment_id_current^year_quarter")),
    cluster = "segment_id_current",
    coefficient = "right_current"
  ),
  historical_assignment_fixed_current_sample = list(
    data = historical_model_data %>% filter(audit_row_id %in% production_data$audit_row_id),
    formula = as.formula(paste0("log(sale_price) ~ ", rhs_historical, " | segment_id_historical^year_quarter")),
    cluster = "segment_id_historical",
    coefficient = "right_historical"
  ),
  historical_assignment_reselected_500ft = list(
    data = historical_model_data %>% filter(abs(signed_dist_ft_historical) <= 500),
    formula = as.formula(paste0("log(sale_price) ~ ", rhs_historical, " | segment_id_historical^year_quarter")),
    cluster = "segment_id_historical",
    coefficient = "right_historical"
  )
)

model_rows <- vector("list", length(model_inputs))
for (i in seq_along(model_inputs)) {
  spec <- model_inputs[[i]]
  fit <- feols(
    spec$formula,
    data = spec$data,
    cluster = as.formula(paste0("~", spec$cluster))
  )
  ct <- coeftable(fit)
  model_rows[[i]] <- tibble(
    specification = names(model_inputs)[i],
    input_n = nrow(spec$data),
    estimate = unname(ct[spec$coefficient, "Estimate"]),
    se = unname(ct[spec$coefficient, "Std. Error"]),
    p_value = unname(ct[spec$coefficient, "Pr(>|t|)"]),
    percent_effect = 100 * expm1(unname(coef(fit)[spec$coefficient])),
    n = nobs(fit),
    segments = n_distinct(spec$data[[spec$cluster]])
  )
}
write_csv(bind_rows(model_rows), "../output/sales_historical_coordinate_model_sensitivity.csv")
