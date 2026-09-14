# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_sample_definition_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/canonical_geometry_helpers.R")
source("../../../shared/code/amenity_distance_helpers.R")

sf_use_s2(FALSE)

sample_order <- c(
  "Legacy names-required",
  "Warranty/trustee, names optional",
  "Production current",
  "Official flags, inclusive",
  "Official flags, exclude nonmarket labels",
  "Official flags, market deed types"
)
amenity_columns <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
control_columns <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage",
  amenity_columns
)

candidates <- as.data.table(read_parquet(
  "../output/sales_sample_definition_candidates.parquet"
))
candidates[, `:=`(
  row_id = as.character(row_id),
  pin = as.character(pin),
  year = suppressWarnings(as.integer(year)),
  sale_date_use = as.Date(sale_date_use)
)]
if (
  anyDuplicated(candidates$row_id) > 0L ||
    any(nchar(candidates$pin) != 14L) ||
    !all(sample_order %in% names(candidates))
) {
  stop("Candidate sales violate the expected row contract.", call. = FALSE)
}

production <- as.data.table(read_parquet(
  "../input/unfiltered_sales_with_hedonics_amenities.parquet"
))
production[, row_id := as.character(row_id)]
production_spatial <- production[, c(
  "row_id", "longitude", "latitude", "ward", "neighbor_ward",
  "ward_pair_id", "segment_id", "dist_m", "signed_dist_m",
  "alderman_own", "alderman_neighbor", "strictness_own",
  "strictness_neighbor", amenity_columns
), with = FALSE]
if (anyDuplicated(production_spatial$row_id) > 0L) {
  stop("Production sales must be unique by row_id.", call. = FALSE)
}

new_sales <- candidates[!row_id %in% production_spatial$row_id]
historical <- fread(
  "../output/sales_sample_definition_historical_coordinates.csv",
  colClasses = list(character = "pin")
)
historical[, `:=`(
  year = suppressWarnings(as.integer(year)),
  historical_longitude = suppressWarnings(as.numeric(longitude)),
  historical_latitude = suppressWarnings(as.numeric(latitude))
)]
historical[, c(
  "longitude", "latitude", "centroid_x_crs_3435", "centroid_y_crs_3435"
) := NULL]
if (anyDuplicated(historical[, .(pin, year)]) > 0L) {
  stop("Historical candidate coordinates must be unique by PIN-year.", call. = FALSE)
}

current_parcels <- fread(
  "../input/parcel_universe_2025_city.csv",
  select = c("pin", "latitude", "longitude"),
  colClasses = list(character = "pin")
)
current_parcels[, `:=`(
  current_latitude = suppressWarnings(as.numeric(latitude)),
  current_longitude = suppressWarnings(as.numeric(longitude))
)]
current_parcels[, c("latitude", "longitude") := NULL]
if (anyDuplicated(current_parcels$pin) > 0L) {
  stop("Current parcel coordinates must be unique by PIN.", call. = FALSE)
}

new_sales <- merge(
  new_sales,
  historical[, .(pin, year, historical_longitude, historical_latitude)],
  by = c("pin", "year"),
  all.x = TRUE,
  sort = FALSE
)
new_sales <- merge(
  new_sales,
  current_parcels,
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
new_sales[, `:=`(
  longitude = fifelse(
    is.finite(historical_longitude),
    historical_longitude,
    current_longitude
  ),
  latitude = fifelse(
    is.finite(historical_latitude),
    historical_latitude,
    current_latitude
  ),
  coordinate_source = fifelse(
    is.finite(historical_longitude) & is.finite(historical_latitude),
    "historical_exact_pin_year",
    fifelse(
      is.finite(current_longitude) & is.finite(current_latitude),
      "current_2025_fallback",
      "unresolved"
    )
  )
)]
unresolved_coordinate_n <- new_sales[coordinate_source == "unresolved", .N]
if (unresolved_coordinate_n > 0L) {
  message(sprintf(
    "Dropping %d candidate sale with no historical or current coordinates.",
    unresolved_coordinate_n
  ))
}
new_sales <- new_sales[coordinate_source != "unresolved"]

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435)
canonical_ward_maps <- load_canonical_ward_maps(
  ward_panel,
  c("2003_2014", "2015_2023")
)
canonical_boundaries <- load_boundary_layers(
  "../input/ward_pair_boundaries.gpkg",
  c("2003_2014", "2015_2023")
)

new_sales_sf <- st_as_sf(
  new_sales,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(3435) %>%
  mutate(
    boundary_year = canonical_boundary_year_from_date(sale_date_use),
    era = canonical_era_from_boundary_year(boundary_year)
  )
boundary_assignments <- assign_points_to_boundaries(
  points_sf = new_sales_sf,
  era_values = new_sales_sf$era,
  ward_maps = canonical_ward_maps,
  boundary_lines = canonical_boundaries,
  chunk_n = 5000L
)
new_sales_sf <- bind_cols(new_sales_sf, boundary_assignments) %>%
  filter(!is.na(ward), !is.na(ward_pair_id))

alderman_terms <- read_csv(
  "../input/chicago_alderman_terms.csv",
  show_col_types = FALSE
) %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    start_date = pmax(start_date, min(new_sales_sf$sale_date_use)),
    end_date = pmin(end_date, max(new_sales_sf$sale_date_use))
  ) %>%
  filter(start_date <= end_date) %>%
  rowwise() %>%
  mutate(sale_date_use = list(seq(start_date, end_date, by = "day"))) %>%
  ungroup() %>%
  select(ward, sale_date_use, alderman) %>%
  unnest(sale_date_use)
if (anyDuplicated(alderman_terms[c("ward", "sale_date_use")]) > 0L) {
  stop("Alderman terms overlap within ward-date.", call. = FALSE)
}

new_sales_spatial <- new_sales_sf %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  left_join(
    alderman_terms,
    by = c("ward", "sale_date_use"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman) %>%
  left_join(
    alderman_terms,
    by = c("neighbor_ward" = "ward", "sale_date_use"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_neighbor = alderman) %>%
  as.data.table()

segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  c("2003_2014", "2015_2023")
)
new_sales_spatial[, pair_dash := normalize_pair_dash(ward_pair_id)]
new_sales_spatial[, era := canonical_era_from_date(
  sale_date_use,
  allow_pre_2003 = TRUE
)]
new_points <- st_as_sf(
  new_sales_spatial,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
new_sales_spatial[, segment_id := assign_points_to_nearest_segments(
  points_sf = new_points,
  era_values = era,
  pair_values = pair_dash,
  segment_layers = segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 50000L
)]

scores <- read_csv(
  "../input/aldermen_uncertainty_scores_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0L || any(!is.finite(scores$score))) {
  stop("Alderman scores must be finite and unique.", call. = FALSE)
}
new_sales_spatial <- new_sales_spatial %>%
  as_tibble() %>%
  left_join(
    scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_own = score) %>%
  left_join(
    scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_m = dist_m * sign
  )

new_amenity_coordinates <- build_unique_coordinate_amenity_table(
  new_sales_spatial,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  100000L
)
new_sales_spatial <- append_amenity_distances(
  new_sales_spatial,
  new_amenity_coordinates,
  "longitude",
  "latitude"
)

cta_stops <- read_amenity_layer("../input/cta_stops.gpkg") %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  )
new_date_points <- new_sales_spatial %>%
  distinct(longitude, latitude, sale_date_use) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(3435)
network_change_dates <- sort(unique(c(
  cta_stops$active_from_date,
  cta_stops$active_to_date + 1
)))
network_change_dates <- network_change_dates[!is.na(network_change_dates)]
network_group <- findInterval(
  as.numeric(new_date_points$sale_date_use),
  as.numeric(network_change_dates)
)
cta_distance_rows <- list()
for (network_i in unique(network_group)) {
  row_i <- which(network_group == network_i)
  date_points <- new_date_points[row_i, ]
  sale_date_i <- date_points$sale_date_use[1]
  active_cta <- cta_stops %>%
    filter(
      active_from_date <= sale_date_i,
      is.na(active_to_date) | active_to_date >= sale_date_i
    )
  nearest_idx <- st_nearest_feature(date_points, active_cta)
  nearest_cta <- active_cta[nearest_idx, ]
  cta_distance_rows[[length(cta_distance_rows) + 1L]] <-
    st_drop_geometry(date_points) %>%
    transmute(
      longitude,
      latitude,
      sale_date_use,
      nearest_cta_stop_dist_ft = as.numeric(st_distance(
        date_points,
        nearest_cta,
        by_element = TRUE
      ))
    )
}
cta_distances <- bind_rows(cta_distance_rows)
new_sales_spatial <- new_sales_spatial %>%
  left_join(
    cta_distances,
    by = c("longitude", "latitude", "sale_date_use"),
    relationship = "many-to-one"
  ) %>%
  select(
    row_id, longitude, latitude, ward, neighbor_ward, ward_pair_id,
    segment_id, dist_m, signed_dist_m, alderman_own, alderman_neighbor,
    strictness_own, strictness_neighbor, all_of(amenity_columns)
  ) %>%
  as.data.table()

all_spatial <- rbindlist(
  list(production_spatial, new_sales_spatial),
  use.names = TRUE,
  fill = TRUE
)
if (anyDuplicated(all_spatial$row_id) > 0L) {
  stop("Combined spatial data must be unique by row_id.", call. = FALSE)
}
analysis_data <- merge(
  candidates,
  all_spatial,
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)
analysis_data[, `:=`(
  year_quarter = paste0(year, "-Q", quarter(sale_date_use)),
  signed_dist_ft = signed_dist_m / 0.3048,
  treated = as.integer(signed_dist_m > 0),
  ward_pair = as.character(ward_pair_id)
)]
analysis_data[, spatial_complete :=
  is.finite(signed_dist_ft) &
  is.finite(strictness_own) &
  is.finite(strictness_neighbor) &
  strictness_own != strictness_neighbor &
  !is.na(segment_id) & segment_id != "" &
  !is.na(ward_pair) & ward_pair != "" &
  is.finite(longitude) & is.finite(latitude)]
analysis_data[, model_controls_complete := Reduce(
  `&`,
  lapply(.SD, is.finite)
), .SDcols = control_columns]

coverage_output <- rbindlist(lapply(sample_order, function(sample_i) {
  sample_flag <- analysis_data[[sample_i]]
  data.table(
    sample = sample_i,
    citywide_n = sum(sample_flag),
    coordinate_available_n = sum(
      sample_flag & analysis_data$row_id %in% all_spatial$row_id
    ),
    spatial_complete_n = sum(sample_flag & analysis_data$spatial_complete),
    within_500ft_n = sum(
      sample_flag & analysis_data$spatial_complete &
        abs(analysis_data$signed_dist_ft) < 500
    ),
    within_500ft_complete_controls_n = sum(
      sample_flag & analysis_data$spatial_complete &
        abs(analysis_data$signed_dist_ft) < 500 &
        analysis_data$model_controls_complete
    )
  )
}))

formula <- as.formula(paste0(
  "log(sale_price_model) ~ treated + signed_dist_ft + ",
  "treated:signed_dist_ft + ",
  paste(control_columns, collapse = " + "),
  " | segment_id^year_quarter"
))
estimate_rows <- list()
count_rows <- list()

for (sample_i in sample_order) {
  sample_prices <- analysis_data[["sale_price_real_2022"]][
    analysis_data[[sample_i]]
  ]
  lower_cutoff <- quantile(sample_prices, 0.01, na.rm = TRUE)
  upper_cutoff <- quantile(sample_prices, 0.99, na.rm = TRUE)

  sample_data <- copy(analysis_data[
    get(sample_i) == TRUE &
      spatial_complete == TRUE &
      model_controls_complete == TRUE &
      abs(signed_dist_ft) < 500
  ])
  sample_data[, sale_price_model := pmin(
    pmax(sale_price_real_2022, lower_cutoff),
    upper_cutoff
  )]
  model <- feols(
    formula,
    data = sample_data,
    cluster = ~ward_pair,
    warn = FALSE,
    notes = FALSE
  )
  coefficient_table <- coeftable(model)
  estimate_rows[[length(estimate_rows) + 1L]] <- data.table(
    sample = sample_i,
    estimate = coefficient_table["treated", "Estimate"],
    std_error = coefficient_table["treated", "Std. Error"],
    p_value = coefficient_table["treated", "Pr(>|t|)"],
    n = nobs(model),
    ward_pair_clusters = uniqueN(sample_data$ward_pair),
    segments = uniqueN(sample_data$segment_id),
    lower_winsor_cutoff = lower_cutoff,
    upper_winsor_cutoff = upper_cutoff
  )
  count_rows[[length(count_rows) + 1L]] <- sample_data[, .N, by = year][
    , sample := sample_i
  ]
}

estimate_output <- rbindlist(estimate_rows)
estimate_output[, `:=`(
  confidence_low = estimate - 1.96 * std_error,
  confidence_high = estimate + 1.96 * std_error,
  percent_effect = 100 * (exp(estimate) - 1)
)]
count_output <- rbindlist(count_rows, use.names = TRUE)
setcolorder(count_output, c("sample", "year", "N"))

estimate_plot <- ggplot(
  estimate_output,
  aes(
    x = factor(sample, levels = rev(sample_order)),
    y = 100 * (exp(estimate) - 1)
  )
) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_errorbar(
    aes(
      ymin = 100 * (exp(confidence_low) - 1),
      ymax = 100 * (exp(confidence_high) - 1)
    ),
    width = 0.15
  ) +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated price discontinuity (%)",
    title = "Sales boundary estimate by transaction sample"
  ) +
  theme_minimal(base_size = 11)

fwrite(
  estimate_output,
  "../output/sales_sample_definition_rd_estimates.csv"
)
ReportData("../output/sales_sample_definition_rd_estimates.csv")
fwrite(
  count_output,
  "../output/sales_sample_definition_rd_counts.csv"
)
ReportData("../output/sales_sample_definition_rd_counts.csv")
fwrite(
  coverage_output,
  "../output/sales_sample_definition_spatial_coverage.csv"
)
ReportData("../output/sales_sample_definition_spatial_coverage.csv")
ggsave(
  "../output/sales_sample_definition_rd_estimates.pdf",
  estimate_plot,
  width = 8,
  height = 5
)
