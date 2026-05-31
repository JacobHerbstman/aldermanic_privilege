# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/price_rd_stress_tests/code")
# market <- "rent"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(market)
}
if (length(cli_args) != 1L) {
  stop("FATAL: Script requires 1 arg: <rent|sales>.", call. = FALSE)
}
market <- cli_args[1]
if (!market %in% c("rent", "sales")) {
  stop("market must be 'rent' or 'sales'.", call. = FALSE)
}

bandwidth_ft <- 500
bins_per_side <- 10
cutoffs_ft <- c(-1000, 0, 1000)

if (market == "rent") {
  source("../../_lib/amenity_distance_helpers.R")

  rent <- read_parquet("../input/rent_with_ward_distances_full.parquet") %>%
    as_tibble()

  if (!"signed_dist_m" %in% names(rent)) {
    stop("Rental input must include signed_dist_m.", call. = FALSE)
  }

  rent <- rent %>%
    mutate(
      file_date = as.Date(file_date),
      year = lubridate::year(file_date),
      year_month = format(file_date, "%Y-%m"),
      signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
      ward_pair = as.character(ward_pair_id),
      segment_id = as.character(segment_id),
      right = as.integer(signed_dist_ft >= 0),
      building_type_factor = factor(coalesce(building_type_clean, "other")),
      log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
      log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
      log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_)
    ) %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= max(abs(cutoffs_ft)) + bandwidth_ft,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair)
    )

  for (flag_col in c(
    "flag_location_questionable",
    "flag_modal_assignment_missing",
    "flag_modal_changes_ward",
    "flag_modal_changes_neighbor_ward",
    "flag_modal_changes_pair",
    "flag_modal_dist_diff_gt100ft"
  )) {
    if (!flag_col %in% names(rent)) {
      rent[[flag_col]] <- FALSE
    }
    rent[[flag_col]] <- coalesce(as.logical(rent[[flag_col]]), FALSE)
  }

  rent <- rent %>%
    mutate(
      flag_clean_location_sample = !flag_location_questionable &
        !flag_modal_assignment_missing &
        !flag_modal_changes_ward &
        !flag_modal_changes_neighbor_ward &
        !flag_modal_changes_pair &
        !flag_modal_dist_diff_gt100ft
    ) %>%
    filter(flag_clean_location_sample, is.finite(longitude), is.finite(latitude))

  rent_coords <- build_unique_coordinate_amenity_table(
    rent,
    "longitude",
    "latitude",
    "../input/schools_2015.gpkg",
    "../input/parks.gpkg",
    "../input/major_streets.gpkg",
    "../input/gis_osm_water_a_free_1.shp",
    chunk_n = 100000L,
    distance_units = "feet"
  )
  rent_coords_sf <- st_as_sf(rent_coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3435)
  rent_coords$nearest_cta_stop_dist_ft <- nearest_distance_ft(
    rent_coords_sf,
    read_amenity_layer("../input/cta_stops.gpkg"),
    chunk_size = 100000L,
    label = "rent coordinates"
  )
  rent <- rent %>%
    left_join(rent_coords, by = c("longitude", "latitude"), relationship = "many-to-one") %>%
    mutate(
      nearest_school_dist_kft = nearest_school_dist_ft / 1000,
      nearest_park_dist_kft = nearest_park_dist_ft / 1000,
      nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
      nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
      lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
    )
  if (anyDuplicated(rent$rent_panel_id) > 0) {
    stop("Rental amenity join expanded rent_panel_id rows.", call. = FALSE)
  }

  rent_controls <- c(
    "log_sqft",
    "log_beds",
    "log_baths",
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "nearest_cta_stop_dist_kft",
    "lake_michigan_dist_kft",
    "building_type_factor"
  )

  plot_input <- rent
  dataset_label <- "Listed rents"
  outcome <- "rent_price"
  time_var <- "year_month"
  controls <- rent_controls
  cluster_var <- "segment_id"
  plot_title <- "Listed Rents: True and Placebo Cutoffs"
  plot_y_label <- "Segment-by-month adjusted log rent"
  output_base <- "../output/rent_placebo_rd_main_style"
} else {
  sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
    as_tibble()

  if (!"signed_dist_m" %in% names(sales)) {
    stop("Sales input must include signed_dist_m.", call. = FALSE)
  }

  sales <- sales %>%
    mutate(
      sale_date = as.Date(sale_date),
      year = lubridate::year(sale_date),
      year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
      signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
      ward_pair = as.character(ward_pair_id),
      segment_id = as.character(segment_id),
      right = as.integer(signed_dist_ft >= 0),
      nearest_school_dist_kft = nearest_school_dist_ft / 1000,
      nearest_park_dist_kft = nearest_park_dist_ft / 1000,
      nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
      lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
    ) %>%
    filter(
      year >= 2006,
      year <= 2022,
      is.finite(sale_price),
      sale_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= max(abs(cutoffs_ft)) + bandwidth_ft,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair)
    )

  sales_controls <- c(
    "log_sqft",
    "log_land_sqft",
    "log_building_age",
    "log_bedrooms",
    "log_baths",
    "has_garage",
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "lake_michigan_dist_kft"
  )

  plot_input <- sales
  dataset_label <- "Home sales"
  outcome <- "sale_price"
  time_var <- "year_quarter"
  controls <- sales_controls
  cluster_var <- "segment_id"
  plot_title <- "Home Sale Prices: True and Placebo Cutoffs"
  plot_y_label <- "Segment-by-quarter adjusted log sale price"
  output_base <- "../output/sales_placebo_rd_main_style"
}

plot_parts <- list()
for (cut_i in cutoffs_ft) {
  cutoff_value <- cut_i
  cutoff_text <- if (cutoff_value == 0) {
    "True boundary"
  } else {
    sprintf("%+dft placebo", as.integer(cutoff_value))
  }
  treatment <- if (cut_i == 0) "right" else "cutoff_right"
  d <- plot_input %>%
    mutate(
      running_ft = signed_dist_ft - cutoff_value,
      cutoff_right = as.integer(running_ft >= 0)
    )

  finite_controls <- controls[vapply(d[controls], is.numeric, logical(1))]
  d <- d %>%
    filter(
      abs(running_ft) <= bandwidth_ft,
      !is.na(segment_id),
      segment_id != "",
      !is.na(.data[[cluster_var]]),
      .data[[cluster_var]] != ""
    )
  if (length(finite_controls) > 0) {
    d <- d %>% filter(if_all(all_of(finite_controls), ~ is.finite(.x)))
  }

  if (nrow(d) == 0 || n_distinct(d[[treatment]]) < 2 || n_distinct(d[[cluster_var]]) < 2) {
    stop(sprintf("Insufficient support for %s / %s.", dataset_label, cutoff_text), call. = FALSE)
  }

  rhs <- paste(c(treatment, controls), collapse = " + ")
  fit <- feols(
    as.formula(paste0("log(", outcome, ") ~ ", rhs, " | segment_id^", time_var)),
    data = d,
    cluster = as.formula(paste0("~", cluster_var))
  )
  if (!treatment %in% names(coef(fit))) {
    stop(sprintf("Model did not estimate %s for %s / %s.", treatment, dataset_label, cutoff_text), call. = FALSE)
  }
  estimate <- unname(coef(fit)[[treatment]])

  removed <- fit$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(d))
  } else {
    setdiff(seq_len(nrow(d)), abs(as.integer(removed)))
  }
  plot_data <- d[keep_idx, , drop = FALSE] %>%
    mutate(
      y_adjusted = as.numeric(resid(fit)) + estimate * .data[[treatment]],
      cutoff_ft = cutoff_value,
      cutoff_label = cutoff_text
    )

  bin_width <- bandwidth_ft / bins_per_side
  plot_parts[[length(plot_parts) + 1L]] <- plot_data %>%
    mutate(
      bin_id = floor(running_ft / bin_width),
      bin_center = (bin_id + 0.5) * bin_width
    ) %>%
    group_by(cutoff_ft, cutoff_label, bin_center) %>%
    summarise(
      mean_y = mean(y_adjusted, na.rm = TRUE),
      side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
      .groups = "drop"
    ) %>%
    mutate(cutoff_label = factor(cutoff_label, levels = c("-1000ft placebo", "True boundary", "+1000ft placebo")))
}

bins <- bind_rows(plot_parts)

plot <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.7) +
  geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.3) +
  scale_color_manual(
    values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
    name = NULL
  ) +
  labs(
    title = plot_title,
    x = "Distance to cutoff (feet; positive = more stringent side)",
    y = plot_y_label
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  facet_wrap(~cutoff_label, nrow = 1)

ggsave(paste0(output_base, ".pdf"), plot, width = 12, height = 5.5, dpi = 300, bg = "white")
