# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/price_rd_stress_tests/code")
# market <- "rent"
# bandwidth_ft <- 500
# bins_per_side <- 10
# placebo_cutoff_ft <- 1000

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(market, bandwidth_ft, bins_per_side, placebo_cutoff_ft)
}
if (length(cli_args) != 4L) {
  stop("FATAL: Script requires 4 args: <rent|sales> <bandwidth_ft> <bins_per_side> <placebo_cutoff_ft>.", call. = FALSE)
}
market <- cli_args[1]
if (!market %in% c("rent", "sales")) {
  stop("market must be 'rent' or 'sales'.", call. = FALSE)
}

bandwidth_ft <- suppressWarnings(as.numeric(cli_args[2]))
bins_per_side <- suppressWarnings(as.numeric(cli_args[3]))
placebo_cutoff_ft <- suppressWarnings(as.numeric(cli_args[4]))
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be a positive number.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0 || bins_per_side != floor(bins_per_side)) {
  stop("bins_per_side must be a positive integer.", call. = FALSE)
}
if (!is.finite(placebo_cutoff_ft) || placebo_cutoff_ft <= 0 || placebo_cutoff_ft != floor(placebo_cutoff_ft)) {
  stop("placebo_cutoff_ft must be a positive integer.", call. = FALSE)
}
bins_per_side <- as.integer(bins_per_side)
placebo_cutoff_ft <- as.integer(placebo_cutoff_ft)
cutoffs_ft <- c(-placebo_cutoff_ft, 0, placebo_cutoff_ft)

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
}

plot_parts <- list()
facet_levels <- character()
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

  finite_controls <- character()
  for (control_i in controls) {
    if (is.numeric(d[[control_i]])) {
      finite_controls <- c(finite_controls, control_i)
    }
  }
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
  fit_row <- coeftable(fit)[rownames(coeftable(fit)) %in% treatment, , drop = FALSE]
  if (nrow(fit_row) != 1L) {
    stop(sprintf("Could not recover the point estimate for %s / %s.", dataset_label, cutoff_text), call. = FALSE)
  }
  std_error <- unname(fit_row[1, "Std. Error"])
  p_value <- unname(fit_row[1, "Pr(>|t|)"])
  stars <- dplyr::case_when(
    is.finite(p_value) & p_value <= 0.01 ~ "***",
    is.finite(p_value) & p_value <= 0.05 ~ "**",
    is.finite(p_value) & p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  facet_label <- sprintf(
    "%s\nJump = %.3f%s (SE %.3f)",
    cutoff_text,
    estimate,
    stars,
    std_error
  )
  facet_levels <- c(facet_levels, facet_label)

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
      cutoff_label = cutoff_text,
      cutoff_facet_label = facet_label
    )

  bin_width <- bandwidth_ft / bins_per_side
  plot_parts[[length(plot_parts) + 1L]] <- plot_data %>%
    mutate(
      bin_id = floor(running_ft / bin_width),
      bin_center = (bin_id + 0.5) * bin_width
    ) %>%
    group_by(cutoff_ft, cutoff_label, cutoff_facet_label, bin_center) %>%
    summarise(
      mean_y = mean(y_adjusted, na.rm = TRUE),
      side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
      .groups = "drop"
    )
}

bins <- bind_rows(plot_parts) %>%
  mutate(cutoff_facet_label = factor(cutoff_facet_label, levels = facet_levels))

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
  facet_wrap(~cutoff_facet_label, nrow = 1)

ggsave(sprintf("../output/%s_placebo_rd_main_style.pdf", market), plot, width = 12, height = 5.5, dpi = 300, bg = "white")
