# Build RD-style diagnostic plots for placebo cutoffs and pruned-boundary samples.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/price_rd_stress_tests/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

bandwidth_ft <- 500
bins_per_side <- 10
cutoffs_ft <- c(-500, 0, 500)

stars <- function(p_value) {
  case_when(
    !is.finite(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_dollar <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

coef_p_value <- function(coef_table, row_name) {
  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)
  if (length(p_col) != 1L) {
    return(NA_real_)
  }
  unname(coef_table[row_name, p_col])
}

plot_label <- function(cutoff_ft) {
  if (cutoff_ft == 0) {
    return("True boundary")
  }
  sprintf("%+dft placebo", as.integer(cutoff_ft))
}

collapse_segment_flags <- function() {
  read_csv(
    "../input/confounded_segment_flags.csv",
    show_col_types = FALSE,
    col_types = cols(segment_id = col_character(), .default = col_guess())
  ) %>%
    mutate(
      segment_id = as.character(segment_id),
      drop_confound = as.logical(drop_confound)
    ) %>%
    filter(!is.na(segment_id), segment_id != "") %>%
    group_by(segment_id) %>%
    summarise(drop_confound = any(drop_confound, na.rm = TRUE), .groups = "drop")
}

fit_plot_data <- function(data, dataset, outcome, time_var, controls, cluster_var, cutoff_ft, sample_name, local_linear = FALSE, use_log = TRUE) {
  cutoff_value <- cutoff_ft
  cutoff_text <- plot_label(cutoff_value)
  treatment <- if (cutoff_ft == 0) "right" else "cutoff_right"
  d <- data %>%
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
    stop(sprintf("Insufficient support for %s / %s / %s.", dataset, sample_name, cutoff_text), call. = FALSE)
  }

  rhs_terms <- c(treatment, controls)
  if (local_linear) {
    rhs_terms <- c(treatment, "running_ft", paste0(treatment, ":running_ft"), controls)
  }
  rhs <- paste(rhs_terms, collapse = " + ")
  lhs <- if (use_log) paste0("log(", outcome, ")") else outcome
  fit <- feols(
    as.formula(paste0(lhs, " ~ ", rhs, " | segment_id^", time_var)),
    data = d,
    cluster = as.formula(paste0("~", cluster_var))
  )
  ct <- coeftable(fit)
  if (!treatment %in% rownames(ct)) {
    stop(sprintf("Model did not estimate %s for %s / %s.", treatment, dataset, cutoff_text), call. = FALSE)
  }

  estimate <- unname(ct[treatment, "Estimate"])
  std_error <- unname(ct[treatment, "Std. Error"])
  p_value <- coef_p_value(ct, treatment)
  pct_change <- if (use_log) 100 * (exp(estimate) - 1) else NA_real_
  estimate_text <- if (use_log) {
    sprintf("%.2f%%%s (SE %.2f)", pct_change, stars(p_value), 100 * std_error)
  } else {
    sprintf("%s%s (SE %s)", format_dollar(estimate), stars(p_value), format_dollar(std_error))
  }
  running_slope <- if (local_linear && "running_ft" %in% rownames(ct)) {
    unname(ct["running_ft", "Estimate"])
  } else {
    0
  }
  interaction_name <- intersect(c(paste0(treatment, ":running_ft"), paste0("running_ft:", treatment)), rownames(ct))
  running_slope_jump <- if (local_linear && length(interaction_name) == 1L) {
    unname(ct[interaction_name, "Estimate"])
  } else {
    0
  }

  removed <- fit$obs_selection$obsRemoved
  keep_idx <- if (is.null(removed)) {
    seq_len(nrow(d))
  } else {
    setdiff(seq_len(nrow(d)), abs(as.integer(removed)))
  }
  plot_data <- d[keep_idx, , drop = FALSE] %>%
    mutate(
      y_adjusted = as.numeric(resid(fit)) +
        estimate * .data[[treatment]] +
        running_slope * running_ft +
        running_slope_jump * .data[[treatment]] * running_ft,
      cutoff_ft = cutoff_value,
      cutoff_label = cutoff_text,
      sample = sample_name,
      dataset = dataset,
      model_type = if_else(local_linear, "Local linear", "Flat"),
      outcome_scale = if_else(use_log, "log", "level")
    )

  bin_width <- bandwidth_ft / bins_per_side
  bins <- plot_data %>%
    mutate(
      bin_id = floor(running_ft / bin_width),
      bin_center = (bin_id + 0.5) * bin_width
    ) %>%
    group_by(dataset, sample, model_type, cutoff_ft, cutoff_label, bin_center) %>%
    summarise(
      n = n(),
      mean_y = mean(y_adjusted, na.rm = TRUE),
      side = if_else(first(bin_center) >= 0, "More Stringent", "Less Stringent"),
      .groups = "drop"
    )

  if (local_linear) {
    line_data <- bind_rows(
      tibble(
        dataset = dataset,
        sample = sample_name,
        model_type = "Local linear",
        cutoff_ft = cutoff_value,
        cutoff_label = cutoff_text,
        x = c(-bandwidth_ft, 0),
        y = running_slope * c(-bandwidth_ft, 0),
        side = "Less Stringent"
      ),
      tibble(
        dataset = dataset,
        sample = sample_name,
        model_type = "Local linear",
        cutoff_ft = cutoff_value,
        cutoff_label = cutoff_text,
        x = c(0, bandwidth_ft),
        y = estimate + (running_slope + running_slope_jump) * c(0, bandwidth_ft),
        side = "More Stringent"
      )
    )
  } else {
    line_data <- bind_rows(
      tibble(
        dataset = dataset,
        sample = sample_name,
        model_type = "Flat",
        cutoff_ft = cutoff_value,
        cutoff_label = cutoff_text,
        x = c(-bandwidth_ft, 0),
        y = mean(plot_data$y_adjusted[plot_data[[treatment]] == 0], na.rm = TRUE),
        side = "Less Stringent"
      ),
      tibble(
        dataset = dataset,
        sample = sample_name,
        model_type = "Flat",
        cutoff_ft = cutoff_value,
        cutoff_label = cutoff_text,
        x = c(0, bandwidth_ft),
        y = mean(plot_data$y_adjusted[plot_data[[treatment]] == 1], na.rm = TRUE),
        side = "More Stringent"
      )
    )
  }

  estimate_row <- tibble(
    dataset = dataset,
    sample = sample_name,
    model_type = if_else(local_linear, "Local linear", "Flat"),
    outcome_scale = if_else(use_log, "log", "level"),
    cutoff_ft = cutoff_value,
    cutoff_label = cutoff_text,
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    pct_change = pct_change,
    estimate_text = estimate_text,
    n_obs = fit$nobs,
    n_segments = n_distinct(plot_data$segment_id),
    n_ward_pairs = n_distinct(plot_data$ward_pair),
    cluster = cluster_var
  )

  list(estimates = estimate_row, bins = bins, lines = line_data)
}

make_rd_plot <- function(parts, title, y_label, output_base, facet = TRUE) {
  estimates <- bind_rows(lapply(parts, `[[`, "estimates")) %>%
    mutate(
      panel_subtitle = sprintf(
        "%s\nJump = %s, N = %s",
        cutoff_label,
        estimate_text,
        format(n_obs, big.mark = ",")
      ),
      panel_subtitle = factor(panel_subtitle, levels = unique(panel_subtitle))
    )
  bins <- bind_rows(lapply(parts, `[[`, "bins")) %>%
    left_join(
      estimates %>% select(dataset, sample, cutoff_ft, panel_subtitle),
      by = c("dataset", "sample", "cutoff_ft"),
      relationship = "many-to-one"
    )
  lines <- bind_rows(lapply(parts, `[[`, "lines")) %>%
    left_join(
      estimates %>% select(dataset, sample, cutoff_ft, panel_subtitle),
      by = c("dataset", "sample", "cutoff_ft"),
      relationship = "many-to-one"
    )

  plot <- ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.7) +
    geom_point(data = bins, aes(x = bin_center, y = mean_y, color = side), size = 2.3) +
    scale_color_manual(
      values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"),
      name = NULL
    ) +
    labs(
      title = title,
      x = "Distance to cutoff (feet; positive = more stringent side)",
      y = y_label
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  if (facet) {
    plot <- plot + facet_wrap(~panel_subtitle, nrow = 1)
  } else {
    subtitle <- estimates$panel_subtitle[[1]]
    plot <- plot + labs(subtitle = subtitle)
  }

  ggsave(paste0(output_base, ".pdf"), plot, width = ifelse(facet, 12, 8.6), height = 5.5, dpi = 300, bg = "white")
  ggsave(paste0(output_base, ".png"), plot, width = ifelse(facet, 12, 8.6), height = 5.5, dpi = 220, bg = "white")

  list(estimates = estimates, bins = bins)
}

message("Loading RD panels...")
rent <- read_parquet("../input/rent_with_ward_distances_full.parquet") %>%
  as_tibble()
sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()
segment_flags <- collapse_segment_flags()

if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(sales) && "signed_dist_m" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
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
  ) %>%
  left_join(segment_flags, by = "segment_id", relationship = "many-to-one") %>%
  mutate(keep_pruned_segment = !is.na(drop_confound) & !drop_confound)

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

message("Computing rental amenity controls for placebo windows...")
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

sales <- sales %>%
  mutate(
    sale_date = as.Date(sale_date),
    year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
    signed_dist_ft = as.numeric(signed_dist),
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
  ) %>%
  left_join(segment_flags, by = "segment_id", relationship = "many-to-one") %>%
  mutate(keep_pruned_segment = !is.na(drop_confound) & !drop_confound)

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

message("Fitting placebo RD-style plots...")
rent_placebo_parts <- lapply(cutoffs_ft, function(cut_i) {
  fit_plot_data(
    rent,
    "Listed rents",
    "rent_price",
    "year_month",
    rent_controls,
    "segment_id",
    cut_i,
    "Placebo cutoffs"
  )
})
sales_placebo_parts <- lapply(cutoffs_ft, function(cut_i) {
  fit_plot_data(
    sales,
    "Home sales",
    "sale_price",
    "year_quarter",
    sales_controls,
    "segment_id",
    cut_i,
    "Placebo cutoffs"
  )
})

message("Fitting pruned true-boundary RD-style plots...")
rent_pruned_part <- list(fit_plot_data(
  rent %>% filter(keep_pruned_segment, abs(signed_dist_ft) <= bandwidth_ft),
  "Listed rents",
  "rent_price",
  "year_month",
  rent_controls,
  "segment_id",
  0,
  "Pruned segments"
))
sales_pruned_part <- list(fit_plot_data(
  sales %>% filter(keep_pruned_segment, abs(signed_dist_ft) <= bandwidth_ft),
  "Home sales",
  "sale_price",
  "year_quarter",
  sales_controls,
  "ward_pair",
  0,
  "Pruned segments"
))

message("Fitting local-linear true-boundary RD-style plots...")
rent_local_linear_part <- list(fit_plot_data(
  rent %>% filter(abs(signed_dist_ft) <= bandwidth_ft),
  "Listed rents",
  "rent_price",
  "year_month",
  rent_controls,
  "segment_id",
  0,
  "Local linear main",
  TRUE
))
sales_local_linear_part <- list(fit_plot_data(
  sales %>% filter(abs(signed_dist_ft) <= bandwidth_ft),
  "Home sales",
  "sale_price",
  "year_quarter",
  sales_controls,
  "ward_pair",
  0,
  "Local linear main",
  TRUE
))

message("Fitting level-outcome true-boundary RD-style plots...")
rent_level_part <- list(fit_plot_data(
  rent %>% filter(abs(signed_dist_ft) <= bandwidth_ft),
  "Listed rents",
  "rent_price",
  "year_month",
  rent_controls,
  "segment_id",
  0,
  "Levels main",
  FALSE,
  FALSE
))
sales_level_part <- list(fit_plot_data(
  sales %>% filter(abs(signed_dist_ft) <= bandwidth_ft),
  "Home sales",
  "sale_price",
  "year_quarter",
  sales_controls,
  "ward_pair",
  0,
  "Levels main",
  FALSE,
  FALSE
))

rent_placebo <- make_rd_plot(
  rent_placebo_parts,
  "Listed Rents: True and Placebo Cutoffs",
  "Segment-by-month adjusted log rent",
  "../output/rent_placebo_rd_main_style",
  TRUE
)
sales_placebo <- make_rd_plot(
  sales_placebo_parts,
  "Home Sale Prices: True and Placebo Cutoffs",
  "Segment-by-quarter adjusted log sale price",
  "../output/sales_placebo_rd_main_style",
  TRUE
)
rent_pruned <- make_rd_plot(
  rent_pruned_part,
  "Listed Rents by Side of Ward Boundary: Pruned Segments",
  "Segment-by-month adjusted log rent",
  "../output/rent_pruned_rd_main_style",
  FALSE
)
sales_pruned <- make_rd_plot(
  sales_pruned_part,
  "Home Sale Prices by Side of Ward Boundary: Pruned Segments",
  "Segment-by-quarter adjusted log sale price",
  "../output/sales_pruned_rd_main_style",
  FALSE
)
rent_local_linear <- make_rd_plot(
  rent_local_linear_part,
  "Listed Rents by Side of Ward Boundary: Local Linear",
  "Segment-by-month adjusted log rent",
  "../output/rent_local_linear_rd_main_style",
  FALSE
)
sales_local_linear <- make_rd_plot(
  sales_local_linear_part,
  "Home Sale Prices by Side of Ward Boundary: Local Linear",
  "Segment-by-quarter adjusted log sale price",
  "../output/sales_local_linear_rd_main_style",
  FALSE
)
rent_level <- make_rd_plot(
  rent_level_part,
  "Listed Rents by Side of Ward Boundary: Levels",
  "Segment-by-month adjusted real listed rent",
  "../output/rent_levels_rd_main_style",
  FALSE
)
sales_level <- make_rd_plot(
  sales_level_part,
  "Home Sale Prices by Side of Ward Boundary: Levels",
  "Segment-by-quarter adjusted real sale price",
  "../output/sales_levels_rd_main_style",
  FALSE
)

write_csv(
  bind_rows(
    rent_placebo$estimates,
    sales_placebo$estimates,
    rent_pruned$estimates,
    sales_pruned$estimates,
    rent_local_linear$estimates,
    sales_local_linear$estimates,
    rent_level$estimates,
    sales_level$estimates
  ),
  "../output/price_rd_main_style_plot_estimates.csv"
)
write_csv(
  bind_rows(
    rent_placebo$bins,
    sales_placebo$bins,
    rent_pruned$bins,
    sales_pruned$bins,
    rent_local_linear$bins,
    sales_local_linear$bins,
    rent_level$bins,
    sales_level$bins
  ),
  "../output/price_rd_main_style_plot_bins.csv"
)

message("Saved RD-style placebo, pruned, local-linear, and level-outcome plots.")
