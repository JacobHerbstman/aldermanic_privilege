# Run true and shifted-boundary placebo RDs for rents and sales.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rd_placebo_boundaries/code")
# bandwidth_ft <- 500
# placebo_cutoffs_ft <- "-500,0,500"

source("../../setup_environment/code/packages.R")
source("../../_lib/amenity_distance_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, placebo_cutoffs_ft)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth_ft> <placebo_cutoffs_ft>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
placebo_cutoffs_ft <- as.numeric(strsplit(cli_args[2], ",", fixed = TRUE)[[1]])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (any(!is.finite(placebo_cutoffs_ft))) {
  stop("placebo_cutoffs_ft must be a comma-separated numeric list.", call. = FALSE)
}
if (!0 %in% placebo_cutoffs_ft) {
  stop("placebo_cutoffs_ft must include 0 for the true boundary.", call. = FALSE)
}

message("=== RD placebo boundaries ===")
message(sprintf("Bandwidth: %.0fft", bandwidth_ft))
message(sprintf("Cutoffs: %s ft", paste(placebo_cutoffs_ft, collapse = ", ")))

rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
  as_tibble()
sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()

if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental input must include rent_panel_id.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental input is not unique by rent_panel_id.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist or signed_dist_m.", call. = FALSE)
}

rent_flag_cols <- c(
  "flag_location_questionable",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft"
)
for (flag_col in rent_flag_cols) {
  if (!flag_col %in% names(rent)) {
    rent[[flag_col]] <- FALSE
  }
  rent[[flag_col]] <- coalesce(as.logical(rent[[flag_col]]), FALSE)
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    score_gap = abs(strictness_own - strictness_neighbor),
    building_type_factor = factor(coalesce(building_type_clean, "other")),
    log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_beds = if_else(is.finite(beds) & beds > 0, log(beds), NA_real_),
    log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft
  ) %>%
  filter(
    flag_clean_location_sample,
    year >= 2014,
    year <= 2022,
    is.finite(rent_price),
    rent_price > 0,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= max(abs(placebo_cutoffs_ft)) + bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    is.finite(score_gap),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    is.finite(longitude),
    is.finite(latitude)
  )

message(sprintf("Rental placebo sample before amenities: %s rows.", format(nrow(rent), big.mark = ",")))
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

if (!"signed_dist" %in% names(sales) && "signed_dist_m" %in% names(sales)) {
  sales <- sales %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(sales)) {
  stop("Sales input must include signed_dist or signed_dist_m.", call. = FALSE)
}

sales <- sales %>%
  mutate(
    sale_date = as.Date(sale_date),
    year = lubridate::year(sale_date),
    year_quarter = paste0(year, "-Q", lubridate::quarter(sale_date)),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    score_gap = abs(strictness_own - strictness_neighbor),
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
    abs(signed_dist_ft) <= max(abs(placebo_cutoffs_ft)) + bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    is.finite(score_gap),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair)
  )

rent_controls <- c(
  "log_sqft",
  "log_beds",
  "log_baths",
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft"
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

add_score_gap_split <- function(data, controls) {
  base_pairs <- data %>%
    filter(
      abs(signed_dist_ft) <= bandwidth_ft,
      if_all(all_of(controls), ~ is.finite(.x))
    ) %>%
    group_by(ward_pair) %>%
    summarise(score_gap_pair = median(score_gap, na.rm = TRUE), .groups = "drop")

  score_gap_median <- median(base_pairs$score_gap_pair, na.rm = TRUE)
  if (!is.finite(score_gap_median)) {
    stop("Could not calculate score-gap median.", call. = FALSE)
  }

  data %>%
    left_join(base_pairs, by = "ward_pair", relationship = "many-to-one") %>%
    mutate(
      score_gap_median = score_gap_median,
      score_gap_split = case_when(
        !is.finite(score_gap_pair) ~ "Missing score gap",
        score_gap_pair > score_gap_median ~ "Above median score gap",
        TRUE ~ "Below median score gap"
      )
    )
}

rent <- add_score_gap_split(rent, rent_controls)
sales <- add_score_gap_split(sales, sales_controls)

fit_cutoff <- function(
    data,
    dataset,
    outcome,
    cut_ft,
    time_var,
    controls,
    include_building_type = FALSE,
    split_var = NULL,
    split_value = NULL) {
  d <- data %>%
    mutate(
      placebo_running_ft = signed_dist_ft - cut_ft,
      placebo_right = as.integer(placebo_running_ft >= 0)
    ) %>%
    filter(
      abs(placebo_running_ft) <= bandwidth_ft,
      if_all(all_of(controls), ~ is.finite(.x))
    )
  if (!is.null(split_var)) {
    d <- d %>%
      filter(.data[[split_var]] == split_value)
  }
  split_label <- if (is.null(split_value)) "All score gaps" else split_value

  support <- tibble(
    dataset = dataset,
    cutoff_ft = cut_ft,
    cutoff_label = if_else(cut_ft == 0, "true boundary", sprintf("%+dft placebo", as.integer(cut_ft))),
    split = split_label,
    score_gap_median = if ("score_gap_median" %in% names(d)) suppressWarnings(first(na.omit(d$score_gap_median))) else NA_real_,
    n_obs = nrow(d),
    n_right = sum(d$placebo_right == 1, na.rm = TRUE),
    n_left = sum(d$placebo_right == 0, na.rm = TRUE),
    n_segments = n_distinct(d$segment_id),
    n_ward_pairs = n_distinct(d$ward_pair),
    n_segment_time_cells = n_distinct(paste(d$segment_id, d[[time_var]]))
  )

  if (nrow(d) < 1000 || n_distinct(d$placebo_right) < 2 || n_distinct(d$segment_id) < 2) {
    return(list(
      estimate = support %>%
        mutate(
          estimate = NA_real_,
          std_error = NA_real_,
          p_value = NA_real_,
          pct_change = NA_real_,
          pct_ci_low = NA_real_,
          pct_ci_high = NA_real_
        ),
      support = support
    ))
  }

  rhs <- paste(c("placebo_right", controls), collapse = " + ")
  if (include_building_type && n_distinct(d$building_type_factor) > 1) {
    rhs <- paste(rhs, "building_type_factor", sep = " + ")
  }
  model <- feols(
    as.formula(paste0("log(", outcome, ") ~ ", rhs, " | segment_id^", time_var)),
    data = d,
    cluster = ~segment_id
  )
  ct <- coeftable(model)
  if (!"placebo_right" %in% rownames(ct)) {
    stop(sprintf("Model failed to estimate placebo_right for %s cutoff %.0f.", dataset, cut_ft), call. = FALSE)
  }

  estimate <- unname(ct["placebo_right", "Estimate"])
  std_error <- unname(ct["placebo_right", "Std. Error"])
  out <- support %>%
    mutate(
      estimate = estimate,
      std_error = std_error,
      p_value = unname(ct["placebo_right", "Pr(>|t|)"]),
      pct_change = 100 * (exp(estimate) - 1),
      pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
      pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1)
    )
  list(estimate = out, support = support)
}

rent_results <- lapply(placebo_cutoffs_ft, function(cut_i) {
  fit_cutoff(rent, "RentHub rents", "rent_price", cut_i, "year_month", rent_controls, TRUE)
})
sales_results <- lapply(placebo_cutoffs_ft, function(cut_i) {
  fit_cutoff(sales, "Home sales", "sale_price", cut_i, "year_quarter", sales_controls, FALSE)
})
split_values <- c("Below median score gap", "Above median score gap")
rent_split_results <- unlist(lapply(split_values, function(split_i) {
  lapply(placebo_cutoffs_ft, function(cut_i) {
    fit_cutoff(
      rent,
      "RentHub rents",
      "rent_price",
      cut_i,
      "year_month",
      rent_controls,
      TRUE,
      "score_gap_split",
      split_i
    )
  })
}), recursive = FALSE)
sales_split_results <- unlist(lapply(split_values, function(split_i) {
  lapply(placebo_cutoffs_ft, function(cut_i) {
    fit_cutoff(
      sales,
      "Home sales",
      "sale_price",
      cut_i,
      "year_quarter",
      sales_controls,
      FALSE,
      "score_gap_split",
      split_i
    )
  })
}), recursive = FALSE)

estimates <- bind_rows(
  bind_rows(lapply(rent_results, `[[`, "estimate")),
  bind_rows(lapply(sales_results, `[[`, "estimate"))
) %>%
  mutate(
    bandwidth_ft = bandwidth_ft,
    cutoff_label = factor(
      cutoff_label,
      levels = c(
        sprintf("%+dft placebo", as.integer(min(placebo_cutoffs_ft))),
        "true boundary",
        sprintf("%+dft placebo", as.integer(max(placebo_cutoffs_ft)))
      )
    )
  )
support <- bind_rows(
  bind_rows(lapply(rent_results, `[[`, "support")),
  bind_rows(lapply(sales_results, `[[`, "support"))
) %>%
  mutate(bandwidth_ft = bandwidth_ft)
split_estimates <- bind_rows(
  bind_rows(lapply(rent_split_results, `[[`, "estimate")),
  bind_rows(lapply(sales_split_results, `[[`, "estimate"))
) %>%
  mutate(
    bandwidth_ft = bandwidth_ft,
    cutoff_label = factor(
      cutoff_label,
      levels = c(
        sprintf("%+dft placebo", as.integer(min(placebo_cutoffs_ft))),
        "true boundary",
        sprintf("%+dft placebo", as.integer(max(placebo_cutoffs_ft)))
      )
    ),
    split = factor(split, levels = split_values)
  )
split_support <- bind_rows(
  bind_rows(lapply(rent_split_results, `[[`, "support")),
  bind_rows(lapply(sales_split_results, `[[`, "support"))
) %>%
  mutate(bandwidth_ft = bandwidth_ft)

write_csv(estimates, "../output/rd_placebo_boundary_estimates.csv")
write_csv(support, "../output/rd_placebo_boundary_support.csv")
write_csv(split_estimates, "../output/rd_placebo_boundary_score_gap_split_estimates.csv")
write_csv(split_support, "../output/rd_placebo_boundary_score_gap_split_support.csv")

plot_one <- function(data, dataset, title) {
  d <- data %>%
    filter(.data$dataset == .env$dataset)
  ggplot(d, aes(x = cutoff_label, y = pct_change, ymin = pct_ci_low, ymax = pct_ci_high)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
    geom_pointrange(color = "#1f77b4", linewidth = 0.5) +
    labs(
      title = title,
      subtitle = "500ft bandwidth; segment-time FE; hedonics + amenities",
      x = NULL,
      y = "Estimated jump at cutoff (%)"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

rent_plot <- plot_one(estimates, "RentHub rents", "RentHub RD Placebo Boundaries")
sales_plot <- plot_one(estimates, "Home sales", "Home Sales RD Placebo Boundaries")

plot_split <- function(data, dataset, title) {
  d <- data %>%
    filter(.data$dataset == .env$dataset)
  ggplot(d, aes(x = cutoff_label, y = pct_change, ymin = pct_ci_low, ymax = pct_ci_high, color = split)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
    geom_pointrange(position = position_dodge(width = 0.45), linewidth = 0.45) +
    labs(
      title = title,
      subtitle = "500ft bandwidth; pair-level median absolute alderman score gap split",
      x = NULL,
      y = "Estimated jump at cutoff (%)",
      color = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

rent_split_plot <- plot_split(split_estimates, "RentHub rents", "RentHub RD Placebo Boundaries By Score-Gap Split")
sales_split_plot <- plot_split(split_estimates, "Home sales", "Home Sales RD Placebo Boundaries By Score-Gap Split")

ggsave("../output/rent_placebo_boundaries.pdf", rent_plot, width = 7, height = 4.8, dpi = 300, bg = "white")
ggsave("../output/rent_placebo_boundaries.png", rent_plot, width = 7, height = 4.8, dpi = 220, bg = "white")
ggsave("../output/sales_placebo_boundaries.pdf", sales_plot, width = 7, height = 4.8, dpi = 300, bg = "white")
ggsave("../output/sales_placebo_boundaries.png", sales_plot, width = 7, height = 4.8, dpi = 220, bg = "white")
ggsave("../output/rent_placebo_boundaries_score_gap_split.pdf", rent_split_plot, width = 8.2, height = 5, dpi = 300, bg = "white")
ggsave("../output/rent_placebo_boundaries_score_gap_split.png", rent_split_plot, width = 8.2, height = 5, dpi = 220, bg = "white")
ggsave("../output/sales_placebo_boundaries_score_gap_split.pdf", sales_split_plot, width = 8.2, height = 5, dpi = 300, bg = "white")
ggsave("../output/sales_placebo_boundaries_score_gap_split.png", sales_split_plot, width = 8.2, height = 5, dpi = 220, bg = "white")

message("Saved RD placebo boundary outputs.")
