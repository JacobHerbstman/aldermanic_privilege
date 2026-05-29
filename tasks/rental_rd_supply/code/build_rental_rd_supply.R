# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_rd_supply/code")
# bandwidth_ft <- 500
# bins_per_side <- 15

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth_ft, bins_per_side)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth_ft> <bins_per_side>", call. = FALSE)
}

bandwidth_ft <- as.numeric(cli_args[1])
bins_per_side <- as.integer(cli_args[2])
if (!is.finite(bandwidth_ft) || bandwidth_ft <= 0) {
  stop("bandwidth_ft must be positive.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be positive.", call. = FALSE)
}
bandwidth_label <- as.character(as.integer(round(bandwidth_ft)))

rent <- read_parquet("../input/rent_with_ward_distances.parquet") %>%
  as_tibble()
if (!"rent_panel_id" %in% names(rent)) {
  stop("Rental input must include rent_panel_id.", call. = FALSE)
}
if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
  stop("Rental input contains missing rent_panel_id values.", call. = FALSE)
}
if (anyDuplicated(rent$rent_panel_id) > 0) {
  stop("Rental input must be unique by rent_panel_id.", call. = FALSE)
}
if (!"signed_dist" %in% names(rent) && "signed_dist_m" %in% names(rent)) {
  rent <- rent %>% mutate(signed_dist = signed_dist_m / 0.3048)
}
if (!"signed_dist" %in% names(rent)) {
  stop("Rental input must include signed_dist in feet or signed_dist_m in meters.", call. = FALSE)
}

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
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    right = as.integer(signed_dist_ft >= 0),
    floorplan_month_key = as.character(rent_panel_id),
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft
  ) %>%
  filter(
    !is.na(file_date),
    year >= 2014,
    year <= 2022,
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= bandwidth_ft,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    !is.na(segment_id),
    segment_id != "",
    !is.na(ward_pair),
    !is.na(right)
  )

if (nrow(rent) == 0) {
  stop("No rental observations remain in the RD window.", call. = FALSE)
}

sample_defs <- tibble::tribble(
  ~sample, ~sample_label,
  "all", "All",
  "clean_location", "Clean location"
)

side_panels <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  sample_label <- sample_defs$sample_label[i]
  d_sample <- rent
  if (sample_name == "clean_location") {
    d_sample <- d_sample %>% filter(flag_clean_location_sample)
  }

  segment_month_template <- d_sample %>%
    distinct(segment_id, ward_pair, year_month) %>%
    tidyr::crossing(tibble(right = 0:1))

  side_counts <- d_sample %>%
    transmute(
      segment_id,
      ward_pair,
      year_month,
      right,
      unit_key = floorplan_month_key
    ) %>%
    filter(!is.na(unit_key), unit_key != "") %>%
    distinct(segment_id, ward_pair, year_month, right, unit_key) %>%
    count(segment_id, ward_pair, year_month, right, name = "n_units")

  side_panel <- segment_month_template %>%
    left_join(
      side_counts,
      by = c("segment_id", "ward_pair", "year_month", "right"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      n_units = as.integer(coalesce(n_units, 0L)),
      sample = sample_name,
      sample_label = sample_label,
      count_definition = "floorplan_month",
      count_label = "Floorplan-months",
      bandwidth_ft = bandwidth_ft
    ) %>%
    arrange(sample, count_definition, segment_id, year_month, right)

  if (anyDuplicated(side_panel[, c("sample", "count_definition", "segment_id", "year_month", "right")]) > 0) {
    stop("Supply side panel contains duplicate segment-month-side rows.", call. = FALSE)
  }
  side_panels[[length(side_panels) + 1]] <- side_panel
}

side_panel <- bind_rows(side_panels)

ppml_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  d_model <- side_panel %>%
    filter(sample == sample_defs$sample[i])
  if (nrow(d_model) < 100 || n_distinct(d_model$segment_id) < 2 || n_distinct(d_model$right) < 2) {
    next
  }
  model <- fepois(n_units ~ right | segment_id^year_month, data = d_model, cluster = ~segment_id)
  if (!"right" %in% names(coef(model))) {
    stop(sprintf("Supply PPML did not estimate right for %s.", sample_defs$sample[i]), call. = FALSE)
  }

  estimate <- coef(model)[["right"]]
  std_error <- se(model)[["right"]]
  ppml_rows[[length(ppml_rows) + 1]] <- tibble(
    sample = sample_defs$sample[i],
    sample_label = sample_defs$sample_label[i],
    count_definition = "floorplan_month",
    estimate = estimate,
    std_error = std_error,
    p_value = pvalue(model)[["right"]],
    pct_change = 100 * (exp(estimate) - 1),
    n_obs = model$nobs,
    n_segments = n_distinct(d_model$segment_id),
    n_ward_pairs = n_distinct(d_model$ward_pair),
    bandwidth_ft = bandwidth_ft
  )
}
ppml <- bind_rows(ppml_rows)

bin_width <- bandwidth_ft / bins_per_side
bin_count <- 2L * bins_per_side
bin_centers <- -bandwidth_ft + (seq_len(bin_count) - 0.5) * bin_width
bin_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  d_sample <- rent
  if (sample_name == "clean_location") {
    d_sample <- d_sample %>% filter(flag_clean_location_sample)
  }

  segment_months <- d_sample %>%
    distinct(segment_id, ward_pair, year_month)
  n_segment_months <- nrow(segment_months)
  if (n_segment_months == 0) {
    next
  }

  d_bins <- d_sample %>%
    mutate(
      bin_index = pmin(
        pmax(floor((signed_dist_ft + bandwidth_ft) / bin_width), 0L),
        bin_count - 1L
      ),
      bin_center = -bandwidth_ft + (bin_index + 0.5) * bin_width
    ) %>%
    distinct(segment_id, ward_pair, year_month, bin_center, floorplan_month_key) %>%
    count(bin_center, name = "total_floorplan_months")

  bin_rows[[length(bin_rows) + 1]] <- tibble(bin_center = bin_centers) %>%
    left_join(d_bins, by = "bin_center", relationship = "one-to-one") %>%
    mutate(
      total_floorplan_months = coalesce(total_floorplan_months, 0L),
      mean_floorplan_months = total_floorplan_months / n_segment_months,
      sample = sample_name,
      sample_label = sample_defs$sample_label[i],
      side = if_else(bin_center >= 0, "More Stringent", "Less Stringent"),
      n_segment_months = n_segment_months,
      bandwidth_ft = bandwidth_ft,
      bins_per_side = bins_per_side
    )
}
bins <- bind_rows(bin_rows)

level_plot_defs <- tibble::tribble(
  ~sample_name, ~title, ~output_path,
  "all", "Rental Listing Supply by Side of Ward Boundary", sprintf("../output/rental_rd_supply_levels_bw%s.pdf", bandwidth_label),
  "clean_location", "Rental Listing Supply by Side of Ward Boundary: Clean Location Sample", sprintf("../output/rental_rd_supply_levels_clean_location_bw%s.pdf", bandwidth_label)
)

for (i in seq_len(nrow(level_plot_defs))) {
  sample_name <- level_plot_defs$sample_name[i]
  d_plot <- bins %>%
    filter(sample == sample_name) %>%
    mutate(side = factor(side, levels = c("Less Stringent", "More Stringent")))
  if (nrow(d_plot) == 0) {
    stop(sprintf("No binned supply rows for sample %s.", sample_name), call. = FALSE)
  }

  ppml_result <- ppml %>%
    filter(sample == sample_name, count_definition == "floorplan_month")
  if (nrow(ppml_result) == 1L) {
    stars <- case_when(
      ppml_result$p_value < 0.01 ~ "***",
      ppml_result$p_value < 0.05 ~ "**",
      ppml_result$p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
    plot_subtitle <- sprintf(
      "PPML jump = %.1f%%%s (SE %.1f), N = %s, %.0fft",
      ppml_result$pct_change,
      stars,
      100 * ppml_result$std_error,
      format(ppml_result$n_obs, big.mark = ","),
      bandwidth_ft
    )
  } else {
    plot_subtitle <- sprintf("Floorplan-month counts per segment-month-bin, %.0fft", bandwidth_ft)
  }

  plot <- ggplot() +
    geom_vline(xintercept = 0, color = "gray30", linetype = "dashed", linewidth = 0.7) +
    geom_point(data = d_plot, aes(x = bin_center, y = mean_floorplan_months, color = side), size = 2.4) +
    scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = NULL) +
    labs(
      title = level_plot_defs$title[i],
      subtitle = plot_subtitle,
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = "Mean floorplan-month count"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggsave(
    level_plot_defs$output_path[i],
    plot,
    width = 8.6,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}
