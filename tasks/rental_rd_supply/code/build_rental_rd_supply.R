# Build RentHub advertised-supply diagnostics around ward-boundary RD samples.

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

message(sprintf("=== Listed Rental Availability RD | bandwidth=%sft | bins per side=%d ===", bandwidth_label, bins_per_side))

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
    property_month_key = as.character(property_key),
    address_missing_flag = coalesce(as.logical(address_missing), FALSE),
    address_stem_month_key = if_else(address_missing_flag, NA_character_, as.character(address_stem)),
    flag_clean_location_sample = !flag_location_questionable &
      !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward &
      !flag_modal_changes_pair &
      !flag_modal_dist_diff_gt100ft,
    flag_no_modal_pair_change_sample = !flag_modal_assignment_missing & !flag_modal_changes_pair,
    flag_no_modal_ward_change_sample = !flag_modal_assignment_missing &
      !flag_modal_changes_ward &
      !flag_modal_changes_neighbor_ward,
    flag_no_questionable_address_sample = !flag_location_questionable
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
  "clean_location", "Clean location",
  "no_modal_pair_change", "No modal pair change",
  "no_modal_ward_change", "No modal ward change",
  "no_questionable_address", "No questionable address"
)

count_defs <- tibble::tribble(
  ~count_definition, ~count_label, ~key_col,
  "floorplan_month", "Floorplan-months", "floorplan_month_key",
  "property_month", "Property-months", "property_month_key",
  "address_stem_month", "Address-stem-months", "address_stem_month_key"
)

filter_sample <- function(df, sample_name) {
  if (sample_name == "all") {
    return(df)
  }
  if (sample_name == "clean_location") {
    return(df %>% filter(flag_clean_location_sample))
  }
  if (sample_name == "no_modal_pair_change") {
    return(df %>% filter(flag_no_modal_pair_change_sample))
  }
  if (sample_name == "no_modal_ward_change") {
    return(df %>% filter(flag_no_modal_ward_change_sample))
  }
  if (sample_name == "no_questionable_address") {
    return(df %>% filter(flag_no_questionable_address_sample))
  }
  stop(sprintf("Unknown sample: %s", sample_name), call. = FALSE)
}

side_panels <- list()
sample_summary <- list()
for (i in seq_len(nrow(sample_defs))) {
  sample_name <- sample_defs$sample[i]
  sample_label <- sample_defs$sample_label[i]
  d_sample <- filter_sample(rent, sample_name)
  message(sprintf("Supply sample %s: %s rows", sample_name, format(nrow(d_sample), big.mark = ",")))

  segment_month_template <- d_sample %>%
    distinct(segment_id, ward_pair, year_month) %>%
    tidyr::crossing(tibble(right = 0:1))

  for (j in seq_len(nrow(count_defs))) {
    key_col <- count_defs$key_col[j]
    d_count <- d_sample %>%
      transmute(
        segment_id,
        ward_pair,
        year_month,
        right,
        unit_key = .data[[key_col]]
      ) %>%
      filter(!is.na(unit_key), unit_key != "")

    side_counts <- d_count %>%
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
        count_definition = count_defs$count_definition[j],
        count_label = count_defs$count_label[j],
        bandwidth_ft = bandwidth_ft
      ) %>%
      arrange(sample, count_definition, segment_id, year_month, right)

    if (anyDuplicated(side_panel[, c("sample", "count_definition", "segment_id", "year_month", "right")]) > 0) {
      stop("Supply side panel contains duplicate segment-month-side rows.", call. = FALSE)
    }

    pair_month_diag <- side_panel %>%
      group_by(segment_id, year_month) %>%
      summarise(n_sides_with_supply = sum(n_units > 0), .groups = "drop")

    sample_summary[[length(sample_summary) + 1]] <- tibble(
      sample = sample_name,
      sample_label = sample_label,
      count_definition = count_defs$count_definition[j],
      count_label = count_defs$count_label[j],
      input_rows = nrow(d_sample),
      side_panel_rows = nrow(side_panel),
      segment_month_cells = nrow(pair_month_diag),
      n_segments = n_distinct(side_panel$segment_id),
      n_ward_pairs = n_distinct(side_panel$ward_pair),
      share_zero_side_cells = mean(side_panel$n_units == 0),
      share_single_sided_segment_month = mean(pair_month_diag$n_sides_with_supply == 1),
      mean_units_per_side_cell = mean(side_panel$n_units),
      bandwidth_ft = bandwidth_ft
    )

    side_panels[[length(side_panels) + 1]] <- side_panel
  }
}

side_panel <- bind_rows(side_panels)
write_parquet(
  as.data.frame(side_panel),
  sprintf("../output/rental_rd_supply_side_panel_bw%s.parquet", bandwidth_label)
)

sample_summary <- bind_rows(sample_summary)
write_csv(sample_summary, sprintf("../output/rental_rd_supply_sample_summary_bw%s.csv", bandwidth_label))

side_means <- side_panel %>%
  group_by(sample, sample_label, count_definition, count_label, right) %>%
  summarise(
    mean_units = mean(n_units),
    se_units = sd(n_units) / sqrt(n()),
    p50_units = median(n_units),
    share_zero = mean(n_units == 0),
    n_side_cells = n(),
    .groups = "drop"
  ) %>%
  mutate(
    side = if_else(right == 1L, "More Stringent", "Less Stringent"),
    ci_low = mean_units - 1.96 * se_units,
    ci_high = mean_units + 1.96 * se_units
  )
write_csv(side_means, sprintf("../output/rental_rd_supply_side_means_bw%s.csv", bandwidth_label))

side_means_plot <- side_means %>%
  filter(count_definition %in% c("floorplan_month", "property_month")) %>%
  mutate(sample_label = factor(sample_label, levels = sample_defs$sample_label))

plot_side_means <- ggplot(side_means_plot, aes(x = sample_label, y = mean_units, color = side)) +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.45)) +
  facet_wrap(~count_label, scales = "free_y") +
  scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = NULL) +
  labs(
    title = "Listed Rental Availability By Side Of Ward Boundary",
    subtitle = sprintf("Mean count per segment-month-side cell within %.0fft", bandwidth_ft),
    x = NULL,
    y = "Mean advertised supply count"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave(
  sprintf("../output/rental_rd_supply_side_means_bw%s.pdf", bandwidth_label),
  plot_side_means,
  width = 9.5,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

ppml_rows <- list()
for (i in seq_len(nrow(sample_defs))) {
  for (j in seq_len(nrow(count_defs))) {
    d_model <- side_panel %>%
      filter(sample == sample_defs$sample[i], count_definition == count_defs$count_definition[j])
    if (nrow(d_model) < 100 || n_distinct(d_model$segment_id) < 2 || n_distinct(d_model$right) < 2) {
      next
    }
    model <- fepois(n_units ~ right | segment_id^year_month, data = d_model, cluster = ~segment_id)
    if (!"right" %in% names(coef(model))) {
      stop(sprintf("Supply PPML did not estimate right for %s / %s.", sample_defs$sample[i], count_defs$count_definition[j]), call. = FALSE)
    }

    estimate <- coef(model)[["right"]]
    std_error <- se(model)[["right"]]
    ppml_rows[[length(ppml_rows) + 1]] <- tibble(
      sample = sample_defs$sample[i],
      sample_label = sample_defs$sample_label[i],
      count_definition = count_defs$count_definition[j],
      count_label = count_defs$count_label[j],
      estimate = estimate,
      std_error = std_error,
      p_value = pvalue(model)[["right"]],
      pct_change = 100 * (exp(estimate) - 1),
      pct_ci_low = 100 * (exp(estimate - 1.96 * std_error) - 1),
      pct_ci_high = 100 * (exp(estimate + 1.96 * std_error) - 1),
      n_obs = model$nobs,
      n_segments = n_distinct(d_model$segment_id),
      n_ward_pairs = n_distinct(d_model$ward_pair),
      mean_units_per_side_cell = mean(d_model$n_units),
      share_zero_side_cells = mean(d_model$n_units == 0),
      bandwidth_ft = bandwidth_ft
    )
  }
}

ppml <- bind_rows(ppml_rows) %>%
  mutate(
    sample_label = factor(sample_label, levels = sample_defs$sample_label),
    count_label = factor(count_label, levels = count_defs$count_label)
  )
write_csv(ppml, sprintf("../output/rental_rd_supply_ppml_bw%s.csv", bandwidth_label))

plot_ppml <- ppml %>%
  filter(count_definition %in% c("floorplan_month", "property_month")) %>%
  ggplot(aes(x = sample_label, y = pct_change, ymin = pct_ci_low, ymax = pct_ci_high, color = count_label)) +
  geom_hline(yintercept = 0, color = "gray55", linetype = "dotted") +
  geom_pointrange(position = position_dodge(width = 0.45), linewidth = 0.45) +
  labs(
    title = "Listed Rental Availability Count Gap",
    subtitle = sprintf("PPML stricter-side effect within %.0fft; segment-by-month FE", bandwidth_ft),
    x = NULL,
    y = "Percent difference on stricter side",
    color = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave(
  sprintf("../output/rental_rd_supply_ppml_bw%s.pdf", bandwidth_label),
  plot_ppml,
  width = 9.5,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

bin_width <- bandwidth_ft / bins_per_side
bin_count <- 2L * bins_per_side
bin_centers <- -bandwidth_ft + (seq_len(bin_count) - 0.5) * bin_width
bin_rows <- list()
for (sample_name in c("all", "clean_location")) {
  d_sample <- filter_sample(rent, sample_name)
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
      sample_label = sample_defs$sample_label[match(sample_name, sample_defs$sample)],
      side = if_else(bin_center >= 0, "More Stringent", "Less Stringent"),
      n_segment_months = n_segment_months,
      bandwidth_ft = bandwidth_ft,
      bins_per_side = bins_per_side
    )
}

bins <- bind_rows(bin_rows)
write_csv(bins, sprintf("../output/rental_rd_supply_bins_bw%s.csv", bandwidth_label))

format_ppml_subtitle <- function(sample_name) {
  result <- ppml %>%
    filter(sample == sample_name, count_definition == "floorplan_month")
  if (nrow(result) != 1L) {
    return(sprintf("Floorplan-month counts per segment-month-bin, %.0fft", bandwidth_ft))
  }
  stars <- case_when(
    result$p_value < 0.01 ~ "***",
    result$p_value < 0.05 ~ "**",
    result$p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
  sprintf(
    "PPML jump = %.1f%%%s (SE %.1f), N = %s, %.0fft",
    result$pct_change,
    stars,
    100 * result$std_error,
    format(result$n_obs, big.mark = ","),
    bandwidth_ft
  )
}

plot_supply_levels <- function(sample_name, title, output_path) {
  d_plot <- bins %>%
    filter(sample == sample_name) %>%
    mutate(side = factor(side, levels = c("Less Stringent", "More Stringent")))
  if (nrow(d_plot) == 0) {
    stop(sprintf("No binned supply rows for sample %s.", sample_name), call. = FALSE)
  }

  line_data <- bind_rows(
    tibble(
      x = c(-bandwidth_ft, 0),
      y = mean(d_plot$mean_floorplan_months[d_plot$side == "Less Stringent"], na.rm = TRUE),
      side = "Less Stringent"
    ),
    tibble(
      x = c(0, bandwidth_ft),
      y = mean(d_plot$mean_floorplan_months[d_plot$side == "More Stringent"], na.rm = TRUE),
      side = "More Stringent"
    )
  )

  plot <- ggplot() +
    geom_vline(xintercept = 0, color = "gray30", linetype = "dashed", linewidth = 0.7) +
    geom_point(data = d_plot, aes(x = bin_center, y = mean_floorplan_months, color = side), size = 2.4) +
    geom_line(data = line_data, aes(x = x, y = y, color = side), linewidth = 1.1) +
    scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = NULL) +
    labs(
      title = title,
      subtitle = format_ppml_subtitle(sample_name),
      x = "Distance to ward boundary (feet; positive = more stringent side)",
      y = "Mean floorplan-month count"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggsave(
    output_path,
    plot,
    width = 8.6,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}

plot_supply_levels(
  "all",
  "Rental Listing Supply by Side of Ward Boundary",
  sprintf("../output/rental_rd_supply_levels_bw%s.pdf", bandwidth_label)
)
plot_supply_levels(
  "clean_location",
  "Rental Listing Supply by Side of Ward Boundary: Clean Location Sample",
  sprintf("../output/rental_rd_supply_levels_clean_location_bw%s.pdf", bandwidth_label)
)

plot_bins <- ggplot(bins, aes(x = bin_center, y = mean_floorplan_months, color = side)) +
  geom_vline(xintercept = 0, color = "gray30", linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_wrap(~sample_label, ncol = 1) +
  scale_color_manual(values = c("Less Stringent" = "#1f77b4", "More Stringent" = "#d62728"), name = NULL) +
  labs(
    title = "Listed Floorplan-Month Availability By Distance Bin",
    subtitle = sprintf("Mean floorplan-months per segment-month-bin within %.0fft", bandwidth_ft),
    x = "Distance to ward boundary (feet; positive = more stringent side)",
    y = "Mean floorplan-month count"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(
  sprintf("../output/rental_rd_supply_bins_bw%s.pdf", bandwidth_label),
  plot_bins,
  width = 8,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

message("Saved RentHub supply diagnostics.")
