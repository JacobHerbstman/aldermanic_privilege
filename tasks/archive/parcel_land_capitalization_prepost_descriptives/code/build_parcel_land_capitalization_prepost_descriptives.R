source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/parcel_land_capitalization_prepost_descriptives/code")
# in_prepost <- "../input/parcel_land_capitalization_prepost_2014_2016.parquet"
# out_dir <- "../output"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    in_prepost,
    out_dir
  )
}

if (length(cli_args) != 2) {
  stop(
    "FATAL: Script requires 2 args: <in_prepost_parquet> <out_dir>",
    call. = FALSE
  )
}

in_prepost <- cli_args[1]
out_dir <- cli_args[2]

stopifnot(file.exists(in_prepost))

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

safe_positive_median <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

save_plot_pair <- function(plot_obj, pdf_path, png_path, width, height) {
  ggsave(pdf_path, plot_obj, width = width, height = height, bg = "white")
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 220, bg = "white")
}

year_scope_labels <- c(
  admin_only = "Admin year only",
  admin_plus_fallback = "Admin + fallback"
)

bandwidth_labels <- c(
  `1000ft` = "1,000 ft",
  `500ft` = "500 ft"
)

treatment_sign_labels <- c(
  to_lenient = "To more lenient",
  no_change = "No change",
  to_stricter = "To stricter"
)

treatment_sign_colors <- c(
  "To more lenient" = "#1B9E77",
  "No change" = "#636363",
  "To stricter" = "#D95F02"
)

prepost <- arrow::read_parquet(in_prepost) %>%
  mutate(
    pin10 = as.character(pin10),
    in_500ft = as.logical(in_500ft),
    in_1000ft = as.logical(in_1000ft),
    switched_2015 = as.logical(switched_2015),
    valid_control_2015 = as.logical(valid_control_2015)
  )

if (nrow(prepost) == 0) {
  stop("Input prepost parquet is empty.", call. = FALSE)
}

if (anyDuplicated(prepost["pin10"]) > 0) {
  stop("Input prepost parquet has duplicate pin10 rows.", call. = FALSE)
}

if (!"admin_only_2014_2016" %in% names(prepost)) {
  stop("Input prepost parquet is missing admin_only_2014_2016.", call. = FALSE)
}

prepost <- prepost %>%
  mutate(admin_only_2014_2016 = as.logical(admin_only_2014_2016))

prepost_base <- bind_rows(
  prepost %>%
    mutate(year_scope = "admin_plus_fallback"),
  prepost %>%
    filter(admin_only_2014_2016 %in% TRUE) %>%
    mutate(year_scope = "admin_only")
)

prepost_views <- bind_rows(
  prepost_base %>%
    mutate(bandwidth = "1000ft"),
  prepost_base %>%
    filter(in_500ft %in% TRUE) %>%
    mutate(bandwidth = "500ft")
) %>%
  mutate(
    year_scope = factor(year_scope, levels = c("admin_only", "admin_plus_fallback")),
    year_scope_label = factor(year_scope_labels[as.character(year_scope)], levels = unname(year_scope_labels)),
    bandwidth = factor(bandwidth, levels = c("1000ft", "500ft")),
    bandwidth_label = factor(bandwidth_labels[as.character(bandwidth)], levels = unname(bandwidth_labels)),
    treatment_sign = factor(treatment_sign, levels = c("to_lenient", "no_change", "to_stricter")),
    treatment_sign_label = factor(treatment_sign_labels[as.character(treatment_sign)], levels = unname(treatment_sign_labels))
  )

treatment_summary <- prepost_views %>%
  group_by(year_scope, year_scope_label, bandwidth, bandwidth_label, treatment_sign, treatment_sign_label) %>%
  summarise(
    n_pin10 = n(),
    n_switchers = sum(switched_2015 %in% TRUE),
    n_valid_controls = sum(valid_control_2015 %in% TRUE),
    n_nonmissing_delta_log_land_psf = sum(!is.na(delta_log_land_psf_2016_minus_2014)),
    n_nonmissing_delta_log_land_sum = sum(!is.na(delta_log_land_sum_2016_minus_2014)),
    median_land_psf_2014 = safe_positive_median(land_psf_2014),
    median_land_psf_2016 = safe_positive_median(land_psf_2016),
    median_land_sum_2014 = safe_positive_median(land_sum_2014),
    median_land_sum_2016 = safe_positive_median(land_sum_2016),
    median_delta_log_land_psf = stats::median(delta_log_land_psf_2016_minus_2014, na.rm = TRUE),
    mean_delta_log_land_psf = mean(delta_log_land_psf_2016_minus_2014, na.rm = TRUE),
    median_delta_log_land_sum = stats::median(delta_log_land_sum_2016_minus_2014, na.rm = TRUE),
    mean_delta_log_land_sum = mean(delta_log_land_sum_2016_minus_2014, na.rm = TRUE),
    median_delta_land_share = stats::median(delta_land_share_2016_minus_2014, na.rm = TRUE),
    share_missing_delta_log_land_psf = mean(is.na(delta_log_land_psf_2016_minus_2014)),
    share_missing_delta_log_land_sum = mean(is.na(delta_log_land_sum_2016_minus_2014)),
    share_building_positive_2016 = mean(building_positive_2016, na.rm = TRUE),
    share_gained_positive_building_2014_2016 = mean(gained_positive_building_2014_2016, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_scope, bandwidth, treatment_sign)

distribution_quantiles <- prepost_views %>%
  group_by(year_scope, year_scope_label, bandwidth, bandwidth_label, treatment_sign, treatment_sign_label) %>%
  summarise(
    p01_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.01),
    p05_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.05),
    p25_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.25),
    p50_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.50),
    p75_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.75),
    p95_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.95),
    p99_delta_log_land_psf = safe_quantile(delta_log_land_psf_2016_minus_2014, 0.99),
    p01_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.01),
    p05_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.05),
    p25_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.25),
    p50_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.50),
    p75_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.75),
    p95_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.95),
    p99_delta_log_land_sum = safe_quantile(delta_log_land_sum_2016_minus_2014, 0.99),
    .groups = "drop"
  ) %>%
  arrange(year_scope, bandwidth, treatment_sign)

level_summary <- bind_rows(
  prepost_views %>%
    transmute(
      year_scope, year_scope_label, bandwidth, bandwidth_label,
      treatment_sign, treatment_sign_label,
      year = 2014L,
      land_psf = land_psf_2014,
      land_sum = land_sum_2014
    ),
  prepost_views %>%
    transmute(
      year_scope, year_scope_label, bandwidth, bandwidth_label,
      treatment_sign, treatment_sign_label,
      year = 2016L,
      land_psf = land_psf_2016,
      land_sum = land_sum_2016
    )
) %>%
  group_by(year_scope, year_scope_label, bandwidth, bandwidth_label, treatment_sign, treatment_sign_label, year) %>%
  summarise(
    n_pin10 = n(),
    n_positive_land_psf = sum(land_psf > 0, na.rm = TRUE),
    n_positive_land_sum = sum(land_sum > 0, na.rm = TRUE),
    median_land_psf = safe_positive_median(land_psf),
    median_land_sum = safe_positive_median(land_sum),
    .groups = "drop"
  ) %>%
  mutate(year_label = factor(year, levels = c(2014L, 2016L)))

psf_density_data <- prepost_views %>%
  filter(is.finite(delta_log_land_psf_2016_minus_2014), !is.na(delta_log_land_psf_2016_minus_2014))

psf_density_limits <- stats::quantile(
  psf_density_data$delta_log_land_psf_2016_minus_2014,
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)

delta_log_land_psf_plot <- ggplot(
  psf_density_data,
  aes(x = delta_log_land_psf_2016_minus_2014, color = treatment_sign_label, fill = treatment_sign_label)
) +
  geom_density(alpha = 0.12, linewidth = 0.8, adjust = 1.1) +
  facet_grid(year_scope_label ~ bandwidth_label) +
  coord_cartesian(xlim = psf_density_limits) +
  scale_color_manual(values = treatment_sign_colors) +
  scale_fill_manual(values = treatment_sign_colors) +
  labs(
    title = "2014 to 2016 change in log land value per square foot",
    subtitle = "Baseline-empty paired parcel sample near pre-2015 ward boundaries",
    x = "Delta log land value per square foot",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

sum_density_data <- prepost_views %>%
  filter(is.finite(delta_log_land_sum_2016_minus_2014), !is.na(delta_log_land_sum_2016_minus_2014))

sum_density_limits <- stats::quantile(
  sum_density_data$delta_log_land_sum_2016_minus_2014,
  probs = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)

delta_log_land_sum_plot <- ggplot(
  sum_density_data,
  aes(x = delta_log_land_sum_2016_minus_2014, color = treatment_sign_label, fill = treatment_sign_label)
) +
  geom_density(alpha = 0.12, linewidth = 0.8, adjust = 1.1) +
  facet_grid(year_scope_label ~ bandwidth_label) +
  coord_cartesian(xlim = sum_density_limits) +
  scale_color_manual(values = treatment_sign_colors) +
  scale_fill_manual(values = treatment_sign_colors) +
  labs(
    title = "2014 to 2016 change in log land assessed value",
    subtitle = "Baseline-empty paired parcel sample near pre-2015 ward boundaries",
    x = "Delta log land assessed value",
    y = "Density",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

median_land_psf_levels_plot <- ggplot(
  level_summary,
  aes(x = year_label, y = median_land_psf, color = treatment_sign_label, group = treatment_sign_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_grid(year_scope_label ~ bandwidth_label) +
  scale_color_manual(values = treatment_sign_colors) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01, big.mark = ",")) +
  labs(
    title = "Median positive land value per square foot in 2014 and 2016",
    subtitle = "Baseline-empty paired parcel sample near pre-2015 ward boundaries",
    x = NULL,
    y = "Median land value per square foot (log scale)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

transition_plot_data <- treatment_summary %>%
  select(
    year_scope_label, bandwidth_label, treatment_sign_label,
    share_building_positive_2016, share_gained_positive_building_2014_2016
  ) %>%
  tidyr::pivot_longer(
    cols = c(share_building_positive_2016, share_gained_positive_building_2014_2016),
    names_to = "metric",
    values_to = "share"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("share_building_positive_2016", "share_gained_positive_building_2014_2016"),
      labels = c("Positive building value in 2016", "Gained positive building value, 2014-2016")
    )
  )

transition_rates_plot <- ggplot(
  transition_plot_data,
  aes(x = treatment_sign_label, y = share, fill = metric)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  facet_grid(year_scope_label ~ bandwidth_label) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Building-value transitions between 2014 and 2016",
    subtitle = "Baseline-empty paired parcel sample near pre-2015 ward boundaries",
    x = NULL,
    y = "Share of parcels",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

readr::write_csv(
  treatment_summary,
  file.path(out_dir, "parcel_land_capitalization_prepost_treatment_summary.csv")
)
readr::write_csv(
  distribution_quantiles,
  file.path(out_dir, "parcel_land_capitalization_prepost_distribution_quantiles.csv")
)
readr::write_csv(
  level_summary,
  file.path(out_dir, "parcel_land_capitalization_prepost_level_summary.csv")
)

save_plot_pair(
  delta_log_land_psf_plot,
  file.path(out_dir, "parcel_land_capitalization_prepost_delta_log_land_psf.pdf"),
  file.path(out_dir, "parcel_land_capitalization_prepost_delta_log_land_psf.png"),
  width = 10.5,
  height = 7.0
)

save_plot_pair(
  delta_log_land_sum_plot,
  file.path(out_dir, "parcel_land_capitalization_prepost_delta_log_land_sum.pdf"),
  file.path(out_dir, "parcel_land_capitalization_prepost_delta_log_land_sum.png"),
  width = 10.5,
  height = 7.0
)

save_plot_pair(
  median_land_psf_levels_plot,
  file.path(out_dir, "parcel_land_capitalization_prepost_median_land_psf_levels.pdf"),
  file.path(out_dir, "parcel_land_capitalization_prepost_median_land_psf_levels.png"),
  width = 10.5,
  height = 7.0
)

save_plot_pair(
  transition_rates_plot,
  file.path(out_dir, "parcel_land_capitalization_prepost_transition_rates.pdf"),
  file.path(out_dir, "parcel_land_capitalization_prepost_transition_rates.png"),
  width = 10.5,
  height = 7.2
)

message(sprintf("Prepared %s", file.path(out_dir, "parcel_land_capitalization_prepost_treatment_summary.csv")))
message(sprintf("Prepared %s", file.path(out_dir, "parcel_land_capitalization_prepost_distribution_quantiles.csv")))
message(sprintf("Prepared %s", file.path(out_dir, "parcel_land_capitalization_prepost_level_summary.csv")))
