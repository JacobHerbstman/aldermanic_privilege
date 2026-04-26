source("../../setup_environment/code/packages.R")

options(dplyr.summarise.inform = FALSE)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_block_land_value_panel_models/code")
# output_dir <- "../output"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(output_dir)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <output_dir>", call. = FALSE)
}

output_dir <- cli_args[1]
stopifnot(dir.exists(output_dir))

coefficient_files <- list.files(output_dir, pattern = "^event_coefficients_.*\\.csv$", full.names = TRUE)
if (length(coefficient_files) == 0) {
  stop("No event coefficient CSV files found in output_dir.", call. = FALSE)
}

coefficients <- purrr::map_dfr(coefficient_files, readr::read_csv, show_col_types = FALSE) %>%
  dplyr::mutate(
    event_time = as.integer(event_time),
    year_window = dplyr::coalesce(year_window, "core"),
    fe_spec = dplyr::coalesce(fe_spec, "segment_year"),
    control_spec = dplyr::coalesce(control_spec, "none"),
    year_window_label = dplyr::case_when(
      year_window == "core" ~ "Core window",
      year_window == "long_pre" ~ "Long pre-period",
      TRUE ~ year_window
    ),
    model_spec_label = dplyr::case_when(
      fe_spec == "segment_year" & control_spec == "none" ~ "Segment-year FE",
      fe_spec == "ward_pair_year" & control_spec == "none" ~ "Ward-pair-year FE",
      fe_spec == "segment_year" & control_spec == "baseline_x_year" ~ "Segment-year FE + controls",
      fe_spec == "ward_pair_year" & control_spec == "baseline_x_year" ~ "Ward-pair-year FE + controls",
      TRUE ~ paste(fe_spec, control_spec, sep = " / ")
    ),
    sample_label = dplyr::case_when(
      sample_scope == "developable_core" ~ "Developable core",
      sample_scope == "history_vacant_core" ~ "History vacant core",
      TRUE ~ sample_scope
    ),
    treatment_group = factor(treatment_group, levels = c("To more lenient", "To stricter", "Strictness change")),
    sample_label = factor(sample_label, levels = c("History vacant core", "Developable core")),
    year_window_label = factor(year_window_label, levels = c("Core window", "Long pre-period"))
  )

add_reference_period <- function(df) {
  reference_rows <- df %>%
    dplyr::distinct(
      sample_scope, sample_label, bandwidth, year_scope, event_year, outcome,
      treatment_spec, year_window, year_window_label, fe_spec, control_spec,
      model_spec_label, treatment_group, display_unit
    ) %>%
    dplyr::mutate(
      term = "reference_period",
      estimate = 0,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = 0,
      conf_high = 0,
      event_time = -1L,
      estimate_display = 0,
      ci_low_display = 0,
      ci_high_display = 0
    )

  dplyr::bind_rows(df, reference_rows) %>%
    dplyr::arrange(sample_label, year_window_label, treatment_group, event_time)
}

plot_building <- function(year_window_i, event_year_i, bandwidth_i, out_file) {
  plot_df <- coefficients %>%
    dplyr::filter(
      outcome == "building_positive_share",
      treatment_spec == "sign",
      year_window == year_window_i,
      event_year == event_year_i,
      bandwidth == bandwidth_i,
      fe_spec == "segment_year",
      control_spec == "none"
    ) %>%
    add_reference_period()

  if (nrow(plot_df) == 0) {
    stop(sprintf("No building-positive coefficients found for %s, event %s, %s.", year_window_i, event_year_i, bandwidth_i), call. = FALSE)
  }

  plot_title <- sprintf(
    "Block-panel building-positive share: %s, event year %s, %s",
    unique(plot_df$year_window_label),
    event_year_i,
    bandwidth_i
  )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = event_time, y = estimate_display)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = -0.5, color = "grey65", linetype = "dashed", linewidth = 0.35) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_low_display, ymax = ci_high_display),
      fill = "#8cb3d9",
      alpha = 0.24
    ) +
    ggplot2::geom_line(color = "#1f5a85", linewidth = 0.55) +
    ggplot2::geom_point(color = "#1f5a85", size = 1.7) +
    ggplot2::facet_grid(sample_label ~ treatment_group) +
    ggplot2::labs(
      title = plot_title,
      x = "Years relative to treatment",
      y = "Effect on positive-building share (percentage points)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(out_file, p, width = 9, height = 5.5, device = "pdf")
  message(sprintf("Saved %s", out_file))
}

plot_land_psf <- function(year_window_i, out_file) {
  plot_df <- coefficients %>%
    dplyr::filter(
      outcome == "log_land_psf_block",
      treatment_spec == "continuous",
      year_window == year_window_i,
      event_year == 2015L,
      bandwidth == "1000ft",
      fe_spec == "segment_year",
      control_spec == "none"
    ) %>%
    add_reference_period()

  if (nrow(plot_df) == 0) {
    stop(sprintf("No log-land-PSF coefficients found for %s.", year_window_i), call. = FALSE)
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = event_time, y = estimate_display)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = -0.5, color = "grey65", linetype = "dashed", linewidth = 0.35) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_low_display, ymax = ci_high_display),
      fill = "#d8a560",
      alpha = 0.24
    ) +
    ggplot2::geom_line(color = "#8a4f05", linewidth = 0.55) +
    ggplot2::geom_point(color = "#8a4f05", size = 1.7) +
    ggplot2::facet_wrap(~sample_label, ncol = 1) +
    ggplot2::labs(
      title = sprintf("Block-panel log land value per sqft: %s, event year 2015, 1000ft", unique(plot_df$year_window_label)),
      x = "Years relative to treatment",
      y = "Effect per strictness unit (100 log points)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(out_file, p, width = 8, height = 5.5, device = "pdf")
  message(sprintf("Saved %s", out_file))
}

plot_building_robustness <- function(out_file) {
  plot_df <- coefficients %>%
    dplyr::filter(
      outcome == "building_positive_share",
      treatment_spec == "sign",
      year_window == "long_pre",
      event_year == 2015L,
      bandwidth == "1000ft",
      fe_spec %in% c("segment_year", "ward_pair_year"),
      control_spec %in% c("none", "baseline_x_year")
    ) %>%
    add_reference_period() %>%
    dplyr::mutate(
      model_spec_label = factor(
        model_spec_label,
        levels = c(
          "Segment-year FE",
          "Ward-pair-year FE",
          "Segment-year FE + controls",
          "Ward-pair-year FE + controls"
        )
      )
    )

  if (nrow(plot_df) == 0) {
    stop("No building-positive robustness coefficients found.", call. = FALSE)
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = event_time, y = estimate_display, color = model_spec_label, fill = model_spec_label)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = -0.5, color = "grey65", linetype = "dashed", linewidth = 0.35) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::facet_grid(sample_label ~ treatment_group) +
    ggplot2::scale_color_manual(values = c("#1f5a85", "#9c4f2f", "#4e7d3a", "#6f4a8e")) +
    ggplot2::scale_fill_manual(values = c("#1f5a85", "#9c4f2f", "#4e7d3a", "#6f4a8e")) +
    ggplot2::labs(
      title = "Block-panel building-positive share robustness: Long pre-period, event year 2015, 1000ft",
      x = "Years relative to treatment",
      y = "Effect on positive-building share (percentage points)",
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggplot2::ggsave(out_file, p, width = 10, height = 6.5, device = "pdf")
  message(sprintf("Saved %s", out_file))
}

plot_building("core", 2015L, "1000ft", file.path(output_dir, "block_building_positive_event2015_core_1000ft.pdf"))
plot_building("core", 2015L, "500ft", file.path(output_dir, "block_building_positive_event2015_core_500ft.pdf"))
plot_building("long_pre", 2015L, "1000ft", file.path(output_dir, "block_building_positive_event2015_longpre_1000ft.pdf"))
plot_building("core", 2012L, "1000ft", file.path(output_dir, "block_building_positive_event2012_core_1000ft.pdf"))
plot_building("long_pre", 2012L, "1000ft", file.path(output_dir, "block_building_positive_event2012_longpre_1000ft.pdf"))
plot_land_psf("core", file.path(output_dir, "block_log_land_psf_event2015_core_1000ft.pdf"))
plot_land_psf("long_pre", file.path(output_dir, "block_log_land_psf_event2015_longpre_1000ft.pdf"))
plot_building_robustness(file.path(output_dir, "block_building_positive_event2015_longpre_1000ft_robustness.pdf"))
