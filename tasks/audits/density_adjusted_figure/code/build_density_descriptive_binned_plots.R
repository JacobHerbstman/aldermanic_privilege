# setwd("tasks/audits/density_adjusted_figure/code")

source("../../../setup_environment/code/packages.R")

panel_specs <- tibble::tribble(
  ~sample,       ~outcome,        ~panel_title,
  "all",         "density_far",   "All construction: Log(FAR)",
  "multifamily", "density_far",   "Multifamily: Log(FAR)",
  "all",         "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

data <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

residualized_line_panels <- vector("list", nrow(panel_specs))
residualized_point_panels <- vector("list", nrow(panel_specs))
residualized_250_panels <- vector("list", nrow(panel_specs))
residualized_250_triangular_panels <- vector("list", nrow(panel_specs))
raw_line_panels <- vector("list", nrow(panel_specs))
binned_results <- vector("list", nrow(panel_specs))
binned_250_results <- vector("list", nrow(panel_specs))
binned_250_triangular_results <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]

  model_data <- data |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
      within_500ft,
      dwelling_units > 0,
      sample_name == "all" | external_multifamily,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(.data[[outcome]]),
      .data[[outcome]] > 0,
      is.finite(pair_average_score),
      is.finite(share_white_own),
      is.finite(share_black_own),
      is.finite(median_hh_income_own),
      is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != ""
    ) |>
    dplyr::mutate(
      log_outcome = log(.data[[outcome]]),
      running_distance_ft = dplyr::if_else(
        side == 1L,
        abs(distance_to_boundary_ft),
        -abs(distance_to_boundary_ft)
      )
    )

  nuisance_model <- fixest::feols(
    log_outcome ~
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data
  )

  removed <- nuisance_model$obs_selection$obsRemoved
  keep_rows <- if (is.null(removed)) {
    seq_len(nrow(model_data))
  } else {
    setdiff(seq_len(nrow(model_data)), abs(as.integer(removed)))
  }
  plot_data <- model_data[keep_rows, , drop = FALSE]

  if (length(stats::residuals(nuisance_model)) != nrow(plot_data)) {
    stop("The nuisance model and plotting sample do not align.")
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      adjusted_log_outcome =
        as.numeric(stats::residuals(nuisance_model)) +
        mean(log_outcome),
      signed_bin = pmax(
        0L,
        pmin(
          floor((running_distance_ft + 500) / 50),
          19L
        )
      ),
      bin_center_ft = -475 + 50 * signed_bin,
      side_label = factor(
        side,
        levels = c(0, 1),
        labels = c("Less stringent", "More stringent")
      )
    )

  bins <- plot_data |>
    dplyr::group_by(signed_bin, bin_center_ft, side_label) |>
    dplyr::summarise(
      raw_mean = mean(log_outcome),
      adjusted_mean = mean(adjusted_log_outcome),
      n_projects = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sample = sample_name,
      outcome
    )
  binned_results[[i]] <- bins

  adjusted_lines <- vector("list", 2L)
  raw_lines <- vector("list", 2L)

  for (side_value in 0:1) {
    side_data <- plot_data |>
      dplyr::filter(side == side_value)

    line_grid <- tibble::tibble(
      running_distance_ft = if (side_value == 0) {
        seq(-500, 0, length.out = 201L)
      } else {
        seq(0, 500, length.out = 201L)
      }
    )

    adjusted_fit <- stats::lm(
      adjusted_log_outcome ~ running_distance_ft,
      data = side_data
    )
    raw_fit <- stats::lm(
      log_outcome ~ running_distance_ft,
      data = side_data
    )

    adjusted_lines[[side_value + 1L]] <- line_grid |>
      dplyr::mutate(
        fitted = stats::predict(adjusted_fit, newdata = line_grid),
        side_label = factor(
          side_value,
          levels = c(0, 1),
          labels = c("Less stringent", "More stringent")
        )
      )
    raw_lines[[side_value + 1L]] <- line_grid |>
      dplyr::mutate(
        fitted = stats::predict(raw_fit, newdata = line_grid),
        side_label = factor(
          side_value,
          levels = c(0, 1),
          labels = c("Less stringent", "More stringent")
        )
      )
  }

  adjusted_lines <- dplyr::bind_rows(adjusted_lines)
  raw_lines <- dplyr::bind_rows(raw_lines)

  common_layers <- list(
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ),
    ggplot2::scale_color_manual(
      values = c(
        "Less stringent" = "#2478B5",
        "More stringent" = "#D92D27"
      ),
      name = NULL
    ),
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ),
    ggplot2::theme_bw(base_size = 10),
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  )

  residualized_line_panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = adjusted_lines,
      ggplot2::aes(
        x = running_distance_ft,
        y = fitted,
        color = side_label,
        group = side_label
      ),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = adjusted_mean,
        color = side_label
      ),
      size = 2.2
    ) +
    common_layers +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Adjusted 50ft binned means with separate linear summaries",
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
    )

  residualized_point_panels[[i]] <- ggplot2::ggplot(
    bins,
    ggplot2::aes(
      x = bin_center_ft,
      y = adjusted_mean,
      color = side_label
    )
  ) +
    ggplot2::geom_point(size = 2.2) +
    common_layers +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Adjusted means in 50ft distance bins",
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
    )

  model_data_250 <- model_data |>
    dplyr::filter(abs(running_distance_ft) <= 250)

  nuisance_model_250 <- fixest::feols(
    log_outcome ~
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data_250
  )

  removed_250 <- nuisance_model_250$obs_selection$obsRemoved
  keep_rows_250 <- if (is.null(removed_250)) {
    seq_len(nrow(model_data_250))
  } else {
    setdiff(seq_len(nrow(model_data_250)), abs(as.integer(removed_250)))
  }
  plot_data_250 <- model_data_250[keep_rows_250, , drop = FALSE]

  if (length(stats::residuals(nuisance_model_250)) != nrow(plot_data_250)) {
    stop("The 250ft nuisance model and plotting sample do not align.")
  }

  plot_data_250 <- plot_data_250 |>
    dplyr::mutate(
      adjusted_log_outcome =
        as.numeric(stats::residuals(nuisance_model_250)) +
        mean(log_outcome),
      signed_bin = pmax(
        0L,
        pmin(
          floor((running_distance_ft + 250) / 50),
          9L
        )
      ),
      bin_center_ft = -225 + 50 * signed_bin,
      side_label = factor(
        side,
        levels = c(0, 1),
        labels = c("Less stringent", "More stringent")
      )
    )

  bins_250 <- plot_data_250 |>
    dplyr::group_by(signed_bin, bin_center_ft, side_label) |>
    dplyr::summarise(
      adjusted_mean = mean(adjusted_log_outcome),
      n_projects = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sample = sample_name,
      outcome
    )
  binned_250_results[[i]] <- bins_250

  residualized_250_panels[[i]] <- ggplot2::ggplot(
    bins_250,
    ggplot2::aes(
      x = bin_center_ft,
      y = adjusted_mean,
      color = side_label
    )
  ) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Less stringent" = "#2478B5",
        "More stringent" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-250, 250),
      breaks = c(-250, -125, 0, 125, 250)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Adjusted means in 50ft distance bins",
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  model_data_250_triangular <- model_data |>
    dplyr::filter(abs(running_distance_ft) < 250) |>
    dplyr::mutate(
      kernel_weight = 1 - abs(running_distance_ft) / 250
    )

  nuisance_model_250_triangular <- fixest::feols(
    log_outcome ~
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data_250_triangular,
    weights = ~kernel_weight
  )

  removed_250_triangular <-
    nuisance_model_250_triangular$obs_selection$obsRemoved
  keep_rows_250_triangular <- if (is.null(removed_250_triangular)) {
    seq_len(nrow(model_data_250_triangular))
  } else {
    setdiff(
      seq_len(nrow(model_data_250_triangular)),
      abs(as.integer(removed_250_triangular))
    )
  }
  plot_data_250_triangular <-
    model_data_250_triangular[keep_rows_250_triangular, , drop = FALSE]

  if (
    length(stats::residuals(nuisance_model_250_triangular)) !=
      nrow(plot_data_250_triangular)
  ) {
    stop("The triangular 250ft nuisance model and plotting sample do not align.")
  }

  plot_data_250_triangular <- plot_data_250_triangular |>
    dplyr::mutate(
      adjusted_log_outcome =
        as.numeric(stats::residuals(nuisance_model_250_triangular)) +
        stats::weighted.mean(log_outcome, kernel_weight),
      signed_bin = pmax(
        0L,
        pmin(
          floor((running_distance_ft + 250) / 50),
          9L
        )
      ),
      bin_center_ft = -225 + 50 * signed_bin,
      side_label = factor(
        side,
        levels = c(0, 1),
        labels = c("Less stringent", "More stringent")
      )
    )

  bins_250_triangular <- plot_data_250_triangular |>
    dplyr::group_by(signed_bin, bin_center_ft, side_label) |>
    dplyr::summarise(
      adjusted_mean = stats::weighted.mean(
        adjusted_log_outcome,
        kernel_weight
      ),
      n_projects = dplyr::n(),
      weight_sum = sum(kernel_weight),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sample = sample_name,
      outcome
    )
  binned_250_triangular_results[[i]] <- bins_250_triangular

  residualized_250_triangular_panels[[i]] <- ggplot2::ggplot(
    bins_250_triangular,
    ggplot2::aes(
      x = bin_center_ft,
      y = adjusted_mean,
      color = side_label
    )
  ) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Less stringent" = "#2478B5",
        "More stringent" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-250, 250),
      breaks = c(-250, -125, 0, 125, 250)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Triangular-weighted adjusted means in 50ft bins",
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  raw_line_panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = raw_lines,
      ggplot2::aes(
        x = running_distance_ft,
        y = fitted,
        color = side_label,
        group = side_label
      ),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = raw_mean,
        color = side_label
      ),
      size = 2.2
    ) +
    common_layers +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Raw 50ft binned means with separate linear summaries",
      x = "Distance to ward boundary (feet)",
      y = "Mean log density"
    )
}

residualized_line_plot <- (
  (residualized_line_panels[[1]] | residualized_line_panels[[2]]) /
    (residualized_line_panels[[3]] | residualized_line_panels[[4]])
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

residualized_point_plot <- (
  (residualized_point_panels[[1]] | residualized_point_panels[[2]]) /
    (residualized_point_panels[[3]] | residualized_point_panels[[4]])
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

residualized_250_plot <- (
  (residualized_250_panels[[1]] | residualized_250_panels[[2]]) /
    (residualized_250_panels[[3]] | residualized_250_panels[[4]])
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

residualized_250_triangular_plot <- (
  (
    residualized_250_triangular_panels[[1]] |
      residualized_250_triangular_panels[[2]]
  ) /
    (
      residualized_250_triangular_panels[[3]] |
        residualized_250_triangular_panels[[4]]
    )
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

raw_line_plot <- (
  (raw_line_panels[[1]] | raw_line_panels[[2]]) /
    (raw_line_panels[[3]] | raw_line_panels[[4]])
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/density_residualized_bins_with_lines.pdf",
  residualized_line_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_residualized_bins_with_lines.png",
  residualized_line_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines.pdf",
  residualized_point_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines.png",
  residualized_point_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_raw_bins_with_lines.pdf",
  raw_line_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_raw_bins_with_lines.png",
  raw_line_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines_250ft.pdf",
  residualized_250_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines_250ft.png",
  residualized_250_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines_250ft_triangular.pdf",
  residualized_250_triangular_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_residualized_bins_without_lines_250ft_triangular.png",
  residualized_250_triangular_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
readr::write_csv(
  dplyr::bind_rows(binned_results),
  "../output/density_descriptive_binned_means.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(binned_250_results),
  "../output/density_descriptive_binned_means_250ft.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(binned_250_triangular_results),
  "../output/density_descriptive_binned_means_250ft_triangular.csv",
  na = ""
)
