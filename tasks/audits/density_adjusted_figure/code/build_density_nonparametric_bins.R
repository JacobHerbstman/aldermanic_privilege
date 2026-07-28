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

estimate_bins <- function(
    sample_name,
    outcome,
    panel_title,
    bin_width_ft,
    bandwidth_ft = 500) {
  bin_breaks <- seq(-bandwidth_ft, bandwidth_ft, by = bin_width_ft)
  bin_levels <- sprintf("bin_%02d", seq_len(length(bin_breaks) - 1L))
  bin_centers <- head(bin_breaks, -1L) + bin_width_ft / 2
  reference_index <- which(bin_centers == -bin_width_ft / 2)
  reference_bin <- bin_levels[reference_index]

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
      ),
      distance_bin = cut(
        running_distance_ft,
        breaks = bin_breaks,
        labels = bin_levels,
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < bandwidth_ft,
      !is.na(distance_bin)
    )

  model <- fixest::feols(
    log_outcome ~
      i(distance_bin, ref = reference_bin) +
      pair_average_score +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    cluster = ~ward_pair
  )

  coefficient_table <- fixest::coeftable(model)
  coefficient_rows <- grepl(
    "^distance_bin::",
    rownames(coefficient_table)
  )
  estimates <- tibble::tibble(
    distance_bin = sub(
      "^distance_bin::",
      "",
      rownames(coefficient_table)[coefficient_rows]
    ),
    estimate = coefficient_table[coefficient_rows, "Estimate"],
    std_error = coefficient_table[coefficient_rows, "Std. Error"],
    p_value = coefficient_table[coefficient_rows, "Pr(>|t|)"]
  )

  cluster_count <- dplyr::n_distinct(model_data$ward_pair)
  critical_value <- stats::qt(0.975, df = cluster_count - 1L)

  results <- tibble::tibble(
    distance_bin = bin_levels,
    bin_start_ft = head(bin_breaks, -1L),
    bin_end_ft = tail(bin_breaks, -1L),
    bin_center_ft = bin_centers
  ) |>
    dplyr::left_join(
      estimates,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        estimate
      ),
      std_error = dplyr::if_else(
        distance_bin == reference_bin,
        NA_real_,
        std_error
      ),
      p_value = dplyr::if_else(
        distance_bin == reference_bin,
        NA_real_,
        p_value
      ),
      ci_low = estimate - critical_value * std_error,
      ci_high = estimate + critical_value * std_error,
      side_label = dplyr::if_else(
        bin_center_ft < 0,
        "Less Stringent",
        "More Stringent"
      ),
      is_reference = distance_bin == reference_bin,
      sample = sample_name,
      outcome,
      bin_width_ft,
      bandwidth_ft,
      n_projects = stats::nobs(model),
      n_ward_pairs = cluster_count
    )

  nearest_strict <- results |>
    dplyr::filter(bin_start_ft == 0)
  nearest_strict_stars <- dplyr::case_when(
    nearest_strict$p_value < 0.01 ~ "***",
    nearest_strict$p_value < 0.05 ~ "**",
    nearest_strict$p_value < 0.10 ~ "*",
    TRUE ~ ""
  )

  plot <- ggplot2::ggplot(
    results,
    ggplot2::aes(
      x = bin_center_ft,
      y = estimate,
      color = side_label,
      group = 1
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_line(
      color = "gray55",
      linewidth = 0.55
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = bin_width_ft * 0.12,
      linewidth = 0.45,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = c(
        -bandwidth_ft,
        -bandwidth_ft / 2,
        0,
        bandwidth_ft / 2,
        bandwidth_ft
      )
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "Nearest-bin difference = %.3f%s (SE %.3f)",
        nearest_strict$estimate,
        nearest_strict_stars,
        nearest_strict$std_error
      ),
      x = "Distance to ward boundary (feet)",
      y = "Difference from the nearest lenient-side bin"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  ribbon_results <- results |>
    dplyr::mutate(
      ribbon_low = dplyr::if_else(is_reference, 0, ci_low),
      ribbon_high = dplyr::if_else(is_reference, 0, ci_high)
    )

  ribbon_plot <- ggplot2::ggplot(
    ribbon_results,
    ggplot2::aes(
      x = bin_center_ft,
      y = estimate,
      color = side_label,
      group = side_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray55",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ribbon_low,
        ymax = ribbon_high,
        fill = side_label
      ),
      alpha = 0.16,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = c(
        -bandwidth_ft,
        -bandwidth_ft / 2,
        0,
        bandwidth_ft / 2,
        bandwidth_ft
      )
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "Nearest-bin difference = %.3f%s (SE %.3f)",
        nearest_strict$estimate,
        nearest_strict_stars,
        nearest_strict$std_error
      ),
      x = "Distance to ward boundary (feet)",
      y = "Difference from the nearest lenient-side bin"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  list(results = results, plot = plot, ribbon_plot = ribbon_plot)
}

all_results <- list()

for (bin_width_ft in c(50, 100)) {
  panels <- vector("list", nrow(panel_specs))
  ribbon_panels <- vector("list", nrow(panel_specs))
  bin_results <- vector("list", nrow(panel_specs))

  for (i in seq_len(nrow(panel_specs))) {
    estimated <- estimate_bins(
      panel_specs$sample[i],
      panel_specs$outcome[i],
      panel_specs$panel_title[i],
      bin_width_ft
    )
    panels[[i]] <- estimated$plot
    ribbon_panels[[i]] <- estimated$ribbon_plot
    bin_results[[i]] <- estimated$results
  }

  combined_plot <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_bins_%dft.pdf",
      bin_width_ft
    ),
    combined_plot,
    width = 12,
    height = 8.5
  )
  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_bins_%dft.png",
      bin_width_ft
    ),
    combined_plot,
    width = 12,
    height = 8.5,
    dpi = 220
  )

  if (bin_width_ft == 100) {
    ribbon_plot <- patchwork::wrap_plots(ribbon_panels, ncol = 2) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(
      "../output/density_nonparametric_bins_100ft_ribbons.pdf",
      ribbon_plot,
      width = 12,
      height = 8.5
    )
    ggplot2::ggsave(
      "../output/density_nonparametric_bins_100ft_ribbons.png",
      ribbon_plot,
      width = 12,
      height = 8.5,
      dpi = 220
    )
  }

  all_results[[as.character(bin_width_ft)]] <- dplyr::bind_rows(bin_results)
}

panels_250ft <- vector("list", nrow(panel_specs))
results_250ft <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  estimated <- estimate_bins(
    panel_specs$sample[i],
    panel_specs$outcome[i],
    panel_specs$panel_title[i],
    bin_width_ft = 50,
    bandwidth_ft = 250
  )
  panels_250ft[[i]] <- estimated$ribbon_plot
  results_250ft[[i]] <- estimated$results
}

combined_250ft <- patchwork::wrap_plots(panels_250ft, ncol = 2) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/density_nonparametric_bins_250ft_50ft_ribbons.pdf",
  combined_250ft,
  width = 12,
  height = 8.5
)
ggplot2::ggsave(
  "../output/density_nonparametric_bins_250ft_50ft_ribbons.png",
  combined_250ft,
  width = 12,
  height = 8.5,
  dpi = 220
)

all_results[["250ft_50ft_bins"]] <- dplyr::bind_rows(results_250ft)

readr::write_csv(
  dplyr::bind_rows(all_results),
  "../output/density_nonparametric_bin_estimates.csv"
)
