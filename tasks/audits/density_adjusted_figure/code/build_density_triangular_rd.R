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

estimate_panel <- function(sample_name, outcome, panel_title, bandwidth_ft) {
  model_data <- data |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
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
    ) |>
    dplyr::filter(
      is.finite(running_distance_ft),
      abs(running_distance_ft) < bandwidth_ft
    ) |>
    dplyr::mutate(
      distance_m = abs(running_distance_ft) / 3.28084,
      lenient_dist = distance_m * as.integer(side == 0L),
      strict_dist = distance_m * as.integer(side == 1L),
      kernel_weight = 1 - abs(running_distance_ft) / bandwidth_ft
    )

  model <- fixest::feols(
    log_outcome ~
      side +
      pair_average_score +
      lenient_dist +
      strict_dist +
      share_white_own +
      share_black_own +
      median_hh_income_own +
      share_bach_plus_own +
      homeownership_rate_own |
      zone_group + segment_id + construction_year,
    data = model_data,
    weights = ~kernel_weight,
    cluster = ~ward_pair
  )

  removed <- model$obs_selection$obsRemoved
  keep_rows <- if (is.null(removed)) {
    seq_len(nrow(model_data))
  } else {
    setdiff(seq_len(nrow(model_data)), abs(as.integer(removed)))
  }
  plot_data <- model_data[keep_rows, , drop = FALSE]

  retained_terms <- c("side", "lenient_dist", "strict_dist")
  retained_coefficients <- stats::coef(model)[retained_terms]
  side_row <- fixest::coeftable(model)["side", ]
  p_value <- unname(side_row["Pr(>|t|)"])
  stars <- dplyr::case_when(
    p_value <= 0.01 ~ "***",
    p_value <= 0.05 ~ "**",
    p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  bin_width_value <- bandwidth_ft / 8

  plot_data <- plot_data |>
    dplyr::mutate(
      adjusted_log_density =
        as.numeric(stats::resid(model)) +
        retained_coefficients[["side"]] * side +
        retained_coefficients[["lenient_dist"]] * lenient_dist +
        retained_coefficients[["strict_dist"]] * strict_dist,
      bin_width_ft = bin_width_value,
      bin = dplyr::case_when(
        running_distance_ft < 0 ~
          pmax(
            pmin(
              floor((running_distance_ft + bandwidth_ft) / bin_width_ft) + 1L,
              8L
            ),
            1L
          ),
        TRUE ~
          pmax(
            pmin(floor(running_distance_ft / bin_width_ft) + 9L, 16L),
            9L
          )
      ),
      bin_center_ft = c(
        seq(
          -bandwidth_ft + bin_width_value / 2,
          -bin_width_value / 2,
          length.out = 8L
        ),
        seq(
          bin_width_value / 2,
          bandwidth_ft - bin_width_value / 2,
          length.out = 8L
        )
      )[bin],
      side_label = dplyr::if_else(
        side == 1L,
        "More Stringent",
        "Less Stringent"
      )
    )

  bins <- plot_data |>
    dplyr::group_by(bin, bin_center_ft, side_label) |>
    dplyr::group_modify(
      function(.x, .y) {
        mean_outcome <- stats::weighted.mean(
          .x$adjusted_log_density,
          .x$kernel_weight
        )
        cluster_scores <- .x |>
          dplyr::mutate(
            weighted_residual = kernel_weight *
              (adjusted_log_density - mean_outcome)
          ) |>
          dplyr::summarise(
            score = sum(weighted_residual),
            .by = ward_pair
          )
        cluster_count <- nrow(cluster_scores)
        mean_se <- if (cluster_count > 1L) {
          sqrt(
            cluster_count / (cluster_count - 1) *
              sum(cluster_scores$score^2) /
              sum(.x$kernel_weight)^2
          )
        } else {
          NA_real_
        }
        critical_value <- if (cluster_count > 1L) {
          stats::qt(0.975, df = cluster_count - 1L)
        } else {
          NA_real_
        }
        tibble::tibble(
          mean_outcome,
          mean_se,
          ci_low = mean_outcome - critical_value * mean_se,
          ci_high = mean_outcome + critical_value * mean_se,
          n_projects = nrow(.x),
          n_ward_pairs = cluster_count
        )
      }
    ) |>
    dplyr::ungroup()

  line_data <- tibble::tibble(
    running_distance_ft = c(
      seq(-bandwidth_ft, 0, length.out = 201L),
      seq(0, bandwidth_ft, length.out = 201L)[-1L]
    )
  ) |>
    dplyr::mutate(
      side = as.integer(running_distance_ft > 0),
      distance_m = abs(running_distance_ft) / 3.28084,
      lenient_dist = distance_m * as.integer(side == 0L),
      strict_dist = distance_m * as.integer(side == 1L),
      side_label = dplyr::if_else(
        side == 1L,
        "More Stringent",
        "Less Stringent"
      ),
      fitted =
        retained_coefficients[["side"]] * side +
        retained_coefficients[["lenient_dist"]] * lenient_dist +
        retained_coefficients[["strict_dist"]] * strict_dist
    )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = running_distance_ft,
        y = fitted,
        color = side_label,
        group = side_label
      ),
      linewidth = 0.75
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = mean_outcome,
        color = side_label,
        size = n_projects
      )
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray60",
      linewidth = 0.35
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#1f77b4",
        "More Stringent" = "#d62728"
      ),
      name = NULL
    ) +
    ggplot2::scale_size_area(
      max_size = 4.5,
      breaks = c(25, 150, 400),
      limits = c(1, 500),
      name = "Projects"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = pretty(c(-bandwidth_ft, bandwidth_ft), n = 5)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "Discontinuity = %.3f%s (SE %.3f); h = %.0fft",
        unname(side_row["Estimate"]),
        stars,
        unname(side_row["Std. Error"]),
        bandwidth_ft
      ),
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
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

  paper_style_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = running_distance_ft,
        y = fitted,
        color = side_label,
        group = side_label
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = mean_outcome,
        fill = side_label
      ),
      shape = 21,
      color = "white",
      stroke = 0.4,
      size = 2.2
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray35",
      linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dotted",
      color = "gray60",
      linewidth = 0.35
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#1f77b4",
        "More Stringent" = "#d62728"
      ),
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Less Stringent" = "#1f77b4",
        "More Stringent" = "#d62728"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = pretty(c(-bandwidth_ft, bandwidth_ft), n = 5)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "Discontinuity = %.3f%s (SE %.3f); h = %.0fft",
        unname(side_row["Estimate"]),
        stars,
        unname(side_row["Std. Error"]),
        bandwidth_ft
      ),
      x = "Distance to ward boundary (feet)",
      y = "Adjusted log density"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  errorbar_plot <- paper_style_plot +
    ggplot2::geom_errorbar(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        ymin = ci_low,
        ymax = ci_high,
        color = side_label
      ),
      width = 18,
      linewidth = 0.45
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = mean_outcome,
        fill = side_label
      ),
      shape = 21,
      color = "white",
      stroke = 0.4,
      size = 2.2
    )

  list(
    plot = plot,
    paper_style_plot = paper_style_plot,
    errorbar_plot = errorbar_plot,
    estimate = tibble::tibble(
      sample = sample_name,
      outcome,
      bandwidth_ft,
      estimate = unname(side_row["Estimate"]),
      se = unname(side_row["Std. Error"]),
      p_value,
      n_obs = stats::nobs(model),
      n_ward_pairs = dplyr::n_distinct(plot_data$ward_pair)
    )
  )
}

selected_bandwidths <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  sample_name <- panel_specs$sample[i]
  outcome <- panel_specs$outcome[i]

  bandwidth_data <- data |>
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
    ) |>
    dplyr::filter(is.finite(running_distance_ft))

  bandwidth_fit <- rdrobust::rdbwselect(
    y = bandwidth_data$log_outcome,
    x = bandwidth_data$running_distance_ft,
    c = 0,
    p = 1,
    kernel = "triangular",
    bwselect = "mserd",
    cluster = bandwidth_data$ward_pair
  )

  selected_bandwidths[[i]] <- tibble::tibble(
    sample = sample_name,
    outcome,
    bandwidth_ft = as.numeric(bandwidth_fit$bws[1L, "h (left)"])
  )
}

selected_bandwidths <- dplyr::bind_rows(selected_bandwidths)

fixed_results <- vector("list", nrow(panel_specs))
selected_results <- vector("list", nrow(panel_specs))

for (i in seq_len(nrow(panel_specs))) {
  fixed_results[[i]] <- estimate_panel(
    sample_name = panel_specs$sample[i],
    outcome = panel_specs$outcome[i],
    panel_title = panel_specs$panel_title[i],
    bandwidth_ft = 500
  )
  selected_results[[i]] <- estimate_panel(
    sample_name = panel_specs$sample[i],
    outcome = panel_specs$outcome[i],
    panel_title = panel_specs$panel_title[i],
    bandwidth_ft = selected_bandwidths$bandwidth_ft[i]
  )
}

fixed_plot <- (
  (fixed_results[[1]]$plot | fixed_results[[2]]$plot) /
    (fixed_results[[3]]$plot | fixed_results[[4]]$plot) +
    patchwork::plot_layout(guides = "collect")
) & ggplot2::theme(legend.position = "bottom")

selected_plot <- (
  (selected_results[[1]]$plot | selected_results[[2]]$plot) /
    (selected_results[[3]]$plot | selected_results[[4]]$plot) +
    patchwork::plot_layout(guides = "collect")
) & ggplot2::theme(legend.position = "bottom")

fixed_paper_style_plot <- (
  (fixed_results[[1]]$paper_style_plot |
    fixed_results[[2]]$paper_style_plot) /
    (fixed_results[[3]]$paper_style_plot |
      fixed_results[[4]]$paper_style_plot)
)

fixed_errorbar_plot <- (
  (fixed_results[[1]]$errorbar_plot |
    fixed_results[[2]]$errorbar_plot) /
    (fixed_results[[3]]$errorbar_plot |
      fixed_results[[4]]$errorbar_plot)
)

ggplot2::ggsave(
  "../output/density_triangular_rd_500ft.pdf",
  fixed_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_triangular_rd_500ft.png",
  fixed_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_triangular_rd_500ft_paper_style.pdf",
  fixed_paper_style_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_triangular_rd_500ft_paper_style.png",
  fixed_paper_style_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_triangular_rd_500ft_errorbars.pdf",
  fixed_errorbar_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_triangular_rd_500ft_errorbars.png",
  fixed_errorbar_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
ggplot2::ggsave(
  "../output/density_triangular_rd_selected_bandwidth.pdf",
  selected_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_triangular_rd_selected_bandwidth.png",
  selected_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)

readr::write_csv(
  dplyr::bind_rows(
    lapply(fixed_results, `[[`, "estimate")
  ) |>
    dplyr::mutate(bandwidth_rule = "fixed 500ft") |>
    dplyr::bind_rows(
      dplyr::bind_rows(lapply(selected_results, `[[`, "estimate")) |>
        dplyr::mutate(bandwidth_rule = "CCT mserd, no controls")
    ) |>
    dplyr::select(
      bandwidth_rule,
      dplyr::everything()
    ),
  "../output/density_triangular_rd_estimates.csv",
  na = ""
)
