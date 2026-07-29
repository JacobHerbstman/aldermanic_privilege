# setwd("tasks/audits/density_adjusted_figure/code")

source("../../../setup_environment/code/packages.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

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

panels <- vector("list", nrow(panel_specs))
estimates <- vector("list", nrow(panel_specs))
flat_panels <- vector("list", nrow(panel_specs))
flat_estimates <- vector("list", nrow(panel_specs))

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
      dplyr::if_all(dplyr::all_of(demographic_controls), is.finite),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != ""
    ) |>
    dplyr::mutate(
      log_outcome = log(.data[[outcome]]),
      running_distance_ft = dplyr::if_else(
        side == 1L,
        abs(distance_to_boundary_ft),
        -abs(distance_to_boundary_ft)
      )
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
  retained_vcov <- stats::vcov(model)[
    retained_terms,
    retained_terms,
    drop = FALSE
  ]
  side_row <- fixest::coeftable(model)["side", ]
  side_p <- unname(side_row["Pr(>|t|)"])
  side_stars <- dplyr::case_when(
    side_p <= 0.01 ~ "***",
    side_p <= 0.05 ~ "**",
    side_p <= 0.10 ~ "*",
    TRUE ~ ""
  )

  plot_data <- plot_data |>
    dplyr::mutate(
      adjusted_log_density =
        as.numeric(stats::resid(model)) +
        retained_coefficients[["side"]] * side +
        retained_coefficients[["lenient_dist"]] * lenient_dist +
        retained_coefficients[["strict_dist"]] * strict_dist,
      bin_width_ft = 62.5,
      bin = dplyr::case_when(
        running_distance_ft < 0 ~
          pmax(floor((running_distance_ft + 500) / bin_width_ft) + 1L, 1L),
        TRUE ~
          pmin(floor(running_distance_ft / bin_width_ft) + 9L, 16L)
      ),
      bin_center_ft = c(
        seq(-468.75, -31.25, length.out = 8L),
        seq(31.25, 468.75, length.out = 8L)
      )[bin]
    )

  bins <- plot_data |>
    dplyr::summarise(
      mean_outcome = mean(adjusted_log_density),
      .by = c(bin, bin_center_ft, side)
    )

  line_data <- tibble::tibble(
    running_distance_ft = c(
      seq(-500, 0, length.out = 201),
      seq(0, 500, length.out = 201)[-1]
    )
  ) |>
    dplyr::mutate(
      side = as.integer(running_distance_ft > 0),
      distance_m = abs(running_distance_ft) / 3.28084,
      lenient_dist = distance_m * as.integer(side == 0L),
      strict_dist = distance_m * as.integer(side == 1L)
    )

  line_matrix <- as.matrix(
    line_data[, c("side", "lenient_dist", "strict_dist")]
  )
  critical_value <- stats::qt(
    0.975,
    df = max(dplyr::n_distinct(plot_data$ward_pair) - 1L, 1L)
  )
  line_data <- line_data |>
    dplyr::mutate(
      fitted = as.numeric(line_matrix %*% retained_coefficients),
      fitted_se = sqrt(pmax(
        rowSums((line_matrix %*% retained_vcov) * line_matrix),
        0
      )),
      lower = fitted - critical_value * fitted_se,
      upper = fitted + critical_value * fitted_se
    )

  panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = running_distance_ft,
        y = fitted,
        color = factor(side),
        group = side
      ),
      linewidth = 0.75,
      alpha = 0.75
    ) +
    ggplot2::geom_point(
      data = bins,
      ggplot2::aes(
        x = bin_center_ft,
        y = mean_outcome,
        fill = factor(side)
      ),
      shape = 21,
      color = "white",
      stroke = 0.35,
      size = 2
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
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "#1f77b4", "1" = "#d62728"),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = sprintf(
        "Adjusted discontinuity = %.3f%s (SE %.3f)",
        unname(side_row["Estimate"]),
        side_stars,
        unname(side_row["Std. Error"])
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

  estimates[[i]] <- tibble::tibble(
    sample = sample_name,
    outcome,
    estimate = unname(side_row["Estimate"]),
    se = unname(side_row["Std. Error"]),
    p_value = side_p,
    n_obs = stats::nobs(model),
    n_ward_pairs = dplyr::n_distinct(plot_data$ward_pair)
  )

  flat_model <- fixest::feols(
    log_outcome ~
      side +
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
  flat_removed <- flat_model$obs_selection$obsRemoved
  flat_keep_rows <- if (is.null(flat_removed)) {
    seq_len(nrow(model_data))
  } else {
    setdiff(seq_len(nrow(model_data)), abs(as.integer(flat_removed)))
  }
  flat_plot_data <- model_data[flat_keep_rows, , drop = FALSE]
  flat_row <- fixest::coeftable(flat_model)["side", ]
  flat_p <- unname(flat_row["Pr(>|t|)"])
  flat_stars <- dplyr::case_when(
    flat_p <= 0.01 ~ "***",
    flat_p <= 0.05 ~ "**",
    flat_p <= 0.10 ~ "*",
    TRUE ~ ""
  )
  flat_plot_data <- flat_plot_data |>
    dplyr::mutate(
      adjusted_log_density =
        as.numeric(stats::resid(flat_model)) +
        unname(flat_row["Estimate"]) * side,
      bin_width_ft = 50,
      bin = dplyr::case_when(
        running_distance_ft < 0 ~
          pmax(floor((running_distance_ft + 500) / bin_width_ft) + 1L, 1L),
        TRUE ~
          pmin(floor(running_distance_ft / bin_width_ft) + 11L, 20L)
      ),
      bin_center_ft = c(
        seq(-475, -25, length.out = 10L),
        seq(25, 475, length.out = 10L)
      )[bin],
      side_label = dplyr::if_else(
        side == 1L,
        "More Stringent",
        "Less Stringent"
      )
    )
  flat_bins <- flat_plot_data |>
    dplyr::summarise(
      mean_outcome = mean(adjusted_log_density),
      .by = c(bin, bin_center_ft, side_label)
    )

  flat_panels[[i]] <- ggplot2::ggplot(
    flat_bins,
    ggplot2::aes(
      x = bin_center_ft,
      y = mean_outcome,
      color = side_label
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
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_color_manual(
      values = c(
        "Less Stringent" = "#1f77b4",
        "More Stringent" = "#d62728"
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = sprintf(
        "Adjusted level difference = %.3f%s (SE %.3f)",
        unname(flat_row["Estimate"]),
        flat_stars,
        unname(flat_row["Std. Error"])
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

  flat_estimates[[i]] <- tibble::tibble(
    sample = sample_name,
    outcome,
    estimate = unname(flat_row["Estimate"]),
    se = unname(flat_row["Std. Error"]),
    p_value = flat_p,
    n_obs = stats::nobs(flat_model),
    n_ward_pairs = dplyr::n_distinct(flat_plot_data$ward_pair)
  )
}

combined_plot <- (
  panels[[1]] | panels[[2]]
) / (
  panels[[3]] | panels[[4]]
)

ggplot2::ggsave(
  "../output/density_model_adjusted_rd.pdf",
  combined_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_model_adjusted_rd.png",
  combined_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
readr::write_csv(
  dplyr::bind_rows(estimates),
  "../output/density_model_adjusted_estimates.csv",
  na = ""
)

flat_combined_plot <- (
  flat_panels[[1]] | flat_panels[[2]]
) / (
  flat_panels[[3]] | flat_panels[[4]]
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/density_flat_comparison_rd.pdf",
  flat_combined_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_flat_comparison_rd.png",
  flat_combined_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
readr::write_csv(
  dplyr::bind_rows(flat_estimates),
  "../output/density_flat_comparison_estimates.csv",
  na = ""
)
