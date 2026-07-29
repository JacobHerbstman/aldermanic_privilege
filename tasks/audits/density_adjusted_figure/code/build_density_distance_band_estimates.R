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

panels <- vector("list", nrow(panel_specs))
results <- vector("list", nrow(panel_specs))
binned_panels <- vector("list", nrow(panel_specs))
binned_results <- vector("list", nrow(panel_specs))

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
      absolute_distance_ft = abs(distance_to_boundary_ft),
      distance_band = pmin(
        floor(absolute_distance_ft / 100) + 1L,
        5L
      ),
      distance_band = factor(
        distance_band,
        levels = 1:5,
        labels = paste0("b", 1:5)
      )
    )

  table_model <- fixest::feols(
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
  table_row <- fixest::coeftable(table_model)["side", ]

  band_model <- fixest::feols(
    log_outcome ~
      distance_band +
      distance_band:side +
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

  band_rows <- fixest::coeftable(band_model)
  band_rows <- band_rows[grepl(":side$", rownames(band_rows)), , drop = FALSE]
  n_pairs <- dplyr::n_distinct(model_data$ward_pair)
  critical_value <- stats::qt(0.975, df = n_pairs - 1L)

  band_counts <- model_data |>
    dplyr::count(distance_band, name = "n_projects_band") |>
    dplyr::left_join(
      model_data |>
        dplyr::group_by(distance_band) |>
        dplyr::summarise(
          n_ward_pairs_band = dplyr::n_distinct(ward_pair),
          .groups = "drop"
        ),
      by = "distance_band"
    )

  band_results <- tibble::tibble(
    sample = sample_name,
    outcome,
    distance_band = factor(
      paste0("b", 1:5),
      levels = paste0("b", 1:5)
    ),
    distance_band_label = factor(
      c("0-100", "100-200", "200-300", "300-400", "400-500"),
      levels = c("0-100", "100-200", "200-300", "300-400", "400-500")
    ),
    estimate = unname(band_rows[, "Estimate"]),
    se = unname(band_rows[, "Std. Error"]),
    p_value = unname(band_rows[, "Pr(>|t|)"]),
    lower = estimate - critical_value * se,
    upper = estimate + critical_value * se,
    model_n = stats::nobs(band_model),
    model_ward_pairs = n_pairs,
    table_estimate = unname(table_row["Estimate"]),
    table_se = unname(table_row["Std. Error"])
  ) |>
    dplyr::left_join(band_counts, by = "distance_band")

  results[[i]] <- band_results

  model_coefficients <- stats::coef(band_model)
  band_names <- as.character(model_data$distance_band)
  band_main_effect <- unname(
    model_coefficients[paste0("distance_band", band_names)]
  )
  band_main_effect[band_names == "b1"] <- 0
  band_side_effect <- unname(
    model_coefficients[paste0("distance_band", band_names, ":side")]
  )
  model_residual <- stats::residuals(band_model)

  if (length(model_residual) != nrow(model_data)) {
    stop("The band model dropped observations before constructing the plot.")
  }

  model_data <- model_data |>
    dplyr::mutate(
      adjusted_outcome = model_residual +
        band_main_effect +
        side * band_side_effect,
      adjusted_outcome = adjusted_outcome +
        mean(log_outcome) -
        mean(adjusted_outcome),
      signed_distance_ft = dplyr::if_else(
        side == 1,
        absolute_distance_ft,
        -absolute_distance_ft
      ),
      signed_bin = pmax(
        0L,
        pmin(
          floor((signed_distance_ft + 500) / 50),
          19L
        )
      ),
      signed_bin_midpoint = -475 + 50 * signed_bin,
      side_label = factor(
        side,
        levels = c(0, 1),
        labels = c("Less stringent", "More stringent")
      )
    )

  band_means <- model_data |>
    dplyr::group_by(distance_band, side, side_label) |>
    dplyr::summarise(
      band_adjusted_mean = mean(adjusted_outcome),
      n_projects_band_side = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      band_number = as.integer(distance_band),
      x_start = dplyr::if_else(
        side == 0,
        -100 * band_number,
        100 * (band_number - 1)
      ),
      x_end = dplyr::if_else(
        side == 0,
        -100 * (band_number - 1),
        100 * band_number
      )
    )

  binned_data <- model_data |>
    dplyr::group_by(
      distance_band,
      side,
      side_label,
      signed_bin_midpoint
    ) |>
    dplyr::summarise(
      adjusted_mean = mean(adjusted_outcome),
      n_projects = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      band_means |>
        dplyr::select(
          distance_band,
          side,
          band_adjusted_mean,
          n_projects_band_side
        ),
      by = c("distance_band", "side")
    ) |>
    dplyr::mutate(
      sample = sample_name,
      outcome
    )

  binned_results[[i]] <- binned_data

  binned_panels[[i]] <- ggplot2::ggplot() +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "gray45",
      linetype = "dashed",
      linewidth = 0.45
    ) +
    ggplot2::geom_segment(
      data = band_means,
      ggplot2::aes(
        x = x_start,
        xend = x_end,
        y = band_adjusted_mean,
        yend = band_adjusted_mean,
        color = side_label
      ),
      linewidth = 0.85
    ) +
    ggplot2::geom_point(
      data = binned_data,
      ggplot2::aes(
        x = signed_bin_midpoint,
        y = adjusted_mean,
        color = side_label,
        size = n_projects
      ),
      alpha = 0.85
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Less stringent" = "#2478B5",
        "More stringent" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_size_continuous(
      range = c(1.5, 4.5),
      name = "Projects in 50ft bin"
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = "Dots: 50ft binned means; lines: 100ft adjusted means",
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

  panels[[i]] <- ggplot2::ggplot(
    band_results,
    ggplot2::aes(
      x = distance_band_label,
      y = estimate,
      group = 1
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "gray55",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_line(
      color = "#176B58",
      linewidth = 0.65
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      color = "#176B58",
      width = 0.12,
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      color = "#176B58",
      size = 2.4
    ) +
    ggplot2::labs(
      title = panel_specs$panel_title[i],
      subtitle = sprintf(
        "Table 2 estimate = %.3f (SE %.3f)",
        unname(table_row["Estimate"]),
        unname(table_row["Std. Error"])
      ),
      x = "Distance from ward boundary (feet)",
      y = "Adjusted difference: more stringent minus less stringent"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )
}

combined_plot <- (
  (panels[[1]] | panels[[2]]) /
    (panels[[3]] | panels[[4]])
)

ggplot2::ggsave(
  "../output/density_distance_band_estimates.pdf",
  combined_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_distance_band_estimates.png",
  combined_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
readr::write_csv(
  dplyr::bind_rows(results),
  "../output/density_distance_band_estimates.csv",
  na = ""
)

binned_plot <- (
  (binned_panels[[1]] | binned_panels[[2]]) /
    (binned_panels[[3]] | binned_panels[[4]])
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/density_residualized_binned_means.pdf",
  binned_plot,
  width = 11.2,
  height = 7.8
)
ggplot2::ggsave(
  "../output/density_residualized_binned_means.png",
  binned_plot,
  width = 11.2,
  height = 7.8,
  dpi = 180
)
readr::write_csv(
  dplyr::bind_rows(binned_results),
  "../output/density_residualized_binned_means.csv",
  na = ""
)
