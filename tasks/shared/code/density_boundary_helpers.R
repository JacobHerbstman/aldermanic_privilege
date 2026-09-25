# Specification and estimation shared by the density boundary tasks: density_main_results, density_appendix_results,
# density_boundary_checks and density_score_robustness.
density_start_year <- 2006L
density_end_year <- 2022L
density_bandwidth_ft <- 500L
density_bin_width_ft <- 100L
# Demographic controls for the building's own ward.
density_controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# Buildings on the same boundary segment while the same two aldermen serve it (joint service), with zoning-group
# fixed effects.
density_fixed_effects <- "segment_id^joint_service + zone_group"
density_cluster <- "ward_pair"
# All construction, multifamily buildings (two or more units, not single-family or townhouse homes), and multifamily
# buildings with five or more units.
density_samples <- tibble::tribble(
  ~sample, ~min_units, ~label,
  "all", 0L, "All residential new construction",
  "multifamily", 2L, "New multifamily construction",
  "multifamily_5plus", 5L, "New multifamily construction, 5+ units"
)
density_bin_edges <- seq(-density_bandwidth_ft, density_bandwidth_ft, by = density_bin_width_ft)
density_bin_labels <- sprintf("bin_%02d", seq_len(length(density_bin_edges) - 1L))
# The reference band is [-100, 0) ft; the first band on the more-stringent side is [0, 100) ft.
density_reference_bin <- density_bin_labels[density_bandwidth_ft / density_bin_width_ft]
density_first_bin <- density_bin_labels[density_bandwidth_ft / density_bin_width_ft + 1L]

density_column_types <- readr::cols(
  building_id = readr::col_character(),
  permit_ids = readr::col_character(),
  ward_pair = readr::col_character(),
  segment_id = readr::col_character(),
  joint_service = readr::col_character(),
  .default = readr::col_guess()
)

stars <- function(p_value) {
  dplyr::case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.10 ~ "*", TRUE ~ "")
}

# Buildings with a measured dwelling density and every regression variable.
density_analysis_sample <- function(buildings) {
  buildings |>
    dplyr::filter(
      construction_year >= density_start_year,
      construction_year <= density_end_year,
      allow_dupac,
      density_dupac > 0,
      !is.na(joint_service),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      ward_pair != "",
      is.finite(share_white_own),
      is.finite(share_black_own),
      is.finite(median_hh_income_own),
      is.finite(share_bach_plus_own),
      is.finite(homeownership_rate_own)
    )
}

filter_density_sample <- function(buildings, sample) {
  min_units <- density_samples$min_units[density_samples$sample == sample]
  dplyr::filter(buildings, .env$sample == "all" | (multifamily & dwelling_units >= .env$min_units))
}

# Keep buildings within the window around the cutoff and assign distance bands; running_distance_ft is positive on
# the more-stringent side (or right of a placebo cutoff).
bin_running_distance <- function(buildings) {
  buildings |>
    dplyr::filter(abs(running_distance_ft) < density_bandwidth_ft) |>
    dplyr::mutate(
      stricter_side = as.integer(running_distance_ft >= 0),
      distance_bin = cut(
        running_distance_ft,
        breaks = density_bin_edges,
        labels = density_bin_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )
}

# Distance-band model and average-difference model (one indicator for the more-stringent side in place of the bands).
fit_density_boundary <- function(model_data, outcome = "log(density_dupac)", controls = density_controls,
                                 fixed_effects = density_fixed_effects) {
  regressors <- paste(c(sprintf("i(distance_bin, ref = '%s')", density_reference_bin), controls), collapse = " + ")
  bins_model <- fixest::feols(
    stats::as.formula(sprintf("%s ~ %s | %s", outcome, regressors, fixed_effects)),
    data = model_data, cluster = stats::as.formula(paste("~", density_cluster)), warn = FALSE, notes = FALSE
  )
  average_model <- fixest::feols(
    stats::as.formula(sprintf("%s ~ %s | %s", outcome, paste(c("stricter_side", controls), collapse = " + "),
      fixed_effects)),
    data = model_data, cluster = stats::as.formula(paste("~", density_cluster)), warn = FALSE, notes = FALSE
  )

  coefficients <- fixest::coeftable(bins_model)
  bin_rows <- match(paste0("distance_bin::", density_bin_labels), rownames(coefficients))
  ward_pairs <- dplyr::n_distinct(model_data[[density_cluster]])
  critical_value <- stats::qt(0.975, df = ward_pairs - 1L)
  bins <- tibble::tibble(
    distance_bin = density_bin_labels,
    bin_center_ft = utils::head(density_bin_edges, -1) + density_bin_width_ft / 2,
    estimate = dplyr::if_else(distance_bin == density_reference_bin, 0, coefficients[bin_rows, "Estimate"]),
    std_error = coefficients[bin_rows, "Std. Error"],
    p_value = coefficients[bin_rows, "Pr(>|t|)"]
  ) |>
    dplyr::mutate(
      ci_low = dplyr::if_else(distance_bin == density_reference_bin, 0, estimate - critical_value * std_error),
      ci_high = dplyr::if_else(distance_bin == density_reference_bin, 0, estimate + critical_value * std_error)
    )
  average <- fixest::coeftable(average_model)["stricter_side", ]

  list(
    bins = bins,
    first_bin = bins[bins$distance_bin == density_first_bin, ],
    average = tibble::tibble(
      estimate = average[["Estimate"]], std_error = average[["Std. Error"]], p_value = average[["Pr(>|t|)"]]
    ),
    observations = stats::nobs(bins_model),
    ward_pairs = ward_pairs
  )
}

# One figure panel in the paper's boundary-plot style.
plot_density_boundary <- function(fit, title, cutoff_ft = 0) {
  sides <- if (cutoff_ft == 0) c("Less Stringent", "More Stringent") else c("Left of cutoff", "Right of cutoff")
  bins <- fit$bins |>
    dplyr::mutate(side = factor(dplyr::if_else(bin_center_ft < 0, sides[1], sides[2]), sides))
  side_colors <- stats::setNames(c("#2478B5", "#D92D27"), sides)

  ggplot2::ggplot(bins, ggplot2::aes(x = bin_center_ft, y = estimate, color = side, group = side)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high, fill = side), alpha = 0.16, color = NA) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(values = side_colors, name = NULL) +
    ggplot2::scale_fill_manual(values = side_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(-density_bandwidth_ft, density_bandwidth_ft),
      breaks = seq(-density_bandwidth_ft, density_bandwidth_ft, length.out = 5)
    ) +
    ggplot2::labs(
      title = sprintf("Dwelling units per acre\n%s (N = %s)", title, format(fit$observations, big.mark = ",")),
      subtitle = sprintf(
        "%s = %.3f%s (SE %.3f)\nAverage difference = %.3f%s (SE %.3f)",
        if (cutoff_ft == 0) "Difference across boundary" else "Difference at placebo cutoff",
        round(fit$first_bin$estimate, 3) + 0, stars(fit$first_bin$p_value), fit$first_bin$std_error,
        round(fit$average$estimate, 3) + 0, stars(fit$average$p_value), fit$average$std_error
      ),
      x = if (cutoff_ft == 0) "Distance to ward boundary (feet)" else "Distance to placebo cutoff (feet)",
      y = "Units per acre (log difference)"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Three panels side by side (all, multifamily, multifamily 5+) with one legend.
save_density_figure <- function(panels, path) {
  figure <- patchwork::wrap_plots(panels, ncol = 3) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(path, figure, width = 18, height = 6, bg = "white")
}
