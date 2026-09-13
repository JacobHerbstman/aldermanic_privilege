# Estimate and plot the same distance-bin specification for paper and audit comparisons.
star_string <- function(p_value) {
  dplyr::case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

estimate_bins <- function(
    data,
    market,
    cutoff_ft,
    donut_ft,
    straight_only,
    property_type_fe,
    panel_title,
    rent_controls, sales_controls, rent_fixed_effects, sales_fixed_effects,
    bandwidth_ft, bin_width_ft, cluster) {
  controls <- strsplit(if (market == "rent") rent_controls else sales_controls, " + ", fixed = TRUE)[[1]]
  if (property_type_fe) {
    controls <- c(
      controls,
      if (market == "rent") {
        "building_type_factor"
      } else {
        "property_class_factor"
      }
    )
  }
  outcome <- if (market == "rent") "rent_price" else "sale_price"
  fixed_effects <- if (market == "rent") {
    rent_fixed_effects
  } else {
    sales_fixed_effects
  }

  bin_edges <- seq(-bandwidth_ft, bandwidth_ft, by = bin_width_ft)
  bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
  reference_bin <- bin_labels[bandwidth_ft / bin_width_ft]

  model_data <- data |>
    dplyr::mutate(
      running_distance_ft = signed_dist_ft - cutoff_ft,
      distance_bin = cut(
        running_distance_ft,
        breaks = bin_edges,
        labels = bin_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < bandwidth_ft,
      donut_ft == 0 | abs(running_distance_ft) >= donut_ft,
      !straight_only | straight_boundary,
      !is.na(distance_bin)
    )

  formula <- stats::as.formula(sprintf(
    "log(%s) ~ i(distance_bin, ref = '%s') + %s | %s",
    outcome,
    reference_bin,
    paste(controls, collapse = " + "),
    fixed_effects
  ))
  model <- fixest::feols(
    formula,
    data = model_data,
    cluster = stats::as.formula(paste("~", cluster)),
    warn = FALSE,
    notes = FALSE
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

  cluster_count <- dplyr::n_distinct(model_data[[cluster]])
  critical_value <- stats::qt(0.975, df = cluster_count - 1L)
  results <- tibble::tibble(
    distance_bin = bin_labels,
    bin_start_ft = head(bin_edges, -1),
    bin_end_ft = tail(bin_edges, -1),
    bin_center_ft = head(bin_edges, -1) + bin_width_ft / 2
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
      ribbon_low = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        ci_low
      ),
      ribbon_high = dplyr::if_else(
        distance_bin == reference_bin,
        0,
        ci_high
      ),
      side_label = dplyr::case_when(
        cutoff_ft == 0 & bin_center_ft < 0 ~ "Less Stringent",
        cutoff_ft == 0 ~ "More Stringent",
        bin_center_ft < 0 ~ "Left of cutoff",
        TRUE ~ "Right of cutoff"
      )
    )

  nearest_above <- results |>
    dplyr::filter(bin_start_ft == 0)
  nearest_stars <- star_string(nearest_above$p_value)

  plot <- ggplot2::ggplot(
    results,
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
        "More Stringent" = "#D92D27",
        "Left of cutoff" = "#2478B5",
        "Right of cutoff" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Less Stringent" = "#2478B5",
        "More Stringent" = "#D92D27",
        "Left of cutoff" = "#2478B5",
        "Right of cutoff" = "#D92D27"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-bandwidth_ft, bandwidth_ft),
      breaks = seq(-bandwidth_ft, bandwidth_ft, length.out = 5)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "%s = %.3f%s (SE %.3f)",
        if (cutoff_ft == 0) {
          "Difference across boundary"
        } else {
          "Difference at placebo cutoff"
        },
        nearest_above$estimate,
        nearest_stars,
        nearest_above$std_error
      ),
      x = if (cutoff_ft == 0) {
        "Distance to ward boundary (feet)"
      } else {
        "Distance to placebo cutoff (feet)"
      },
      y = if (market == "rent") {
        "Rent (log difference)"
      } else {
        "Sale price (log difference)"
      }
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

  summary <- tibble::tibble(
    market = market,
    property_type_fe = property_type_fe,
    n = stats::nobs(model),
    ward_pair_clusters = cluster_count,
    estimate = nearest_above$estimate,
    std_error = nearest_above$std_error,
    p_value = nearest_above$p_value,
    confidence_low = nearest_above$ci_low,
    confidence_high = nearest_above$ci_high,
    percent_effect = 100 * (exp(nearest_above$estimate) - 1),
    percent_confidence_low = 100 * (exp(nearest_above$ci_low) - 1),
    percent_confidence_high = 100 * (exp(nearest_above$ci_high) - 1)
  )

  list(plot = plot, summary = summary)
}
