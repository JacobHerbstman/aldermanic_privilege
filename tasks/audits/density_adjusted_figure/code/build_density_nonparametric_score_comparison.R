# setwd("tasks/audits/density_adjusted_figure/code")

source("../../../setup_environment/code/packages.R")

score_labels <- c(
  current_no_income = "Neither income nor bachelor's share",
  education_added_back = "Bachelor's share only",
  income_added_back = "Income only",
  all_covariates = "Income and bachelor's share"
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

scores <- readr::read_csv(
  "../input/current_income_scores.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(cutoff == 2022L) |>
  dplyr::select(variant, alderman, score)

if (anyDuplicated(scores[c("variant", "alderman")]) > 0L) {
  stop("Scores are not unique by variant and alderman.", call. = FALSE)
}

estimate_variant <- function(variant, sample_name, outcome, panel_title) {
  score_map <- scores |>
    dplyr::filter(.data$variant == .env$variant) |>
    dplyr::select(alderman, score)

  model_data <- data |>
    dplyr::left_join(
      score_map |>
        dplyr::rename(alderman_own = alderman, score_own = score),
      by = "alderman_own",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      score_map |>
        dplyr::rename(alderman_neighbor = alderman, score_neighbor = score),
      by = "alderman_neighbor",
      relationship = "many-to-one"
    ) |>
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
      is.finite(score_own),
      is.finite(score_neighbor),
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
      side = as.integer(score_own > score_neighbor),
      pair_average_score = (score_own + score_neighbor) / 2,
      running_distance_ft = dplyr::if_else(
        side == 1L,
        abs(distance_to_boundary_ft),
        -abs(distance_to_boundary_ft)
      ),
      log_outcome = log(.data[[outcome]]),
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      abs(running_distance_ft) < 500,
      !is.na(distance_bin)
    )

  if (nrow(model_data) == 0L) {
    stop(sprintf("No observations remain for %s, %s, %s.", variant, sample_name, outcome))
  }

  model <- fixest::feols(
    log_outcome ~
      i(distance_bin, ref = "bin_05") +
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
  coefficient_rows <- grepl("^distance_bin::", rownames(coefficient_table))
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
    distance_bin = sprintf("bin_%02d", 1:10),
    bin_start_ft = seq(-500, 400, by = 100),
    bin_end_ft = seq(-400, 500, by = 100),
    bin_center_ft = seq(-450, 450, by = 100)
  ) |>
    dplyr::left_join(
      estimates,
      by = "distance_bin",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(distance_bin == "bin_05", 0, estimate),
      std_error = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        std_error
      ),
      p_value = dplyr::if_else(
        distance_bin == "bin_05",
        NA_real_,
        p_value
      ),
      ci_low = estimate - critical_value * std_error,
      ci_high = estimate + critical_value * std_error,
      ribbon_low = dplyr::if_else(distance_bin == "bin_05", 0, ci_low),
      ribbon_high = dplyr::if_else(distance_bin == "bin_05", 0, ci_high),
      side_label = dplyr::if_else(
        bin_center_ft < 0,
        "Less Stringent",
        "More Stringent"
      ),
      variant,
      score_label = unname(score_labels[variant]),
      sample = sample_name,
      outcome,
      n_projects = stats::nobs(model),
      n_ward_pairs = cluster_count
    )

  nearest_strict <- results |>
    dplyr::filter(bin_start_ft == 0)
  stars <- dplyr::case_when(
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
      limits = c(-500, 500),
      breaks = c(-500, -250, 0, 250, 500)
    ) +
    ggplot2::labs(
      title = panel_title,
      subtitle = sprintf(
        "Nearest-bin difference = %.3f%s (SE %.3f)",
        nearest_strict$estimate,
        stars,
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

  list(results = results, plot = plot)
}

all_results <- list()

for (variant in names(score_labels)) {
  panels <- vector("list", nrow(panel_specs))
  variant_results <- vector("list", nrow(panel_specs))

  for (i in seq_len(nrow(panel_specs))) {
    estimated <- estimate_variant(
      variant,
      panel_specs$sample[i],
      panel_specs$outcome[i],
      panel_specs$panel_title[i]
    )
    panels[[i]] <- estimated$plot
    variant_results[[i]] <- estimated$results
  }

  combined_plot <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(
      title = unname(score_labels[variant])
    ) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_bins_100ft_%s_ribbons.pdf",
      variant
    ),
    combined_plot,
    width = 12,
    height = 8.5
  )
  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_bins_100ft_%s_ribbons.png",
      variant
    ),
    combined_plot,
    width = 12,
    height = 8.5,
    dpi = 220
  )

  all_results[[variant]] <- dplyr::bind_rows(variant_results)
}

comparison_results <- dplyr::bind_rows(all_results)

sample_counts <- comparison_results |>
  dplyr::distinct(variant, sample, outcome, n_projects) |>
  tidyr::pivot_wider(names_from = variant, values_from = n_projects)

if (any(apply(sample_counts[names(score_labels)], 1, function(x) length(unique(x))) != 1L)) {
  stop("The estimation sample changes across score definitions.", call. = FALSE)
}

readr::write_csv(
  comparison_results,
  "../output/density_nonparametric_score_comparison.csv"
)
