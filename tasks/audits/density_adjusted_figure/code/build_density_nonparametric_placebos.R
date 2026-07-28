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

land_area_recovery <- readr::read_csv(
  "../input/commercial_land_adjudication_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    assessor_land_sqft <= 1,
    exact_construction_year_components,
    is.finite(project_land_area_sqft),
    project_land_area_sqft > 0
  ) |>
  dplyr::select(project_id, recovered_land_sqft = project_land_area_sqft)

if (anyDuplicated(land_area_recovery$project_id) > 0) {
  stop("Recovered land areas are not unique by project ID.", call. = FALSE)
}

data <- data |>
  dplyr::left_join(
    land_area_recovery,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    land_sqft = dplyr::coalesce(recovered_land_sqft, land_sqft),
    density_far = dplyr::if_else(
      allow_far,
      building_sqft / land_sqft,
      NA_real_
    ),
    density_dupac = dplyr::if_else(
      allow_dupac,
      43560 * dwelling_units / land_sqft,
      NA_real_
    ),
    true_distance_ft = signed_distance_m / 0.3048
  )

estimate_placebo <- function(
    cutoff_ft,
    sample_name,
    outcome,
    panel_title) {
  model_data <- data |>
    dplyr::mutate(
      running_distance_ft = true_distance_ft - cutoff_ft,
      distance_bin = cut(
        running_distance_ft,
        breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10),
        include.lowest = TRUE,
        right = FALSE
      )
    ) |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
      within_1500ft,
      abs(running_distance_ft) < 500,
      !is.na(distance_bin),
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
    dplyr::mutate(log_outcome = log(.data[[outcome]]))

  if (nrow(model_data) == 0L) {
    stop(sprintf(
      "No observations remain for cutoff %d, %s, %s.",
      cutoff_ft,
      sample_name,
      outcome
    ))
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
      cutoff_side = dplyr::if_else(
        bin_center_ft < 0,
        "Below Placebo Cutoff",
        "Above Placebo Cutoff"
      ),
      cutoff_ft,
      sample = sample_name,
      outcome,
      n_projects = stats::nobs(model),
      n_ward_pairs = cluster_count
    )

  nearest_above <- results |>
    dplyr::filter(bin_start_ft == 0)
  stars <- dplyr::case_when(
    nearest_above$p_value < 0.01 ~ "***",
    nearest_above$p_value < 0.05 ~ "**",
    nearest_above$p_value < 0.10 ~ "*",
    TRUE ~ ""
  )

  plot <- ggplot2::ggplot(
    results,
    ggplot2::aes(
      x = bin_center_ft,
      y = estimate,
      color = cutoff_side,
      group = cutoff_side
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
        fill = cutoff_side
      ),
      alpha = 0.16,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_color_manual(
      values = c(
        "Below Placebo Cutoff" = "#2478B5",
        "Above Placebo Cutoff" = "#D92D27"
      ),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Below Placebo Cutoff" = "#2478B5",
        "Above Placebo Cutoff" = "#D92D27"
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
        nearest_above$estimate,
        stars,
        nearest_above$std_error
      ),
      x = "Distance to placebo cutoff (feet)",
      y = "Difference from the nearest below-cutoff bin"
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

for (cutoff_ft in c(-1000, 1000)) {
  panels <- vector("list", nrow(panel_specs))
  cutoff_results <- vector("list", nrow(panel_specs))

  for (i in seq_len(nrow(panel_specs))) {
    estimated <- estimate_placebo(
      cutoff_ft,
      panel_specs$sample[i],
      panel_specs$outcome[i],
      panel_specs$panel_title[i]
    )
    panels[[i]] <- estimated$plot
    cutoff_results[[i]] <- estimated$results
  }

  cutoff_title <- if (cutoff_ft < 0) {
    "Placebo cutoff 1,000 feet inside the less-stringent ward"
  } else {
    "Placebo cutoff 1,000 feet inside the more-stringent ward"
  }
  cutoff_label <- if (cutoff_ft < 0) "neg1000ft" else "pos1000ft"

  combined_plot <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(title = cutoff_title) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_placebo_%s_100ft_ribbons.pdf",
      cutoff_label
    ),
    combined_plot,
    width = 12,
    height = 8.5
  )
  ggplot2::ggsave(
    sprintf(
      "../output/density_nonparametric_placebo_%s_100ft_ribbons.png",
      cutoff_label
    ),
    combined_plot,
    width = 12,
    height = 8.5,
    dpi = 220
  )

  all_results[[cutoff_label]] <- dplyr::bind_rows(cutoff_results)
}

readr::write_csv(
  dplyr::bind_rows(all_results),
  "../output/density_nonparametric_placebo_estimates.csv"
)
