# setwd("tasks/density_appendix_results/code")

source("../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../input/new_construction_analysis_data.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::mutate(true_distance_ft = signed_distance_m / 0.3048)

if (anyDuplicated(projects$project_id) > 0L) {
  stop("New-construction data must be unique by project ID.")
}

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "All construction: Log(FAR)",
  "multifamily", "density_far", "Multifamily: Log(FAR)",
  "all", "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

check_specs <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft, ~figure_title,
  "placebo_neg1000ft", -1000, 0,
  "Placebo cutoff 1,000 feet inside the less-stringent ward",
  "placebo_pos1000ft", 1000, 0,
  "Placebo cutoff 1,000 feet inside the more-stringent ward",
  "donut25ft", 0, 25,
  "True ward boundary, excluding projects within 25 feet",
  "donut50ft", 0, 50,
  "True ward boundary, excluding projects within 50 feet"
)

for (check_i in seq_len(nrow(check_specs))) {
  panels <- vector("list", nrow(panel_specs))

  for (panel_i in seq_len(nrow(panel_specs))) {
    cutoff_ft <- check_specs$cutoff_ft[check_i]
    donut_ft <- check_specs$donut_ft[check_i]
    sample_name <- panel_specs$sample[panel_i]
    outcome <- panel_specs$outcome[panel_i]

    model_data <- projects |>
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
        construction_year >= 2006L,
        construction_year <= 2022L,
        within_1500ft,
        abs(running_distance_ft) < 500,
        donut_ft == 0 | abs(running_distance_ft) >= donut_ft,
        !is.na(distance_bin),
        dwelling_units > 0,
        sample_name == "all" | external_multifamily,
        allow_far,
        allow_dupac,
        is.finite(density_far),
        density_far > 0,
        is.finite(density_dupac),
        density_dupac > 0,
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

    model <- fixest::feols(
      log_outcome ~
        i(distance_bin, ref = "bin_05") +
        share_white_own +
        share_black_own +
        median_hh_income_own +
        share_bach_plus_own +
        homeownership_rate_own |
        zone_group + segment_id + construction_year,
      data = model_data,
      cluster = ~ward_pair,
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
        estimate = dplyr::if_else(
          distance_bin == "bin_05",
          0,
          estimate
        ),
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
        ribbon_low = dplyr::if_else(
          distance_bin == "bin_05",
          0,
          ci_low
        ),
        ribbon_high = dplyr::if_else(
          distance_bin == "bin_05",
          0,
          ci_high
        ),
        cutoff_side = dplyr::case_when(
          cutoff_ft == 0 & bin_center_ft < 0 ~ "Less Stringent",
          cutoff_ft == 0 ~ "More Stringent",
          bin_center_ft < 0 ~ "Below Placebo Cutoff",
          TRUE ~ "Above Placebo Cutoff"
        ),
        check = check_specs$check[check_i],
        cutoff_ft,
        donut_ft,
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

    panels[[panel_i]] <- ggplot2::ggplot(
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
          "Above Placebo Cutoff" = "#D92D27",
          "Less Stringent" = "#2478B5",
          "More Stringent" = "#D92D27"
        ),
        name = NULL
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Below Placebo Cutoff" = "#2478B5",
          "Above Placebo Cutoff" = "#D92D27",
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
        title = panel_specs$panel_title[panel_i],
        subtitle = sprintf(
          "Nearest-bin difference = %.3f%s (SE %.3f)",
          nearest_above$estimate,
          stars,
          nearest_above$std_error
        ),
        x = if (cutoff_ft == 0) {
          "Distance to ward boundary (feet)"
        } else {
          "Distance to placebo cutoff (feet)"
        },
        y = if (cutoff_ft == 0) {
          "Difference from nearest less-stringent bin"
        } else {
          "Difference from nearest below-cutoff bin"
        }
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

  }

  combined_plot <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(
      title = check_specs$figure_title[check_i]
    ) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    sprintf(
      "../output/density_%s.pdf",
      check_specs$check[check_i]
    ),
    combined_plot,
    width = 12,
    height = 8.5,
    bg = "white"
  )
}
