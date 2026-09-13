# setwd("tasks/density_appendix_results/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
# placebo_ft <- 1000
# donut_inner_ft <- 25
# donut_outer_ft <- 50

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(start_year, end_year, bandwidth_ft, bin_width_ft, controls, fixed_effects, cluster, placebo_ft, donut_inner_ft, donut_outer_ft)
stopifnot(length(cli_args) == 10L)
start_year <- as.integer(cli_args[1])
end_year <- as.integer(cli_args[2])
bandwidth_ft <- as.integer(cli_args[3])
bin_width_ft <- as.integer(cli_args[4])
controls <- cli_args[5]
fixed_effects <- cli_args[6]
cluster <- cli_args[7]
placebo_ft <- as.integer(cli_args[8])
donut_inner_ft <- as.integer(cli_args[9])
donut_outer_ft <- as.integer(cli_args[10])
stopifnot(start_year <= end_year, bandwidth_ft > 0, bin_width_ft > 0,
  bandwidth_ft %% bin_width_ft == 0)
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, by = bin_width_ft)
bin_labels <- sprintf("bin_%02d", seq_len(length(bin_edges) - 1L))
reference_bin <- bin_labels[bandwidth_ft / bin_width_ft]

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
)

if (anyDuplicated(projects$project_id) > 0L) {
  stop("New-construction data must be unique by project ID.")
}

projects <- projects |>
  dplyr::mutate(true_distance_ft = signed_distance_m / 0.3048)

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "Floor-area ratio\nAll residential new construction",
  "multifamily", "density_far", "Floor-area ratio\nNew multifamily construction",
  "all", "density_dupac", "Dwelling units per acre\nAll residential new construction",
  "multifamily", "density_dupac", "Dwelling units per acre\nNew multifamily construction"
)

check_specs <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft,
  paste0("placebo_neg", placebo_ft, "ft"), -placebo_ft, 0,
  paste0("placebo_pos", placebo_ft, "ft"), placebo_ft, 0,
  paste0("donut", donut_inner_ft, "ft"), 0, donut_inner_ft,
  paste0("donut", donut_outer_ft, "ft"), 0, donut_outer_ft
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
          breaks = bin_edges,
          labels = bin_labels,
          include.lowest = TRUE,
          right = FALSE
        )
      ) |>
      dplyr::filter(
        construction_year >= start_year,
        construction_year <= end_year,
        abs(true_distance_ft) <= placebo_ft + bandwidth_ft,
        abs(running_distance_ft) < bandwidth_ft,
        donut_ft == 0 | abs(running_distance_ft) >= donut_ft,
        !is.na(distance_bin),
        sample_name == "all" | external_multifamily,
        density_eligible,
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
      stats::as.formula(sprintf(
        "log_outcome ~ i(distance_bin, ref = '%s') + %s | %s",
        reference_bin, controls, fixed_effects
      )),
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
        cutoff_side = dplyr::case_when(
          cutoff_ft == 0 & bin_center_ft < 0 ~ "Less Stringent",
          cutoff_ft == 0 ~ "More Stringent",
          bin_center_ft < 0 ~ "Left of cutoff",
          TRUE ~ "Right of cutoff"
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
          "Left of cutoff" = "#2478B5",
          "Right of cutoff" = "#D92D27",
          "Less Stringent" = "#2478B5",
          "More Stringent" = "#D92D27"
        ),
        name = NULL
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Left of cutoff" = "#2478B5",
          "Right of cutoff" = "#D92D27",
          "Less Stringent" = "#2478B5",
          "More Stringent" = "#D92D27"
        ),
        guide = "none"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(-bandwidth_ft, bandwidth_ft),
        breaks = seq(-bandwidth_ft, bandwidth_ft, length.out = 5)
      ) +
      ggplot2::labs(
        title = panel_specs$panel_title[panel_i],
        subtitle = sprintf(
          "%s = %.3f%s (SE %.3f)",
          if (cutoff_ft == 0) {
            "Difference across boundary"
          } else {
            "Difference at placebo cutoff"
          },
          nearest_above$estimate,
          stars,
          nearest_above$std_error
        ),
        x = if (cutoff_ft == 0) {
          "Distance to ward boundary (feet)"
        } else {
          "Distance to placebo cutoff (feet)"
        },
        y = if (outcome == "density_far") {
          "Floor-area ratio (log difference)"
        } else {
          "Units per acre (log difference)"
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

  }

  combined_plot <- patchwork::wrap_plots(panels, ncol = 2) +
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
