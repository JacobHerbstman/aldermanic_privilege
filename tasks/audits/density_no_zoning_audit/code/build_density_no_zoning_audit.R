# setwd("tasks/audits/density_no_zoning_audit/code")

source("../../../setup_environment/code/packages.R")

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

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "All construction: Log(FAR)",
  "multifamily", "density_far", "Multifamily: Log(FAR)",
  "all", "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

model_specs <- tibble::tribble(
  ~specification, ~fixed_effects, ~require_zoning,
  "Current zoning controls", "zone_group + segment_id + construction_year", TRUE,
  "No zoning controls, same sample", "segment_id + construction_year", TRUE,
  "No zoning controls, all eligible projects", "segment_id + construction_year", FALSE
)

model_results <- list()
no_zoning_panels <- vector("list", nrow(panel_specs))

for (spec_index in seq_len(nrow(model_specs))) {
  specification <- model_specs$specification[spec_index]
  fixed_effects <- model_specs$fixed_effects[spec_index]
  require_zoning <- model_specs$require_zoning[spec_index]

  for (panel_index in seq_len(nrow(panel_specs))) {
    sample_name <- panel_specs$sample[panel_index]
    outcome <- panel_specs$outcome[panel_index]

    model_data <- projects |>
      dplyr::filter(
        construction_year >= 2006L,
        construction_year <= 2022L,
        within_500ft,
        dwelling_units > 0,
        sample_name == "all" | external_multifamily,
        allow_far,
        allow_dupac,
        is.finite(density_far),
        density_far > 0,
        is.finite(density_dupac),
        density_dupac > 0,
        is.finite(pair_average_score),
        is.finite(share_white_own),
        is.finite(share_black_own),
        is.finite(median_hh_income_own),
        is.finite(share_bach_plus_own),
        is.finite(homeownership_rate_own),
        !is.na(segment_id),
        segment_id != "",
        !is.na(ward_pair),
        ward_pair != ""
      ) |>
      dplyr::mutate(
        running_distance_ft = signed_distance_m / 0.3048,
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

    if (require_zoning) {
      model_data <- model_data |>
        dplyr::filter(!is.na(zone_group))
    }

    model <- fixest::feols(
      stats::as.formula(paste0(
        "log_outcome ~ ",
        "i(distance_bin, ref = 'bin_05') + ",
        "pair_average_score + ",
        "share_white_own + share_black_own + ",
        "median_hh_income_own + share_bach_plus_own + ",
        "homeownership_rate_own | ",
        fixed_effects
      )),
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
        side_label = dplyr::if_else(
          bin_center_ft < 0,
          "Less Stringent",
          "More Stringent"
        ),
        specification,
        sample = sample_name,
        outcome,
        n_projects = stats::nobs(model),
        n_ward_pairs = cluster_count
      )

    model_results[[length(model_results) + 1L]] <- results

    if (specification == "No zoning controls, same sample") {
      nearest_stringent <- results |>
        dplyr::filter(bin_start_ft == 0)
      stars <- dplyr::case_when(
        nearest_stringent$p_value < 0.01 ~ "***",
        nearest_stringent$p_value < 0.05 ~ "**",
        nearest_stringent$p_value < 0.10 ~ "*",
        TRUE ~ ""
      )

      no_zoning_panels[[panel_index]] <- ggplot2::ggplot(
        results,
        ggplot2::aes(
          x = bin_center_ft,
          y = estimate,
          color = side_label,
          fill = side_label,
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
          ggplot2::aes(ymin = ribbon_low, ymax = ribbon_high),
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
          title = panel_specs$panel_title[panel_index],
          subtitle = sprintf(
            "Nearest-bin difference = %.3f%s (SE %.3f)",
            nearest_stringent$estimate,
            stars,
            nearest_stringent$std_error
          ),
          x = "Distance to ward boundary (feet)",
          y = "Difference from nearest less-stringent bin"
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
  }
}

results <- dplyr::bind_rows(model_results)
readr::write_csv(
  results,
  "../output/current_density_no_zoning_estimates.csv",
  na = ""
)

no_zoning_plot <- patchwork::wrap_plots(
  no_zoning_panels,
  ncol = 2
) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "../output/current_density_no_zoning.pdf",
  no_zoning_plot,
  width = 12,
  height = 8.5,
  bg = "white"
)
