# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../setup_environment/code/packages.R")
setFixest_notes(FALSE)

regime_scores <- readRDS("../output/regime_scores.rds")
pooled_scores <- readRDS("../output/score_leaveout_data.rds")$baseline_score |>
  dplyr::select(alderman, score)

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
if (anyDuplicated(projects$project_id)) {
  stop("Construction input is not unique by project ID.", call. = FALSE)
}

add_endpoint_scores <- function(data, scores, prefix) {
  data |>
    dplyr::left_join(
      scores |>
        dplyr::transmute(alderman_own = alderman, !!paste0(prefix, "_own") := score),
      by = "alderman_own",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      scores |>
        dplyr::transmute(
          alderman_neighbor = alderman,
          !!paste0(prefix, "_neighbor") := score
        ),
      by = "alderman_neighbor",
      relationship = "many-to-one"
    )
}

projects <- projects |>
  add_endpoint_scores(pooled_scores, "pooled") |>
  add_endpoint_scores(regime_scores$early |> dplyr::select(alderman, score), "early") |>
  add_endpoint_scores(
    regime_scores$transition |> dplyr::select(alderman, score),
    "transition"
  ) |>
  dplyr::mutate(
    dated_score_own = dplyr::if_else(
      construction_year <= 2014L,
      early_own,
      transition_own
    ),
    dated_score_neighbor = dplyr::if_else(
      construction_year <= 2014L,
      early_neighbor,
      transition_neighbor
    ),
    pooled_distance_ft = abs(distance_to_boundary_ft) * sign(pooled_own - pooled_neighbor),
    dated_distance_ft = abs(distance_to_boundary_ft) *
      sign(dated_score_own - dated_score_neighbor),
    pooled_pair_average = (pooled_own + pooled_neighbor) / 2,
    dated_pair_average = (dated_score_own + dated_score_neighbor) / 2,
    complete_pooled = is.finite(pooled_own) & is.finite(pooled_neighbor) &
      pooled_own != pooled_neighbor,
    complete_dated = is.finite(dated_score_own) & is.finite(dated_score_neighbor) &
      dated_score_own != dated_score_neighbor
  )

base_sample <- projects |>
  dplyr::filter(
    construction_year >= 2006L,
    construction_year <= 2022L,
    within_500ft,
    dwelling_units > 0,
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
    ward_pair != "",
    abs(distance_to_boundary_ft) < 500
  )

panel_specs <- tibble::tribble(
  ~sample, ~outcome, ~panel_title,
  "all", "density_far", "All construction: Log(FAR)",
  "multifamily", "density_far", "Multifamily: Log(FAR)",
  "all", "density_dupac", "All construction: Log(DUPAC)",
  "multifamily", "density_dupac", "Multifamily: Log(DUPAC)"
)

plot_versions <- list(
  pooled_full = list(
    label = "Current pooled score; full sample",
    data = base_sample |> dplyr::filter(complete_pooled),
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  pooled_common = list(
    label = "Current pooled score; common sample",
    data = base_sample |> dplyr::filter(complete_pooled, complete_dated),
    distance = "pooled_distance_ft",
    pair_average = "pooled_pair_average"
  ),
  dated_common = list(
    label = "Period-specific score; same common sample",
    data = base_sample |> dplyr::filter(complete_pooled, complete_dated),
    distance = "dated_distance_ft",
    pair_average = "dated_pair_average"
  )
)

for (version_name in names(plot_versions)) {
  version <- plot_versions[[version_name]]
  panels <- vector("list", nrow(panel_specs))

  for (i in seq_len(nrow(panel_specs))) {
    sample_name <- panel_specs$sample[i]
    outcome <- panel_specs$outcome[i]
    model_data <- version$data |>
      dplyr::filter(sample_name == "all" | external_multifamily) |>
      dplyr::mutate(
        running_distance_ft = .data[[version$distance]],
        pair_average = .data[[version$pair_average]],
        log_outcome = log(.data[[outcome]]),
        distance_bin = cut(
          running_distance_ft,
          breaks = seq(-500, 500, by = 100),
          labels = sprintf("bin_%02d", 1:10),
          include.lowest = TRUE,
          right = FALSE
        )
      ) |>
      dplyr::filter(!is.na(distance_bin))

    model <- fixest::feols(
      log_outcome ~
        i(distance_bin, ref = "bin_05") +
        pair_average +
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
      bin_center_ft = seq(-450, 450, by = 100)
    ) |>
      dplyr::left_join(estimates, by = "distance_bin", relationship = "one-to-one") |>
      dplyr::mutate(
        estimate = dplyr::if_else(distance_bin == "bin_05", 0, estimate),
        std_error = dplyr::if_else(distance_bin == "bin_05", NA_real_, std_error),
        p_value = dplyr::if_else(distance_bin == "bin_05", NA_real_, p_value),
        ci_low = estimate - critical_value * std_error,
        ci_high = estimate + critical_value * std_error,
        ribbon_low = dplyr::if_else(distance_bin == "bin_05", 0, ci_low),
        ribbon_high = dplyr::if_else(distance_bin == "bin_05", 0, ci_high),
        side_label = dplyr::if_else(
          bin_center_ft < 0,
          "Less Stringent",
          "More Stringent"
        )
      )

    nearest <- results |>
      dplyr::filter(bin_start_ft == 0)
    stars <- dplyr::case_when(
      nearest$p_value < 0.01 ~ "***",
      nearest$p_value < 0.05 ~ "**",
      nearest$p_value < 0.10 ~ "*",
      TRUE ~ ""
    )

    panels[[i]] <- ggplot2::ggplot(
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
        ggplot2::aes(ymin = ribbon_low, ymax = ribbon_high, fill = side_label),
        alpha = 0.16,
        color = NA
      ) +
      ggplot2::geom_line(linewidth = 0.65) +
      ggplot2::geom_point(size = 2.3) +
      ggplot2::scale_color_manual(
        values = c("Less Stringent" = "#2478B5", "More Stringent" = "#D92D27"),
        name = NULL
      ) +
      ggplot2::scale_fill_manual(
        values = c("Less Stringent" = "#2478B5", "More Stringent" = "#D92D27"),
        guide = "none"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(-500, 500),
        breaks = c(-500, -250, 0, 250, 500)
      ) +
      ggplot2::labs(
        title = panel_specs$panel_title[i],
        subtitle = sprintf(
          "Nearest-bin difference = %.3f%s (SE %.3f)",
          nearest$estimate,
          stars,
          nearest$std_error
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

  combined <- patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(title = version$label) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave(
    paste0("../output/regime_density_rd_", version_name, ".pdf"),
    combined,
    width = 12,
    height = 8.5,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0("../output/regime_density_rd_", version_name, ".png"),
    combined,
    width = 12,
    height = 8.5,
    dpi = 180,
    bg = "white"
  )
}
