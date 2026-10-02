# setwd("tasks/presentation_figures/code")
# Slide versions of the paper's density figures (tasks/density_main_results, tasks/density_appendix_results): the same
# estimates from tasks/shared/code/density_boundary_helpers.R, reporting the average difference, with larger text.
placebo_ft <- 1000L
donut_inner_ft <- 25L
donut_outer_ft <- 50L

source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample()
stopifnot(!anyDuplicated(buildings$building_id))

figures <- tibble::tribble(
  ~file, ~cutoff_ft, ~donut_ft,
  "density_rd_slides.pdf", 0L, 0L,
  sprintf("density_placebo_neg%dft_slides.pdf", placebo_ft), -placebo_ft, 0L,
  sprintf("density_placebo_pos%dft_slides.pdf", placebo_ft), placebo_ft, 0L,
  sprintf("density_donut%dft_slides.pdf", donut_inner_ft), 0L, donut_inner_ft,
  sprintf("density_donut%dft_slides.pdf", donut_outer_ft), 0L, donut_outer_ft
)
for (f in seq_len(nrow(figures))) {
  data <- buildings |>
    dplyr::mutate(running_distance_ft = signed_distance_m / 0.3048 - figures$cutoff_ft[f]) |>
    bin_running_distance() |>
    dplyr::filter(abs(running_distance_ft) >= figures$donut_ft[f])
  panels <- lapply(seq_len(nrow(density_samples)), function(i) {
    fit <- fit_density_boundary(filter_density_sample(data, density_samples$sample[i]))
    plot_density_boundary(fit, density_samples$label[i], figures$cutoff_ft[f]) +
      ggplot2::labs(
        title = sprintf("%s (N = %s)", c("All construction", "Multifamily (2+ units)", "Multifamily (5+ units)")[i],
          format(fit$observations, big.mark = ",")),
        subtitle = sprintf("Average difference: %.3f%s (%.3f)",
          round(fit$average$estimate, 3) + 0, stars(fit$average$p_value), fit$average$std_error),
        y = "Log units per acre"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13),
        plot.subtitle = ggplot2::element_text(face = "bold", size = 14),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 10)
      )
  })
  figure <- patchwork::wrap_plots(panels, ncol = 3) + patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave(file.path("../output", figures$file[f]), figure, width = 13, height = 5.2, bg = "white")
}
