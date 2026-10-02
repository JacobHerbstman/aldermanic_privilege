# setwd("tasks/presentation_figures/code")
# Slide versions of the paper's price figures (tasks/price_boundary_results), drawn from its saved estimates and
# reporting the average difference, in the same style as the density slides.
source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

estimates <- readr::read_csv("../input/price_boundary_estimates.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(estimates[c("market", "check", "distance_bin")]))

panel <- function(market, check, title) {
  rows <- estimates[estimates$market == market & estimates$check == check, ]
  stopifnot(nrow(rows) == length(density_bin_labels))
  fit <- list(bins = rows, observations = rows$observations[1],
    first_bin = rows[rows$distance_bin == density_first_bin, ],
    average = tibble::tibble(estimate = rows$average_estimate[1], std_error = rows$average_std_error[1],
      p_value = rows$average_p_value[1]))
  cutoff_ft <- c(placebo_neg1000ft = -1000, placebo_pos1000ft = 1000)[check]
  plot_density_boundary(fit, title, if (is.na(cutoff_ft)) 0 else cutoff_ft) +
    ggplot2::labs(
      title = sprintf("%s (N = %s)", title, format(fit$observations, big.mark = ",")),
      subtitle = sprintf("Average difference: %.3f%s (%.3f)", round(fit$average$estimate, 3) + 0,
        stars(fit$average$p_value), fit$average$std_error),
      y = if (market == "rent") "Log rent" else "Log sale price"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10)
    )
}
save_panels <- function(panels, file, width, height) {
  figure <- patchwork::wrap_plots(panels, ncol = 2) + patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave(file.path("../output", file), figure, width = width, height = height, bg = "white")
}

save_panels(list(panel("rent", "main", "Listed rents"), panel("sales", "main", "Home sales")), "price_boundary_main_slides.pdf", 11, 5.2)
save_panels(list(panel("rent", "placebo_neg1000ft", "Rents: 1,000 ft inside less-stringent side"),
  panel("rent", "placebo_pos1000ft", "Rents: 1,000 ft inside more-stringent side"),
  panel("sales", "placebo_neg1000ft", "Sales: 1,000 ft inside less-stringent side"),
  panel("sales", "placebo_pos1000ft", "Sales: 1,000 ft inside more-stringent side")), "price_boundary_placebos_slides.pdf", 14, 7.6)
save_panels(list(panel("rent", "straight", "Listed rents"), panel("sales", "straight", "Home sales")),
  "price_boundary_straight_slides.pdf", 11, 5.2)
save_panels(list(panel("rent", "donut25ft", "Rents: exclude nearest 25 ft"), panel("rent", "donut50ft", "Rents: exclude nearest 50 ft"),
  panel("sales", "donut25ft", "Sales: exclude nearest 25 ft"), panel("sales", "donut50ft", "Sales: exclude nearest 50 ft")),
  "price_boundary_donuts_slides.pdf", 14, 7.6)
