# setwd("tasks/audits/density_window_comparison/code")
# format <- "pdf"
source("../../../setup_environment/code/packages.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) args <- format
stopifnot(length(args) == 1L, args[1] %in% c("pdf", "png"))
format <- args[1]
estimates <- readr::read_csv("../output/density_window_estimates.csv", show_col_types = FALSE) |>
  dplyr::mutate(sample = factor(sample, c("all", "multifamily"),
    c("All residential construction", "Multifamily construction")),
    outcome = factor(outcome, c("density_far", "density_dupac"), c("FAR", "DUPAC")))
plot <- ggplot2::ggplot(estimates, ggplot2::aes(window_ft, percent_difference)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey45", linetype = "dashed") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = percent_ci_low, ymax = percent_ci_high),
    width = 12, color = "#235C88", linewidth = 0.65) +
  ggplot2::geom_line(color = "#235C88", linewidth = 0.8) +
  ggplot2::geom_point(color = "#235C88", size = 2.8) +
  ggplot2::facet_grid(outcome ~ sample) +
  ggplot2::scale_x_continuous(breaks = c(100, 200, 300, 400, 500)) +
  ggplot2::labs(title = "Density differences across progressively wider boundary windows",
    subtitle = "Each point compares all eligible construction on the two sides within the stated distance",
    x = "Maximum distance from the boundary on each side (feet)",
    y = "Adjusted density difference: more-stringent side (%)",
    caption = "95% confidence intervals; standard errors clustered by ward pair. Same neighborhood controls, zoning, segment and year fixed effects.\nEach window re-estimates a single side indicator without distance bins or slopes. This is not the paper's nearest-band coefficient.") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "#EEF3F7"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0, size = 9))
ggplot2::ggsave(sprintf("../output/density_windows.%s", format), plot,
  width = 12, height = 8, dpi = 160, bg = "white")
