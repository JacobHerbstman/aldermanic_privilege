# setwd("tasks/audits/density_window_comparison/code")
# outcome_scale <- "log"
# format <- "pdf"
source("../../../setup_environment/code/packages.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) args <- c(outcome_scale, format)
stopifnot(length(args) == 2L, args[1] %in% c("log", "levels"), args[2] %in% c("pdf", "png"))
outcome_scale <- args[1]
format <- args[2]
if (outcome_scale == "log") {
  estimates <- readr::read_csv("../output/density_window_estimates.csv", show_col_types = FALSE)
} else {
  estimates <- readr::read_csv("../output/density_window_estimates_levels.csv", show_col_types = FALSE)
}
estimates <- estimates |>
  dplyr::mutate(sample = factor(sample, c("all", "multifamily"),
    c("All residential construction", "Multifamily construction")),
    outcome = factor(outcome, c("density_far", "density_dupac"), c("FAR", "DUPAC")),
    effect = if (outcome_scale == "log") percent_difference else estimate,
    lower = if (outcome_scale == "log") percent_ci_low else ci_low,
    upper = if (outcome_scale == "log") percent_ci_high else ci_high)
plot <- ggplot2::ggplot(estimates, ggplot2::aes(window_ft, effect)) +
  ggplot2::geom_hline(yintercept = 0, color = "grey45", linetype = "dashed") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
    width = 12, color = "#235C88", linewidth = 0.65) +
  ggplot2::geom_line(color = "#235C88", linewidth = 0.8) +
  ggplot2::geom_point(color = "#235C88", size = 2.8) +
  ggplot2::facet_grid(outcome ~ sample, scales = if (outcome_scale == "log") "fixed" else "free_y") +
  ggplot2::scale_x_continuous(breaks = c(100, 200, 300, 400, 500)) +
  ggplot2::labs(title = if (outcome_scale == "log") "Density differences across progressively wider boundary windows" else "Density differences in levels across boundary windows",
    subtitle = "Each point compares all eligible construction on the two sides within the stated distance",
    x = "Maximum distance from the boundary on each side (feet)",
    y = if (outcome_scale == "log") "Adjusted density difference: more-stringent side (%)" else "Adjusted difference: FAR units (top); dwelling units per acre (bottom)",
    caption = "95% confidence intervals; standard errors clustered by ward pair. Same neighborhood controls, zoning, segment and year fixed effects.\nEach window re-estimates a single side indicator without distance bins or slopes. This is not the paper's nearest-band coefficient.") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "#EEF3F7"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0, size = 9))
ggplot2::ggsave(sprintf(if (outcome_scale == "log") "../output/density_windows.%s" else "../output/density_windows_levels.%s", format), plot,
  width = 12, height = 8, dpi = 160, bg = "white")
