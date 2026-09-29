# setwd("tasks/explore_land_prices/code")
# Exploratory: the land-price estimates of estimate_land_boundary.R (stricter against more lenient side, within 500 ft)
# at the real ward boundaries and at the placebo lines, for vacant-lot sales, teardowns followed by a new building and
# not, and all land sales, by the processing-time index and the stall rate. 95 percent intervals use the t
# distribution with (ward pairs - 1) degrees of freedom, as in the paper's figures.
panels <- tidyr::expand_grid(
  tibble::tibble(measure = c("processing_time_index", "stall_rate"),
    measure_label = c("by the processing-time index", "by the alderman's stall rate")),
  tibble::tibble(sample = c("vacant", "teardown_redeveloped", "teardown_not_redeveloped", "all"),
    sample_label = c("Vacant-lot sales", "Teardowns followed by a new building", "Teardowns not followed by one",
      "All land sales"))
)

source("../../setup_environment/code/packages.R")

stars <- function(p) dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
estimates <- read_csv("../output/land_boundary_estimates.csv", show_col_types = FALSE) |>
  filter(design == "stricter_side", within_ft == 500) |>
  inner_join(panels, by = c("measure", "sample"), relationship = "many-to-one") |>
  mutate(line = factor(if_else(offset_ft == 0, "Ward boundary", "Placebo line inside one ward"),
      c("Placebo line inside one ward", "Ward boundary")),
    critical_value = stats::qt(0.975, df = ward_pairs - 1),
    ci_low = estimate - critical_value * std_error, ci_high = estimate + critical_value * std_error)
line_colors <- c("Placebo line inside one ward" = "#2478B5", "Ward boundary" = "#D92D27")

plots <- lapply(seq_len(nrow(panels)), function(i) {
  data <- filter(estimates, measure == panels$measure[i], sample == panels$sample[i])
  boundary <- filter(data, offset_ft == 0)
  ggplot(data, aes(x = offset_ft, y = estimate, color = line)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 60, linewidth = 0.55) +
    geom_point(size = 2.6) +
    scale_color_manual(values = line_colors, name = NULL) +
    scale_x_continuous(breaks = c(-1000, -750, -500, 0, 500, 750, 1000)) +
    labs(title = sprintf("%s\n%s, within 500 ft (N = %s)", panels$sample_label[i], panels$measure_label[i],
        format(boundary$sales, big.mark = ",")),
      subtitle = sprintf("Stricter side difference = %.3f%s (SE %.3f)", boundary$estimate, stars(boundary$p_value),
        boundary$std_error),
      x = "Position of the line relative to the ward boundary (feet)",
      y = "Log price per sq ft, stricter minus lenient side") +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 14, face = "bold"), axis.title = element_text(size = 9),
      axis.text = element_text(size = 8), panel.grid.minor = element_blank())
})
figure <- patchwork::wrap_plots(plots, ncol = 4, byrow = TRUE) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("../output/land_boundary_placebo_lines.png", figure, width = 24, height = 8.5, bg = "white")
