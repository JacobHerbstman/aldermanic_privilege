# setwd("tasks/explore_alderman_measures/code")
# Exploratory: the boundary-count estimates of boundary_counts.R at the real ward boundaries and at the placebo lines,
# for upzoning applications (by the stall rate, and by the stall rate from applications away from boundaries) and
# dwelling units built (by the stall rate), within 500 and 250 ft. 95 percent intervals use the t distribution with
# (ward pairs - 1) degrees of freedom, as in the paper's figures.
panels <- tibble::tribble(
  ~outcome, ~measure, ~label,
  "upzoning_applications", "stall_rate", "Upzoning applications\nby the alderman's stall rate",
  "upzoning_applications", "stall_rate_away", "Upzoning applications\nby the stall rate away from boundaries",
  "dwelling_units", "stall_rate", "Dwelling units built\nby the alderman's stall rate"
)

source("../../setup_environment/code/packages.R")

stars <- function(p) dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
estimates <- read_csv("../output/boundary_side_counts.csv", show_col_types = FALSE) |>
  inner_join(panels, by = c("outcome", "measure"), relationship = "many-to-one") |>
  mutate(line = factor(if_else(offset_ft == 0, "Ward boundary", "Placebo line inside one ward"),
      c("Placebo line inside one ward", "Ward boundary")),
    critical_value = stats::qt(0.975, df = ward_pairs - 1),
    ci_low = estimate - critical_value * std_error, ci_high = estimate + critical_value * std_error)
line_colors <- c("Placebo line inside one ward" = "#2478B5", "Ward boundary" = "#D92D27")

plots <- lapply(seq_len(nrow(panels)), function(i) lapply(c(500, 250), function(width) {
  data <- filter(estimates, outcome == panels$outcome[i], measure == panels$measure[i], within_ft == width)
  boundary <- filter(data, offset_ft == 0)
  ggplot(data, aes(x = offset_ft, y = estimate, color = line)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray55", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 60, linewidth = 0.55) +
    geom_point(size = 2.6) +
    scale_color_manual(values = line_colors, name = NULL) +
    scale_x_continuous(breaks = c(-1000, -750, -500, 0, 500, 750, 1000)) +
    labs(title = sprintf("%s, within %d ft", panels$label[i], width),
      subtitle = sprintf("Boundary estimate = %.3f%s (SE %.3f)", boundary$estimate, stars(boundary$p_value),
        boundary$std_error),
      x = "Position of the line relative to the ward boundary (feet)",
      y = "Log difference in count per SD stricter") +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 14, face = "bold"), axis.title = element_text(size = 9),
      axis.text = element_text(size = 8), panel.grid.minor = element_blank())
}))
figure <- patchwork::wrap_plots(unlist(plots, recursive = FALSE), ncol = 2, byrow = TRUE) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave("../output/boundary_counts_placebo_lines.png", figure, width = 12, height = 12.75, bg = "white")
