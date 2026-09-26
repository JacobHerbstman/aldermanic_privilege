# setwd("tasks/presentation_figures/code")
# Slide versions of the paper's event-study figures (tasks/run_event_study_permit), drawn from its saved coefficients
# and pooled estimates, without the paper's panel labels.
source("../../setup_environment/code/packages.R")
source("../../shared/code/density_boundary_helpers.R")

combined <- readr::read_csv("../input/permit_event_study_high_discretion_stable_signed_500ft_coefficients.csv",
  show_col_types = FALSE)
separate <- readr::read_csv("../input/permit_event_study_high_discretion_stable_separate_500ft_coefficients.csv",
  show_col_types = FALSE)
stopifnot(!anyDuplicated(combined$event_time), !anyDuplicated(separate[c("direction", "event_time")]))

event_plot <- function(rows, title, color, ylim = NULL) {
  ggplot2::ggplot(rows, ggplot2::aes(event_time, estimate)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high), fill = color, alpha = 0.18) +
    ggplot2::geom_line(color = color, linewidth = 0.85) +
    ggplot2::geom_point(color = color, size = 2) +
    ggplot2::scale_x_continuous(breaks = rows$event_time) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Pooled Estimate = %.3f%s (SE %.3f)", rows$pooled_estimate[1], stars(rows$pooled_p_value[1]),
        rows$pooled_se[1]),
      x = "Years since 2015 redistricting",
      y = "Effect on annual permits (log points)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14.5, face = "bold"),
      axis.title.x = ggplot2::element_text(hjust = 0.5, margin = ggplot2::margin(t = 8))
    )
}

ggplot2::ggsave("../output/permit_event_study_high_discretion_stable_signed_500ft_slides.pdf",
  event_plot(combined, NULL, "#176B58"), width = 7.6, height = 5.2, bg = "white")

# Both directions on one vertical scale, as in the paper.
separate_limits <- range(c(separate$ci_low, separate$ci_high))
separate_figure <- event_plot(dplyr::filter(separate, direction == "stricter"), "Assigned to more stringent aldermen",
    "#D92D27", separate_limits) +
  event_plot(dplyr::filter(separate, direction == "lenient"), "Assigned to more lenient aldermen", "#2478B5",
    separate_limits)
ggplot2::ggsave("../output/permit_event_study_high_discretion_stable_separate_500ft_slides.pdf", separate_figure,
  width = 11.2, height = 4.3, bg = "white")
