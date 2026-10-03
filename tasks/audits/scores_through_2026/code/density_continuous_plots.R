# setwd("tasks/audits/scores_through_2026/code")
# Exploratory: the paper's density figure (tasks/density_main_results) with the continuous treatment, for the
# published score and the score through June 2026 without self-certification permits. Each 100 ft band's
# coefficient is the difference in log units per acre per standard deviation of score gap between the two sides,
# relative to the band just inside the less-stringent side. As in the binary figure, nothing else varies with distance,
# so bands and subtitle (the difference across a boundary per SD of gap) assume an effect proportional to the gap. Paper's
# 2006-2022 construction data and density specification (tasks/shared/code/density_boundary_helpers.R).
new_version <- "through_2026_no_self_cert"

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")

published <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
new <- read_csv("../output/scores_through_2026.csv", show_col_types = FALSE) |> filter(score_version == new_version)
score_versions <- list(published = setNames(published$uncertainty_index, published$alderman),
  through_2026_no_self_cert = setNames(new$uncertainty_index, new$alderman))
version_labels <- c(published = "Published 2006-2022 score",
  through_2026_no_self_cert = "Score through June 2026, no self-certification permits")

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  filter(abs(distance_to_boundary_ft) < density_bandwidth_ft, !is.na(alderman_own), !is.na(alderman_neighbor))

panels <- list()
for (version in names(score_versions)) {
  score <- score_versions[[version]]
  scored <- buildings |>
    mutate(running_distance_ft = abs(distance_to_boundary_ft) * sign(score[alderman_own] - score[alderman_neighbor]),
      score_gap = abs(score[alderman_own] - score[alderman_neighbor])) |>
    filter(running_distance_ft != 0) |>
    bin_running_distance() |>
    mutate(stricter_side_gap = stricter_side * score_gap)
  for (i in seq_len(nrow(density_samples))) {
    d <- filter_density_sample(scored, density_samples$sample[i])
    bands <- fixest::feols(as.formula(sprintf(
      "log(density_dupac) ~ i(distance_bin, score_gap, ref = '%s') + %s | %s",
      density_reference_bin, density_controls, density_fixed_effects)),
      data = d, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
    average <- fixest::feols(as.formula(sprintf("log(density_dupac) ~ stricter_side_gap + %s | %s", density_controls,
      density_fixed_effects)), data = d, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
    ct <- fixest::coeftable(bands)
    rows <- match(paste0("distance_bin::", density_bin_labels, ":score_gap"), rownames(ct))
    critical_value <- stats::qt(0.975, df = n_distinct(d$ward_pair) - 1L)
    fit <- list(
      bins = tibble(distance_bin = density_bin_labels,
          bin_center_ft = utils::head(density_bin_edges, -1) + density_bin_width_ft / 2,
          estimate = if_else(distance_bin == density_reference_bin, 0, ct[rows, "Estimate"]),
          std_error = ct[rows, "Std. Error"]) |>
        mutate(ci_low = if_else(distance_bin == density_reference_bin, 0, estimate - critical_value * std_error),
          ci_high = if_else(distance_bin == density_reference_bin, 0, estimate + critical_value * std_error)),
      average = tibble(estimate = coef(average)[["stricter_side_gap"]], std_error = fixest::se(average)[["stricter_side_gap"]],
        p_value = fixest::pvalue(average)[["stricter_side_gap"]]),
      observations = nobs(bands)
    )
    panels[[length(panels) + 1]] <- plot_density_boundary(fit, paste0(density_samples$label[i], "\n", version_labels[[version]])) +
      ggplot2::labs(subtitle = sprintf("Difference per SD of score gap = %.3f%s (SE %.3f)", fit$average$estimate,
          stars(fit$average$p_value), fit$average$std_error),
        y = "Units per acre per SD of score gap (log difference)")
  }
}
figure <- patchwork::wrap_plots(panels, ncol = 3) + patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave("../output/density_continuous_rd.pdf", figure, width = 18, height = 12, bg = "white")
