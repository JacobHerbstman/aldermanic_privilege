# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory: the paper's density figure (tasks/density_main_results) with the continuous treatment. Each 100 ft
# band's coefficient is the difference in log units per acre per standard deviation of the score gap between the two
# sides, relative to the band just inside the less-stringent side. As in the binary figure, nothing else varies with
# distance, so the bands and the subtitle (the difference across a boundary per SD of gap, as in
# density_continuous_suite.R) assume an effect proportional to the gap. Paper's data, score and density
# specification (tasks/shared/code/density_boundary_helpers.R).

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  mutate(running_distance_ft = signed_distance_m / 0.3048, score_gap = abs(strictness_own - strictness_neighbor)) |>
  bin_running_distance() |>
  mutate(stricter_side_gap = stricter_side * score_gap)

panels <- list()
for (i in seq_len(nrow(density_samples))) {
  d <- filter_density_sample(buildings, density_samples$sample[i])
  bands <- fixest::feols(as.formula(sprintf("log(density_dupac) ~ i(distance_bin, score_gap, ref = '%s') + %s | %s",
    density_reference_bin, density_controls, density_fixed_effects)), data = d, cluster = ~ward_pair, warn = FALSE,
    notes = FALSE)
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
  panels[[i]] <- plot_density_boundary(fit, density_samples$label[i]) +
    ggplot2::labs(subtitle = sprintf("Difference per SD of score gap = %.3f%s (SE %.3f)", fit$average$estimate,
        stars(fit$average$p_value), fit$average$std_error),
      y = "Units per acre per SD of score gap (log difference)")
}
save_density_figure(panels, "../output/density_continuous_rd.pdf")
