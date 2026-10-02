# setwd("tasks/audits/outcomes_through_2026/code")
# Exploratory: the paper's density boundary design (tasks/shared/code/density_boundary_helpers.R: log units per acre,
# segment-by-joint-service and zoning-group fixed effects, ward demographic controls) on new construction through
# 2026. Each sample is estimated with the score through June 2026 without self-certification permits (the score in
# the extended analysis data) and with the published 2006-2022 score; the more-stringent side is decided by each
# score. The paper's 2006-2022 construction data are estimated the same way for comparison.
periods <- tibble::tribble(
  ~period, ~first_year, ~last_year,
  "2006_2026", 2006L, 2026L,
  "2006_2022", 2006L, 2022L,
  "2023_2026", 2023L, 2026L
)

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")
# The helper's sample ends in 2022; the extended data run through 2026.
density_end_year <- 2026L

scores <- read_csv("../input/scores_through_2026.csv", show_col_types = FALSE)
published <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
score_versions <- list(
  through_2026_no_self_cert = with(filter(scores, score_version == "through_2026_no_self_cert"),
    setNames(uncertainty_index, alderman)),
  published_through_2022 = setNames(published$uncertainty_index, published$alderman)
)
data_versions <- list(
  extended = read_csv("../output/new_construction_analysis_data_through_2026.csv", show_col_types = FALSE,
    col_types = density_column_types),
  paper = read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE, col_types = density_column_types)
)

results <- list()
for (data_version in names(data_versions)) for (score_version in names(score_versions)) for (p in seq_len(nrow(periods))) {
  if (data_version == "paper" && periods$last_year[p] > 2022L) next
  score <- score_versions[[score_version]]
  d <- data_versions[[data_version]] |>
    density_analysis_sample() |>
    filter(between(construction_year, periods$first_year[p], periods$last_year[p]),
      abs(distance_to_boundary_ft) < density_bandwidth_ft, !is.na(alderman_own), !is.na(alderman_neighbor)) |>
    mutate(score_own = unname(score[alderman_own]), score_neighbor = unname(score[alderman_neighbor])) |>
    filter(is.finite(score_own), is.finite(score_neighbor), score_own != score_neighbor) |>
    mutate(running_distance_ft = abs(distance_to_boundary_ft) * sign(score_own - score_neighbor)) |>
    bin_running_distance()
  for (i in seq_len(nrow(density_samples))) {
    ds <- filter_density_sample(d, density_samples$sample[i])
    fit <- fit_density_boundary(ds)
    results[[length(results) + 1]] <- bind_rows(
      mutate(fit$average, statistic = "average_difference"),
      transmute(fit$first_bin, estimate, std_error, p_value, statistic = "first_band_difference")
    ) |>
      mutate(data_version, score_version, period = periods$period[p], sample = density_samples$sample[i],
        observations = fit$observations, buildings_2023_2026 = sum(ds$construction_year >= 2023L),
        ward_pairs = fit$ward_pairs, .before = 1)
  }
}
write_csv(bind_rows(results), "../output/density_results_through_2026.csv")
