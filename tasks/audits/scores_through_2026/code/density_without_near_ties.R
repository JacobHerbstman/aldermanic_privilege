# setwd("tasks/audits/scores_through_2026/code")
# Exploratory (follow-up to density_side_flips.R): the paper's density result without the ward 1/32 boundary
# (Proco Joe Moreno and Scott Waguespack, whose published scores differ by 0.09 SD), and without alderman pairs whose
# scores nearly tie. Near ties are judged by each score's own gap, or by either score's gap so that both scores use
# the same buildings. Paper's 2006-2022 construction data and density specification
# (tasks/shared/code/density_boundary_helpers.R), published score and the score through June 2026 without
# self-certification permits.
dropped_ward_pair <- "1_32"
gap_cutoffs <- c(0.10, 0.25, 0.50)
new_version <- "through_2026_no_self_cert"

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")

published <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
new <- read_csv("../output/scores_through_2026.csv", show_col_types = FALSE) |> filter(score_version == new_version)
score_versions <- list(published = setNames(published$uncertainty_index, published$alderman),
  through_2026_no_self_cert = setNames(new$uncertainty_index, new$alderman))

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  filter(abs(distance_to_boundary_ft) < density_bandwidth_ft, !is.na(alderman_own), !is.na(alderman_neighbor)) |>
  mutate(gap_published = abs(score_versions$published[alderman_own] - score_versions$published[alderman_neighbor]),
    gap_new = abs(score_versions[[new_version]][alderman_own] - score_versions[[new_version]][alderman_neighbor]))
stopifnot(dropped_ward_pair %in% buildings$ward_pair, !anyNA(buildings$gap_published), !anyNA(buildings$gap_new))

restrictions <- c(list(all = rep(TRUE, nrow(buildings)), without_ward_pair = buildings$ward_pair != dropped_ward_pair),
  setNames(lapply(gap_cutoffs, \(g) pmin(buildings$gap_published, buildings$gap_new) >= g),
    sprintf("gap_both_scores_at_least_%.2f", gap_cutoffs)))

results <- list()
for (version in names(score_versions)) {
  score <- score_versions[[version]]
  own_gap <- if (version == "published") buildings$gap_published else buildings$gap_new
  version_restrictions <- c(restrictions,
    setNames(lapply(gap_cutoffs, \(g) own_gap >= g), sprintf("gap_this_score_at_least_%.2f", gap_cutoffs)))
  for (r in names(version_restrictions)) {
    d <- buildings[version_restrictions[[r]], ] |>
      mutate(running_distance_ft = abs(distance_to_boundary_ft) * sign(score[alderman_own] - score[alderman_neighbor])) |>
      filter(running_distance_ft != 0) |>
      bin_running_distance()
    for (i in seq_len(nrow(density_samples))) {
      ds <- filter_density_sample(d, density_samples$sample[i])
      fit <- fit_density_boundary(ds)
      results[[length(results) + 1]] <- bind_rows(
        mutate(fit$average, statistic = "average_difference"),
        transmute(fit$first_bin, estimate, std_error, p_value, statistic = "first_band_difference")
      ) |>
        mutate(score_version = version, restriction = r, sample = density_samples$sample[i],
          observations = fit$observations, ward_pairs = fit$ward_pairs, .before = 1)
    }
  }
}
write_csv(bind_rows(results), "../output/density_without_near_ties.csv")
