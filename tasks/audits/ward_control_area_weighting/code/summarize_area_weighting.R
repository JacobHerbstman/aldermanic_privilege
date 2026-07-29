# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/ward_control_area_weighting/code")

source("../../../setup_environment/code/packages.R")

area_results <- read_csv(
  "../output/current_income_score_results.csv",
  show_col_types = FALSE
) %>%
  filter(variant == "all_covariates") %>%
  mutate(assignment = "area_weighted")

centroid_results <- read_csv(
  "../input/centroid_main_results.csv",
  show_col_types = FALSE
) %>%
  filter(variant == "all_covariates") %>%
  mutate(assignment = "centroid")

main_results <- bind_rows(centroid_results, area_results) %>%
  select(
    assignment, analysis, sample, outcome, treatment,
    estimate, std_error, p_value, n, effect_percent
  ) %>%
  arrange(analysis, sample, outcome, treatment, assignment)

score_comparisons <- list()
score_movements <- list()

for (cutoff in c(2014L, 2022L)) {
  centroid <- read_csv(
    sprintf("../input/centroid_score_through%d.csv", cutoff),
    show_col_types = FALSE
  ) %>%
    transmute(alderman, centroid_score = uncertainty_index)
  area <- read_csv(
    sprintf("../output/alderman_scores_area_weighted_through%d.csv", cutoff),
    show_col_types = FALSE
  ) %>%
    transmute(alderman, area_weighted_score = uncertainty_index)

  comparison <- inner_join(
    centroid,
    area,
    by = "alderman",
    relationship = "one-to-one"
  ) %>%
    mutate(
      cutoff,
      score_change = area_weighted_score - centroid_score,
      absolute_score_change = abs(score_change),
      centroid_rank = rank(-centroid_score, ties.method = "average"),
      area_weighted_rank = rank(-area_weighted_score, ties.method = "average"),
      rank_change = area_weighted_rank - centroid_rank
    )

  score_comparisons[[as.character(cutoff)]] <- comparison %>%
    summarise(
      cutoff = first(cutoff),
      n_aldermen = n(),
      score_correlation = cor(centroid_score, area_weighted_score),
      rank_correlation = cor(
        centroid_score,
        area_weighted_score,
        method = "spearman"
      ),
      mean_absolute_score_change = mean(absolute_score_change),
      maximum_absolute_score_change = max(absolute_score_change),
      maximum_absolute_rank_change = max(abs(rank_change))
    )
  score_movements[[as.character(cutoff)]] <- comparison
}

write_csv(main_results, "../output/area_weighted_main_results.csv")
write_csv(bind_rows(score_comparisons), "../output/area_weighted_score_comparison.csv")
write_csv(
  bind_rows(score_movements) %>%
    arrange(cutoff, desc(absolute_score_change)),
  "../output/area_weighted_score_movements.csv"
)
