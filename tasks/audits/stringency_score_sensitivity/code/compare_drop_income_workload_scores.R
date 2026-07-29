# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

permits <- read_csv(
  "../input/permit_nonnegative_score_sample.csv",
  show_col_types = FALSE,
  col_types = cols(id = col_character(), pin = col_character(), .default = col_guess())
) %>%
  mutate(month = as.yearmon(month))

current_scores <- read_csv("../output/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(spec_id == "controls_drop_income") %>%
  select(cutoff, alderman, current_score = score, current_rank = strictness_rank)

comparison_rows <- list()
summary_rows <- list()
stage1_rows <- list()

for (cutoff_i in c(2014L, 2022L)) {
  config <- default_uncertainty_config()
  positive_workload <- build_residualized_uncertainty_index(
    permits = permits %>%
      filter(
        month <= as.yearmon(sprintf("%d-12", cutoff_i)),
        processing_time > 0
      ),
    config = config,
    variant_id = "controls_drop_income_positive_workload",
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
    construction_rule = "No-income score using positive-duration high-discretion workload"
  )

  comparison <- positive_workload$alderman_index %>%
    transmute(
      cutoff = cutoff_i,
      alderman,
      n_positive_permits = n_permits,
      positive_workload_score = uncertainty_index,
      positive_workload_rank = min_rank(desc(positive_workload_score))
    ) %>%
    left_join(
      current_scores %>% filter(cutoff == cutoff_i),
      by = c("cutoff", "alderman"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      score_change = positive_workload_score - current_score,
      absolute_score_change = abs(score_change),
      rank_change = positive_workload_rank - current_rank,
      absolute_rank_change = abs(rank_change)
    )

  comparison_rows[[length(comparison_rows) + 1L]] <- comparison
  summary_rows[[length(summary_rows) + 1L]] <- comparison %>%
    summarise(
      cutoff = cutoff_i,
      aldermen = n(),
      pearson = cor(current_score, positive_workload_score),
      spearman = cor(current_score, positive_workload_score, method = "spearman"),
      mean_absolute_score_change = mean(absolute_score_change),
      median_absolute_score_change = median(absolute_score_change),
      maximum_absolute_score_change = max(absolute_score_change),
      maximum_absolute_rank_change = max(absolute_rank_change),
      stage1_observations = positive_workload$metadata$stage1_nobs,
      stage2_ward_months = positive_workload$metadata$stage2_nobs
    )
  stage1_rows[[length(stage1_rows) + 1L]] <- positive_workload$stage1_terms %>%
    filter(term == "n_permits_wm_l1") %>%
    transmute(
      cutoff = cutoff_i,
      term,
      estimate,
      std_error,
      p_value
    )
}

bind_rows(comparison_rows) %>%
  arrange(cutoff, desc(absolute_score_change), alderman) %>%
  write_csv("../output/drop_income_workload_score_comparison.csv")
bind_rows(summary_rows) %>%
  left_join(bind_rows(stage1_rows), by = "cutoff", relationship = "one-to-one") %>%
  write_csv("../output/drop_income_workload_score_summary.csv")
