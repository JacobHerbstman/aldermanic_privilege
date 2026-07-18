# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

read_csv("../output/alderman_scores.csv", show_col_types = FALSE) %>%
  filter(spec_id == "controls_drop_income") %>%
  select(cutoff, alderman, n_permits, score, strictness_rank) %>%
  arrange(cutoff, strictness_rank, alderman) %>%
  write_csv("../output/drop_income_alderman_scores.csv")

read_csv("../output/score_stage1_terms.csv", show_col_types = FALSE) %>%
  filter(spec_id == "controls_drop_income") %>%
  select(cutoff, term, estimate, std_error, p_value) %>%
  left_join(
    read_csv("../output/score_metadata.csv", show_col_types = FALSE) %>%
      filter(spec_id == "controls_drop_income") %>%
      select(cutoff, stage1_nobs, stage1_r2, stage2_nobs, stage2_r2),
    by = "cutoff",
    relationship = "many-to-one"
  ) %>%
  arrange(cutoff, term) %>%
  write_csv("../output/drop_income_first_stage_results.csv")
