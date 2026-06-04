# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")

source("../../setup_environment/code/packages.R")

score_df <- read_csv(
  "../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  filter(
    is.finite(mean_resid_low),
    is.finite(n_low),
    n_low > 0
  ) %>%
  mutate(
    uncertainty_index = as.numeric(scale(mean_resid_low)),
    raw_mean_resid_low = mean_resid_low
  ) %>%
  transmute(
    alderman,
    uncertainty_index,
    raw_mean_resid_low,
    n_low
  ) %>%
  arrange(desc(uncertainty_index), alderman)

if (nrow(score_df) == 0) {
  stop("No aldermen with finite residualized low-discretion means.", call. = FALSE)
}

write_csv(score_df, "../output/alderman_uncertainty_index_low_discretion_residualized.csv")
