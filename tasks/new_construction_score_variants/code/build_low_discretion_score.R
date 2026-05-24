## Build alderman placebo score from residualized low-discretion processing times

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# residualized_wide_input <- "../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# score_output <- "../output/alderman_uncertainty_index_low_discretion_residualized.csv"
# metadata_output <- "../output/score_variant_metadata_low_discretion_residualized.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(residualized_wide_input, score_output, metadata_output)
}

if (length(args) < 3) {
  stop(
    "FATAL: Script requires 3 args: <residualized_wide_input> <score_output> <metadata_output>",
    call. = FALSE
  )
}

residualized_wide_input <- args[1]
score_output <- args[2]
metadata_output <- args[3]

variant_id <- if (length(args) >= 4) args[4] else "low_discretion_residualized"
variant_label <- if (length(args) >= 5) args[5] else "Residualized low-discretion score"

score_df <- read_csv(residualized_wide_input, show_col_types = FALSE) %>%
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

metadata <- tibble(
  variant_id = variant_id,
  variant_label = variant_label,
  source_measure = "mean_resid_low",
  construction_rule = "Z-scored alderman mean residualized low-discretion log processing time",
  weighting = "None in score construction",
  n_aldermen = nrow(score_df),
  mean_raw = mean(score_df$raw_mean_resid_low, na.rm = TRUE),
  sd_raw = sd(score_df$raw_mean_resid_low, na.rm = TRUE),
  min_raw = min(score_df$raw_mean_resid_low, na.rm = TRUE),
  max_raw = max(score_df$raw_mean_resid_low, na.rm = TRUE),
  mean_n_low = mean(score_df$n_low, na.rm = TRUE),
  median_n_low = median(score_df$n_low, na.rm = TRUE)
)

write_csv(score_df, score_output)
write_csv(metadata, metadata_output)

message("Saved low-discretion score outputs:")
message("  Score: ", score_output)
message("  Metadata: ", metadata_output)
