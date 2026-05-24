## Build alderman placebo score from residualized low-discretion processing times

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# residualized_wide_input <- "../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# score_output <- "../output/alderman_uncertainty_index_low_discretion_residualized.csv"
# variant_id <- "low_discretion_residualized"
# variant_label <- "Residualized low-discretion score"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(residualized_wide_input, score_output, variant_id, variant_label)
}

if (length(args) != 4) {
  stop(
    "FATAL: Script requires 4 args: <residualized_wide_input> <score_output> <variant_id> <variant_label>",
    call. = FALSE
  )
}

residualized_wide_input <- args[1]
score_output <- args[2]
variant_id <- args[3]
variant_label <- args[4]

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

write_csv(score_df, score_output)

message("Saved low-discretion score:")
message("  Variant: ", variant_label)
message("  Score: ", score_output)
