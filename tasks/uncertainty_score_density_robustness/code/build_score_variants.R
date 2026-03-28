## Build alternative alderman stringency score variants

source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/uncertainty_score_density_robustness/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# baseline_score_input <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv"
# variant_id <- "days_unlogged"
# score_output <- "../output/alderman_uncertainty_index_days_unlogged.csv"
# metadata_output <- "../output/score_variant_metadata_days_unlogged.csv"
# stage1_terms_output <- "../output/score_variant_stage1_terms_days_unlogged.csv"
# stage1_table_output <- "../output/stage1_regression_days_unlogged.tex"
# stage2_table_output <- "../output/stage2_regression_days_unlogged.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(permits_input, baseline_score_input, variant_id, score_output, metadata_output, stage1_terms_output, stage1_table_output, stage2_table_output)
}

if (length(args) >= 6) {
  permits_input <- args[1]
  baseline_score_input <- args[2]
  variant_id <- args[3]
  score_output <- args[4]
  metadata_output <- args[5]
  stage1_terms_output <- args[6]
  stage1_table_output <- ifelse(length(args) >= 7, args[7], "NA")
  stage2_table_output <- ifelse(length(args) >= 8, args[8], "NA")
} else {
  stop(
    "FATAL: Script requires at least 6 args: <permits_input> <baseline_score_input> <variant_id> <score_output> <metadata_output> <stage1_terms_output> [<stage1_table_output>] [<stage2_table_output>]",
    call. = FALSE
  )
}

baseline_config <- default_uncertainty_config()
permits <- load_uncertainty_permits(permits_input)

result <- switch(
  variant_id,
  baseline = {
    baseline_score <- read_csv(baseline_score_input, show_col_types = FALSE)
    rerun <- build_residualized_uncertainty_index(
      permits = permits,
      config = baseline_config,
      variant_id = variant_id,
      stage1_outcome = "log_processing_time",
      drop_covariates = character(),
      construction_rule = variant_construction_rule(variant_id)
    )
    rerun$alderman_index <- baseline_score
    rerun
  },
  raw_rank_days = build_raw_rank_uncertainty_index(
    permits = permits,
    config = baseline_config,
    variant_id = variant_id,
    construction_rule = variant_construction_rule(variant_id)
  ),
  days_unlogged = build_residualized_uncertainty_index(
    permits = permits,
    config = baseline_config,
    variant_id = variant_id,
    stage1_outcome = "processing_time",
    drop_covariates = character(),
    construction_rule = variant_construction_rule(variant_id)
  ),
  reduced_ses = build_residualized_uncertainty_index(
    permits = permits,
    config = baseline_config,
    variant_id = variant_id,
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
    construction_rule = variant_construction_rule(variant_id)
  ),
  drop_bach = build_residualized_uncertainty_index(
    permits = permits,
    config = baseline_config,
    variant_id = variant_id,
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus"),
    construction_rule = variant_construction_rule(variant_id)
  ),
  drop_bach_pop = build_residualized_uncertainty_index(
    permits = permits,
    config = baseline_config,
    variant_id = variant_id,
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus", "pop_total_10k"),
    construction_rule = variant_construction_rule(variant_id)
  ),
  stop("Unknown variant_id: ", variant_id, call. = FALSE)
)

write_csv(result$alderman_index, score_output)
write_csv(result$metadata, metadata_output)
write_csv(result$stage1_terms, stage1_terms_output)

if (!is.null(result$stage1_model) && !is.na(stage1_table_output) && nzchar(stage1_table_output) && stage1_table_output != "NA") {
  write_stage1_regression_table(result$stage1_model, stage1_table_output, result$stage1_outcome)
  message("Saved: ", stage1_table_output)
}

if (!is.null(result$stage2_model) && !is.na(stage2_table_output) && nzchar(stage2_table_output) && stage2_table_output != "NA") {
  write_stage2_regression_table(result$stage2_model, stage2_table_output)
  message("Saved: ", stage2_table_output)
}

message("Saved: ", score_output)
message("Saved: ", metadata_output)
message("Saved: ", stage1_terms_output)
