## Create Alderman Uncertainty Index
## This script runs permit-level residualization and computes alderman-level moments
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_uncertainty_index/code")
# Rscript create_uncertainty_index.R "TRUE" "TRUE" "TRUE" "FALSE" "TRUE" "N_PERMITS" "LAG1" "BOTH"

source("uncertainty_index_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 8) {
  permit_type_fe <- cli_args[1]
  review_type_fe <- cli_args[2]
  include_porch <- cli_args[3]
  ca_fe <- cli_args[4]
  two_stage <- cli_args[5]
  stage2_weight <- cli_args[6]
  volume_ctrl <- cli_args[7]
  volume_stage <- cli_args[8]
} else {
  stop(
    "FATAL: Script requires 8 args: <permit_type_fe> <review_type_fe> <include_porch> <ca_fe> <two_stage> <stage2_weight> <volume_ctrl> <volume_stage>",
    call. = FALSE
  )
}

config <- list(
  permit_type_fe = toupper(permit_type_fe) == "TRUE",
  review_type_fe = toupper(review_type_fe) == "TRUE",
  include_porch = toupper(include_porch) == "TRUE",
  ca_fe = toupper(ca_fe) == "TRUE",
  two_stage = toupper(two_stage) == "TRUE",
  stage2_weight = toupper(stage2_weight),
  volume_ctrl = toupper(volume_ctrl),
  volume_stage = toupper(volume_stage)
)

if (!config$stage2_weight %in% c("N_PERMITS", "SQRT_N_PERMITS", "NONE")) {
  stop("stage2_weight must be one of: N_PERMITS, SQRT_N_PERMITS, NONE", call. = FALSE)
}

message("=== Creating Alderman Uncertainty Index ===")
message("  PERMIT_TYPE_FE: ", config$permit_type_fe)
message("  REVIEW_TYPE_FE: ", config$review_type_fe)
message("  INCLUDE_PORCH: ", config$include_porch)
message("  CA_FE: ", config$ca_fe)
message("  TWO_STAGE: ", config$two_stage)
message("  STAGE2_WEIGHT: ", config$stage2_weight)
message("  VOLUME_CTRL: ", config$volume_ctrl)
message("  VOLUME_STAGE: ", config$volume_stage)

output_suffix <- build_uncertainty_output_suffix(config)
output_file <- paste0("../output/alderman_uncertainty_index_", output_suffix, ".csv")
stage1_output <- paste0("../output/stage1_regression_", output_suffix, ".tex")
stage2_output <- paste0("../output/stage2_regression_", output_suffix, ".tex")
plot_output <- paste0("../output/uncertainty_index_", output_suffix, ".pdf")

permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv")
message("Permits loaded: ", nrow(permits))

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = character(),
  construction_rule = "Baseline residualized score"
)

message("Stage 1 observations: ", result$metadata$stage1_nobs)
message("Stage 1 adjusted R-squared: ", round(result$metadata$stage1_r2, 4))
message("Aldermen with scores: ", nrow(result$alderman_index))

write_stage1_regression_table(result$stage1_model, stage1_output, result$stage1_outcome)
message("Saved: ", stage1_output)

if (!is.null(result$stage2_model)) {
  write_stage2_regression_table(result$stage2_model, stage2_output)
  message("Saved: ", stage2_output)
}

write_uncertainty_plot(result$alderman_index, plot_output)
message("Saved: ", plot_output)

write_csv(result$alderman_index, output_file)
message("Saved: ", output_file)
