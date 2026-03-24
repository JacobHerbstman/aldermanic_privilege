## Build permit-subset alderman stringency score
## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_density_robustness/code")
# Rscript build_new_construction_score.R "../input/permits_for_uncertainty_index.csv" "../output/alderman_uncertainty_index_new_construction.csv" "../output/score_variant_metadata_new_construction.csv" "../output/score_variant_stage1_terms_new_construction.csv" "../output/stage1_regression_new_construction.tex" "../output/stage2_regression_new_construction.tex" "../output/uncertainty_index_new_construction.pdf"

source("../../create_alderman_uncertainty_index/code/uncertainty_index_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 7) {
  permits_input <- args[1]
  score_output <- args[2]
  metadata_output <- args[3]
  stage1_terms_output <- args[4]
  stage1_table_output <- args[5]
  stage2_table_output <- args[6]
  plot_output <- args[7]
} else {
  stop(
    "FATAL: Script requires 7 args: <permits_input> <score_output> <metadata_output> <stage1_terms_output> <stage1_table_output> <stage2_table_output> <plot_output>",
    call. = FALSE
  )
}

variant_id <- if (length(args) >= 8) args[8] else "new_construction"
variant_label <- if (length(args) >= 9) args[9] else "New construction only"
permit_types_csv <- if (length(args) >= 10) args[10] else "new_construction"
stage1_outcome <- "log_processing_time"
drop_covariates <- c("share_bach_plus")
permit_types <- strsplit(permit_types_csv, ",", fixed = TRUE)[[1]] |> trimws()
construction_rule <- paste0(
  "Paper baseline score using permits with permit_type_clean in {",
  paste(permit_types, collapse = ", "),
  "}"
)

config <- default_uncertainty_config()

permits_raw <- load_uncertainty_permits(permits_input) %>%
  filter(permit_type_clean %in% permit_types)

if (nrow(permits_raw) == 0) {
  stop("No permits found for the requested permit subset.", call. = FALSE)
}

prepared <- prepare_uncertainty_sample(
  permits = permits_raw,
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)

if (nrow(prepared$permits) == 0) {
  stop("No permits remain after preparing the new-construction sample.", call. = FALSE)
}

covariates <- get_stage1_covariates(
  prepared$place_covariates,
  prepared$include_volume_stage1,
  prepared$volume_var,
  drop_covariates = drop_covariates
)

requested_fe_terms <- get_stage1_fe_terms(config)
stage1_candidate <- prepared$permits

for (col in unique(c(stage1_outcome, covariates))) {
  stage1_candidate <- stage1_candidate %>%
    filter(!is.na(.data[[col]]) & is.finite(.data[[col]]))
}
for (col in requested_fe_terms) {
  stage1_candidate <- stage1_candidate %>%
    filter(!is.na(.data[[col]]))
}

active_fe_terms <- requested_fe_terms[vapply(
  requested_fe_terms,
  function(col) dplyr::n_distinct(stage1_candidate[[col]]) > 1,
  logical(1)
)]
dropped_fe_terms <- setdiff(requested_fe_terms, active_fe_terms)

if (length(active_fe_terms) == 0) {
  stop("No stage-1 fixed effects remain after filtering to the requested permit subset.", call. = FALSE)
}

stage1_result <- fit_stage1_model(
  permits = prepared$permits,
  stage1_outcome = stage1_outcome,
  covariates = covariates,
  fe_terms = active_fe_terms,
  variant_id = variant_id
)

index_result <- build_two_stage_index(
  permits_for_reg = stage1_result$permits_for_reg,
  include_volume_stage2 = prepared$include_volume_stage2,
  volume_var = prepared$volume_var,
  stage2_weight = config$stage2_weight
)

metadata <- tibble(
  variant_id = variant_id,
  variant_label = variant_label,
  construction_rule = construction_rule,
  permit_type_filter = paste0(
    "permit_type_clean %in% c(",
    paste(sprintf("\"%s\"", permit_types), collapse = ", "),
    ")"
  ),
  stage1_outcome = stage1_outcome,
  drop_covariates = paste(drop_covariates, collapse = ";"),
  stage2_weight = config$stage2_weight,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage,
  volume_var = prepared$volume_var,
  include_volume_stage1 = prepared$include_volume_stage1,
  include_volume_stage2 = prepared$include_volume_stage2,
  requested_fe_terms = paste(requested_fe_terms, collapse = ";"),
  stage1_fe_terms = paste(active_fe_terms, collapse = ";"),
  dropped_fe_terms = paste(dropped_fe_terms, collapse = ";"),
  n_permits_filtered_raw = nrow(permits_raw),
  n_permits_filtered_prepared = nrow(prepared$permits),
  n_aldermen = nrow(index_result$alderman_index),
  n_permits_used = sum(!is.na(stage1_result$permits_for_reg$resid)),
  stage1_nobs = stage1_result$stage1_nobs,
  stage1_r2 = stage1_result$stage1_r2,
  stage2_nobs = index_result$stage2_nobs,
  stage2_r2 = index_result$stage2_r2
)

write_csv(index_result$alderman_index, score_output)
write_csv(metadata, metadata_output)
write_csv(stage1_result$stage1_terms, stage1_terms_output)

write_stage1_regression_table(stage1_result$model, stage1_table_output, stage1_outcome)
if (!is.null(index_result$stage2_model)) {
  write_stage2_regression_table(index_result$stage2_model, stage2_table_output)
}
write_uncertainty_plot(index_result$alderman_index, plot_output)

message("Saved new-construction score outputs:")
message("  Variant: ", variant_label)
message("  Score: ", score_output)
message("  Metadata: ", metadata_output)
message("  Stage 1 terms: ", stage1_terms_output)
message("  Stage 1 table: ", stage1_table_output)
message("  Stage 2 table: ", stage2_table_output)
message("  Plot: ", plot_output)
