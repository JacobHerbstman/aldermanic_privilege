## Build restricted-renovation alderman stringency score

source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# permits_input <- "../output/permits_for_uncertainty_index_restricted_renovation.csv"
# score_output <- "../output/alderman_uncertainty_index_restricted_renovation.csv"
# metadata_output <- "../output/score_variant_metadata_restricted_renovation.csv"
# stage1_terms_output <- "../output/score_variant_stage1_terms_restricted_renovation.csv"
# stage1_table_output <- "../output/stage1_regression_restricted_renovation.tex"
# stage2_table_output <- "../output/stage2_regression_restricted_renovation.tex"
# plot_output <- "../output/uncertainty_index_restricted_renovation.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permits_input,
    score_output,
    metadata_output,
    stage1_terms_output,
    stage1_table_output,
    stage2_table_output,
    plot_output
  )
}

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

variant_id <- if (length(args) >= 8) args[8] else "restricted_renovation"
variant_label <- if (length(args) >= 9) args[9] else "Restricted renovation"

permits <- load_uncertainty_permits(permits_input)
config <- default_uncertainty_config()

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = variant_id,
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = paste(
    "Through-2022 baseline residualized score keeping all non-renovation high-discretion permits",
    "and only broad substantive renovation text buckets:",
    "addition_expansion, unit_reconfiguration, tenant_commercial_buildout,",
    "rehab_interior_remodel, facade_masonry_envelope, systems_mep."
  )
)

result$metadata <- result$metadata %>%
  mutate(
    variant_label = .env$variant_label,
    n_input_permits = nrow(permits),
    n_kept_renovation_permits = sum(permits$permit_type_clean == "renovation", na.rm = TRUE),
    n_nonrenovation_permits = sum(permits$permit_type_clean != "renovation", na.rm = TRUE),
    renovation_keep_buckets = paste(
      c(
        "addition_expansion",
        "unit_reconfiguration",
        "tenant_commercial_buildout",
        "rehab_interior_remodel",
        "facade_masonry_envelope",
        "systems_mep"
      ),
      collapse = ";"
    )
  )

write_csv(result$alderman_index, score_output)
write_csv(result$metadata, metadata_output)
write_csv(result$stage1_terms, stage1_terms_output)

write_stage1_regression_table(result$stage1_model, stage1_table_output, result$stage1_outcome)
if (!is.null(result$stage2_model)) {
  write_stage2_regression_table(result$stage2_model, stage2_table_output)
}
write_uncertainty_plot(result$alderman_index, plot_output)

message("Saved restricted-renovation score outputs:")
message("  Variant: ", variant_label)
message("  Score: ", score_output)
message("  Metadata: ", metadata_output)
message("  Stage 1 terms: ", stage1_terms_output)
message("  Stage 1 table: ", stage1_table_output)
message("  Stage 2 table: ", stage2_table_output)
message("  Plot: ", plot_output)
