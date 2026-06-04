## Build alternative alderman stringency score variants

source("../../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/uncertainty_score_density_robustness/code")
# variant_id <- "days_unlogged"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(variant_id)
}

if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <variant_id>", call. = FALSE)
}

variant_id <- args[1]
baseline_score_input <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
baseline_config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv")

max_permit_year <- str_match(basename(baseline_score_input), "through([0-9]{4})")[, 2] %>%
  as.integer()
if (is.finite(max_permit_year)) {
  permits <- permits %>% filter(year <= max_permit_year)
}

result <- switch(
  variant_id,
  baseline = {
    baseline_score <- read_csv(baseline_score_input, show_col_types = FALSE)
    rerun <- build_residualized_uncertainty_index(
      permits = permits,
      config = baseline_config,
      variant_id = variant_id,
      stage1_outcome = "log_processing_time",
      drop_covariates = c("share_bach_plus"),
      construction_rule = variant_construction_rule(variant_id)
    )
    baseline_check <- rerun$alderman_index %>%
      transmute(alderman, computed_score = uncertainty_index) %>%
      inner_join(
        baseline_score %>%
          transmute(alderman, baseline_score = uncertainty_index),
        by = "alderman",
        relationship = "one-to-one"
      )

    if (nrow(baseline_check) != nrow(baseline_score) || nrow(baseline_check) != nrow(rerun$alderman_index)) {
      stop("Rerun baseline score does not match the aldermen in the baseline score input.", call. = FALSE)
    }
    if (max(abs(baseline_check$computed_score - baseline_check$baseline_score), na.rm = TRUE) > 1e-8) {
      stop("Rerun baseline score does not match the baseline score input.", call. = FALSE)
    }

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

write_csv(result$alderman_index, sprintf("../output/alderman_uncertainty_index_%s.csv", variant_id))
write_csv(result$metadata, sprintf("../output/score_variant_metadata_%s.csv", variant_id))
write_csv(result$stage1_terms, sprintf("../output/score_variant_stage1_terms_%s.csv", variant_id))

stage1_table_output <- sprintf("../output/stage1_regression_%s.tex", variant_id)
stage2_table_output <- sprintf("../output/stage2_regression_%s.tex", variant_id)
if (!is.null(result$stage1_model)) {
  write_stage1_regression_table(result$stage1_model, stage1_table_output, result$stage1_outcome)
  message("Saved: ", stage1_table_output)
}

if (!is.null(result$stage2_model)) {
  write_stage2_regression_table(result$stage2_model, stage2_table_output)
  message("Saved: ", stage2_table_output)
}

message("Saved: ", sprintf("../output/alderman_uncertainty_index_%s.csv", variant_id))
message("Saved: ", sprintf("../output/score_variant_metadata_%s.csv", variant_id))
message("Saved: ", sprintf("../output/score_variant_stage1_terms_%s.csv", variant_id))
