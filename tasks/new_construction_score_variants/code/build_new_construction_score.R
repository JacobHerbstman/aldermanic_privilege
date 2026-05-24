## Build permit-subset alderman stringency score

source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_variants/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# score_output <- "../output/alderman_uncertainty_index_new_construction.csv"
# variant_id <- "new_construction"
# variant_label <- "New construction only"
# permit_types_csv <- "new_construction"
# max_permit_year <- 2022

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    permits_input,
    score_output,
    variant_id,
    variant_label,
    permit_types_csv,
    max_permit_year
  )
}

if (length(args) == 6) {
  permits_input <- args[1]
  score_output <- args[2]
  variant_id <- args[3]
  variant_label <- args[4]
  permit_types_csv <- args[5]
  max_permit_year <- as.integer(args[6])
} else {
  stop(
    "FATAL: Script requires 6 args: <permits_input> <score_output> <variant_id> <variant_label> <permit_types_csv> <max_permit_year>",
    call. = FALSE
  )
}

if (!is.finite(max_permit_year)) {
  stop("max_permit_year must be a valid integer.", call. = FALSE)
}
stage1_outcome <- "log_processing_time"
drop_covariates <- c("share_bach_plus")
permit_types <- strsplit(permit_types_csv, ",", fixed = TRUE)[[1]] |> trimws()

config <- default_uncertainty_config()

permits_raw <- load_uncertainty_permits(permits_input) %>%
  filter(year <= max_permit_year) %>%
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

write_csv(index_result$alderman_index, score_output)

message("Saved new-construction score:")
message("  Variant: ", variant_label)
message("  Score: ", score_output)
