# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_uncertainty_index/code")
# permit_type_fe <- TRUE
# review_type_fe <- TRUE
# include_porch <- TRUE
# ca_fe <- FALSE
# two_stage <- TRUE
# stage2_weight <- "N_PERMITS"
# volume_ctrl <- "LAG1"
# volume_stage <- "BOTH"
# max_permit_cutoff <- "2022"
# write_paper_bundle <- TRUE

source("../../_lib/alderman_uncertainty_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    permit_type_fe,
    review_type_fe,
    include_porch,
    ca_fe,
    two_stage,
    stage2_weight,
    volume_ctrl,
    volume_stage,
    max_permit_cutoff,
    write_paper_bundle
  )
}

if (length(cli_args) != 10) {
  stop(
    "FATAL: Script requires 10 args: <permit_type_fe> <review_type_fe> <include_porch> <ca_fe> <two_stage> <stage2_weight> <volume_ctrl> <volume_stage> <max_permit_cutoff> <write_paper_bundle>.",
    call. = FALSE
  )
}

permit_type_fe <- cli_args[1]
review_type_fe <- cli_args[2]
include_porch <- cli_args[3]
ca_fe <- cli_args[4]
two_stage <- cli_args[5]
stage2_weight <- cli_args[6]
volume_ctrl <- cli_args[7]
volume_stage <- cli_args[8]
max_permit_cutoff <- cli_args[9]
if (!toupper(cli_args[10]) %in% c("TRUE", "FALSE")) {
  stop("write_paper_bundle must be TRUE or FALSE.", call. = FALSE)
}
write_paper_bundle <- toupper(cli_args[10]) == "TRUE"

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

if (grepl("^\\d{4}$", max_permit_cutoff)) {
  max_permit_month <- as.yearmon(as.Date(paste0(max_permit_cutoff, "-12-01")))
} else if (grepl("^\\d{4}-\\d{2}$", max_permit_cutoff)) {
  max_permit_month <- as.yearmon(as.Date(paste0(max_permit_cutoff, "-01")))
} else {
  stop("max_permit_cutoff must use YYYY or YYYY-MM format.", call. = FALSE)
}

cutoff_label <- if (grepl("^\\d{4}$", max_permit_cutoff)) {
  paste0("through", max_permit_cutoff)
} else {
  paste0("through", gsub("-", "", max_permit_cutoff))
}

output_suffix <- build_uncertainty_output_suffix(config, cutoff_label)

permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv")

permits <- permits %>%
  filter(month <= max_permit_month)

if (nrow(permits) == 0) {
  stop("No permits remain after applying the permit cutoff.", call. = FALSE)
}

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
  construction_rule = "Residualized alderman score used in the paper"
)

write_csv(result$alderman_index, sprintf("../output/alderman_uncertainty_index_%s.csv", output_suffix))

if (write_paper_bundle) {
  write_stage1_regression_table(result$stage1_model, sprintf("../output/stage1_regression_%s.tex", output_suffix), result$stage1_outcome)
  write_uncertainty_plot(result$alderman_index, sprintf("../output/uncertainty_index_%s.pdf", output_suffix))
}
