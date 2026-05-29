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

source("../../_lib/alderman_uncertainty_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(permit_type_fe, review_type_fe, include_porch, ca_fe, two_stage, stage2_weight, volume_ctrl, volume_stage)
}

if (length(cli_args) != 8) {
  stop(
    "FATAL: Script requires 8 args: <permit_type_fe> <review_type_fe> <include_porch> <ca_fe> <two_stage> <stage2_weight> <volume_ctrl> <volume_stage>",
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

max_permit_month_raw <- Sys.getenv("MAX_PERMIT_MONTH", "")
max_permit_year_raw <- Sys.getenv("MAX_PERMIT_YEAR", "2022")
score_only <- tolower(Sys.getenv("SCORE_ONLY", "false")) %in% c("true", "1", "yes")

if (nzchar(max_permit_month_raw)) {
  if (!grepl("^\\d{4}-\\d{2}$", max_permit_month_raw)) {
    stop("MAX_PERMIT_MONTH must use YYYY-MM format.", call. = FALSE)
  }
  max_permit_month <- as.yearmon(as.Date(paste0(max_permit_month_raw, "-01")))
} else if (nzchar(max_permit_year_raw)) {
  year_value <- suppressWarnings(as.integer(max_permit_year_raw))
  if (!is.finite(year_value)) {
    stop("MAX_PERMIT_YEAR must be a valid integer year.", call. = FALSE)
  }
  max_permit_month <- as.yearmon(as.Date(sprintf("%04d-12-01", year_value)))
} else {
  max_permit_month <- NA
}

cutoff_label <- if (!nzchar(max_permit_month_raw) && nzchar(max_permit_year_raw)) {
  paste0("through", max_permit_year_raw)
} else if (!is.na(max_permit_month)) {
  paste0("through", format(as.Date(max_permit_month), "%Y%m"))
} else {
  "throughlatest"
}

output_suffix <- build_uncertainty_output_suffix(config, cutoff_label)
output_file <- sprintf("../output/alderman_uncertainty_index_%s.csv", output_suffix)
stage1_output <- sprintf("../output/stage1_regression_%s.tex", output_suffix)
plot_output <- sprintf("../output/uncertainty_index_%s.pdf", output_suffix)

permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv")

if (!is.na(max_permit_month)) {
  permits <- permits %>%
    filter(month <= max_permit_month)
}

if (nrow(permits) == 0) {
  stop("No permits remain after applying the permit cutoff.", call. = FALSE)
}

result <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "Baseline residualized score dropping share_bach_plus"
)

if (!score_only) {
  write_stage1_regression_table(result$stage1_model, stage1_output, result$stage1_outcome)
  write_uncertainty_plot(result$alderman_index, plot_output)
}

write_csv(result$alderman_index, output_file)
