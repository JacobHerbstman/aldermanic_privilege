## Create Alderman Uncertainty Index
## This script runs permit-level residualization and computes alderman-level moments

source("../../_lib/alderman_uncertainty_helpers.R")

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

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(permit_type_fe, review_type_fe, include_porch, ca_fe, two_stage, stage2_weight, volume_ctrl, volume_stage)
}

if (length(cli_args) < 8) {
  stop(
    "FATAL: Script requires 8 args: <permit_type_fe> <review_type_fe> <include_porch> <ca_fe> <two_stage> <stage2_weight> <volume_ctrl> <volume_stage>",
    call. = FALSE
  )
}

if (length(cli_args) >= 8) {
  permit_type_fe <- cli_args[1]
  review_type_fe <- cli_args[2]
  include_porch <- cli_args[3]
  ca_fe <- cli_args[4]
  two_stage <- cli_args[5]
  stage2_weight <- cli_args[6]
  volume_ctrl <- cli_args[7]
  volume_stage <- cli_args[8]
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

permits_input_path <- Sys.getenv("PERMITS_INPUT_PATH", "../input/permits_for_uncertainty_index.csv")
uncertainty_output_dir <- Sys.getenv("UNCERTAINTY_OUTPUT_DIR", "../output")
max_permit_month_raw <- Sys.getenv("MAX_PERMIT_MONTH", "")
max_permit_year_raw <- Sys.getenv("MAX_PERMIT_YEAR", "")

parse_cutoff_month <- function(month_raw, year_raw) {
  if (nzchar(month_raw)) {
    if (!grepl("^\\d{4}-\\d{2}$", month_raw)) {
      stop("MAX_PERMIT_MONTH must use YYYY-MM format.", call. = FALSE)
    }
    return(as.yearmon(as.Date(paste0(month_raw, "-01"))))
  }
  if (nzchar(year_raw)) {
    year_value <- suppressWarnings(as.integer(year_raw))
    if (!is.finite(year_value)) {
      stop("MAX_PERMIT_YEAR must be a valid integer year.", call. = FALSE)
    }
    return(as.yearmon(as.Date(sprintf("%04d-12-01", year_value))))
  }
  NA
}

max_permit_month <- parse_cutoff_month(max_permit_month_raw, max_permit_year_raw)
cutoff_label <- if (!nzchar(max_permit_month_raw) && nzchar(max_permit_year_raw)) {
  paste0("through", max_permit_year_raw)
} else if (!is.na(max_permit_month)) {
  paste0("through", format(as.Date(max_permit_month), "%Y%m"))
} else {
  "throughlatest"
}

output_suffix <- build_uncertainty_output_suffix(config, cutoff_label)
output_file <- file.path(uncertainty_output_dir, paste0("alderman_uncertainty_index_", output_suffix, ".csv"))
stage1_output <- file.path(uncertainty_output_dir, paste0("stage1_regression_", output_suffix, ".tex"))
stage2_output <- file.path(uncertainty_output_dir, paste0("stage2_regression_", output_suffix, ".tex"))
plot_output <- file.path(uncertainty_output_dir, paste0("uncertainty_index_", output_suffix, ".pdf"))
score_coverage_output <- file.path(uncertainty_output_dir, paste0("score_coverage_", output_suffix, ".csv"))
score_month_coverage_output <- file.path(uncertainty_output_dir, paste0("score_alderman_month_coverage_", output_suffix, ".csv"))
score_rank_changes_output <- file.path(uncertainty_output_dir, paste0("score_rank_changes_", output_suffix, ".csv"))

permits_all <- load_uncertainty_permits(permits_input_path)
permits <- permits_all
message("Permits loaded before cutoff: ", nrow(permits))

if (!is.na(max_permit_month)) {
  permits <- permits %>%
    filter(month <= max_permit_month)
  message("Permits after month <= ", as.character(max_permit_month), " cutoff: ", nrow(permits))
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

message("Stage 1 observations: ", result$metadata$stage1_nobs)
message("Stage 1 adjusted R-squared: ", round(result$metadata$stage1_r2, 4))
message("Aldermen with scores: ", nrow(result$alderman_index))

coverage_month <- permits %>%
  count(alderman, month, year, name = "n_permits") %>%
  mutate(
    cutoff_label = cutoff_label,
    has_score = alderman %in% result$alderman_index$alderman,
    post_2023_term = month >= as.yearmon("2023-05")
  ) %>%
  arrange(alderman, month)

score_coverage <- coverage_month %>%
  summarise(
    cutoff_label = first(cutoff_label),
    n_permits_window = sum(n_permits),
    n_months = n_distinct(month),
    first_month = as.character(min(month)),
    last_month = as.character(max(month)),
    n_permits_post_2023 = sum(n_permits[post_2023_term], na.rm = TRUE),
    n_months_post_2023 = n_distinct(month[post_2023_term]),
    has_score = first(has_score),
    .by = alderman
  ) %>%
  left_join(
    result$alderman_index %>% rename(n_permits_score_sample = n_permits),
    by = "alderman",
    relationship = "one-to-one"
  ) %>%
  mutate(
    score_rank = if_else(!is.na(uncertainty_index), min_rank(desc(uncertainty_index)), NA_integer_)
  ) %>%
  arrange(score_rank, alderman)

baseline_result <- build_residualized_uncertainty_index(
  permits = permits_all %>% filter(year <= 2022),
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "Baseline residualized score dropping share_bach_plus"
)

rank_changes <- result$alderman_index %>%
  transmute(
    alderman,
    n_permits_current = n_permits,
    uncertainty_index_current = uncertainty_index,
    rank_current = min_rank(desc(uncertainty_index))
  ) %>%
  full_join(
    baseline_result$alderman_index %>%
      transmute(
        alderman,
        n_permits_through2022 = n_permits,
        uncertainty_index_through2022 = uncertainty_index,
        rank_through2022 = min_rank(desc(uncertainty_index))
      ),
    by = "alderman",
    relationship = "one-to-one"
  ) %>%
  mutate(
    rank_change = rank_current - rank_through2022,
    score_change = uncertainty_index_current - uncertainty_index_through2022,
    new_score_after_2022 = is.na(rank_through2022) & !is.na(rank_current),
    dropped_after_2022 = !is.na(rank_through2022) & is.na(rank_current)
  ) %>%
  arrange(desc(new_score_after_2022), rank_current, alderman)

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
write_csv(score_coverage, score_coverage_output)
message("Saved: ", score_coverage_output)
write_csv(coverage_month, score_month_coverage_output)
message("Saved: ", score_month_coverage_output)
write_csv(rank_changes, score_rank_changes_output)
message("Saved: ", score_rank_changes_output)
