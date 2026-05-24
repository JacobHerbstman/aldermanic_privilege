## Compare baseline and permit-subset alderman stringency scores

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_comparison/code")
# baseline_input <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# new_input <- "../input/alderman_uncertainty_index_new_construction.csv"
# joined_output <- "../output/score_comparison_new_construction_vs_baseline.csv"
# variant_id <- "new_construction"
# variant_label <- "New construction only"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(baseline_input, new_input, joined_output, variant_id, variant_label)
}

if (length(args) == 5) {
  baseline_input <- args[1]
  new_input <- args[2]
  joined_output <- args[3]
  variant_id <- args[4]
  variant_label <- args[5]
} else {
  stop(
    "FATAL: Script requires 5 args: <baseline_input> <new_input> <joined_output> <variant_id> <variant_label>",
    call. = FALSE
  )
}

score_col_name <- paste0(variant_id, "_score")
rank_col_name <- paste0(variant_id, "_rank")

baseline_scores <- read_csv(baseline_input, show_col_types = FALSE) %>%
  transmute(
    alderman,
    baseline_score = as.numeric(uncertainty_index)
  )
if (anyDuplicated(baseline_scores$alderman) > 0) {
  stop("Baseline score input has duplicate alderman rows.", call. = FALSE)
}

new_scores <- read_csv(new_input, show_col_types = FALSE) %>%
  transmute(
    alderman,
    !!score_col_name := as.numeric(uncertainty_index)
  )
if (anyDuplicated(new_scores$alderman) > 0) {
  stop("Variant score input has duplicate alderman rows.", call. = FALSE)
}

joined <- baseline_scores %>%
  inner_join(new_scores, by = "alderman", relationship = "one-to-one") %>%
  mutate(
    baseline_rank = rank(-baseline_score, ties.method = "average"),
    !!rank_col_name := rank(-.data[[score_col_name]], ties.method = "average"),
    rank_change = .data[[rank_col_name]] - baseline_rank,
    abs_rank_change = abs(rank_change)
  ) %>%
  arrange(.data[[rank_col_name]], alderman)

if (nrow(joined) == 0) {
  stop("No aldermen matched across baseline and permit-subset score files.", call. = FALSE)
}

write_csv(joined, joined_output)

message("Saved score comparison:")
message("  Variant: ", variant_id, " (", variant_label, ")")
message("  Joined CSV: ", joined_output)
