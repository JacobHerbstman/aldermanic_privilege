## Compare baseline and permit-subset alderman stringency scores

source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_score_comparison/code")
# variant_id <- "new_construction"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(variant_id)
}

if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <variant_id>", call. = FALSE)
}

variant_id <- args[1]
variant_labels <- c(
  new_construction = "New construction only",
  new_construction_demolition = "New construction + demolition",
  restricted_renovation = "Restricted renovation"
)
if (!variant_id %in% names(variant_labels)) {
  stop("variant_id must be one of: new_construction, new_construction_demolition, restricted_renovation", call. = FALSE)
}

variant_label <- variant_labels[[variant_id]]
score_col_name <- paste0(variant_id, "_score")
rank_col_name <- paste0(variant_id, "_rank")

baseline_scores <- read_csv(
  "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  transmute(
    alderman,
    baseline_score = as.numeric(uncertainty_index)
  )
if (anyDuplicated(baseline_scores$alderman) > 0) {
  stop("Baseline score input has duplicate alderman rows.", call. = FALSE)
}

new_scores <- read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", variant_id),
  show_col_types = FALSE
) %>%
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

joined_output <- sprintf("../output/score_comparison_%s_vs_baseline.csv", variant_id)
write_csv(joined, joined_output)

message("Saved score comparison:")
message("  Variant: ", variant_id, " (", variant_label, ")")
message("  Joined CSV: ", joined_output)
