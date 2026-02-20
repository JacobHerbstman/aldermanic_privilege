# Merge Alderman Uncertainty Scores into Parcel Data
# This script loads pre-scores parcel data and merges in uncertainty scores
# to create signed distances for RD analysis

source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# PARSE COMMAND LINE ARGUMENTS
# -----------------------------------------------------------------------------

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_in_scores/code")
# score_file <- "../input/aldermen_uncertainty_scores.csv"
# score_column <- "uncertainty_index"
# Rscript merge_scores.R "../input/aldermen_uncertainty_scores.csv" "uncertainty_index"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 2) {
  score_file <- cli_args[1]
  score_column <- cli_args[2]
} else {
  if (!exists("score_file") || !exists("score_column")) {
    stop("FATAL: Script requires 2 args: <score_file> <score_column>", call. = FALSE)
  }
}

SCORE_FILE <- score_file
SCORE_COLUMN <- score_column

cat("=== Merging Alderman Scores ===\n")
cat("Score file:", SCORE_FILE, "\n")
cat("Score column:", SCORE_COLUMN, "\n")

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------

cat("\nLoading pre-scores parcel data...\n")
parcels <- read_csv("../input/parcels_pre_scores.csv", show_col_types = FALSE)
cat("Parcels loaded:", nrow(parcels), "\n")

cat("Loading uncertainty scores...\n")
scores <- read_csv(SCORE_FILE, show_col_types = FALSE)
cat("Aldermen with scores:", nrow(scores), "\n")

# Check that the score column exists
if (!SCORE_COLUMN %in% names(scores)) {
  stop(paste("Score column", SCORE_COLUMN, "not found in scores file. Available columns:", 
             paste(names(scores), collapse = ", ")))
}

# -----------------------------------------------------------------------------
# MERGE SCORES AND CALCULATE SIGNED DISTANCES
# -----------------------------------------------------------------------------

cat("\nMerging scores for own and neighbor aldermen...\n")

# Rename score column to standardized name for merging
scores_for_merge <- scores %>%
  select(alderman, score = all_of(SCORE_COLUMN))

parcels_with_scores <- parcels %>%
  # --- JOIN 1: Own Alderman Score ---
  left_join(scores_for_merge, by = c("alderman_own" = "alderman")) %>%
  rename(strictness_own = score) %>%
  # --- JOIN 2: Neighbor Alderman Score ---
  left_join(scores_for_merge, by = c("alderman_neighbor" = "alderman")) %>%
  rename(strictness_neighbor = score) %>%
  # Calculate sign and signed distance
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance = dist_to_boundary * sign
  )

# Report merge stats
n_with_own <- sum(!is.na(parcels_with_scores$strictness_own))
n_with_neighbor <- sum(!is.na(parcels_with_scores$strictness_neighbor))
n_with_signed <- sum(!is.na(parcels_with_scores$signed_distance))

cat(sprintf("Parcels with own score: %d (%.1f%%)\n", n_with_own, 100*n_with_own/nrow(parcels)))
cat(sprintf("Parcels with neighbor score: %d (%.1f%%)\n", n_with_neighbor, 100*n_with_neighbor/nrow(parcels)))
cat(sprintf("Parcels with valid signed distance: %d (%.1f%%)\n", n_with_signed, 100*n_with_signed/nrow(parcels)))

# Filter to valid signed distances
parcels_final <- parcels_with_scores %>%
  filter(!is.na(signed_distance))

cat("\nFinal dataset size:", nrow(parcels_final), "\n")

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------

cat("\nSaving output...\n")
write_csv(parcels_final, "../output/parcels_with_ward_distances.csv")

# Summary stats
summary_stats <- parcels_final %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman_own, na.rm = TRUE),
    mean_dist = mean(signed_distance, na.rm = TRUE),
    median_dist = median(signed_distance, na.rm = TRUE),
    .by = c(boundary_year, construction_year)
  ) %>%
  arrange(construction_year)

write_csv(summary_stats, "../output/boundary_distance_summary.csv")

cat("\n=== Score merge complete ===\n")
cat("Output: ../output/parcels_with_ward_distances.csv\n")
cat("Rows:", nrow(parcels_final), "\n")