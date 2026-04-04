# Merge Alderman Uncertainty Scores into Parcel Data
# This script loads pre-scores parcel data and merges in uncertainty scores
# to create signed distances for RD analysis

source("../../setup_environment/code/packages.R")

# PARSE COMMAND LINE ARGUMENTS


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_in_scores/code")
# score_file <- "../input/aldermen_uncertainty_scores.csv"
# score_column <- "uncertainty_index"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(score_file, score_column)
}

if (length(cli_args) < 2) {
  stop("FATAL: Script requires 2 args: <score_file> <score_column>", call. = FALSE)
}

score_file <- cli_args[1]
score_column <- cli_args[2]

parcels_input <- Sys.getenv("PARCELS_INPUT_PATH", "../input/parcels_pre_scores.csv")
segment_lookup_input <- Sys.getenv("SEGMENT_LOOKUP_PATH", "../input/parcel_segment_ids.csv")
merge_output <- Sys.getenv("MERGE_OUTPUT_PATH", "../output/parcels_with_ward_distances.csv")
merge_summary_output <- Sys.getenv("MERGE_SUMMARY_OUTPUT_PATH", "../output/boundary_distance_summary.csv")
max_construction_year_raw <- Sys.getenv("MAX_CONSTRUCTION_YEAR", "2023")
max_construction_year <- if (nzchar(max_construction_year_raw)) suppressWarnings(as.integer(max_construction_year_raw)) else NA_integer_

if (nzchar(max_construction_year_raw) && !is.finite(max_construction_year)) {
  stop("MAX_CONSTRUCTION_YEAR must be a valid integer year.", call. = FALSE)
}

cat("=== Merging Alderman Scores ===\n")
cat("Score file:", score_file, "\n")
cat("Score column:", score_column, "\n")
cat("Parcels input:", parcels_input, "\n")
cat("Segment lookup input:", segment_lookup_input, "\n")
cat("Merged output:", merge_output, "\n")
cat("Summary output:", merge_summary_output, "\n")
cat("Max construction year:", ifelse(is.finite(max_construction_year), max_construction_year, "none"), "\n")

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------

cat("\nLoading pre-scores parcel data...\n")
parcels <- read_csv(parcels_input, show_col_types = FALSE)
cat("Parcels loaded:", nrow(parcels), "\n")

cat("Loading parcel segment lookup...\n")
segment_lookup <- read_csv(segment_lookup_input, show_col_types = FALSE)
cat("Segment lookup rows:", nrow(segment_lookup), "\n")

cat("Loading uncertainty scores...\n")
scores <- read_csv(score_file, show_col_types = FALSE)
cat("Aldermen with scores:", nrow(scores), "\n")

# Check that the score column exists
if (!score_column %in% names(scores)) {
  stop(paste("Score column", score_column, "not found in scores file. Available columns:",
             paste(names(scores), collapse = ", ")))
}

if (!all(c("pin", "segment_id") %in% names(segment_lookup))) {
  stop("Segment lookup must contain columns: pin, segment_id", call. = FALSE)
}
if (anyDuplicated(segment_lookup$pin) > 0) {
  stop("Segment lookup has duplicate pin values; expected one row per pin.", call. = FALSE)
}

segment_lookup <- segment_lookup %>%
  mutate(pin = as.character(pin), segment_id = as.character(segment_id))
parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  left_join(segment_lookup, by = "pin")

if (is.finite(max_construction_year)) {
  if (!"construction_year" %in% names(parcels)) {
    stop("Parcels input must contain construction_year when MAX_CONSTRUCTION_YEAR is set.", call. = FALSE)
  }
  n_before_year_cutoff <- nrow(parcels)
  parcels <- parcels %>%
    filter(!is.na(construction_year), construction_year <= max_construction_year)
  cat("Parcels after construction_year <=", max_construction_year, ":", nrow(parcels),
      "(", n_before_year_cutoff - nrow(parcels), "dropped)\n")
}

# -----------------------------------------------------------------------------
# MERGE SCORES AND CALCULATE SIGNED DISTANCES
# -----------------------------------------------------------------------------

cat("\nMerging scores for own and neighbor aldermen...\n")

# Rename score column to standardized name for merging
scores_for_merge <- scores %>%
  select(alderman, score = all_of(score_column))

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
n_in            <- nrow(parcels_with_scores)
n_missing_own   <- sum(is.na(parcels_with_scores$strictness_own))
n_missing_nbr   <- sum(is.na(parcels_with_scores$strictness_neighbor))
n_tied          <- sum(!is.na(parcels_with_scores$strictness_own) &
                         !is.na(parcels_with_scores$strictness_neighbor) &
                         parcels_with_scores$strictness_own == parcels_with_scores$strictness_neighbor)
n_dropped       <- sum(is.na(parcels_with_scores$signed_distance))

cat(sprintf("Parcels with own score:      %d (%.1f%%)\n", n_in - n_missing_own,  100*(n_in - n_missing_own)/n_in))
cat(sprintf("Parcels with neighbor score: %d (%.1f%%)\n", n_in - n_missing_nbr,  100*(n_in - n_missing_nbr)/n_in))
cat(sprintf("Tied (equal scores, dropped): %d\n", n_tied))
cat(sprintf("Total dropped (NA sign):     %d (%.1f%%)\n", n_dropped, 100*n_dropped/n_in))

# Filter to valid signed distances
parcels_final <- parcels_with_scores %>%
  filter(!is.na(signed_distance))

cat("\nFinal dataset size:", nrow(parcels_final), "\n")

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------

cat("\nSaving output...\n")
write_csv(parcels_final, merge_output)

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

write_csv(summary_stats, merge_summary_output)

cat("\n=== Score merge complete ===\n")
cat("Output:", merge_output, "\n")
cat("Rows:", nrow(parcels_final), "\n")
