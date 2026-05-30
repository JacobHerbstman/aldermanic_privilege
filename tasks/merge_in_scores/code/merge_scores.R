# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_in_scores/code")
# score_column <- "uncertainty_index"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(score_column)
}

merge_output_tag <- Sys.getenv("MERGE_RESULT_TAG", "")
if (!nzchar(merge_output_tag)) {
  merge_output_tag <- Sys.getenv("MERGE_OUTPUT_TAG", "")
}
if (nzchar(merge_output_tag) && !grepl("^[A-Za-z0-9_-]+$", merge_output_tag)) {
  stop("MERGE_RESULT_TAG may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

parcels_input <- Sys.getenv("PARCELS_INPUT_PATH", "../input/parcels_pre_scores.csv")
segment_lookup_input <- Sys.getenv("SEGMENT_LOOKUP_PATH", "../input/parcel_segment_ids.csv")
score_file <- Sys.getenv("SCORES_INPUT_PATH", "../input/aldermen_uncertainty_scores.csv")
merge_output <- Sys.getenv(
  "MERGE_OUTPUT_PATH",
  if (nzchar(merge_output_tag)) sprintf("../temp/parcels_with_ward_distances_%s.csv", merge_output_tag) else "../output/parcels_with_ward_distances.csv"
)
merge_summary_output <- Sys.getenv("MERGE_SUMMARY_OUTPUT_PATH", "")
score_coverage_output <- Sys.getenv("SCORE_COVERAGE_OUTPUT_PATH", "")
max_construction_year_raw <- Sys.getenv("MAX_CONSTRUCTION_YEAR", "2026")
max_construction_year <- if (nzchar(max_construction_year_raw)) suppressWarnings(as.integer(max_construction_year_raw)) else NA_integer_

if (nzchar(max_construction_year_raw) && !is.finite(max_construction_year)) {
  stop("MAX_CONSTRUCTION_YEAR must be a valid integer year.", call. = FALSE)
}

if (length(cli_args) == 1) {
  score_column <- cli_args[1]
} else if (length(cli_args) == 2) {
  score_file <- cli_args[1]
  score_column <- cli_args[2]
} else {
  stop("FATAL: Script requires <score_column> or legacy <score_file> <score_column>.", call. = FALSE)
}

parcels <- read_csv(
  parcels_input,
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

if (!"dist_to_boundary_m" %in% names(parcels)) {
  stop("Parcels input must contain dist_to_boundary_m.", call. = FALSE)
}

segment_lookup <- read_csv(
  segment_lookup_input,
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    segment_reason = col_character()
  )
)

scores <- read_csv(score_file, show_col_types = FALSE)

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
  mutate(
    pin = as.character(pin),
    segment_id = as.character(segment_id),
    dist_to_segment_m = if ("dist_to_segment_m" %in% names(.)) as.numeric(dist_to_segment_m) else NA_real_
  )
parcels <- parcels %>%
  mutate(pin = as.character(pin)) %>%
  left_join(segment_lookup, by = "pin", relationship = "many-to-one")

if (is.finite(max_construction_year)) {
  if (!"construction_year" %in% names(parcels)) {
    stop("Parcels input must contain construction_year when MAX_CONSTRUCTION_YEAR is set.", call. = FALSE)
  }
  parcels <- parcels %>%
    filter(!is.na(construction_year), construction_year <= max_construction_year)
}

scores_for_merge <- scores %>%
  select(alderman, score = all_of(score_column))
if (anyDuplicated(scores_for_merge$alderman) > 0) {
  stop("Scores input has duplicate alderman values; expected one row per alderman.", call. = FALSE)
}

parcels_with_scores <- parcels %>%
  left_join(scores_for_merge, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores_for_merge, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance = dist_to_boundary * sign,
    signed_distance_m = dist_to_boundary_m * sign
  )

parcels_final <- parcels_with_scores %>%
  filter(!is.na(signed_distance))

write_csv(parcels_final, merge_output)

if (nzchar(merge_summary_output)) {
  parcels_final %>%
    summarise(
      n_parcels = n(),
      n_wards = n_distinct(ward),
      n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
      n_aldermen = n_distinct(alderman_own, na.rm = TRUE),
      mean_signed_distance_m = mean(signed_distance_m, na.rm = TRUE),
      median_signed_distance_m = median(signed_distance_m, na.rm = TRUE),
      .by = c(boundary_year, construction_year)
    ) %>%
    arrange(construction_year) %>%
    write_csv(merge_summary_output)
}
if (nzchar(score_coverage_output)) {
  bind_rows(
    parcels_with_scores %>%
      transmute(
        boundary_year,
        construction_year,
        ward_pair,
        side = "own",
        alderman = alderman_own,
        has_score = !is.na(strictness_own)
      ),
    parcels_with_scores %>%
      transmute(
        boundary_year,
        construction_year,
        ward_pair,
        side = "neighbor",
        alderman = alderman_neighbor,
        has_score = !is.na(strictness_neighbor)
      )
  ) %>%
    summarise(
      n_rows = n(),
      n_aldermen = n_distinct(alderman, na.rm = TRUE),
      n_missing_alderman = sum(is.na(alderman) | alderman == ""),
      n_with_score = sum(has_score, na.rm = TRUE),
      n_missing_score = sum(!has_score, na.rm = TRUE),
      score_coverage_pct = 100 * n_with_score / n_rows,
      .by = c(boundary_year, construction_year, ward_pair, side)
    ) %>%
    arrange(construction_year, boundary_year, ward_pair, side) %>%
    write_csv(score_coverage_output)
}
