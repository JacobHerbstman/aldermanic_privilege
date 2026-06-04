# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_in_scores/code")
# score_column <- "uncertainty_index"
# max_construction_year <- 2026

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(score_column, max_construction_year)
}

if (length(cli_args) != 2) {
  stop("FATAL: Script requires <score_column> <max_construction_year>.", call. = FALSE)
}

score_column <- cli_args[1]
max_construction_year <- suppressWarnings(as.integer(cli_args[2]))
if (!is.finite(max_construction_year)) {
  stop("max_construction_year must be a valid integer year.", call. = FALSE)
}

parcels <- read_csv(
  "../input/parcels_pre_scores.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

if (!"dist_to_boundary_m" %in% names(parcels)) {
  stop("Parcels input must contain dist_to_boundary_m.", call. = FALSE)
}

segment_lookup <- read_csv(
  "../input/parcel_segment_ids.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    segment_reason = col_character()
  )
)

scores <- read_csv("../input/aldermen_uncertainty_scores.csv", show_col_types = FALSE)

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
    stop("Parcels input must contain construction_year when max_construction_year is set.", call. = FALSE)
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

write_csv(parcels_final, "../output/parcels_with_ward_distances.csv")
