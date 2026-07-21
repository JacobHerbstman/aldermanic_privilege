# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_density_robustness/code")
# max_construction_year <- 2026

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(max_construction_year)
}
if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <max_construction_year>.", call. = FALSE)
}

max_construction_year <- suppressWarnings(as.integer(cli_args[1]))
if (!is.finite(max_construction_year)) {
  stop("max_construction_year must be a valid integer year.", call. = FALSE)
}

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
if (!"dist_to_boundary_m" %in% names(parcels)) {
  stop("Parcels input must contain dist_to_boundary_m.", call. = FALSE)
}
if (!all(c("construction_year", "construction_zone_group", "segment_id") %in% names(parcels))) {
  stop("Parcels input is missing construction year, construction zoning, or segment assignment.", call. = FALSE)
}

scores <- read_csv("../input/aldermen_uncertainty_scores.csv", show_col_types = FALSE)
if (!"uncertainty_index" %in% names(scores)) {
  stop("Score column uncertainty_index not found in scores file.", call. = FALSE)
}

scores_for_merge <- scores %>%
  select(alderman, score = uncertainty_index)
if (anyDuplicated(scores_for_merge$alderman) > 0) {
  stop("Scores input has duplicate alderman values; expected one row per alderman.", call. = FALSE)
}

parcels_final <- parcels %>%
  select(-any_of(c("strictness_own", "strictness_neighbor", "sign", "signed_distance", "signed_distance_m"))) %>%
  filter(!is.na(construction_year), construction_year <= max_construction_year) %>%
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
  ) %>%
  filter(!is.na(signed_distance))

write_csv(parcels_final, "../temp/parcels_with_ward_distances_low_discretion_residualized.csv")
