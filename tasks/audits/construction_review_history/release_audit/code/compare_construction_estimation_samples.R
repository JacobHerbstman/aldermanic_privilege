# setwd("tasks/working_paper_release_audit/code")
source("../../setup_environment/code/packages.R")

before <- readr::read_csv("../reference/new_construction_analysis_data_before_reconnection.csv",
  col_types = readr::cols(project_id = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
after <- readr::read_csv("../input/current_construction_analysis.csv",
  col_types = readr::cols(project_id = "c", component_pins = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
ledger <- readr::read_csv("../input/preferred_new_construction_project_ledger.csv",
  col_types = readr::cols(component_pins = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(before$project_id), !anyDuplicated(after$project_id), !anyDuplicated(ledger$project_id))

# Match on project ID; recorded source links separately identify renamed or split records.
# Counts describe the eligible data before the estimator removes singleton fixed effects.
samples <- list(before = before, after = after)
for (version in names(samples)) {
  data <- samples[[version]]
  data$complete_covariates <- with(data, is.finite(share_white_own) & is.finite(share_black_own) &
    is.finite(median_hh_income_own) & is.finite(share_bach_plus_own) & is.finite(homeownership_rate_own) &
    !is.na(zone_group) & zone_group != "" & !is.na(segment_id) & segment_id != "" &
    !is.na(ward_pair) & ward_pair != "")
  data$score_pair_usable <- with(data, is.finite(strictness_own) & is.finite(strictness_neighbor) &
    strictness_own != strictness_neighbor & is.finite(signed_distance_m))
  data$common_500 <- with(data, construction_year >= 2006 & construction_year <= 2022 &
    within_500ft & abs(signed_distance_m / 0.3048) < 500 & (version == "after" | dwelling_units > 0) &
    complete_covariates & score_pair_usable)
  data$far_usable <- with(data, allow_far & is.finite(density_far) & density_far > 0)
  data$dupac_usable <- with(data, allow_dupac & is.finite(density_dupac) & density_dupac > 0)
  data$far_sample <- dplyr::coalesce(data$common_500 & data$far_usable &
    (version == "after" | data$dupac_usable), FALSE)
  data$dupac_sample <- dplyr::coalesce(data$common_500 & data$dupac_usable &
    (version == "after" | data$far_usable), FALSE)
  samples[[version]] <- data
}
fields <- c("project_id", "construction_year", "dwelling_units", "building_sqft", "land_sqft",
  "density_far", "density_dupac", "within_500ft", "distance_to_boundary_ft", "allow_far", "allow_dupac",
  "external_multifamily", "ward_pair", "segment_id", "zone_group", "alderman_own", "alderman_neighbor",
  "strictness_own", "strictness_neighbor", "signed_distance_m", "complete_covariates", "score_pair_usable",
  "far_sample", "dupac_sample", "share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own")
comparison <- full_join(samples$before |> select(all_of(fields)) |> mutate(present = TRUE),
  samples$after |> select(all_of(fields)) |> mutate(present = TRUE),
  by = "project_id", suffix = c("_before", "_after"), relationship = "one-to-one") |>
  mutate(present_before = coalesce(present_before, FALSE), present_after = coalesce(present_after, FALSE)) |>
  left_join(ledger |> select(project_id, source_project_ids, component_pins, decision_reason,
    year_source, units_source, building_source, land_source), by = "project_id", relationship = "one-to-one")
for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft",
  "distance_to_boundary_ft", "ward_pair", "segment_id", "zone_group", "external_multifamily",
  "alderman_own", "alderman_neighbor", "share_white_own", "share_black_own",
  "median_hh_income_own", "share_bach_plus_own", "homeownership_rate_own")) {
  x <- comparison[[paste0(field, "_before")]]
  y <- comparison[[paste0(field, "_after")]]
  equal <- if (is.numeric(x)) abs(x - y) <= 1e-8 else x == y
  comparison[[paste0(field, "_changed")]] <- comparison$present_before & comparison$present_after &
    !coalesce(equal | (is.na(x) & is.na(y)), FALSE)
}
for (outcome in c("far", "dupac")) {
  old <- coalesce(comparison[[paste0(outcome, "_sample_before")]], FALSE)
  new <- coalesce(comparison[[paste0(outcome, "_sample_after")]], FALSE)
  comparison[[paste0(outcome, "_transition")]] <- case_when(old & new ~ "retained",
    old & !new ~ "removed", !old & new ~ "added", TRUE ~ "outside_both_samples")
}
summary_rows <- list()
for (sample in c("all", "multifamily")) {
  for (outcome in c("far", "dupac")) {
    old <- coalesce(comparison[[paste0(outcome, "_sample_before")]], FALSE) &
      (sample == "all" | coalesce(comparison$external_multifamily_before, FALSE))
    new <- coalesce(comparison[[paste0(outcome, "_sample_after")]], FALSE) &
      (sample == "all" | coalesce(comparison$external_multifamily_after, FALSE))
    summary_rows[[length(summary_rows) + 1L]] <- tibble(sample, outcome,
      before = sum(old), after = sum(new), retained = sum(old & new), removed = sum(old & !new), added = sum(!old & new),
      retained_year_changed = sum(old & new & comparison$construction_year_changed),
      retained_units_changed = sum(old & new & comparison$dwelling_units_changed),
      retained_building_area_changed = sum(old & new & comparison$building_sqft_changed),
      retained_land_area_changed = sum(old & new & comparison$land_sqft_changed),
      retained_zoning_changed = sum(old & new & comparison$zone_group_changed),
      retained_segment_changed = sum(old & new & comparison$segment_id_changed))
  }
}
summary <- bind_rows(summary_rows)
stopifnot(all(summary$before == summary$retained + summary$removed),
  all(summary$after == summary$retained + summary$added))
readr::write_csv(comparison |> arrange(project_id), "../output/construction_estimation_project_changes.csv", na = "")
readr::write_csv(summary, "../output/construction_estimation_sample_comparison.csv", na = "")
