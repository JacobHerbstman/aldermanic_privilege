# setwd("tasks/audits/density_common_sample/code")
source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = readr::cols(project_id = "c", ward_pair = "c", segment_id = "c", .default = readr::col_guess()))
scores <- readr::read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE) |>
  select(alderman, uncertainty_index)
boundaries <- readr::read_csv("../input/density_boundary_characteristics.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(scores$alderman),
  !anyDuplicated(boundaries$project_id))
projects <- projects |>
  select(-strictness_own, -strictness_neighbor) |>
  left_join(scores |> rename(alderman_own = alderman, strictness_own = uncertainty_index),
    by = "alderman_own", relationship = "many-to-one") |>
  left_join(scores |> rename(alderman_neighbor = alderman, strictness_neighbor = uncertainty_index),
    by = "alderman_neighbor", relationship = "many-to-one") |>
  left_join(boundaries, by = "project_id", relationship = "one-to-one") |>
  mutate(true_distance_ft = abs(signed_distance_m / 0.3048) * sign(strictness_own - strictness_neighbor),
    far_valid = allow_far & is.finite(density_far) & density_far > 0,
    dupac_valid = allow_dupac & is.finite(density_dupac) & density_dupac > 0,
    both_valid = far_valid & dupac_valid) |>
  filter(construction_year >= 2006L, construction_year <= 2022L, within_1500ft,
    is.finite(share_white_own), is.finite(share_black_own), is.finite(median_hh_income_own),
    is.finite(share_bach_plus_own), is.finite(homeownership_rate_own), !is.na(zone_group),
    !is.na(segment_id), segment_id != "", !is.na(ward_pair), ward_pair != "")

checks <- tibble::tribble(
  ~check, ~cutoff_ft, ~donut_ft, ~boundary_rule, ~score_gap,
  "main", 0, 0, "all", 0,
  "placebo_neg1000ft", -1000, 0, "all", 0,
  "placebo_pos1000ft", 1000, 0, "all", 0,
  "donut25ft", 0, 25, "all", 0,
  "donut50ft", 0, 50, "all", 0,
  "limited_expressway_water", 0, 0, "simple_overlap_keep", 0,
  "limited_physical_features", 0, 0, "share_based_keep", 0,
  "straight_boundary", 0, 0, "straight_boundary", 0,
  "score_gap_025", 0, 0, "all", 0.25,
  "score_gap_050", 0, 0, "all", 0.50)

results <- list()
for (i in seq_len(nrow(checks))) {
  candidates <- projects |>
    mutate(running_distance_ft = true_distance_ft - checks$cutoff_ft[i],
      distance_bin = cut(running_distance_ft, breaks = seq(-500, 500, by = 100),
        labels = sprintf("bin_%02d", 1:10), include.lowest = TRUE, right = FALSE)) |>
    filter(abs(running_distance_ft) < 500, !is.na(distance_bin),
      checks$donut_ft[i] == 0 | abs(running_distance_ft) >= checks$donut_ft[i],
      abs(strictness_own - strictness_neighbor) >= checks$score_gap[i])
  if (checks$cutoff_ft[i] == 0) candidates <- candidates |> filter(within_500ft)
  if (checks$boundary_rule[i] != "all") {
    stopifnot(!anyNA(candidates[[checks$boundary_rule[i]]]))
    candidates <- candidates |> filter(.data[[checks$boundary_rule[i]]])
  }
  for (sample in c("all", "multifamily")) {
    for (outcome in c("density_far", "density_dupac")) {
      for (eligibility in c("outcome_specific", "common")) {
        model_data <- candidates |> filter(sample == "all" | external_multifamily,
          if (outcome == "density_far") far_valid else dupac_valid,
          eligibility == "outcome_specific" | both_valid) |>
          mutate(log_outcome = log(.data[[outcome]]))
        model <- fixest::feols(log_outcome ~ i(distance_bin, ref = "bin_05") +
          share_white_own + share_black_own + median_hh_income_own +
          share_bach_plus_own + homeownership_rate_own | zone_group + segment_id + construction_year,
          data = model_data, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
        coefficient <- fixest::coeftable(model)["distance_bin::bin_06", ]
        results[[length(results) + 1L]] <- tibble(check = checks$check[i], sample, outcome, eligibility,
          n_candidates = nrow(model_data), n = nobs(model), estimate = unname(coefficient["Estimate"]),
          std_error = unname(coefficient["Std. Error"]), p_value = unname(coefficient["Pr(>|t|)"]),
          percent_effect = 100 * expm1(unname(coefficient["Estimate"])))
      }
    }
  }
}
results <- bind_rows(results)
stopifnot(all(is.finite(results$estimate)), all(is.finite(results$std_error)),
  all(is.finite(results$p_value)), all(is.finite(results$percent_effect)))
common <- results |> filter(eligibility == "common") |>
  summarise(same_sample_size = n_distinct(n) == 1L, .by = c(check, sample))
stopifnot(all(common$same_sample_size))
readr::write_csv(results, "../output/density_common_sample_estimates.csv")
cat("Main DUPAC candidates lost under common eligibility:\n")
print(projects |> filter(within_500ft, abs(true_distance_ft) < 500, dupac_valid, !both_valid) |>
  select(project_id, source_addresses, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, decision_reason), n = Inf, width = Inf)
