# setwd("tasks/audits/uea_feedback_checks/code")
# Exploratory (UEA feedback): every density specification in the paper and its appendix, with the binary
# more-stringent-side indicator and with that indicator times the score gap between the two sides (the difference
# across a boundary per standard deviation of score gap). Average difference within 500 ft, as in
# tasks/shared/code/density_boundary_helpers.R; the variants follow tasks/density_boundary_checks,
# tasks/density_appendix_results and tasks/density_score_robustness. The building leave-out score of
# density_score_robustness is omitted: its estimates equal the main ones to three decimals.
placebo_ft <- 1000L
donut_ft <- c(25L, 50L)
gap_thresholds <- c(0.25, 0.50)

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/density_boundary_helpers.R")

buildings <- readr::read_csv("../input/new_construction_analysis_data.csv", show_col_types = FALSE,
  col_types = density_column_types) |>
  density_analysis_sample() |>
  left_join(readr::read_csv("../input/density_boundary_characteristics.csv", show_col_types = FALSE,
    col_types = readr::cols(building_id = readr::col_character(), .default = readr::col_guess())),
    by = "building_id", relationship = "one-to-one") |>
  mutate(score_gap = abs(strictness_own - strictness_neighbor))
stopifnot(!anyDuplicated(buildings$building_id), !anyNA(buildings$straight_boundary))

# Each variant: the cutoff (0 or a placebo line), a donut, a sample restriction, the fixed effects and the outcome.
variants <- tibble::tribble(
  ~variant, ~cutoff_ft, ~donut, ~keep, ~fixed_effects, ~outcome,
  "Main", 0L, 0L, "all", density_fixed_effects, "log(density_dupac)",
  "Limited expressway or water overlap", 0L, 0L, "simple_overlap_keep", density_fixed_effects, "log(density_dupac)",
  "Limited physical-feature or arterial overlap", 0L, 0L, "share_based_keep", density_fixed_effects, "log(density_dupac)",
  "Straight boundary segment", 0L, 0L, "straight_boundary", density_fixed_effects, "log(density_dupac)",
  "Segment x construction-year FE", 0L, 0L, "all", "segment_id^construction_year + zone_group", "log(density_dupac)",
  sprintf("Donut %d ft", donut_ft[1]), 0L, donut_ft[1], "all", density_fixed_effects, "log(density_dupac)",
  sprintf("Donut %d ft", donut_ft[2]), 0L, donut_ft[2], "all", density_fixed_effects, "log(density_dupac)",
  sprintf("Placebo %d ft inside less-stringent side", placebo_ft), -placebo_ft, 0L, "all", density_fixed_effects, "log(density_dupac)",
  sprintf("Placebo %d ft inside more-stringent side", placebo_ft), placebo_ft, 0L, "all", density_fixed_effects, "log(density_dupac)",
  sprintf("Score gap at least %.2f SD", gap_thresholds[1]), 0L, 0L, "gap_1", density_fixed_effects, "log(density_dupac)",
  sprintf("Score gap at least %.2f SD", gap_thresholds[2]), 0L, 0L, "gap_2", density_fixed_effects, "log(density_dupac)",
  "FAR instead of DUPAC", 0L, 0L, "far", density_fixed_effects, "log(density_far)"
)

results <- list()
for (v in seq_len(nrow(variants))) {
  d <- buildings |>
    mutate(running_distance_ft = signed_distance_m / 0.3048 - variants$cutoff_ft[v]) |>
    bin_running_distance() |>
    filter(abs(running_distance_ft) >= variants$donut[v]) |>
    mutate(stricter_side_gap = stricter_side * score_gap)
  d <- switch(variants$keep[v],
    all = d,
    gap_1 = filter(d, score_gap >= gap_thresholds[1]),
    gap_2 = filter(d, score_gap >= gap_thresholds[2]),
    far = filter(d, allow_far, density_far > 0),
    d[d[[variants$keep[v]]], ])
  for (i in seq_len(nrow(density_samples))) {
    ds <- filter_density_sample(d, density_samples$sample[i])
    for (term in c("stricter_side", "stricter_side_gap")) {
      model <- fixest::feols(as.formula(sprintf("%s ~ %s + %s | %s", variants$outcome[v], term, density_controls,
        variants$fixed_effects[v])), data = ds, cluster = ~ward_pair, warn = FALSE, notes = FALSE)
      results[[length(results) + 1]] <- tibble(variant = variants$variant[v], sample = density_samples$sample[i],
        treatment = if (term == "stricter_side") "binary" else "continuous",
        estimate = coef(model)[[term]], std_error = fixest::se(model)[[term]],
        t_stat = estimate / std_error, p_value = fixest::pvalue(model)[[term]], observations = nobs(model),
        ward_pairs = n_distinct(ds$ward_pair), mean_gap = mean(ds$score_gap))
    }
  }
}
write_csv(bind_rows(results), "../output/density_continuous_suite.csv")
