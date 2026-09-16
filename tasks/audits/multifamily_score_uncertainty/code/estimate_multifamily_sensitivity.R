# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/multifamily_score_uncertainty/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
# fragile_thresholds <- c(0.75, 0.95)
# workers <- 4

library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 9L)
  start_year <- as.integer(args[1])
  end_year <- as.integer(args[2])
  bandwidth_ft <- as.numeric(args[3])
  bin_width_ft <- as.numeric(args[4])
  controls <- args[5]
  fixed_effects <- args[6]
  cluster <- args[7]
  fragile_thresholds <- as.numeric(strsplit(args[8], ",", fixed = TRUE)[[1]])
  workers <- as.integer(args[9])
}
stopifnot(end_year == 2022L, start_year <= end_year, bandwidth_ft %% bin_width_ft == 0,
  all(fragile_thresholds > .5 & fragile_thresholds < 1), workers >= 1)
setFixest_nthreads(1)
setDTthreads(1)

# Freeze the paper's common FAR/DUPAC multifamily sample and its observed geography.
projects <- fread("../input/new_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
stopifnot(!anyDuplicated(projects$project_id))
projects[, paper_distance := signed_distance_m / .3048]
numeric_controls <- strsplit(controls, " + ", fixed = TRUE)[[1]]
projects <- projects[construction_year >= start_year & construction_year <= end_year &
  density_eligible == TRUE & external_multifamily == TRUE &
  is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft &
  !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
  Reduce(`&`, lapply(projects[, ..numeric_controls], is.finite))]
stopifnot(all(is.finite(projects$density_far) & projects$density_far > 0),
  all(is.finite(projects$density_dupac) & projects$density_dupac > 0))
projects[, `:=`(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor))]
projects[, pair := paste(alderman_a, alderman_b, sep = " / ")]
baseline_scores <- fread("../input/baseline_through2022.csv")
stopifnot(!anyDuplicated(baseline_scores$alderman))
projects[, baseline_gap := baseline_scores$score[match(alderman_own, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_neighbor, baseline_scores$alderman)]]
stopifnot(all(is.finite(projects$baseline_gap)), all(sign(projects$baseline_gap) == sign(projects$paper_distance)))
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)

# Reuse this regression for the baseline, score draws, and all pair stress tests.
fit_density <- function(d, distance) {
  d <- copy(d)
  d[, distance_bin := cut(distance, bin_edges, labels = FALSE, include.lowest = TRUE, right = FALSE)]
  stopifnot(!anyNA(d$distance_bin))
  results <- list()
  for (outcome in c("density_far", "density_dupac")) {
    model <- feols(as.formula(sprintf("log(%s) ~ i(distance_bin, ref = %d) + %s | %s",
      outcome, reference_bin, controls, fixed_effects)), d,
      cluster = as.formula(paste("~", cluster)), notes = FALSE, warn = FALSE)
    stopifnot(main_term %in% names(coef(model)))
    if (outcome == "density_far") far_observations <- obs(model)
    if (outcome == "density_dupac") stopifnot(identical(obs(model), far_observations))
    tab <- coeftable(model)[main_term, ]
    critical <- qt(.975, degrees_freedom(model, type = "t"))
    results[[outcome]] <- data.table(outcome, estimate = unname(tab[1]), std_error = unname(tab[2]), p_value = unname(tab[4]),
      ci_low = unname(tab[1] - critical * tab[2]), ci_high = unname(tab[1] + critical * tab[2]),
      n = nobs(model), input_n = nrow(d), alderman_pairs = uniqueN(d$pair[obs(model)]),
      ward_pairs = uniqueN(d[[cluster]][obs(model)]))
  }
  rbindlist(results)
}

baseline <- fit_density(projects, projects$paper_distance)
stopifnot(all(baseline$n == nrow(projects)))
# Reversing EVERY boundary is a relabelling and must negate the coefficient.
reversed <- fit_density(projects, -projects$paper_distance)
stopifnot(max(abs(baseline$estimate + reversed$estimate)) < 1e-6,
  max(abs(baseline$std_error - reversed$std_error)) < 1e-6)
cat("Baseline:", nrow(projects), "buildings;", uniqueN(projects$pair), "alderman pairs.\n")

pair_info <- projects[, .(buildings = .N,
  wards = paste(sort(unique(ward_pair)), collapse = "; ")), by = .(pair, alderman_a, alderman_b)]
pair_info[, baseline_gap := baseline_scores$score[match(alderman_a, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_b, baseline_scores$alderman)]]
pair_info[, baseline_stricter := fifelse(baseline_gap > 0, alderman_a, alderman_b)]
stability <- fread("../input/bootstrap_pairs.csv")[cutoff == end_year]
stopifnot(!anyDuplicated(stability[, .(method, pair)]))
for (method_name in c("property", "quarter", "year")) {
  s <- stability[method == method_name]
  pair_info[, paste0(method_name, "_retention") := s$retain_order[match(pair, s$pair)]]
}
stopifnot(!anyNA(pair_info))
SaveData(pair_info, "pair", "../output/multifamily_pairs.csv")

# Carry each coherent citywide score vector through the fixed building sample.
# These are score-only perturbations: outcomes, controls and locations are not resampled.
scores <- as.data.table(arrow::read_parquet("../input/resamples_through2022.parquet"))
stopifnot(!anyDuplicated(scores[, .(method, draw, alderman)]))
draw_results <- list()
for (method_name in unique(scores$method)) {
  method_scores <- scores[method == method_name]
  draw_numbers <- sort(unique(method_scores$draw))
  for (first_draw in seq(1L, length(draw_numbers), by = 50L)) {
    draws <- draw_numbers[first_draw:min(first_draw + 49L, length(draw_numbers))]
    fits <- parallel::mclapply(draws, function(draw_number) {
      s <- method_scores[draw == draw_number]
      gap <- s$score[match(projects$alderman_own, s$alderman)] - s$score[match(projects$alderman_neighbor, s$alderman)]
      stopifnot(all(is.finite(gap)), all(gap != 0))
      result <- fit_density(projects, abs(projects$paper_distance) * sign(gap))
      stopifnot(all(result$n == baseline$n))
      result[, `:=`(method = method_name, draw = draw_number,
        switched_buildings = sum(sign(gap) != sign(projects$baseline_gap)),
        switched_pairs = uniqueN(projects$pair[sign(gap) != sign(projects$baseline_gap)]))]
      result
    }, mc.cores = workers, mc.set.seed = FALSE)
    stopifnot(all(vapply(fits, is.data.table, logical(1))))
    draw_results[[length(draw_results) + 1L]] <- rbindlist(fits)
    cat(method_name, "density draws completed:", max(draws), "\n")
  }
}
SaveData(rbindlist(draw_results), c("method", "draw", "outcome"), "../output/multifamily_score_draws.csv")

# Choose fragile sets using score stability only, never their density outcomes.
baseline[, `:=`(scenario = "baseline", method = "baseline", threshold = 0, pair = "all",
  selected_pairs = 0L, selected_buildings = 0L)]
scenarios <- list(baseline)
for (method_name in c("property", "quarter", "year")) for (threshold in fragile_thresholds) {
  selected <- pair_info[get(paste0(method_name, "_retention")) < threshold, pair]
  affected <- projects$pair %in% selected
  for (action in c("drop_fragile", "flip_fragile")) {
    d <- if (action == "drop_fragile") projects[!affected] else copy(projects)
    distance <- d$paper_distance
    if (action == "flip_fragile") distance[d$pair %in% selected] <- -distance[d$pair %in% selected]
    result <- fit_density(d, distance)
    result[, `:=`(scenario = action, method = method_name, threshold = threshold, pair = "all",
      selected_pairs = length(selected), selected_buildings = sum(affected))]
    scenarios[[length(scenarios) + 1L]] <- result
  }
}

# Examine every pair individually, so stable influential pairs are visible too.
individual <- parallel::mclapply(pair_info$pair, function(selected_pair) {
  affected <- projects$pair == selected_pair
  results <- lapply(c("drop_one", "flip_one"), function(action) {
    d <- if (action == "drop_one") projects[!affected] else copy(projects)
    distance <- d$paper_distance
    if (action == "flip_one") distance[d$pair == selected_pair] <- -distance[d$pair == selected_pair]
    result <- fit_density(d, distance)
    result[, `:=`(scenario = action, method = "individual", threshold = 0, pair = selected_pair,
      selected_pairs = 1L, selected_buildings = sum(affected))]
    result
  })
  rbindlist(results)
}, mc.cores = workers, mc.set.seed = FALSE)
stopifnot(all(vapply(individual, is.data.table, logical(1))))
results <- rbindlist(c(scenarios, individual))
results[, `:=`(percent_effect = 100 * expm1(estimate),
  percent_low = 100 * expm1(ci_low), percent_high = 100 * expm1(ci_high),
  change_log_coefficient = estimate - baseline$estimate[match(outcome, baseline$outcome)])]
SaveData(results, c("scenario", "method", "threshold", "pair", "outcome"), "../output/multifamily_pair_sensitivity.csv")
