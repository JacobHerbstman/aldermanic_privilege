# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_ordering_drivers/code")
# sample_name <- "multifamily"
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# bin_width_ft <- 100
# controls <- "share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own"
# fixed_effects <- "zone_group + segment_id + construction_year"
# cluster <- "ward_pair"
# workers <- 4
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 9L)
  sample_name <- args[1]
  start_year <- as.integer(args[2])
  end_year <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  bin_width_ft <- as.numeric(args[5])
  controls <- args[6]
  fixed_effects <- args[7]
  cluster <- args[8]
  workers <- as.integer(args[9])
}
stopifnot(sample_name %in% c("all", "multifamily"), end_year == 2022L,
  start_year <= end_year, bandwidth_ft %% bin_width_ft == 0, workers >= 1L)
setFixest_nthreads(1)
setDTthreads(1)

# Keep the previously fitted common FAR/DUPAC sample and its fixed geography.
projects <- fread("../input/new_construction_analysis_data.csv", colClasses = c(project_id = "character", ward_pair = "character", segment_id = "character"))
stopifnot(!anyDuplicated(projects$project_id))
projects[, paper_distance := signed_distance_m / .3048]
numeric_controls <- strsplit(controls, " + ", fixed = TRUE)[[1]]
projects <- projects[construction_year >= start_year & construction_year <= end_year & density_eligible == TRUE &
  (sample_name == "all" | external_multifamily == TRUE) & is.finite(paper_distance) & abs(paper_distance) < bandwidth_ft &
  !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) & ward_pair != "" &
  Reduce(`&`, lapply(projects[, ..numeric_controls], is.finite))]
stopifnot(all(is.finite(projects$density_far) & projects$density_far > 0),
  all(is.finite(projects$density_dupac) & projects$density_dupac > 0))
projects[, `:=`(alderman_a = pmin(alderman_own, alderman_neighbor), alderman_b = pmax(alderman_own, alderman_neighbor))]
projects[, pair := paste(alderman_a, alderman_b, sep = " / ")]
raw <- fread("../input/raw_processing_pairs.csv")[market == paste0("density_", sample_name) & cutoff == end_year & measure == "mean_log_days"]
stopifnot(!anyDuplicated(raw$pair), !any(raw$agreement == "tie"), setequal(projects$pair, raw$pair))
index <- match(projects$pair, raw$pair)
projects[, `:=`(disagree = raw$agreement[index] == "reverse",
  raw_distance = abs(paper_distance) * sign(raw$raw_a[index] - raw$raw_b[index]) * fifelse(alderman_own == alderman_a, 1, -1))]
score_direction <- sign(raw$score_a[index] - raw$score_b[index]) * ifelse(projects$alderman_own == projects$alderman_a, 1, -1)
stopifnot(all(sign(projects$paper_distance) == score_direction),
  all(projects$disagree == (sign(projects$paper_distance) != sign(projects$raw_distance))))
counts <- projects[, .(observations = .N), by = pair]
stopifnot(all(counts$observations == raw$observations[match(counts$pair, raw$pair)]))
bin_edges <- seq(-bandwidth_ft, bandwidth_ft, bin_width_ft)
reference_bin <- bandwidth_ft / bin_width_ft
main_term <- paste0("distance_bin::", reference_bin + 1L)

fit_density <- function(d, distance) {
  d <- copy(d)
  d[, distance_bin := cut(distance, bin_edges, labels = FALSE, include.lowest = TRUE, right = FALSE)]
  stopifnot(!anyNA(d$distance_bin))
  results <- list()
  for (outcome in c("density_far", "density_dupac")) {
    model <- feols(as.formula(sprintf("log(%s) ~ i(distance_bin, ref = %d) + %s | %s",
      outcome, reference_bin, controls, fixed_effects)), d, cluster = as.formula(paste("~", cluster)), notes = FALSE, warn = FALSE)
    stopifnot(main_term %in% names(coef(model)))
    if (outcome == "density_far") far_rows <- obs(model) else stopifnot(identical(far_rows, obs(model)))
    tab <- coeftable(model)[main_term, ]
    results[[outcome]] <- data.table(sample = sample_name, outcome, estimate = unname(tab[1]), std_error = unname(tab[2]),
      p_value = unname(tab[4]), n = nobs(model), input_n = nrow(d), ward_pairs = uniqueN(d[[cluster]][obs(model)]))
  }
  rbindlist(results)
}
baseline <- fit_density(projects, projects$paper_distance)
raw_fit <- fit_density(projects, projects$raw_distance)
stopifnot(all(baseline$n == nrow(projects)), all(raw_fit$n == nrow(projects)))
previous <- fread(sprintf("../input/density_%s_results.csv", sample_name))
for (scenario_name in c("baseline", "reverse_disagreements")) {
  current <- if (scenario_name == "baseline") baseline else raw_fit
  old <- previous[scenario == scenario_name]
  old <- old[match(current$outcome, old$outcome)]
  stopifnot(max(abs(current$estimate - old$estimate)) < 1e-8,
    max(abs(current$std_error - old$std_error)) < 1e-8, identical(current$n, old$n))
}

# Pair trials act on each disagreement. Alderman trials act on all of that person's
# disagreeing pairs; deletion instead removes ALL their comparisons, including agreements.
disagreements <- raw[agreement == "reverse"]
aldermen <- sort(unique(c(disagreements$alderman_a, disagreements$alderman_b)))
selections <- rbind(data.table(kind = "pair", name = disagreements$pair), data.table(kind = "alderman", name = aldermen))
trial_results <- parallel::mclapply(seq_len(nrow(selections)), function(i) {
  kind_name <- selections$kind[i]
  selected_name <- selections$name[i]
  involved <- if (kind_name == "pair") projects$pair == selected_name else
    projects$alderman_a == selected_name | projects$alderman_b == selected_name
  switched <- involved & projects$disagree
  results <- list()
  for (action in c("switch_from_original", "restore_from_raw", "drop_from_original", "drop_from_raw")) {
    dropping <- action %in% c("drop_from_original", "drop_from_raw")
    d <- if (dropping) projects[!involved] else copy(projects)
    distance <- if (action %in% c("restore_from_raw", "drop_from_raw")) d$raw_distance else d$paper_distance
    if (action == "switch_from_original") distance[switched] <- d$raw_distance[switched]
    if (action == "restore_from_raw") distance[switched] <- d$paper_distance[switched]
    result <- fit_density(d, distance)
    if (!dropping) stopifnot(all(result$n == nrow(projects)))
    result[, `:=`(kind = kind_name, name = selected_name, action = action, involved_projects = sum(involved),
      switched_projects = sum(switched), switched_pairs = uniqueN(projects$pair[switched]),
      near_projects = sum(involved & abs(projects$paper_distance) < bin_width_ft),
      near_switched_projects = sum(switched & abs(projects$paper_distance) < bin_width_ft))]
    results[[action]] <- result
  }
  rbindlist(results)
}, mc.cores = workers, mc.set.seed = FALSE)
errors <- vapply(trial_results, inherits, logical(1), "try-error")
if (any(errors)) stop(trial_results[[which(errors)[1]]])
stopifnot(all(vapply(trial_results, is.data.table, logical(1))))
results <- rbindlist(trial_results)
results[, `:=`(percent_effect = 100 * expm1(estimate),
  original_estimate = baseline$estimate[match(outcome, baseline$outcome)],
  raw_estimate = raw_fit$estimate[match(outcome, raw_fit$outcome)])]
results[, `:=`(change_from_original = estimate - original_estimate, change_from_raw = estimate - raw_estimate)]
SaveData(results, c("sample", "outcome", "kind", "name", "action"), sprintf("../output/%s_driver_results.csv", sample_name))

# Retain observable measurements and dates for every affected project.
evidence <- projects[disagree == TRUE, .(project_id, source_family, source_addresses, component_pins, pair,
  alderman_own, alderman_neighbor, ward, neighbor_ward, construction_year, dwelling_units,
  building_sqft, land_sqft, density_far, density_dupac, paper_distance, raw_distance, segment_id, zone_group)]
SaveData(evidence, "project_id", sprintf("../output/%s_project_evidence.csv", sample_name))
side_summaries <- list()
for (band in c("within_500ft", "within_100ft")) {
  d <- if (band == "within_100ft") projects[disagree == TRUE & abs(paper_distance) < bin_width_ft] else projects[disagree == TRUE]
  side_summaries[[band]] <- d[, .(band, projects = .N, ward = paste(sort(unique(ward)), collapse = "; "),
    first_year = min(construction_year), last_year = max(construction_year), median_units = as.numeric(median(dwelling_units)),
    median_far = median(density_far), median_dupac = median(density_dupac), mean_log_far = mean(log(density_far)),
    mean_log_dupac = mean(log(density_dupac))), by = .(pair, alderman_own)]
}
SaveData(rbindlist(side_summaries), c("pair", "alderman_own", "band"), sprintf("../output/%s_pair_sides.csv", sample_name))
cat(sample_name, ":", nrow(disagreements), "disagreeing pairs;", length(aldermen), "alderman groups;", nrow(results), "trial estimates.\n")
