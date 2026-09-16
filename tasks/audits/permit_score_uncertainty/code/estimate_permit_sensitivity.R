# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_score_uncertainty/code")
# outcome <- "n_high_discretion_application"
# remap_year <- 2015
# event_window <- 5
# bandwidth_ft <- 500
# fixed_effects <- "block_id + ward_pair_id^year"
# cluster <- "ward_pair_id"
# fragile_thresholds <- c(0.75, 0.95)
# workers <- 4
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 8L)
  outcome <- args[1]
  remap_year <- as.integer(args[2])
  event_window <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  fixed_effects <- args[5]
  cluster <- args[6]
  fragile_thresholds <- as.numeric(strsplit(args[7], ",", fixed = TRUE)[[1]])
  workers <- as.integer(args[8])
}
stopifnot(remap_year == 2015L, event_window >= 2L, bandwidth_ft > 0,
  all(fragile_thresholds > .5 & fragile_thresholds < 1), workers >= 1L)
setFixest_nthreads(1)
setDTthreads(1)

# Preserve the paper's block panel, stable-incumbent sample, and positive pre-period volume rule.
panel <- as.data.table(arrow::read_parquet("../input/permit_block_year_panel_2015.parquet"))
panel <- panel[dist_m <= bandwidth_ft * .3048 & relative_year >= -event_window & relative_year <= event_window &
  !is.na(strictness_change_frozen) & !is.na(ward_pair_id) & ward_pair_id != "" & stable_both == TRUE]
stopifnot(!anyDuplicated(panel[, .(block_id, year)]), all(panel$year - panel$relative_year == remap_year))
panel[, pre_volume := sum(n_high_discretion_application[relative_year < 0]), by = block_id]
panel <- panel[pre_volume > 0]
panel[, `:=`(permits = get(outcome), post = as.integer(relative_year >= 0), paper_direction = sign(strictness_change_frozen))]
stopifnot(all(is.finite(panel$permits) & panel$permits >= 0),
  all((panel$paper_direction != 0) == (panel$ward_origin != panel$ward_dest)))
baseline_scores <- fread("../input/baseline_through2014.csv")
stopifnot(!anyDuplicated(baseline_scores$alderman))
panel[, baseline_gap := baseline_scores$score[match(alderman_dest_2014, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_origin_2014, baseline_scores$alderman)]]
stopifnot(all(is.finite(panel$baseline_gap)), all(sign(panel$baseline_gap) == panel$paper_direction))
event_terms <- paste0("relative_year::", setdiff(-event_window:event_window, -1L), ":direction")
pre_terms <- paste0("relative_year::", -event_window:-2L, ":direction")

# The same two Poisson specifications are refitted for every score vector and pair test.
fit_permits <- function(d, direction) {
  d <- copy(d)
  d[, `:=`(direction = direction, post_signed = post * direction)]
  pooled <- fepois(as.formula(paste("permits ~ post_signed |", fixed_effects)), d,
    cluster = as.formula(paste("~", cluster)), notes = FALSE)
  event <- fepois(as.formula(paste("permits ~ i(relative_year, direction, ref = -1) |", fixed_effects)), d,
    cluster = as.formula(paste("~", cluster)), notes = FALSE)
  stopifnot(pooled$convStatus, event$convStatus, identical(obs(pooled), obs(event)),
    "post_signed" %in% names(coef(pooled)), all(event_terms %in% names(coef(event))))
  pre_beta <- coef(event)[pre_terms]
  pre_vcov <- vcov(event)[pre_terms, pre_terms]
  pre_f <- drop(t(pre_beta) %*% solve(pre_vcov, pre_beta)) / length(pre_terms)
  pre_p <- pf(pre_f, length(pre_terms), degrees_freedom(event, type = "t"), lower.tail = FALSE)
  results <- list()
  for (specification in c("pooled", "event")) {
    model <- if (specification == "pooled") pooled else event
    terms <- if (specification == "pooled") "post_signed" else event_terms
    tab <- coeftable(model)[terms, , drop = FALSE]
    df <- degrees_freedom(model, type = "t")
    results[[specification]] <- data.table(specification, term = terms,
      estimate = tab[, 1], std_error = tab[, 2], p_value = 2 * pt(-abs(tab[, 1] / tab[, 2]), df),
      ci_low = tab[, 1] - qt(.975, df) * tab[, 2], ci_high = tab[, 1] + qt(.975, df) * tab[, 2],
      n = nobs(model), blocks = uniqueN(d$block_id[obs(model)]), ward_pairs = uniqueN(d$ward_pair_id[obs(model)]),
      reassigned_blocks = uniqueN(d$block_id[obs(model)][d$paper_direction[obs(model)] != 0]), pretrend_p_value = pre_p)
  }
  list(results = rbindlist(results), observations = obs(pooled))
}

baseline_fit <- fit_permits(panel, panel$paper_direction)
baseline <- baseline_fit$results
# All-zero Poisson fixed-effect groups are omitted by the existing estimator.
# Retain its fitted observations for every score-only reversal.
panel <- panel[baseline_fit$observations]
reversed <- fit_permits(panel, -panel$paper_direction)$results
stopifnot(max(abs(baseline$estimate + reversed$estimate)) < 1e-6,
  max(abs(baseline$std_error - reversed$std_error)) < 1e-6)
blocks <- unique(panel[, .(block_id, ward_pair_id, paper_direction, alderman_origin_2014, alderman_dest_2014)])
stopifnot(!anyDuplicated(blocks$block_id))
pair_info <- unique(blocks[paper_direction != 0, .(ward_pair_id,
  alderman_a = pmin(alderman_origin_2014, alderman_dest_2014),
  alderman_b = pmax(alderman_origin_2014, alderman_dest_2014))])
pair_info[, pair := paste(alderman_a, alderman_b, sep = " / ")]
stopifnot(!anyDuplicated(pair_info$ward_pair_id), !anyDuplicated(pair_info$pair))
counts <- blocks[, .(blocks = .N, reassigned_blocks = sum(paper_direction != 0)), by = ward_pair_id]
pair_info <- merge(pair_info, counts, by = "ward_pair_id", all.x = TRUE, sort = FALSE)
pair_info[, baseline_gap := baseline_scores$score[match(alderman_a, baseline_scores$alderman)] -
  baseline_scores$score[match(alderman_b, baseline_scores$alderman)]]
pair_info[, baseline_stricter := fifelse(baseline_gap > 0, alderman_a, alderman_b)]
stability <- fread("../input/bootstrap_pairs.csv")[cutoff == remap_year - 1L]
stopifnot(!anyDuplicated(stability[, .(method, pair)]))
for (method_name in c("property", "quarter", "year")) {
  s <- stability[method == method_name]
  pair_info[, paste0(method_name, "_retention") := s$retain_order[match(pair, s$pair)]]
}
stopifnot(!anyNA(pair_info))
SaveData(pair_info, "ward_pair_id", "../output/permit_pairs.csv")
cat("Baseline:", nrow(panel), "block-years;", uniqueN(panel$block_id), "blocks;",
  sum(blocks$paper_direction != 0), "reassigned blocks;", nrow(pair_info), "informative pairs.\n")
print(baseline[specification == "pooled"])

# Each saved pre-remap score vector changes direction only for reassigned blocks.
scores <- as.data.table(arrow::read_parquet("../input/resamples_through2014.parquet"))
stopifnot(!anyDuplicated(scores[, .(method, draw, alderman)]))
draw_results <- list()
for (method_name in unique(scores$method)) {
  method_scores <- scores[method == method_name]
  draw_numbers <- sort(unique(method_scores$draw))
  for (first_draw in seq(1L, length(draw_numbers), by = 50L)) {
    draws <- draw_numbers[first_draw:min(first_draw + 49L, length(draw_numbers))]
    fits <- parallel::mclapply(draws, function(draw_number) {
      s <- method_scores[draw == draw_number]
      gap <- s$score[match(panel$alderman_dest_2014, s$alderman)] - s$score[match(panel$alderman_origin_2014, s$alderman)]
      stopifnot(all(is.finite(gap)), all((gap != 0) == (panel$paper_direction != 0)))
      fit <- fit_permits(panel, sign(gap))
      stopifnot(identical(fit$observations, seq_len(nrow(panel))))
      result <- fit$results
      changed <- sign(gap) != panel$paper_direction
      result[, `:=`(method = method_name, draw = draw_number,
        reversed_blocks = uniqueN(panel$block_id[changed]), reversed_pairs = uniqueN(panel$ward_pair_id[changed]))]
      result
    }, mc.cores = workers, mc.set.seed = FALSE)
    stopifnot(all(vapply(fits, is.data.table, logical(1))))
    draw_results[[length(draw_results) + 1L]] <- rbindlist(fits)
    cat(method_name, "permit draws completed:", max(draws), "\n")
  }
}
SaveData(rbindlist(draw_results), c("method", "draw", "specification", "term"), "../output/permit_score_draws.csv")

# Removing a pair removes its reassigned blocks AND its unchanged comparison blocks.
# Reversing a pair keeps its unchanged blocks at direction zero.
baseline[, `:=`(scenario = "baseline", method = "baseline", threshold = 0, pair = "all",
  selected_pairs = 0L, selected_blocks = 0L, selected_reassigned_blocks = 0L)]
tests <- list(baseline)
selections <- list()
for (method_name in c("property", "quarter", "year")) for (threshold in fragile_thresholds) {
  selected <- pair_info[get(paste0(method_name, "_retention")) < threshold, ward_pair_id]
  selections[[length(selections) + 1L]] <- list(method = method_name, threshold = threshold, pair = "all", selected = selected)
}
for (i in seq_len(nrow(pair_info))) selections[[length(selections) + 1L]] <- list(method = "individual", threshold = 0,
  pair = pair_info$pair[i], selected = pair_info$ward_pair_id[i])
fits <- parallel::mclapply(selections, function(selection) {
  selected <- selection$selected
  selected_counts <- pair_info[ward_pair_id %in% selected]
  results <- list()
  for (action in c("drop", "flip")) {
    d <- if (action == "drop") panel[!ward_pair_id %in% selected] else copy(panel)
    direction <- d$paper_direction
    if (action == "flip") direction[d$ward_pair_id %in% selected] <- -direction[d$ward_pair_id %in% selected]
    fit <- fit_permits(d, direction)
    stopifnot(identical(fit$observations, seq_len(nrow(d))))
    result <- fit$results
    result[, `:=`(scenario = paste0(action, if (selection$method == "individual") "_one" else "_fragile"),
      method = selection$method, threshold = selection$threshold, pair = selection$pair,
      selected_pairs = length(selected), selected_blocks = sum(selected_counts$blocks),
      selected_reassigned_blocks = sum(selected_counts$reassigned_blocks))]
    results[[action]] <- result
  }
  rbindlist(results)
}, mc.cores = workers, mc.set.seed = FALSE)
stopifnot(all(vapply(fits, is.data.table, logical(1))))
results <- rbindlist(c(tests, fits))
results[, `:=`(percent_effect = 100 * expm1(estimate), percent_low = 100 * expm1(ci_low),
  percent_high = 100 * expm1(ci_high), change_log_coefficient = estimate - baseline$estimate[match(term, baseline$term)])]
SaveData(results, c("scenario", "method", "threshold", "pair", "specification", "term"), "../output/permit_pair_sensitivity.csv")
