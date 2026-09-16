# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/raw_log_score_sensitivity/code")
# outcome <- "n_high_discretion_application"
# remap_year <- 2015
# event_window <- 5
# bandwidth_ft <- 500
# fixed_effects <- "block_id + ward_pair_id^year"
# cluster <- "ward_pair_id"
library(data.table)
library(fixest)
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 6L)
  outcome <- args[1]
  remap_year <- as.integer(args[2])
  event_window <- as.integer(args[3])
  bandwidth_ft <- as.numeric(args[4])
  fixed_effects <- args[5]
  cluster <- args[6]
}
stopifnot(remap_year == 2015L, event_window >= 2L, bandwidth_ft > 0)
setFixest_nthreads(1)
setDTthreads(1)

# Preserve the stable-incumbent panel and the existing positive pre-period volume rule.
panel <- as.data.table(arrow::read_parquet("../input/permit_block_year_panel_2015.parquet"))
panel <- panel[dist_m <= bandwidth_ft * .3048 & relative_year >= -event_window & relative_year <= event_window &
  !is.na(strictness_change_frozen) & !is.na(ward_pair_id) & ward_pair_id != "" & stable_both == TRUE]
stopifnot(!anyDuplicated(panel[, .(block_id, year)]), all(panel$year - panel$relative_year == remap_year))
panel[, pre_volume := sum(n_high_discretion_application[relative_year < 0]), by = block_id]
panel <- panel[pre_volume > 0]
panel[, `:=`(permits = get(outcome), post = as.integer(relative_year >= 0), paper_direction = sign(strictness_change_frozen))]
stopifnot(all(is.finite(panel$permits) & panel$permits >= 0),
  all((panel$paper_direction != 0) == (panel$ward_origin != panel$ward_dest)))
raw <- fread("../input/raw_processing_pairs.csv")[market == "permit_remap" & cutoff == remap_year - 1L & measure == "mean_log_days"]
stopifnot(!anyDuplicated(raw$pair), nrow(raw) > 0L, !any(raw$agreement == "tie"))
panel[, `:=`(alderman_a = pmin(alderman_origin_2014, alderman_dest_2014), alderman_b = pmax(alderman_origin_2014, alderman_dest_2014))]
panel[, pair := paste(alderman_a, alderman_b, sep = " / ")]
index <- match(panel$pair, raw$pair)
reassigned <- panel$paper_direction != 0
stopifnot(!anyNA(index[reassigned]))
panel[, raw_direction := 0]
panel[reassigned, raw_direction := sign(raw$raw_a[index[reassigned]] - raw$raw_b[index[reassigned]]) *
  fifelse(alderman_dest_2014 == alderman_a, 1, -1)]
score_direction <- sign(raw$score_a[index[reassigned]] - raw$score_b[index[reassigned]]) *
  ifelse(panel$alderman_dest_2014[reassigned] == panel$alderman_a[reassigned], 1, -1)
stopifnot(all(score_direction == panel$paper_direction[reassigned]))
event_terms <- paste0("relative_year::", setdiff(-event_window:event_window, -1L), ":direction")
pre_terms <- paste0("relative_year::", -event_window:-2L, ":direction")

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
    results[[specification]] <- data.table(analysis = "permit_remap", outcome, specification, term = terms,
      estimate = tab[, 1], std_error = tab[, 2], p_value = 2 * pt(-abs(tab[, 1] / tab[, 2]), df),
      ci_low = tab[, 1] - qt(.975, df) * tab[, 2], ci_high = tab[, 1] + qt(.975, df) * tab[, 2],
      input_n = nrow(d), n = nobs(model), blocks = uniqueN(d$block_id[obs(model)]),
      ward_pairs = uniqueN(d$ward_pair_id[obs(model)]),
      reassigned_blocks = uniqueN(d$block_id[obs(model)][d$paper_direction[obs(model)] != 0]), pretrend_p_value = pre_p)
  }
  list(results = rbindlist(results), observations = obs(pooled))
}
fit <- fit_permits(panel, panel$paper_direction)
baseline <- fit$results
panel <- panel[fit$observations]
block_info <- unique(panel[, .(block_id, ward_pair_id, pair, paper_direction, raw_direction)])
stopifnot(!anyDuplicated(block_info$block_id))
counts <- block_info[paper_direction != 0, .(observations = .N), by = pair]
stopifnot(setequal(counts$pair, raw$pair), all(counts$observations == raw$observations[match(counts$pair, raw$pair)]))
pair_wards <- unique(block_info[paper_direction != 0, .(pair, ward_pair_id)])
stopifnot(!anyDuplicated(pair_wards$pair), !anyDuplicated(pair_wards$ward_pair_id))
selected <- pair_wards[pair %in% raw[agreement == "reverse", pair], ward_pair_id]
stopifnot(all((panel$paper_direction != panel$raw_direction) ==
  (panel$ward_pair_id %in% selected & panel$paper_direction != 0)))
reversed <- fit_permits(panel, -panel$paper_direction)$results
stopifnot(max(abs(baseline$estimate + reversed$estimate)) < 1e-6,
  max(abs(baseline$std_error - reversed$std_error)) < 1e-6)

# Drop the whole geographic comparison, including its unchanged control blocks.
# Reversal changes direction for reassigned blocks only; unchanged blocks stay at zero.
baseline[, `:=`(scenario = "baseline", selected_pairs = 0L, selected_blocks = 0L, selected_reassigned_blocks = 0L)]
results <- list(baseline)
for (action in c("drop_disagreements", "reverse_disagreements")) {
  d <- if (action == "drop_disagreements") panel[!ward_pair_id %in% selected] else copy(panel)
  direction <- d$paper_direction
  if (action == "reverse_disagreements") direction[d$ward_pair_id %in% selected] <- -direction[d$ward_pair_id %in% selected]
  stopifnot(all(direction == d$raw_direction))
  fit <- fit_permits(d, direction)
  if (action == "reverse_disagreements") stopifnot(identical(fit$observations, seq_len(nrow(panel))))
  result <- fit$results
  result[, `:=`(scenario = action, selected_pairs = length(selected),
    selected_blocks = nrow(block_info[ward_pair_id %in% selected]),
    selected_reassigned_blocks = nrow(block_info[ward_pair_id %in% selected & paper_direction != 0]))]
  results[[action]] <- result
}
results <- rbindlist(results)
results[, `:=`(percent_effect = 100 * expm1(estimate), percent_low = 100 * expm1(ci_low), percent_high = 100 * expm1(ci_high))]
SaveData(results, c("analysis", "outcome", "scenario", "specification", "term"), "../output/permit_results.csv")
print(results[specification == "pooled", .(scenario, n, blocks, percent_effect, p_value, pretrend_p_value)])
