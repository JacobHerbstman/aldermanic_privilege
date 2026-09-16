# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/score_robustness_review/code")
library(data.table)
source("../../../shared/code/save_data.R")

raw_aldermen <- fread("../output/raw_processing_by_alderman.csv")
raw_pairs <- fread("../output/raw_processing_pairs.csv")
raw_agreement <- fread("../output/raw_processing_agreement.csv")
raw_correlations <- fread("../output/raw_processing_correlations.csv")
density_results <- fread("../input/multifamily_pair_sensitivity.csv")
density_results[, outcome := fifelse(outcome == "density_far", "Multifamily FAR", "Multifamily DUPAC")]
permit_results <- fread("../input/permit_pair_sensitivity.csv")[specification == "pooled"][, outcome := "Permits"]
sales_results <- fread("../input/sales_pair_sensitivity.csv")[, outcome := "Sale prices"]
rent_results <- fread("../input/rent_pair_sensitivity.csv")[, outcome := "Rents"]
tests <- rbindlist(list(density_results, permit_results, sales_results, rent_results), use.names = TRUE, fill = TRUE)
density_draws <- fread("../input/multifamily_score_summary.csv")
density_draws[, outcome := fifelse(outcome == "density_far", "Multifamily FAR", "Multifamily DUPAC")]
permit_draws <- fread("../input/permit_score_summary.csv")[specification == "pooled"][, outcome := "Permits"]
price_draws <- fread("../input/price_score_summary.csv")[, outcome := fifelse(market == "sales", "Sale prices", "Rents")]
draws <- rbindlist(list(density_draws, permit_draws, price_draws), use.names = TRUE, fill = TRUE)
draws[, original_sign_share := fifelse(outcome %in% c("Sale prices", "Rents"), positive_share, negative_share)]
stopifnot(!anyDuplicated(draws[, .(outcome, method)]))
summary <- list()
for (outcome_name in c("Permits", "Multifamily FAR", "Multifamily DUPAC", "Sale prices", "Rents")) {
  baseline <- tests[outcome == outcome_name & scenario == "baseline"]
  stopifnot(nrow(baseline) == 1L)
  for (method_name in c("property", "quarter", "year")) for (cutoff in c(.75, .95)) {
    d <- draws[outcome == outcome_name & method == method_name]
    dropped <- tests[outcome == outcome_name & method == method_name & threshold == cutoff & scenario == "drop_fragile"]
    flipped <- tests[outcome == outcome_name & method == method_name & threshold == cutoff & scenario == "flip_fragile"]
    stopifnot(nrow(d) == 1L, nrow(dropped) == 1L, nrow(flipped) == 1L)
    summary[[length(summary) + 1L]] <- data.table(outcome = outcome_name, method = method_name, threshold = cutoff,
      baseline_effect = baseline$percent_effect, baseline_p = baseline$p_value, baseline_n = baseline$n,
      draw_median = d$percent_median, draw_low = d$percent_low, draw_high = d$percent_high,
      original_sign_share = d$original_sign_share, selected_pairs = dropped$selected_pairs,
      drop_effect = dropped$percent_effect, drop_p = dropped$p_value, drop_n = dropped$n,
      flip_effect = flipped$percent_effect, flip_p = flipped$p_value)
  }
}
summary <- rbindlist(summary)
SaveData(summary, c("outcome", "method", "threshold"), "../output/robustness_summary.csv")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(row)
      paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
labels <- c(permit_remap = "Permit event study", density_all = "All construction", density_multifamily = "Multifamily construction",
  sales = "Sale prices", rent = "Rents", all_citywide_pairs = "All possible alderman pairs")
metric_names <- c(mean_days = "Average days", median_days = "Median days", mean_log_days = "Average log days")
raw_table <- raw_agreement[measure == "mean_days" & market != "all_citywide_pairs", .(Sample = labels[market],
  `Score period` = paste0("2006–", cutoff), `Agreeing pairs / all pairs` = paste(agree_pairs, "/", pairs),
  `Pairs agreeing (%)` = round(100 * pair_agreement, 1), `Observations on agreeing pairs (%)` = round(100 * observation_agreement, 1))]
all_raw <- raw_agreement[, .(Sample = labels[market], Through = cutoff, Measure = metric_names[measure],
  `Agreeing pairs` = agree_pairs, `Reversed pairs` = reverse_pairs, `Tied pairs` = tied_pairs, `All pairs` = pairs,
  `Pairs agreeing (%)` = round(100 * pair_agreement, 1), `Observations agreeing (%)` = round(100 * observation_agreement, 1))]
raw_examples <- raw_pairs[measure == "mean_days" & agreement == "reverse" & market != "all_citywide_pairs"][order(market, -observations), head(.SD, 5), by = market]
example_table <- raw_examples[, .(Sample = labels[market], Pair = pair, Observations = observations,
  `First alderman: days` = round(raw_a, 1), `Second alderman: days` = round(raw_b, 1), `Score ranks as slower` = score_slower)]
correlation_table <- raw_correlations[, .(Through = cutoff, Measure = metric_names[measure], Aldermen = aldermen,
  Permits = permits, Pearson = round(pearson, 3), Spearman = round(spearman, 3))]
preview <- summary[method == "year" & threshold == .75, .(Outcome = outcome,
  `Original effect (%)` = round(baseline_effect, 1), `Original p-value` = round(baseline_p, 3),
  `Median across score draws (%)` = round(draw_median, 1), `Middle 95% of score draws (%)` = sprintf("%.1f to %.1f", draw_low, draw_high),
  `Same sign (%)` = round(100 * original_sign_share, 1), `Drop uncertain pairs: effect (%)` = round(drop_effect, 1),
  `Drop: p-value` = round(drop_p, 3), `Observations retained (%)` = round(100 * drop_n / baseline_n, 1))]
full_summary <- summary[, .(Outcome = outcome, Method = method, `Retention below` = threshold, `Original effect (%)` = round(baseline_effect, 2),
  `Draw median (%)` = round(draw_median, 2), `Score-only range (%)` = sprintf("%.2f to %.2f", draw_low, draw_high),
  `Affected pairs` = selected_pairs, `Drop: effect (%)` = round(drop_effect, 2), `Drop: p` = round(drop_p, 3),
  `Reverse: effect (%)` = round(flip_effect, 2), `Reverse: p` = round(flip_p, 3))]
alderman_table <- raw_aldermen[order(cutoff, -mean_days), .(Through = cutoff, Alderman = alderman, Wards = wards, Permits = n_permits,
  `Average days` = round(mean_days, 1), `Median days` = median_days, `Geometric mean days` = round(geometric_mean_days, 1), `Score` = round(score, 3))]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Score robustness: evidence and proposed additions</title><style>body{font:16px/1.5 system-ui;max-width:1300px;margin:35px auto;padding:0 20px;color:#183445}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:13px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{width:100%}.wide{overflow-x:auto}blockquote{border-left:3px solid #34708d;margin:20px 0;padding:5px 20px}a{color:#176184}</style>',
  '<h1>Score robustness: evidence and proposed appendix additions</h1><p>This is a review draft. No manuscript wording or exhibits have been changed.</p>',
  '<h2>Raw processing days versus the adjusted score</h2><p>Average days means the arithmetic mean of application-to-issuance time, with every permit weighted equally. The comparison uses exactly the permits entering the score regression: 71,059 through 2014 and 120,505 through 2022. The local pairs and observation counts are the recorded fitted samples. The event-study denominator counts reassigned blocks; the other rows count construction projects, sales or rental observations. These counts are not regression influence weights.</p>',
  html_table(raw_table), '<p>The raw comparison does not adjust for permit mix, calendar time, location or ward characteristics. Agreement measures how much the adjustment changes ordering; it is not independent validation of alderman behavior. Median and mean-log comparisons below distinguish sensitivity to long delays and the logarithmic transformation. Ties remain in the denominator and are reported separately.</p>',
  '<img src="raw_processing_comparison.png" alt="Raw processing days and the adjusted score">', html_table(correlation_table),
  '<details><summary>All measures and local pair counts</summary><div class="wide">', html_table(all_raw), '</div></details>',
  '<h2>Large local disagreements</h2><p>These are the five reversed pairs representing the most observations in each sample. The order of names in each pair matches the two raw-day columns.</p>', html_table(example_table),
  '<h2>Proposed appendix table: uncertainty in scores and estimated effects</h2><p>This compact view uses annual reweighting and the 75% retention cutoff consistently for all five outcomes. The complete property, quarter and annual results and both cutoffs remain available below. The permit effect is the pooled signed post-remap estimate; other effects compare the two 100-foot bands across the boundary. Score-draw ranges hold observed outcomes fixed and are not full confidence intervals. The conditional p-values treat each fitted sample and ordering as fixed.</p>',
  '<div class="wide">', html_table(preview), '</div>',
  '<details><summary>All methods, both cutoffs, and deliberate reversals</summary><div class="wide">', html_table(full_summary), '</div></details>',
  '<h2>Proposed text for review</h2><p><strong>Main-text addition after score construction:</strong></p>',
  '<blockquote>I assess how uncertainty in the estimated scores changes the local comparisons used in the analysis. Appendix B reports the stability of these orderings across reweighted permit samples and the sensitivity of the outcome estimates to the resulting changes in ordering.</blockquote>',
  '<p><strong>Appendix addition:</strong></p><blockquote>I repeatedly re-estimate both stages of the stringency score, assigning common weights to permits on the same recorded property, in the same calendar quarter, or in the same calendar year. Each draw produces a citywide ordering of aldermen, which I use to re-estimate the outcome models while holding the observed outcomes and covariates fixed. I also omit comparisons whose original ordering is retained in fewer than 75% of draws, and report a broader 95% cutoff. These exercises describe sensitivity to the estimated scores; the score-draw ranges capture variation from the scores alone.</blockquote>',
  '<blockquote>The permitting, sale-price and rental estimates retain their original signs in at least 98.8% of annual draws. The corresponding shares are 92.4% for multifamily FAR and 81.4% for multifamily DUPAC. Omitting the less stable comparisons leaves both multifamily point estimates negative but reduces the DUPAC difference from 17.4% to 7.7%; both density estimates become imprecise. The original rental estimate also remains statistically imprecise. The results therefore differ in their sensitivity to uncertainty in local ordering.</blockquote>',
  '<p><strong>Associated wording for review:</strong> Appendix B currently calls the fixed effect “the persistent alderman-specific difference in adjusted processing time.” I recommend “the alderman-specific difference in adjusted processing time.” The local resampling checks do not establish persistence over time.</p>',
  '<p><strong>Recommendation:</strong> Add the local-ordering and outcome-sensitivity evidence to Appendix B, with a short main-text pointer. Present raw-time agreement as a descriptive comparison of adjusted and unadjusted measures. Retain unfavorable grouped reversals and the earlier chronological split in the research record and supplementary results; avoid a blanket claim of stability over time. The chronological split examines a different question from reweighting permits across the same period. Formal rank confidence sets, as in <a href="https://home.uchicago.edu/amshaikh/webfiles/rankingsconf.pdf">Mogstad and coauthors</a>, are a further inference step; these perturbation frequencies are not such confidence sets.</p>',
  '<details><summary>Raw processing times and score for every alderman</summary>', html_table(alderman_table), '</details>',
  '<p>Downloads: <a href="raw_processing_by_alderman.csv">every alderman</a>, <a href="raw_processing_pairs.csv">every pair</a>, <a href="raw_processing_agreement.csv">agreement counts</a>, <a href="robustness_summary.csv">combined robustness results</a>. <a href="../README.md">Sources and definitions</a>.</p>'), '../output/score_robustness_review.html')
