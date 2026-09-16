# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/multifamily_score_uncertainty/code")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")

draws <- fread("../output/multifamily_score_draws.csv")
results <- fread("../output/multifamily_pair_sensitivity.csv")
pairs <- fread("../output/multifamily_pairs.csv")
stopifnot(!anyDuplicated(draws[, .(method, draw, outcome)]), !anyDuplicated(pairs$pair))
baseline <- results[scenario == "baseline"]
summary <- draws[, .(draws = .N, median_estimate = median(estimate), mean_estimate = mean(estimate),
  score_only_sd = sd(estimate), low = quantile(estimate, .025), high = quantile(estimate, .975),
  negative_share = mean(estimate < 0), median_switched_buildings = as.numeric(median(switched_buildings)),
  median_switched_pairs = as.numeric(median(switched_pairs))), by = .(method, outcome)]
summary[, `:=`(percent_median = 100 * expm1(median_estimate), percent_low = 100 * expm1(low),
  percent_high = 100 * expm1(high))]
SaveData(summary, c("method", "outcome"), "../output/multifamily_score_summary.csv")

individual <- results[scenario %in% c("drop_one", "flip_one")]
stopifnot(!anyDuplicated(individual[, .(scenario, pair, outcome)]))
individual <- merge(individual, pairs, by = "pair", all.x = TRUE, sort = FALSE)
individual[, change_percentage_points := percent_effect - baseline$percent_effect[match(outcome, baseline$outcome)]]
SaveData(individual, c("scenario", "pair", "outcome"), "../output/multifamily_pair_influence.csv")

outcome_names <- c(density_far = "FAR", density_dupac = "DUPAC")
method_names <- c(property = "Reweight properties", quarter = "Reweight quarters", year = "Reweight years")
summary[, `:=`(metric = outcome_names[outcome], reweighting = method_names[method])]
baseline[, metric := outcome_names[outcome]]
plot <- ggplot(summary, aes(percent_median, factor(reweighting, levels = rev(unname(method_names))))) +
  geom_vline(xintercept = 0, color = "grey65") +
  geom_vline(data = baseline, aes(xintercept = percent_effect), color = "#ae5e3a", linetype = "dashed") +
  geom_linerange(aes(xmin = percent_low, xmax = percent_high), color = "#34708d", linewidth = 1) +
  geom_point(color = "#16465e", size = 2.8) + facet_wrap(~metric, ncol = 2) +
  labs(title = "Multifamily density estimates when the scores change",
    subtitle = "Points: median estimate. Bars: middle 95% of score draws. Dashed line: estimate with the paper's score.",
    x = "Density difference on the more-stringent side (%)", y = NULL,
    caption = "Buildings, outcomes and locations stay fixed. These ranges include score uncertainty only; they are not full confidence intervals.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
ggsave("../output/multifamily_score_draws.pdf", plot, width = 11, height = 4.3, bg = "white")
ggsave("../output/multifamily_score_draws.png", plot, width = 11, height = 4.3, dpi = 150, bg = "white")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(row)
      paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
draw_table <- summary[, .(Reweighting = reweighting, Outcome = metric,
  `Median effect (%)` = round(percent_median, 1),
  `Middle 95% of draws (%)` = sprintf("%.1f to %.1f", percent_low, percent_high),
  `Negative estimates (%)` = round(100 * negative_share, 1))]
baseline_table <- baseline[, .(Outcome = metric, Buildings = n, `Effect (%)` = round(percent_effect, 1),
  `Conditional 95% CI (%)` = sprintf("%.1f to %.1f", percent_low, percent_high), `Conditional p-value` = round(p_value, 3))]
scenario_names <- c(drop_fragile = "Drop fragile pairs", flip_fragile = "Reverse fragile pairs")
scenario_table <- results[scenario %in% names(scenario_names)][order(method, threshold, scenario, outcome),
  .(Method = method, Test = scenario_names[scenario], `Order retention below` = threshold,
    Outcome = outcome_names[outcome], `Affected pairs` = selected_pairs, `Affected buildings` = selected_buildings,
    `Buildings fitted` = n, `Effect (%)` = round(percent_effect, 1),
    `Conditional 95% CI (%)` = sprintf("%.1f to %.1f", percent_low, percent_high), `Conditional p-value` = round(p_value, 3))]

# Display the ten largest one-pair changes; the complete table includes every pair.
importance <- individual[, .(largest_change = max(abs(change_log_coefficient))), by = pair][order(-largest_change)]
top <- importance$pair[seq_len(min(10L, nrow(importance)))]
top_table <- copy(pairs[match(top, pair)])
for (action in c("drop_one", "flip_one")) for (outcome_name in names(outcome_names)) {
  s <- individual[scenario == action & outcome == outcome_name]
  top_table[, paste(action, outcome_names[outcome_name]) := round(s$percent_effect[match(pair, s$pair)], 1)]
}
top_table <- top_table[, .(Pair = pair, Buildings = buildings,
  `Annual order retention (%)` = round(100 * year_retention, 1),
  `Drop: FAR (%)` = `drop_one FAR`, `Reverse: FAR (%)` = `flip_one FAR`,
  `Drop: DUPAC (%)` = `drop_one DUPAC`, `Reverse: DUPAC (%)` = `flip_one DUPAC`)]
one_pair_summary <- individual[, .(Tests = .N, `Smallest effect (%)` = round(min(percent_effect), 1),
  `Largest effect (%)` = round(max(percent_effect), 1)), by = .(Test = scenario, Outcome = outcome_names[outcome])]

writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Multifamily density and score uncertainty</title><style>body{font:16px/1.5 system-ui;max-width:1250px;margin:35px auto;padding:0 20px;color:#183445}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:13px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{width:100%}.wide{overflow-x:auto}a{color:#176184}</style>',
  '<h1>Does uncertainty about aldermen change the multifamily result?</h1>',
  '<p>The original sample contains ', unique(baseline$n), ' multifamily buildings and ', unique(baseline$alderman_pairs),
  ' alderman comparisons. All tests use the original 500-foot window, ten distance bins, controls, fixed effects and ward-pair clustering. The reported effect compares buildings within 100 feet on the two sides of the boundary.</p>',
  '<h2>Original estimates</h2>', html_table(baseline_table),
  '<h2>Carry each score draw through the regression</h2>',
  '<p>Each of the ', unique(summary$draws), ' draws per method supplies one complete citywide score vector, using the paper\'s existing two-stage method. That vector determines which side is more stringent at every boundary. Buildings, density measurements, controls and geographic assignments remain fixed. The ranges below describe variation from the score alone and must not be read as full confidence intervals combining score and outcome uncertainty.</p>',
  '<img src="multifamily_score_draws.png" alt="Multifamily estimates across score draws">', html_table(draw_table),
  '<h2>Drop or reverse the uncertain comparisons together</h2>',
  '<p>Pairs are selected solely because their original ordering survives fewer than the stated fraction of score draws. Thresholds were set before examining density effects. Dropping them changes the sample. Reversing them keeps the buildings and flips their signed distances. These deliberate local reversals can be inconsistent with a single citywide ranking: they are stress tests, not an alternative estimated score. Conditional intervals and p-values below treat each tested sample and ordering as fixed.</p>',
  '<div class="wide">', html_table(scenario_table[Method == "year"]), '</div>',
  '<details><summary>Show the same tests using property and quarter reweighting</summary><div class="wide">',
  html_table(scenario_table[Method != "year"]), '</div></details>',
  '<h2>Change one pair at a time</h2>',
  '<p>Every pair is dropped once and reversed once, regardless of its score stability. These estimates are not additive pair contributions. The range across all one-pair tests is:</p>', html_table(one_pair_summary),
  '<p>The ten largest changes in either log coefficient are shown below, together with their annual score stability. Each cell is the resulting density effect, not the change from the original effect.</p>',
  '<div class="wide">', html_table(top_table), '</div>',
  '<p>Download <a href="multifamily_pair_influence.csv">every one-pair result</a>, <a href="multifamily_pair_sensitivity.csv">all stress tests</a>, or <a href="multifamily_score_draws.csv">every score-draw estimate</a>. <a href="../README.md">Methods and verification</a>. The production scores and manuscript are unchanged.</p>'), "../output/multifamily_score_sensitivity.html")
