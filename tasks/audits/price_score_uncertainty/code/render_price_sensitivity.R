# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/price_score_uncertainty/code")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")

draws <- rbind(fread("../output/sales_score_draws.csv"), fread("../output/rent_score_draws.csv"))
results <- rbind(fread("../output/sales_pair_sensitivity.csv"), fread("../output/rent_pair_sensitivity.csv"))
pairs <- rbind(fread("../output/sales_pairs.csv"), fread("../output/rent_pairs.csv"))
stopifnot(!anyDuplicated(draws[, .(market, method, draw)]), !anyDuplicated(pairs[, .(market, pair)]))
baseline <- results[scenario == "baseline"]
summary <- draws[, .(draws = .N, median_estimate = median(estimate), mean_estimate = mean(estimate), score_only_sd = sd(estimate),
  low = quantile(estimate, .025), high = quantile(estimate, .975), positive_share = mean(estimate > 0),
  median_reversed_records = as.numeric(median(reversed_records)), median_reversed_pairs = as.numeric(median(reversed_pairs))), by = .(market, method)]
summary[, `:=`(percent_median = 100 * expm1(median_estimate), percent_low = 100 * expm1(low), percent_high = 100 * expm1(high))]
SaveData(summary, c("market", "method"), "../output/price_score_summary.csv")
individual <- results[scenario %in% c("drop_one", "flip_one")]
stopifnot(!anyDuplicated(individual[, .(market, scenario, pair)]))
individual <- merge(individual, pairs, by = c("market", "pair"), all.x = TRUE, sort = FALSE)
individual[, change_percentage_points := percent_effect - baseline$percent_effect[match(market, baseline$market)]]
SaveData(individual, c("market", "scenario", "pair"), "../output/price_pair_influence.csv")

market_names <- c(sales = "Sale prices", rent = "Rents")
method_names <- c(property = "Reweight properties", quarter = "Reweight quarters", year = "Reweight years")
summary[, `:=`(outcome = factor(market_names[market], levels = unname(market_names)), reweighting = method_names[method])]
baseline[, outcome := factor(market_names[market], levels = unname(market_names))]
plot <- ggplot(summary, aes(percent_median, factor(reweighting, levels = rev(unname(method_names))))) +
  geom_vline(xintercept = 0, color = "grey65") +
  geom_vline(data = baseline, aes(xintercept = percent_effect), color = "#ae5e3a", linetype = "dashed") +
  geom_linerange(aes(xmin = percent_low, xmax = percent_high), color = "#34708d", linewidth = 1) +
  geom_point(color = "#16465e", size = 2.8) + facet_wrap(~outcome, ncol = 2) +
  labs(title = "Sale-price and rent estimates when the scores change",
    subtitle = "Points: median estimate. Bars: middle 95% of score draws. Dashed line: estimate with the paper's score.",
    x = "Price difference on the more-stringent side (%)", y = NULL,
    caption = "Observed prices, controls and locations stay fixed. These ranges include score uncertainty only; they are not full confidence intervals.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
ggsave("../output/price_score_draws.pdf", plot, width = 11, height = 4.3, bg = "white")
ggsave("../output/price_score_draws.png", plot, width = 11, height = 4.3, dpi = 150, bg = "white")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(row)
      paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
baseline_table <- baseline[, .(Outcome = outcome, Observations = n, Pairs = alderman_pairs, `Effect (%)` = round(percent_effect, 2),
  `Conditional 95% CI (%)` = sprintf("%.2f to %.2f", percent_low, percent_high), `Conditional p-value` = round(p_value, 3))]
draw_table <- summary[, .(Outcome = outcome, Reweighting = reweighting, `Median effect (%)` = round(percent_median, 2),
  `Middle 95% of draws (%)` = sprintf("%.2f to %.2f", percent_low, percent_high), `Positive estimates (%)` = round(100 * positive_share, 1))]
scenario_names <- c(drop_fragile = "Drop uncertain pairs", flip_fragile = "Reverse uncertain pairs")
scenario_table <- results[scenario %in% names(scenario_names)][order(method, threshold, scenario, market),
  .(Outcome = market_names[market], Method = method, Test = scenario_names[scenario], `Retention below` = threshold,
    `Affected pairs` = selected_pairs, `Affected observations` = selected_records, `Observations fitted` = n,
    `Effect (%)` = round(percent_effect, 2), `Conditional 95% CI (%)` = sprintf("%.2f to %.2f", percent_low, percent_high),
    `Conditional p-value` = round(p_value, 3))]
importance <- individual[, .(largest_change = max(abs(change_log_coefficient))), by = .(market, pair)][order(market, -largest_change)]
top_keys <- importance[, head(.SD, 10), by = market][, .(market, pair)]
top <- merge(top_keys, pairs, by = c("market", "pair"), all.x = TRUE, sort = FALSE)
for (action in c("drop_one", "flip_one")) {
  selected <- individual[scenario == action, .(market, pair, percent_effect)]
  setnames(selected, "percent_effect", action)
  stopifnot(!anyDuplicated(selected[, .(market, pair)]))
  top <- merge(top, selected, by = c("market", "pair"), all.x = TRUE, sort = FALSE)
}
top_table <- top[, .(Outcome = market_names[market], Pair = pair, Observations = records,
  `Annual order retention (%)` = round(100 * year_retention, 1), `Drop: effect (%)` = round(drop_one, 2), `Reverse: effect (%)` = round(flip_one, 2))]
one_pair_summary <- individual[, .(Tests = .N, `Smallest effect (%)` = round(min(percent_effect), 2),
  `Largest effect (%)` = round(max(percent_effect), 2)), by = .(Outcome = market_names[market], Test = scenario)]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Sale prices, rents and score uncertainty</title><style>body{font:16px/1.5 system-ui;max-width:1250px;margin:35px auto;padding:0 20px;color:#183445}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:13px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{width:100%}.wide{overflow-x:auto}a{color:#176184}</style>',
  '<h1>Do uncertain alderman comparisons change prices and rents?</h1>',
  '<p>These are the paper\'s boundary comparisons within 100 feet on each side, estimated with ten distance bins inside 500 feet. Sales use 2006–2022 transactions and segment-by-quarter fixed effects; rents use 2014–2022 observations and segment-by-month fixed effects. Each keeps its existing property characteristics, amenity controls, property-type indicators and ward-pair clustering. Rental observations can repeat for the same home over time.</p>',
  '<h2>Original estimates</h2>', html_table(baseline_table),
  '<h2>Carry each complete score draw through both regressions</h2><p>Each of ', unique(summary$draws), ' draws per method supplies one citywide score vector from the existing two-stage estimator. That determines the more-stringent side of every boundary. Observed prices, controls, locations and the fitted sample stay fixed. The ranges and positive shares describe score sensitivity, not full confidence intervals or probabilities that the causal effect is positive.</p>',
  '<img src="price_score_draws.png" alt="Sale-price and rent estimates across score draws">', html_table(draw_table),
  '<h2>Drop or reverse uncertain pairs together</h2><p>The same 75% and 95% retention cutoffs as the density and permit audits select the pairs, using score draws only. Each selected pair is dropped in full or reversed in full. Dropping changes the sample; additional fixed-effect singletons may then be omitted by the original estimator. Reversing selected pairs is a deliberate stress test that can conflict with a single citywide ranking. Conditional intervals and p-values treat the chosen sample and ordering as fixed.</p>',
  '<div class="wide">', html_table(scenario_table[Method == "year"]), '</div>',
  '<details><summary>Property and quarter classifications</summary><div class="wide">', html_table(scenario_table[Method != "year"]), '</div></details>',
  '<h2>Change one pair at a time</h2><p>Every observed pair is dropped once and reversed once, including stable pairs. The complete range is:</p>', html_table(one_pair_summary),
  '<p>The ten largest absolute log-coefficient changes for each market are below. Cells show the resulting price effect, not additive contributions.</p><div class="wide">', html_table(top_table), '</div>',
  '<p>Download <a href="price_pair_influence.csv">every individual-pair result</a>, <a href="sales_pair_sensitivity.csv">all sales stress tests</a>, <a href="rent_pair_sensitivity.csv">all rent stress tests</a>, <a href="sales_score_draws.csv">every sales draw</a>, or <a href="rent_score_draws.csv">every rent draw</a>. <a href="../README.md">Methods and verification</a>. Production data, scores and manuscript are unchanged.</p>'), '../output/price_score_sensitivity.html')
