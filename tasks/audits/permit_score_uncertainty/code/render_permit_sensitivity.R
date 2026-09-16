# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_score_uncertainty/code")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")

draws <- fread("../output/permit_score_draws.csv")
results <- fread("../output/permit_pair_sensitivity.csv")
pairs <- fread("../output/permit_pairs.csv")
stopifnot(!anyDuplicated(draws[, .(method, draw, specification, term)]), !anyDuplicated(pairs$pair))
baseline <- results[scenario == "baseline"]
summary <- draws[, .(draws = .N, median_estimate = median(estimate), mean_estimate = mean(estimate),
  score_only_sd = sd(estimate), low = quantile(estimate, .025), high = quantile(estimate, .975),
  negative_share = mean(estimate < 0), median_reversed_blocks = as.numeric(median(reversed_blocks)),
  median_reversed_pairs = as.numeric(median(reversed_pairs))), by = .(method, specification, term)]
summary[, `:=`(percent_median = 100 * expm1(median_estimate), percent_low = 100 * expm1(low), percent_high = 100 * expm1(high))]
SaveData(summary, c("method", "specification", "term"), "../output/permit_score_summary.csv")
individual <- results[scenario %in% c("drop_one", "flip_one") & specification == "pooled"]
stopifnot(!anyDuplicated(individual[, .(scenario, pair)]))
individual <- merge(individual, pairs, by = "pair", all.x = TRUE, sort = FALSE, suffixes = c("", "_pair"))
individual[, change_percentage_points := percent_effect - baseline[specification == "pooled", percent_effect]]
SaveData(individual, c("scenario", "pair"), "../output/permit_pair_influence.csv")

# Add the omitted 2014 reference only for display; it is zero by normalization.
event_summary <- summary[specification == "event"]
event_summary[, event_time := as.integer(sub(":direction", "", sub("relative_year::", "", term)))]
event_summary <- rbind(event_summary, data.table(method = unique(event_summary$method), event_time = -1L,
  median_estimate = 0, low = 0, high = 0), fill = TRUE)
method_names <- c(property = "Reweight properties", quarter = "Reweight quarters", year = "Reweight years")
event_summary[, reweighting := factor(method_names[method], levels = unname(method_names))]
baseline_events <- baseline[specification == "event"]
baseline_events[, event_time := as.integer(sub(":direction", "", sub("relative_year::", "", term)))]
baseline_events <- rbind(baseline_events, data.table(event_time = -1L, estimate = 0), fill = TRUE)
plot <- ggplot(event_summary, aes(event_time, median_estimate)) +
  geom_hline(yintercept = 0, color = "grey65") + geom_vline(xintercept = -.5, color = "grey65", linetype = "dotted") +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "#34708d", alpha = .2) +
  geom_line(color = "#34708d", linewidth = .8) + geom_point(color = "#34708d", size = 1.4) +
  geom_line(data = baseline_events, aes(y = estimate), color = "#ae5e3a", linetype = "dashed", linewidth = .8) +
  facet_wrap(~reweighting, nrow = 1) + scale_x_continuous(breaks = -5:5) +
  labs(title = "Permit event study when the pre-remap scores change",
    subtitle = "Blue: median and middle 95% of score draws at each year. Dashed brown: paper's score.",
    x = "Years since the 2015 remap", y = "Effect on annual permits (log points)",
    caption = "Permit outcomes stay fixed. Shading shows score uncertainty only, separately at each year; it is not a full or simultaneous confidence band.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
ggsave("../output/permit_score_events.pdf", plot, width = 12, height = 4.6, bg = "white")
ggsave("../output/permit_score_events.png", plot, width = 12, height = 4.6, dpi = 150, bg = "white")

stress <- results[method == "year" & specification == "event"]
stress[, event_time := as.integer(sub(":direction", "", sub("relative_year::", "", term)))]
reference <- unique(stress[, .(scenario, threshold)])[, `:=`(event_time = -1L, estimate = 0)]
stress <- rbind(stress, reference, fill = TRUE)
scenario_names <- c(baseline = "Original estimate", drop_fragile = "Drop uncertain pairs", flip_fragile = "Reverse uncertain pairs")
stress[, test := scenario_names[scenario]]
stress[, retention := paste0("Original ordering retained in fewer than ", 100 * threshold, "% of annual draws")]
original <- rbindlist(lapply(unique(stress$retention), function(label) copy(baseline_events)[,
  `:=`(retention = label, test = "Original estimate")]))
plot <- ggplot(stress, aes(event_time, estimate, color = test)) +
  geom_hline(yintercept = 0, color = "grey65") + geom_vline(xintercept = -.5, color = "grey65", linetype = "dotted") +
  geom_line(linewidth = .8) + geom_point(size = 1.6) + geom_line(data = original, linewidth = .8) +
  facet_wrap(~retention, nrow = 1) + scale_x_continuous(breaks = -5:5) +
  scale_color_manual(values = c("Original estimate" = "#183445", "Drop uncertain pairs" = "#247967", "Reverse uncertain pairs" = "#ae5e3a")) +
  labs(title = "Remove or reverse uncertain comparisons in the permit event study", x = "Years since the 2015 remap",
    y = "Effect on annual permits (log points)", color = NULL,
    caption = "Dropping removes each pair's reassigned and unchanged blocks. Reversing changes only treatment direction. Lines show point estimates.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", plot.caption = element_text(size = 8))
ggsave("../output/permit_fragile_events.pdf", plot, width = 12, height = 4.6, bg = "white")
ggsave("../output/permit_fragile_events.png", plot, width = 12, height = 4.6, dpi = 150, bg = "white")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(row)
      paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
pooled <- baseline[specification == "pooled"]
baseline_table <- pooled[, .(Blocks = blocks, `Reassigned blocks` = reassigned_blocks, `Block-years` = n,
  `Pooled effect (%)` = round(percent_effect, 1), `Conditional p-value` = round(p_value, 3), `Pretrend p-value` = round(pretrend_p_value, 3))]
draw_table <- summary[specification == "pooled", .(Method = method_names[method], `Median effect (%)` = round(percent_median, 1),
  `Middle 95% of draws (%)` = sprintf("%.1f to %.1f", percent_low, percent_high), `Negative estimates (%)` = round(100 * negative_share, 1))]
scenario_table <- results[scenario %in% c("drop_fragile", "flip_fragile") & specification == "pooled"][order(method, threshold, scenario),
  .(Method = method, Test = scenario_names[scenario], `Retention below` = threshold,
    `Affected pairs` = selected_pairs, `Affected reassigned blocks` = selected_reassigned_blocks,
    `Blocks fitted` = blocks, `Pooled effect (%)` = round(percent_effect, 1),
    `Conditional 95% CI (%)` = sprintf("%.1f to %.1f", percent_low, percent_high),
    `Conditional p-value` = round(p_value, 3), `Pretrend p-value` = round(pretrend_p_value, 3))]
importance <- individual[, .(largest_change = max(abs(change_log_coefficient))), by = pair][order(-largest_change)]
top <- copy(pairs[match(head(importance$pair, 10), pair)])
for (action in c("drop_one", "flip_one")) {
  d <- individual[scenario == action]
  top[, (action) := d$percent_effect[match(pair, d$pair)]]
}
top_table <- top[, .(Pair = pair, Wards = ward_pair_id, `Reassigned blocks` = reassigned_blocks,
  `Annual order retention (%)` = round(100 * year_retention, 1), `Drop: effect (%)` = round(drop_one, 1), `Reverse: effect (%)` = round(flip_one, 1))]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Permit event study and score uncertainty</title><style>body{font:16px/1.5 system-ui;max-width:1300px;margin:35px auto;padding:0 20px;color:#183445}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:13px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{width:100%}.wide{overflow-x:auto}a{color:#176184}</style>',
  '<h1>Does score uncertainty change the permit event study?</h1>',
  '<p>The main high-discretion specification uses the 2010–2020 block panel, stable incumbents, a 500-foot distance limit, block and boundary-pair-by-year fixed effects, and boundary-pair clustering. It uses only pre-remap permits to estimate the scores. The pooled signed effect combines both reassignment directions under the paper\'s equal-and-opposite restriction.</p>',
  html_table(baseline_table), '<h2>Carry complete score draws through the event study</h2>',
  '<p>Each of ', unique(summary$draws), ' draws per method re-estimates the paper\'s two-stage scores through 2014 and supplies one citywide ordering. The permit regressions then keep all observed outcomes, blocks and years fixed. The ranges and negative shares below describe score sensitivity, not full confidence intervals or probabilities that the true causal effect is negative. Annual reweighting has nine pre-remap time blocks.</p>',
  html_table(draw_table), '<img src="permit_score_events.png" alt="Permit event paths under score reweighting">',
  '<h2>Remove or reverse uncertain pairs together</h2><p>Thresholds are the same 75% and 95% order-retention cutoffs used for the multifamily audit. Dropping removes each selected pair\'s reassigned blocks and its unchanged comparison blocks. Reversing keeps those blocks and flips treatment direction; unchanged blocks remain unchanged. Deliberately reversing selected pairs may conflict with a single citywide ranking. These are stress tests, and dropping changes the sample. Conditional intervals and p-values treat the tested sample and ranking as fixed.</p>',
  '<div class="wide">', html_table(scenario_table[Method == "year"]), '</div>',
  '<img src="permit_fragile_events.png" alt="Permit event study after dropping or reversing fragile pairs">',
  '<details><summary>Property and quarter classifications</summary><div class="wide">', html_table(scenario_table[Method != "year"]), '</div></details>',
  '<h2>Change each informative pair separately</h2><p>All ', nrow(pairs), ' pairs with reassigned blocks are dropped once and reversed once. The ten largest absolute changes in the pooled log coefficient are below. These are sensitivity checks, not additive contributions to the Poisson estimate. Every annual coefficient and pretrend test is retained in the downloadable results.</p>',
  html_table(top_table), '<p>Download <a href="permit_pair_influence.csv">all one-pair pooled results</a>, <a href="permit_pair_sensitivity.csv">all pooled and annual stress tests</a>, or <a href="permit_score_draws.csv">all score-draw refits</a>. <a href="../README.md">Methods and verification</a>. Production scores, data and manuscript are unchanged.</p>'), '../output/permit_score_sensitivity.html')
