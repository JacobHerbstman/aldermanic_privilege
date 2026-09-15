# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/raw_log_score_sensitivity/code")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")
boundary <- rbindlist(list(fread("../output/density_all_results.csv"), fread("../output/density_multifamily_results.csv"),
  fread("../output/sales_results.csv"), fread("../output/rent_results.csv")))
permits <- fread("../output/permit_results.csv")
summary <- rbindlist(list(boundary[, .(analysis, outcome, scenario, estimate, std_error, p_value, ci_low, ci_high,
    percent_effect, percent_low, percent_high, input_n, n, ward_pairs, selected_pairs, selected_observations)],
  permits[specification == "pooled", .(analysis, outcome, scenario, estimate, std_error, p_value, ci_low, ci_high,
    percent_effect, percent_low, percent_high, input_n, n, ward_pairs, selected_pairs,
    selected_observations = selected_reassigned_blocks)]))
summary[, label := fcase(analysis == "permit_remap", "Permits", analysis == "sales", "Sale prices", analysis == "rent", "Rents",
  analysis == "density_all" & outcome == "density_far", "All construction: FAR",
  analysis == "density_all" & outcome == "density_dupac", "All construction: DUPAC",
  analysis == "density_multifamily" & outcome == "density_far", "Multifamily: FAR",
  analysis == "density_multifamily" & outcome == "density_dupac", "Multifamily: DUPAC")]
labels <- c("Permits", "All construction: FAR", "All construction: DUPAC", "Multifamily: FAR", "Multifamily: DUPAC", "Sale prices", "Rents")
summary[, display_order := match(label, labels)]
setorder(summary, display_order, scenario)
stopifnot(nrow(summary) == 21L, !anyNA(summary), !anyDuplicated(summary[, .(analysis, outcome, scenario)]))
SaveData(summary, c("analysis", "outcome", "scenario"), "../output/raw_log_summary.csv")

scenario_labels <- c(baseline = "Original score", drop_disagreements = "Drop disagreements",
  reverse_disagreements = "Order by raw log time")
summary[, `:=`(comparison = factor(scenario, levels = names(scenario_labels), labels = scenario_labels),
  label = factor(label, levels = labels))]
plot <- ggplot(summary, aes(percent_effect, comparison, color = comparison)) +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_segment(aes(x = percent_low, xend = percent_high, yend = comparison), linewidth = .7) + geom_point(size = 2) +
  facet_wrap(~label, ncol = 2, scales = "free_x") + scale_y_discrete(limits = rev(scenario_labels)) +
  scale_color_manual(values = c("#1b4c72", "#c07b19", "#9b333e"), guide = "none") +
  labs(title = "Results under adjusted-score and raw-log-time orderings", x = "Estimated difference (%)", y = NULL,
    caption = "Intervals are 95% confidence intervals conditional on each sample and ordering.\nBoundary estimates compare the two 100-foot bands; the permit estimate pools 2015-2020.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("../output/raw_log_effects.pdf", plot, width = 12, height = 9, bg = "white")
ggsave("../output/raw_log_effects.png", plot, width = 12, height = 9, dpi = 150, bg = "white")

events <- permits[specification == "event"]
events[, event_year := as.integer(sub("relative_year::(-?[0-9]+):direction", "\\1", term))]
reference <- copy(events[, .SD[1], by = scenario])
reference[, `:=`(event_year = -1L, percent_effect = 0, percent_low = 0, percent_high = 0)]
events <- rbind(events, reference)
events[, comparison := factor(scenario, levels = names(scenario_labels), labels = scenario_labels)]
event_plot <- ggplot(events, aes(event_year, percent_effect)) + geom_hline(yintercept = 0, color = "gray60") +
  geom_vline(xintercept = -.5, linetype = "dashed", color = "gray60") +
  geom_errorbar(aes(ymin = percent_low, ymax = percent_high), width = .15, color = "#1b4c72") +
  geom_line(color = "#1b4c72") + geom_point(color = "#1b4c72") + facet_wrap(~comparison, nrow = 1) +
  scale_x_continuous(breaks = -5:5) + labs(title = "Permit event study under alternative local orderings",
    x = "Years relative to the 2015 remap", y = "Estimated difference (%)",
    caption = "The year before the remap is the reference. Intervals condition on the ordering and sample.") + theme_minimal(base_size = 10)
ggsave("../output/raw_log_permit_events.pdf", event_plot, width = 12, height = 4.5, bg = "white")
ggsave("../output/raw_log_permit_events.png", event_plot, width = 12, height = 4.5, dpi = 150, bg = "white")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(apply(as.data.frame(d), 1, function(row) paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
summary[, effect_p := sprintf("%+.2f%% (p = %.3f)", percent_effect, p_value)]
table <- dcast(summary, label ~ comparison, value.var = "effect_p")
setnames(table, "label", "Outcome")
samples <- dcast(summary, label ~ comparison, value.var = "n")
setnames(samples, "label", "Outcome")
detail <- summary[, .(Outcome = label, Comparison = comparison, `Effect (%)` = round(percent_effect, 3),
  `95% interval (%)` = sprintf("%.2f to %.2f", percent_low, percent_high), `p-value` = signif(p_value, 4),
  Observations = n, `Input observations` = input_n, `Ward pairs` = ward_pairs, `Disagreeing pairs acted on` = selected_pairs,
  `Affected observations (reassigned blocks for permits)` = selected_observations)]
pretrends <- permits[specification == "pooled", .(Comparison = scenario_labels[scenario],
  `Pretrend joint p-value` = round(pretrend_p_value, 3), Blocks = blocks, `Reassigned blocks` = reassigned_blocks)]
before <- rbindlist(list(fread("../records/density_all_before_corrections.csv"),
  fread("../records/density_multifamily_before_corrections.csv")))
stopifnot(!anyDuplicated(before[, .(analysis, outcome, scenario)]))
changes <- merge(summary[analysis %in% c("density_all", "density_multifamily")],
  before[, .(analysis, outcome, scenario, previous_effect = percent_effect, previous_n = n)],
  by = c("analysis", "outcome", "scenario"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(changes) == 12L, !anyNA(changes$previous_effect))
setorder(changes, display_order, scenario)
changes <- changes[, .(Outcome = label, Comparison = comparison,
  `Before corrections (%)` = round(previous_effect, 2), `After corrections (%)` = round(percent_effect, 2),
  `Previous projects` = previous_n, `Current projects` = n)]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Results using raw log permit times</title><style>body{font:16px/1.5 system-ui;max-width:1200px;margin:35px auto;padding:0 20px;color:#183445}table{border-collapse:collapse;width:100%;font-size:14px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:9px}th{background:#edf3f6}img{width:100%}.wide{overflow-x:auto}a{color:#176184}</style>',
  '<h1>Results using raw log permit times</h1><p>Original score uses the paper’s adjusted ordering. Drop disagreements keeps comparisons where that ordering agrees with each alderman’s average log processing time. Order by raw log time keeps the original fitted observations and reverses every disagreement. It is exactly the result of using raw average log processing time to order all local pairs.</p>',
  '<p>Construction includes the September 15 measurement corrections. Sales, rents, permits and both score orderings use their preserved inputs. Outcome-regression controls, fixed effects, clustering and distance restrictions are unchanged. The manuscript has not been updated.</p>',
  html_table(table), '<p>Effects are 100 × (exp(coefficient) − 1). Boundary results compare the first 100 feet on each side, fitting the existing distance-bin model within 500 feet. Permit results are the pooled signed effect during 2015–2020, allowing equal-and-opposite effects of reassignment toward slower and faster processing. A positive value means a higher outcome on the side, or following reassignment toward the alderman, classified as slower by the column’s ordering.</p>',
  '<img src="raw_log_effects.png" alt="Seven outcome estimates and conditional confidence intervals">',
  '<h2>What the construction corrections changed</h2>', html_table(changes),
  '<h2>Fitted sample sizes</h2>', html_table(samples),
  '<p>Construction counts are projects, sales and rents count their recorded observations, and permit counts are block-years. Dropping a permit comparison removes both reassigned blocks and its unchanged comparison blocks. Reversal keeps unchanged blocks at direction zero. Fixed-effect estimation can remove additional observations after a deletion; the full table records both input and fitted counts.</p>',
  '<h2>Permit event-study paths</h2><img src="raw_log_permit_events.png" alt="Permit event-study estimates for each ordering">', html_table(pretrends),
  '<p>The pair selection uses processing times only, never outcome estimates. There are no mean-log ties among these pairs. Raw log times are a coherent alternative citywide ordering, so reversing every disagreement is not an arbitrary set of inconsistent pair reversals. Dropping changes the sample. These checks do not identify which ordering captures alderman behavior better. Raw times mix permit composition, calendar time and location, and both orderings are estimated from the same permits. Confidence intervals and p-values condition on the sample and ordering; they do not incorporate score-estimation uncertainty.</p>',
  '<details><summary>Complete numerical results</summary><div class="wide">', html_table(detail), '</div></details>',
  '<p><a href="raw_log_summary.csv">Download the summary</a> · <a href="permit_results.csv">All permit event-study coefficients</a> · <a href="../README.md">Sources and definitions</a></p>'), '../output/raw_log_results.html')
