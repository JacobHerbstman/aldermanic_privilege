# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/developer_score_results/code")
library(data.table)
library(ggplot2)
results <- fread("../output/developer_estimates.csv")
curves <- fread("../output/developer_profiles.csv")
coverage <- fread("../output/developer_coverage.csv")
labels <- c("density_all_density_far" = "FAR: all construction", "density_multifamily_density_far" = "FAR: multifamily",
  "density_all_density_dupac" = "DUPAC: all construction", "density_multifamily_density_dupac" = "DUPAC: multifamily",
  "rent_all_rent_price" = "Rents", "sales_all_sale_price" = "Sale prices", "permits_stable_high_discretion" = "Permits")
versions <- c(paper_full = "Original ranking: full sample", paper_common = "Original ranking: common sample",
              developer_common = "Developer ranking: common sample")
results[, label := labels[paste(market, sample, outcome, sep = "_")]]
curves[, label := labels[paste(market, sample, outcome, sep = "_")]]
main <- results[specification %in% c("boundary_100ft", "signed")]
main[, cell := sprintf("%+.1f%% (p=%.3f); N=%s", percent_effect, p_value, format(n, big.mark = ",", trim = TRUE))]
comparison <- dcast(main, label ~ version, value.var = "cell")
setcolorder(comparison, c("label", names(versions)))
setnames(comparison, c("Outcome", unname(versions)))
dev <- main[version == "developer_common", .(Outcome = label, `Effect (%)` = percent_effect,
  `Lower 95%` = percent_low, `Upper 95%` = percent_high, `p-value` = p_value, N = n)]
pretrends <- unique(curves[market == "permits", .(Ranking = versions[version], `Pretrend p-value` = pretrend_p_value)])

# Both helpers are reused to present the saved tables and plots without external assets.
table_html <- function(x) {
  escape <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", as.character(s), fixed = TRUE), fixed = TRUE)
  columns <- lapply(x, function(z) if (is.integer(z)) format(z, big.mark = ",", trim = TRUE) else
    if (is.numeric(z)) sprintf("%.3f", z) else escape(z))
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>", paste0("<td>", vapply(columns, `[`, character(1), i), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<table><tr>", paste0("<th>", escape(names(x)), "</th>", collapse = ""), "</tr>", paste(rows, collapse = ""), "</table>")
}
plot_html <- function(p, height) paste0("<div class='plot'>", svglite::stringSVG(print(p), width = 11, height = height), "</div>")
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank()))
boundary <- curves[version == "developer_common" & market != "permits"]
boundary[, label := factor(label, levels = unname(labels[1:6]))]
boundary[, side := x >= 0]
p <- ggplot(boundary, aes(x, estimate, group = side)) + geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#185c83", alpha = .15) +
  geom_line(color = "#185c83") + geom_point(color = "#185c83") +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = "Distance to boundary (feet); positive = higher developer funding", y = "Log difference",
    title = "Density and prices under the developer ranking", subtitle = "Reference: the nearest 100 feet on the lower-funding side")
events <- curves[market == "permits"]
events[, ranking := factor(versions[version], levels = unname(versions))]
p_event <- ggplot(events, aes(x, estimate)) + geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = -.5, linetype = "dashed", color = "grey60") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#185c83", alpha = .15) +
  geom_line(color = "#185c83") + geom_point(color = "#185c83") + facet_wrap(~ranking, ncol = 1) +
  scale_x_continuous(breaks = -5:5) + labs(x = "Years since the 2015 remap", y = "Permit effect (log points)",
    title = "Permit reassignment comparison", subtitle = "Developer ranking uses donations received in 2006–2014")
coverage_table <- coverage[, .(Market = market, `Original observations` = input_rows,
  `Missing funding` = missing_score_rows, `Equal shares` = tied_score_rows, `Common observations` = common_rows,
  `Changed direction` = reversed_rows, `Changed direction (%)` = 100 * reversed_rows / common_ordered_rows)]
permit_reversal <- coverage[market == "permits", sprintf(
  "For permits, the reversal percentage uses the %s reassigned blocks in the common sample; %s change direction under developer funding.",
  common_ordered_rows, reversed_rows)]
html <- c("<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Developer donations in the main specifications</title><style>body{font:17px/1.55 system-ui,sans-serif;color:#25353d;max-width:1150px;margin:36px auto;padding:0 24px}h1{line-height:1.2}h2{margin-top:36px}table{border-collapse:collapse;width:100%;font-size:14px;margin:24px 0}td,th{padding:10px;border-bottom:1px solid #d4dfe3;text-align:left}th{background:#eef4f6}.plot svg{width:100%;height:auto}.note{padding:18px;background:#eef4f6;border-left:4px solid #185c83}a{color:#185c83}</style>",
  "<h1>Developer donations in the main specifications</h1><p>Exploratory comparison · September 14, 2026 · score_robustness</p>",
  "<p class='note'>The developer score is the share of eligible campaign contribution dollars identified as developer-linked. Higher means <b>more developer funding</b>; it is not labelled more or less stringent. All results below keep that direction. Reversing the comparison reverses its interpretation.</p>",
  "<p>The original models are reestimated on their full samples, then on exactly the observations usable under the developer ranking. Controls, fixed effects, clustering, the 500-foot window, and 100-foot distance bins match the paper. Boundary effects compare the nearest 100 feet on either side. Permit estimates summarize 2015–2020 and impose equal-and-opposite reassignment effects.</p>",
  "<h2>Main estimates</h2>", table_html(comparison),
  "<p>For the developer column, negative means a lower outcome on the higher-funding side, or fewer permits following reassignment toward higher funding. Percent effects are 100 × (exp(coefficient) − 1). A large p-value does not establish a zero effect.</p>",
  "<h2>Developer estimates and uncertainty</h2>", table_html(dev), plot_html(p, 10),
  "<h2>Permit dynamics</h2>", plot_html(p_event, 10), table_html(pretrends),
  "<p>The pretrend test jointly compares the four pre-remap coefficients with zero. It uses the same clustered F test as the paper. It does not test every identifying assumption.</p>",
  "<h2>Coverage and changed ordering</h2>", table_html(coverage_table),
  paste0("<p>Density and price counts are observations; permit counts are census blocks before Poisson's automatic removals. ",
    permit_reversal, " Unchanged blocks remain comparison observations. Boundaries with missing or equal developer shares cannot be ordered. In the permit check, the entire such ward pair is removed, including its unchanged comparison blocks. The two common-sample versions use identical fitted observations, verified for every specification.</p>"),
  "<p>Boundary rankings use donations received while in office during 2006–2022; permit rankings use only 2006–2014. An observed zero developer share is retained, but missing campaign receipts are not replaced by zero. The existing documented classification is used without adding donors to improve the fit.</p>",
  "<p><a href='../README.md'>Definitions, source snapshots, and replication notes</a> · <a href='developer_estimates.csv'>All estimates, including separate reassignment directions</a> · <a href='developer_profiles.csv'>All plotted estimates</a> · <a href='developer_scores.csv'>Developer scores</a></p>",
  "<p>The manuscript and production data are unchanged.</p></html>")
writeLines(html, "../output/developer_results.html")
