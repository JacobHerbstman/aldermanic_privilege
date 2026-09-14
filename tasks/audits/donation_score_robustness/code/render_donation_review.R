# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/donation_score_robustness/code")
library(data.table)
library(ggplot2)
library(ggrepel)

measures <- fread("../output/donation_measures.csv")
correlations <- fread("../output/donation_correlations.csv")
stability <- fread("../output/donation_stability.csv")
gaps <- fread("../output/donation_rank_gaps.csv")
counts <- fread("../output/cleaning_counts.csv")
donors <- fread("../output/donor_classifications.csv")
coverage <- fread("../output/committee_coverage.csv")

labels <- c(
  union_recorded_or_name = "All union-linked gifts",
  union_direct = "Union organizations and PACs",
  union_direct_no_pending = "Unions: omit 27 pending names",
  union_individual_or_other = "Union-linked people / other",
  trades_direct = "Construction-trade organizations",
  trades_legacy = "Trades including linked people",
  education_direct = "Teachers / education unions",
  service_direct = "Service / public-sector unions",
  generic_labor_direct = "Other labor organizations",
  real_estate_strict = "Literal real-estate wording",
  developer_explicit = "Literal developer wording",
  real_estate_broad = "Broad property / development",
  nonunion_construction = "Private construction donors",
  land_use_professional = "Architects / land-use professionals",
  estate_name_evidence = "Real estate: donor-name evidence",
  development_coalition = "Real estate + construction + trades",
  real_estate_verified = "Real estate + documented firms",
  developer_verified = "Developers + documented firms",
  union_original_rules = "Union gifts: original vocabulary")
metric_labels <- c(dollar_share = "Dollar\nshare", real_dollar_share = "Real dollar\nshare",
  receipt_share = "Receipt\nshare", donor_name_share = "Donor-name\nshare",
  real_dollars_per_year = "Real dollars\nper year", share_without_largest_donor = "Remove largest\ndonor")
definition_labels <- c(personal_in_office = "Personal / in office", personal_calendar = "Personal / all dates",
  named_in_office = "Broader committees", with_inkind = "Include in-kind",
  gross_transfers = "Include own transfers", legacy_cycle = "Old election cycles")
selected <- c("union_direct", "trades_direct", "education_direct", "service_direct",
              "real_estate_verified", "developer_verified", "real_estate_broad", "development_coalition")
base <- correlations[cohort == "available" & period == "main" & definition == "personal_in_office" &
                       metric == "dollar_share" & score == "paper_score"]
base[, label := factor(labels[sector], levels = rev(unname(labels)))]

# Reused only to format the saved tables and embed self-contained scientific plots.
escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
table_html <- function(x) {
  cells <- lapply(x, function(z) if (is.numeric(z)) ifelse(is.na(z), "—", format(round(z, 3), trim = TRUE)) else ifelse(is.na(z), "—", escape(z)))
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>", paste0("<td>", vapply(cells, `[`, character(1), i), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='table'><table><thead><tr>", paste0("<th>", escape(names(x)), "</th>", collapse = ""),
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}
plot_html <- function(p, width = 10, height = 6) {
  paste0("<div class='plot'>", svglite::stringSVG(print(p), width = width, height = height), "</div>")
}
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot"))

ranking_plot <- ggplot(base, aes(y = label)) +
  geom_vline(xintercept = 0, color = "grey65") +
  geom_point(aes(x = pearson, color = "Levels (Pearson)"), size = 2.4, shape = 17) +
  geom_point(aes(x = spearman, color = "Ranks (Spearman)"), size = 2.4) +
  scale_color_manual(values = c("Levels (Pearson)" = "#a75b25", "Ranks (Spearman)" = "#185c83")) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "Correlation with the submitted stringency score", y = NULL, color = NULL,
       title = "Which donation measures agree with the permit ranking?",
       subtitle = "2006–2022 donations while in office; personal campaign committees") +
  theme(legend.position = "bottom")

metric_data <- correlations[cohort == "available" & period == "main" & definition == "personal_in_office" & score == "paper_score"]
metric_data[, `:=`(label = factor(labels[sector], levels = rev(unname(labels))),
                   metric_label = factor(metric_labels[metric], levels = unname(metric_labels)))]
metric_plot <- ggplot(metric_data, aes(metric_label, label, fill = spearman)) +
  geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", spearman)), size = 3.2) +
  scale_fill_gradient2(low = "#619bbc", mid = "white", high = "#dd9479", limits = c(-1, 1), name = "Rank\ncorrelation") +
  labs(x = NULL, y = NULL, title = "Does changing the donation measure change the comparison?")

recipient_data <- correlations[cohort == "common_main" & period == "main" & metric == "dollar_share" &
                                score == "paper_score" & sector %in% selected]
recipient_data[, `:=`(label = factor(labels[sector], levels = rev(labels[selected])),
                      recipient = factor(definition_labels[definition], levels = unname(definition_labels)))]
recipient_plot <- ggplot(recipient_data, aes(recipient, label, fill = spearman)) +
  geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f\nN=%d", spearman, n)), size = 3) +
  scale_fill_gradient2(low = "#619bbc", mid = "white", high = "#dd9479", limits = c(-1, 1), name = "Rank\ncorrelation") +
  labs(x = NULL, y = NULL, title = "Recipient and timing choices, using the same aldermen") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

scatter_data <- gaps[sector %in% c("union_direct", "trades_direct", "real_estate_verified", "developer_verified")]
scatter_data[, label := factor(labels[sector], levels = labels[c("union_direct", "trades_direct", "real_estate_verified", "developer_verified")])]
# Labels show the four largest residual disagreements within each displayed sector.
scatter_labels <- scatter_data[order(-abs(score_residual)), head(.SD, 4), by = sector]
scatter_plot <- ggplot(scatter_data, aes(100 * dollar_share, paper_score)) +
  geom_hline(yintercept = 0, color = "grey80") + geom_point(alpha = .65, color = "#185c83") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#a75b25", linewidth = .6) +
  geom_text_repel(data = scatter_labels, aes(label = full_name), seed = 17, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(~ label, scales = "free_x") +
  labs(x = "Share of eligible contribution dollars (%)", y = "Submitted stringency score",
       title = "Where the measures disagree", subtitle = "Higher scores indicate longer adjusted permit processing times")

time_data <- dcast(measures[period %in% c("early", "late") & definition == "personal_in_office" &
                             sector %in% selected], person_key + sector ~ period, value.var = "dollar_share")
time_data <- time_data[is.finite(early) & is.finite(late)]
time_data[, label := factor(labels[sector], levels = labels[selected])]
time_plot <- ggplot(time_data, aes(100 * early, 100 * late)) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_point(color = "#185c83", alpha = .7) + facet_wrap(~ label, ncol = 2, scales = "free") +
  labs(x = "Donation share, 2006–2013 (%)", y = "Donation share, 2014–2022 (%)",
       title = "Are donation rankings stable for the same people?")
time_table <- stability[comparison == "same_person_over_time", .(Measure = labels[sector],
  Aldermen = n, Pearson = pearson, Spearman = spearman)]

between <- stability[comparison == "between_donation_measures" & left_measure %in% selected & right_measure %in% selected]
between[, `:=`(left = factor(labels[left_measure], levels = rev(labels[selected])),
                right = factor(labels[right_measure], levels = labels[selected]))]
between_plot <- ggplot(between, aes(right, left, fill = spearman)) +
  geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", spearman)), size = 3) +
  scale_fill_gradient2(low = "#619bbc", mid = "white", high = "#dd9479", limits = c(-1, 1), name = "Rank\ncorrelation") +
  labs(x = NULL, y = NULL, title = "Which donation measures rank aldermen similarly to one another?") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

main_table <- base[, .(Measure = labels[sector], Aldermen = n, `Positive sector receipts` = positive,
  Pearson = pearson, `Pearson lower 95%` = pearson_low, `Pearson upper 95%` = pearson_high,
  Spearman = spearman, `Leave-one-out low` = leave_one_out_low, `Leave-one-out high` = leave_one_out_high)]
legacy_table <- correlations[cohort == "available" & period == "legacy_1999_2023" & metric == "dollar_share" & score == "paper_score",
  .(Measure = labels[sector], Aldermen = n, Pearson = pearson, Spearman = spearman)]
history_table <- correlations[cohort == "available" & definition == "personal_calendar" &
  period %in% c("before", "main", "after_complete_years", "latest_partial_year") &
  metric == "dollar_share" & score == "paper_score" & sector %in% selected,
  .(Period = period, Measure = labels[sector], Aldermen = n, Pearson = pearson, Spearman = spearman)]
rank_table <- scatter_data[order(-abs(gap_if_donations_signal_leniency)), head(.SD, 4), by = sector][,
  .(Measure = labels[sector], Alderman = full_name, `Donation share (%)` = 100 * dollar_share,
    `Donation percentile` = 100 * donation_percentile, `Leniency percentile` = 100 * leniency_percentile,
    `Permit observations` = n_permits)]
unclear <- donors[real_estate_broad == TRUE & real_estate_verified == FALSE & union_organization == FALSE][order(-amount)]
unclear <- head(unclear, 15)[, .(Donor = donor_name, `1994–2026 dollars` = amount, Occupations = occupations, Employers = employers)]
coverage_years <- coverage[, .(People = uniqueN(alderman_id), Receipts = sum(receipts),
  Dollars = sum(dollars), `Recognized own transfers` = sum(own_transfer_dollars),
  `No job or employer supplied` = sum(missing_job_dollars)), by = .(Year = year)]

html <- c("<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
"<title>Campaign funding and permit stringency</title><style>body{font:17px/1.55 system-ui,sans-serif;color:#26343b;max-width:1150px;margin:40px auto;padding:0 22px}h1{font-size:34px;line-height:1.2}h2{margin-top:46px}a{color:#185c83}.note{padding:18px 22px;background:#eef4f6;border-left:4px solid #185c83}.plot svg{width:100%;height:auto}.table{overflow-x:auto}table{border-collapse:collapse;font-size:13px;width:100%;margin:20px 0}td,th{padding:9px 10px;border-bottom:1px solid #d5dfe4;text-align:left}th{background:#eef4f6}details{margin:22px 0}summary{cursor:pointer;font-weight:600}.small{font-size:14px;color:#52636d}</style>",
"<h1>Campaign funding and permit stringency</h1><p>Exploratory comparison · September 14, 2026 · score_robustness</p>",
"<div class='note'><b>Read the sign first.</b> A negative correlation means aldermen with more funding from that sector tend to have <i>lower</i> permit stringency. A correlation near zero means the two measures give little agreement in rankings. Neither measure is assumed to be the truth.</div>",
sprintf("<p>The main comparison covers <b>%d aldermen</b> with eligible receipts while in office in 2006–2022 and a submitted score. Each sector's dollar share divides its recorded contributions by the person's total eligible itemized contributions and transfers. A zero share is retained when fundraising is observed; missing fundraising is not a zero.</p>", base[sector == "union_direct", n]),
"<p>The benchmark is the submitted score through 2022. It has not been reestimated. All of this work is exploratory; the paper and production results are unchanged.</p>",
plot_html(ranking_plot, height = 8.5),
"<details><summary>Every main comparison, confidence interval, and leave-one-alderman-out range</summary>", table_html(main_table),
"<p class='small'>Pearson intervals treat alderman pairs as independent and condition on the estimated scores. They do not account for score estimation error or multiple exploratory comparisons. Spearman uses average ranks for tied donations.</p></details>",
"<h2>Definition sensitivity</h2><p>The denominator and donor classification can both matter. These comparisons retain every declared measure, including definitions that disagree with the permit ranking.</p>",
plot_html(metric_plot, height = 9), plot_html(recipient_plot, height = 6),
"<p class='small'>The second chart uses people observed under every displayed definition. Old election cycles assign earlier donations to the eventual elected alderman. Broader committees and calendar dates can include other political activity.</p>",
"<details><summary>The earlier pooled donation output, compared with today's submitted score</summary>", table_html(legacy_table),
"<p>The earlier output pools its recorded election cycles from 1999 through 2023. It has a different time window, recipient set, and classification process.</p></details>",
"<h2>Individual disagreements</h2>", plot_html(scatter_plot, height = 7),
"<p>The labels mark the largest departures from each plotted line. The table below instead compares donation and leniency percentiles under the explicit hypothesis that more development-linked giving signals leniency. These are disagreements to understand, not evidence that a score or donor record is wrong.</p>", table_html(rank_table),
"<h2>Stability over time</h2><p>Only the same people observed in both donation periods enter each comparison. The gray line marks unchanged dollar shares; the table also compares rankings. Tenure and the number of receipts can differ across periods.</p>",
plot_html(time_plot, height = 10), table_html(time_table),
"<h2>Agreement between donation measures</h2><p>Correlated sectors need not imply the same policy preferences. A broad measure includes several narrower groups, so part of its correlation with them is mechanical.</p>", plot_html(between_plot, height = 8),
"<h2>The longer donation history</h2><p>1994–2005 and 2023–2025 use calendar dates for the same recorded people and committees, including periods outside alderman service. <b>2026 is partial</b>: receipts end August 1 and the CPI file ends in June. These are not complete-year funding estimates or current alderman rankings.</p>", table_html(history_table),
"<h2>Cleaning choices and remaining classification limits</h2><p>The cleaning distinguishes union organizations/PACs from union-linked individuals; adds documented Teamsters and police-organization rules; and distinguishes literal real-estate wording from documented company names and broader development wording. The 27 pending auto-seeded union names have a separate exclusion comparison. Other preliminary union judgments remain identified as preliminary.</p>",
"<p>The named-firm additions use company information from the receipt and a source-backed company classification. They do not infer a donor's historical occupation from a current website. Vague employers and unidentified LLCs remain a limitation, particularly for developer funding. No definition was weighted or chosen to maximize correlation with permits.</p>",
"<details><summary>Largest names entering only the broad real-estate definition</summary><p>These remain outside the narrower classification unless stronger recorded evidence identifies them. Values cover the full saved history.</p>", table_html(unclear), "</details>",
"<details><summary>Source exclusions and annual coverage</summary>", table_html(counts), table_html(coverage_years), "</details>",
"<p><a href='../README.md'>Complete definitions and chronological cleaning notes</a> · <a href='../sources/sector_name_evidence.csv'>Named-organization rules and sources</a> · <a href='donation_measures.csv'>All alderman measures</a> · <a href='donation_correlations.csv'>All correlations</a> · <a href='donation_stability.csv'>All stability comparisons</a> · <a href='donation_rank_gaps.csv'>All individual disagreements</a></p>",
"<p class='small'>The local raw snapshot is preserved with a hash but has not yet been included in a public source archive for this new audit.</p></html>")
writeLines(html, "../output/donation_review.html")
