# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_ordering_drivers/code")
library(data.table)
library(ggplot2)
source("../../../shared/code/save_data.R")
results <- rbindlist(list(fread("../output/all_driver_results.csv"), fread("../output/multifamily_driver_results.csv")))
raw <- fread("../input/raw_processing_pairs.csv")[measure == "mean_log_days" & market %in% c("density_all", "density_multifamily")]
evidence <- fread("../output/multifamily_project_evidence.csv")
sides <- fread("../output/multifamily_pair_sides.csv")
summary <- dcast(results, sample + outcome + kind + name + involved_projects + switched_projects + switched_pairs +
  near_projects + near_switched_projects + original_estimate + raw_estimate ~ action, value.var = "percent_effect")
summary[, `:=`(original_effect = 100 * expm1(original_estimate), raw_effect = 100 * expm1(raw_estimate))]
summary[, switch_change_pp := switch_from_original - original_effect]
stopifnot(!anyDuplicated(summary[, .(sample, outcome, kind, name)]), !anyNA(summary))
SaveData(summary, c("sample", "outcome", "kind", "name"), "../output/density_driver_summary.csv")
top_pairs <- summary[kind == "pair"][order(sample, outcome, -switch_change_pp), head(.SD, 5), by = .(sample, outcome)]
top_aldermen <- summary[kind == "alderman"][order(sample, outcome, -switch_change_pp), head(.SD, 5), by = .(sample, outcome)]
leading_pair <- top_pairs[sample == "multifamily" & outcome == "density_dupac", name][1]
nearby <- evidence[pair == leading_pair & abs(paper_distance) < 100]

# Trace the selected buildings back to their preserved commercial assessments.
assessor <- fread("../input/commercial_valuation_data.csv", colClasses = "character")
assessor[, source_row := .I]
assessor[, project_id := paste0("commercial_", gsub("[^0-9]", "", keypin))]
history <- assessor[project_id %in% nearby$project_id, .(project_id, source_row, keypin, pins, year, address,
  studiounits, `_1brunits`, `_2brunits`, `_3brunits`, `_4brunits`, tot_units, bldgsf, landsf, yearbuilt)]
SaveData(history, "source_row", "../output/leading_pair_assessor_history.csv")

sample_labels <- c(all = "All construction", multifamily = "Multifamily")
outcome_labels <- c(density_far = "FAR", density_dupac = "DUPAC")
top_pairs[, panel := paste(sample_labels[sample], outcome_labels[outcome], sep = ": ")]
top_pairs[, display_name := paste(name, panel, sep = "||| ")]
top_pairs[, display_name := factor(display_name, levels = unique(display_name[order(switch_change_pp)]))]
plot <- ggplot(top_pairs, aes(switch_change_pp, display_name)) + geom_col(fill = "#23617d", width = .7) +
  facet_wrap(~panel, scales = "free", ncol = 1) + scale_y_discrete(labels = function(x) sub("\\|\\|\\|.*$", "", x)) +
  labs(title = "Which individual pair switches weaken the negative density estimate most?",
    x = "Change in the reported effect (percentage points)", y = NULL,
    caption = "Each bar switches only that pair from the original ordering.\nThese changes are not additive.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("../output/density_drivers.pdf", plot, width = 10, height = 11, bg = "white")
ggsave("../output/density_drivers.png", plot, width = 10, height = 11, dpi = 150, bg = "white")

html_table <- function(d) {
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(apply(as.data.frame(d), 1, function(row) paste0("<tr>", paste0("<td>", escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
corrections <- rbindlist(list(fread("../output/all_dupac_corrections.csv"), fread("../output/multifamily_dupac_corrections.csv")))
project_influence <- rbindlist(list(fread("../output/all_dupac_projects.csv"), fread("../output/multifamily_dupac_projects.csv")))
pair_influence <- rbindlist(list(fread("../output/all_dupac_pairs.csv"), fread("../output/multifamily_dupac_pairs.csv")))
step_labels <- c(before = "Before corrections", madison_counts_only_in_old = "Correct only Madison apartment counts",
  madison_all_changes_in_old = "Correct Madison counts and years", all_except_madison = "All corrections except Madison",
  after = "All adopted corrections")
steps <- corrections[scenario %in% names(step_labels)]
steps[, step := match(scenario, names(step_labels))]
setorder(steps, sample, step)
steps <- steps[, .(Sample = sample_labels[sample], Change = step_labels[scenario],
  `DUPAC effect (%)` = round(percent_effect, 2), `p-value` = round(p_value, 3), Projects = n)]
individual_changes <- corrections[scenario == "apply_one_to_old"][order(sample, -abs(change_from_before_pp)), head(.SD, 10), by = sample]
individual_changes <- individual_changes[, .(Sample = sample_labels[sample], Address = address,
  `Old units` = old_units, `New units` = new_units, `Old year` = old_year, `New year` = new_year,
  `Apply only this correction (%)` = round(percent_effect, 2), `Change (percentage points)` = round(change_from_before_pp, 2))]
leaders <- project_influence[order(sample, -change_pp), head(.SD, 10), by = sample]
leaders <- leaders[, .(Sample = sample_labels[sample], Address = fifelse(is.na(address) | address == "", project_id, address), Year = construction_year,
  Units = dwelling_units, `Lot sqft` = land_sqft, DUPAC = round(density_dupac, 2),
  Side = fifelse(signed_distance_ft > 0, "More stringent", "Less stringent"),
  `Distance (ft)` = round(abs(signed_distance_ft), 1),
  `Current effect (%)` = round(current_effect, 2), `Remove this project (%)` = round(percent_effect, 2),
  `Additional FE omissions` = additional_omitted, `In 87-case review` = in_recent_review)]
pair_leaders <- pair_influence[order(sample, -change_pp), head(.SD, 10), by = sample]
pair_leaders <- pair_leaders[, .(Sample = sample_labels[sample], Pair = pair, Projects = removed_projects,
  `Current effect (%)` = round(current_effect, 2), `Remove this comparison (%)` = round(percent_effect, 2),
  `p-value` = round(p_value, 3), `Additional FE omissions` = additional_omitted)]
joint <- corrections[scenario == "drop_most_influential_current", .(Sample = sample_labels[sample],
  `Current effect (%)` = round(current_effect, 2), `Remove five leading projects (%)` = round(percent_effect, 2),
  `p-value` = round(p_value, 3), `Remaining projects` = n)]
group_table <- top_aldermen[, .(Sample = sample_labels[sample], Outcome = outcome_labels[outcome], Alderman = name,
  `Switched pairs` = switched_pairs, `Switched projects` = switched_projects, `Original (%)` = round(original_effect, 2),
  `Switch this alderman's disagreements (%)` = round(switch_from_original, 2), `All raw (%)` = round(raw_effect, 2),
  `Restore this alderman's comparisons (%)` = round(restore_from_raw, 2),
  `Drop alderman; original ordering (%)` = round(drop_from_original, 2), `Drop alderman; raw ordering (%)` = round(drop_from_raw, 2))]
pair_table <- top_pairs[, .(Sample = sample_labels[sample], Outcome = outcome_labels[outcome], Pair = name,
  Projects = involved_projects, `Within 100 ft` = near_projects, `Original (%)` = round(original_effect, 2),
  `Switch only this pair (%)` = round(switch_from_original, 2), `All raw (%)` = round(raw_effect, 2),
  `Restore only this pair (%)` = round(restore_from_raw, 2), `Drop this pair; original (%)` = round(drop_from_original, 2))]
key <- raw[market == "density_multifamily" & pair == leading_pair]
ordering <- data.table(Alderman = c(key$alderman_a, key$alderman_b),
  `Adjusted score` = round(c(key$score_a, key$score_b), 3),
  `Average log days` = round(c(key$raw_a, key$raw_b), 3),
  `Geometric mean days` = round(exp(c(key$raw_a, key$raw_b)), 1))
project_table <- nearby[, .(Address = source_addresses, Alderman = alderman_own, Year = construction_year, Units = dwelling_units,
  `Building sqft` = building_sqft, `Land sqft` = land_sqft, FAR = round(density_far, 2), DUPAC = round(density_dupac, 2),
  `Distance (ft)` = round(abs(paper_distance), 1))]
side_table <- sides[pair == leading_pair, .(Alderman = alderman_own, Band = band, Projects = projects,
  `First construction year` = first_year, `Last construction year` = last_year,
  `Median FAR` = round(median_far, 2), `Median DUPAC` = round(median_dupac, 2))]
history_table <- history[, .(Address = address, `Assessment year` = year, `Recorded units` = tot_units,
  `Building sqft` = bldgsf, `Land sqft` = landsf, `Recorded build year` = yearbuilt, `Source row` = source_row)]
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Which projects drive DUPAC?</title><style>body{font:16px/1.5 system-ui;max-width:1300px;margin:35px auto;padding:0 20px;color:#183445}table{border-collapse:collapse;width:100%;font-size:13px;margin:20px 0}th,td{text-align:left;border-bottom:1px solid #d9e2e7;padding:8px}th{background:#edf3f6}img{max-width:100%}.wide{overflow-x:auto}a{color:#176184}</style>',
  '<h1>Which projects drive DUPAC?</h1><p>These new checks separate the effect of the adopted measurement corrections from the influence of projects in the corrected data. Scores, controls, fixed effects, clustering and distance bins are held at the existing specification. Effects compare the first 100 feet on each side of the boundary.</p>',
  '<h2>How the corrections changed the result</h2>', html_table(steps),
  '<p>The Madison checks concern 1100 and 1048 W Madison. The count-only check changes only their DUPAC values in the original sample. The counts-and-years check replaces their full records, including construction-year controls. Each intermediate row is a diagnostic counterfactual, not a proposed dataset.</p>',
  '<h2>Largest individual corrections</h2><p>Each row applies only that project’s adopted change to the old data. Missing old or new values mean the record is outside that version’s estimation sample. These effects need not add.</p><div class="wide">', html_table(individual_changes), '</div>',
  '<h2>Projects that most support the current negative estimate</h2><p>Every fitted project was removed once and the unchanged regression was re-estimated. These are the ten largest movements toward zero in each sample. Removing a project can also leave an otherwise singleton fixed-effect group; extra omissions are shown. Influence alone does not establish a measurement error or justify exclusion.</p><div class="wide">', html_table(leaders), '</div>',
  '<p>Removing the five leading projects together gives the following diagnostic. The group is selected for its influence on this outcome, so this is not an alternative preferred estimate or a valid exclusion rule.</p>', html_table(joint),
  '<h2>Local comparisons that most support the current negative estimate</h2><p>Each row removes all projects for one named alderman pair. These groups overlap the individual-project checks and their effects should not be added.</p><div class="wide">', html_table(pair_leaders), '</div>',
  '<p>Downloads: <a href="all_dupac_corrections.csv">all-construction corrections</a> · <a href="multifamily_dupac_corrections.csv">multifamily corrections</a> · <a href="all_dupac_projects.csv">every construction project</a> · <a href="multifamily_dupac_projects.csv">every multifamily project</a> · <a href="all_dupac_pairs.csv">all-construction pairs</a> · <a href="multifamily_dupac_pairs.csv">multifamily pairs</a>.</p>',
  '<hr><h2>Earlier score-ordering diagnosis: pre-correction data</h2><p>The rest of this page preserves the earlier analysis. In that version, Burnett’s comparisons account for most of the multifamily DUPAC change when replacing adjusted-score ordering with raw log processing-time ordering. Hopkins also matters substantially for multifamily FAR. These are not the corrected-data deletion checks above.</p>',
  '<p>Every trial uses the existing common FAR/DUPAC sample, controls, fixed effects and clustering. Switching a pair keeps its projects and changes the direction of their boundary distances. Restoring starts from raw ordering and puts that comparison back in its original direction. Dropping removes all projects in the named pair, or all comparisons involving the named alderman. These are influence diagnostics selected after examining the estimates, not proposed exclusion rules.</p>',
  '<h2>Alderman comparisons jointly</h2><p>Each row changes one alderman’s comparisons. Pair membership overlaps: Burnett–Fioretti belongs to both men, so their effects must not be added.</p><div class="wide">', html_table(group_table), '</div>',
  '<h2>Largest individual pair switches</h2><p>The five largest upward movements in each density estimate are shown. All disagreements and all aldermen involved in them are in the downloadable table. Switching and restoring need not have equal effects because the other comparison directions differ.</p><div class="wide">', html_table(pair_table), '</div>',
  '<img src="density_drivers.png" alt="Individual pair switches with the largest upward changes in density estimates">',
  '<h2>Inside the leading multifamily DUPAC pair</h2><p>', leading_pair, '</p>', html_table(ordering),
  '<p>The original score labels Fioretti as more stringent; raw log time labels Burnett as slower. The full comparison contains 27 multifamily projects. These four are within 100 feet of the boundary; projects farther away also enter the fitted regression.</p>', html_table(project_table), html_table(side_table),
  '<h2>A unit-count conflict in two influential buildings</h2><p>The preserved 2024 Assessor rows assign 81 units each to 1100 and 1048 W Madison. Their 2021 rows instead report 9 and 24, with identical building and land areas. The newer values were accepted by the source-priority rule. These are source conflicts, not a newly introduced arithmetic error.</p>', html_table(history_table),
  '<p><strong>Follow-up finding: the individual 81-unit counts are wrong.</strong> The <a href="https://www.taylorjohnson.com/tj-news/waterton-acquires-81-unit-apartment-community-in-chicagos-west-loop/">buyer’s acquisition announcement</a> describes four buildings with 81 apartments altogether. Our existing records count 81 + 81 + 54 = 216 for that community. The <a href="https://yochicago.com/madison-aberdeen-place-apartments-3-n-aberdeen-west-loop/">building profile</a> assigns 9 units to 4 N Aberdeen and 18 to 3 N Aberdeen. The <a href="https://www.luxurychicagoapartments.com/blog/pre-leasing-begins-madison-aberdeen-place-west-loop/">original leasing agent</a> confirms the other 54 in two 27-unit buildings at 20 and 22. Recorded new-building permits link 1100 Madison to 4 N Aberdeen and 1048 Madison to 3 N Aberdeen.</p>',
  '<p>The recommended replacements are 9 and 18 apartments, respectively, with 2013 completion supported by <a href="https://www.chicagomag.com/real-estate/april-2014/developer-drawn-to-low-rise-rentals-lines-up-two-more-in-west-loop-for-summer/">contemporaneous reporting</a>. The older Assessor value of 24 for 1048 remains a conflicting source, not the recommended replacement. The earlier audit detected both count changes but accepted the newer counts whenever the permit did not support the older count; it required no affirmative support for 81. That was inadequate. The corrections have since been adopted in production; these regression results still use the preserved pre-correction data. See the <a href="../../construction_measurement_review/count_land_review.md">measurement review</a> for the adopted decisions. The <a href="../README.md">follow-up evidence</a> records the sources, recommendation and limitations.</p>',
  '<p>Conditional p-values, sample sizes and log coefficients for every trial are retained in the result files. The interventions are not additive decompositions and do not establish a causal effect of one named alderman. This page records the pre-correction analysis; the paper has not been updated for the adopted measurement corrections.</p>',
  '<p><a href="density_driver_summary.csv">All pair and alderman comparisons</a> · <a href="multifamily_project_evidence.csv">Affected multifamily projects</a> · <a href="all_project_evidence.csv">All affected construction projects</a> · <a href="leading_pair_assessor_history.csv">Leading pair: Assessor history</a> · <a href="../README.md">Definitions and evidence</a></p>'), '../output/density_drivers.html')
