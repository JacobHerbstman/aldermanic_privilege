# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/construction_measurement_review/code")
library(data.table)
source("../../../shared/code/save_data.R")
projects <- fread("../output/project_screen.csv")
pairs <- fread("../output/measurement_pairs.csv")
raw <- fread("../output/commercial_source_rows.csv", colClasses = "character")
notes <- fread("../review_notes.csv", colClasses = "character")
prior_decisions <- fread("../prior_decision_review.csv", colClasses = "character")
prior_counts <- fread("../input/prior_unit_review.csv")
rules <- fread("../output/source_rule_summary.csv")
rule_checks <- fread("../output/source_rule_checks.csv")
count_land <- fread("../count_land_review.csv", na.strings = "", colClasses = list(character = "assessor_row_ids"))
influential <- fread("../influential_building_review.csv", na.strings = "")
stopifnot(!anyDuplicated(influential$project_id), nrow(influential) == 13,
  all(influential$adopted == FALSE))
ReportData("../influential_building_review.csv", "project_id")
stopifnot(!anyDuplicated(rule_checks$project_id), setequal(rule_checks$project_id, projects$project_id))
stopifnot(!anyDuplicated(count_land$project_id), !anyDuplicated(count_land$case_number),
  setequal(count_land$project_id, rule_checks[same_areas_unit_change_rows > 0, project_id]),
  all(count_land$adopted == TRUE), all(!is.na(count_land$evidence)),
  all(!is.na(count_land$proposed_sample_action)),
  all(!is.na(count_land[followup_reviewed == TRUE, followup_evidence])))
comparison <- merge(count_land, projects[, .(project_id, dwelling_units, land_sqft,
  sample_check = in_main_500ft_sample)], by = "project_id", sort = FALSE)
stopifnot(nrow(comparison) == nrow(count_land),
  all(comparison$current_units == comparison$dwelling_units),
  all(comparison$current_land_sqft == comparison$land_sqft),
  all(comparison$in_main_500ft_sample == comparison$sample_check))
stopifnot(!anyDuplicated(notes$project_id), all(notes$project_id %in% projects$project_id))
stopifnot(!anyDuplicated(prior_decisions$project_id), !anyDuplicated(prior_counts$project_id),
  setequal(prior_decisions$project_id, projects[flagged == TRUE & !is.na(decision_reason), project_id]))
ReportData("../review_notes.csv", "project_id")
ReportData("../prior_decision_review.csv", "project_id")
ReportData("../count_land_review.csv", "project_id")
projects <- merge(projects, notes, by = "project_id", all.x = TRUE, sort = FALSE)
projects[!is.na(reviewed_address) & reviewed_address != "", address := reviewed_address]
flags <- c("unit_change", "stable_area_unit_conflict", "cleared_without_unit_support", "residential_unit_conflict", "development_permit_conflict",
  "nearby_repeated_units", "nearby_repeated_land", "unusual_floor_per_unit", "shared_parcel")
counts <- rbindlist(lapply(flags, function(f) data.table(Check = f, Citywide = sum(projects[[f]]),
  `Main 500 ft sample` = sum(projects[[f]] & projects$in_main_500ft_sample))))
# Separate source contradictions from clues, then show where comparisons are unavailable.
direct_concern <- with(projects, unit_change | cleared_without_unit_support |
  residential_unit_conflict | development_permit_conflict | shared_parcel)

# Reconcile the original flags with existing evidence. A prior decision only
# addresses the measurement it actually reviewed; it does not certify a project.
reconciliation <- merge(projects[flagged == TRUE], prior_decisions, by = "project_id", all.x = TRUE, sort = FALSE)
reconciliation <- merge(reconciliation, prior_counts[, .(project_id,
  previous_count_source = recommended_units_source, previous_count_review = unit_review_reason)],
  by = "project_id", all.x = TRUE, sort = FALSE)
reconciliation[, reconciliation_status := fifelse(project_id %in% projects[direct_concern, project_id],
  "Source disagreement still needs reconciliation", "Repeated values or unusual ratio only; not adjudicated")]
reconciliation[!is.na(prior_flag_assessment), reconciliation_status := "Earlier decision leaves the flagged question open"]
reconciliation[prior_flag_assessment == "earlier_decision_addresses_flag",
  reconciliation_status := "Earlier documented decision addresses the flag"]
reconciliation[!is.na(status), reconciliation_status := "Case reviewed; measurement question remains"]
reconciliation[status == "current_count_supported", reconciliation_status := "Current count supported by reviewed evidence"]
reconciliation[allow_far %in% FALSE | allow_dupac %in% FALSE |
  !is.finite(building_sqft) | building_sqft <= 0 | !is.finite(land_sqft) | land_sqft <= 0,
  reconciliation_status := "Already outside the common FAR and DUPAC sample"]
reconciliation[status == "confirmed_count_error", reconciliation_status := "Confirmed count error; correction not applied"]
setorder(reconciliation, project_id)
reconciliation <- reconciliation[, .(project_id, address, in_main_500ft_sample, dwelling_units,
  building_sqft, land_sqft, construction_year, allow_far, allow_dupac, screen_reasons,
  reconciliation_status, status, evidence, recommendation, source_url,
  prior_flag_assessment, assessment, earlier_decision, original_count_source,
  original_evidence, original_urls, original_caveat, record_reference,
  previous_count_source, previous_count_review, recommended_units, earlier_review_still_selected)]
stopifnot(setequal(reconciliation$project_id, projects[flagged == TRUE, project_id]))
SaveData(reconciliation, "project_id", "../output/flag_reconciliation.csv")
reconciliation_counts <- reconciliation[, .(Projects = .N,
  `Original main 500 ft sample` = sum(in_main_500ft_sample)), by = .(Finding = reconciliation_status)]
coverage_groups <- list(
  "Flagged: source disagreement or an unsupported earlier review" = direct_concern,
  "Flagged only by nearby repeats or unusual floor area per apartment" = projects$flagged & !direct_concern,
  "Flagged with an existing recorded decision (any subject)" = projects$flagged & !is.na(projects$decision_reason),
  "Flagged with an earlier commercial count review" = projects$flagged & !is.na(projects$permit_supports_2021),
  "Unflagged residential projects without a comparable historical assessment" = with(projects,
    !flagged & source_family == "residential" & comparable_residential_history %in% FALSE),
  "Unflagged commercial projects without a positive older comparison count" = with(projects,
    !flagged & source_family == "commercial" & !(is.finite(earlier_units) & earlier_units > 0)),
  "Unflagged commercial projects with changing floor or lot measurements" = with(projects,
    !flagged & source_family == "commercial" &
      ((is.finite(earlier_building) & earlier_building > 0 & is.finite(building_sqft) & building_sqft > 0 & earlier_building != building_sqft) |
       (is.finite(earlier_land) & earlier_land > 0 & is.finite(land_sqft) & land_sqft > 0 & earlier_land != land_sqft))),
  "Development records without an available positive permit count" = with(projects,
    project_kind == "class_297" & !(is.finite(permit_units) & permit_units > 0)))
coverage <- rbindlist(lapply(names(coverage_groups), function(label) data.table(
  Finding = label, Citywide = sum(coverage_groups[[label]]),
  `Main 500 ft sample` = sum(coverage_groups[[label]] & projects$in_main_500ft_sample))))
escape <- function(x) {
  x[is.na(x)] <- ""
  gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
}
html_table <- function(d) paste0("<table><thead><tr>", paste0("<th>", escape(names(d)), "</th>", collapse = ""),
  "</tr></thead><tbody>", paste(apply(as.data.frame(d), 1, function(r)
    paste0("<tr>", paste0("<td>", escape(r), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
# The approved review is compared with the preserved pre-adoption dataset.
review_counts <- count_land[, .(Projects = .N,
  `Original main 500 ft sample` = sum(in_main_500ft_sample)), by = .(Recommendation = proposed_sample_action)]
setorder(count_land, case_number)
case_sections <- character(nrow(count_land))
for (i in seq_len(nrow(count_land))) {
  r <- count_land[i]
  links <- strsplit(ifelse(is.na(r$source_urls), "", r$source_urls), ";", fixed = TRUE)[[1]]
  links <- links[nzchar(links)]
  stopifnot(all(grepl("^https://", links)), all(!grepl('["<>]', links)))
  source_links <- if (length(links)) paste0('<a href="', escape(links), '">External source ',
    seq_along(links), '</a>', collapse = ' · ') else 'Recorded Assessor and permit evidence below.'
  case_sections[i] <- paste0('<article class="review-case" data-main="', r$in_main_500ft_sample,
    '" id="review-case-', r$case_number, '"><h3>', r$case_number, '. ', escape(r$address),
    '</h3><p><strong>', escape(r$recommendation), '</strong> · ',
    ifelse(r$in_main_500ft_sample, 'In the main 500-foot sample', 'Outside the main 500-foot sample'),
    '</p><p>Count: ', r$current_units, ' → <strong>',
    ifelse(is.na(r$recommended_units), 'unresolved / excluded', r$recommended_units),
    '</strong>. Land: ', r$current_land_sqft, ' → <strong>',
    ifelse(is.na(r$recommended_land_sqft), 'unresolved / excluded', r$recommended_land_sqft),
    '</strong> square feet. Current construction year: ', r$current_construction_year,
    '. Current floor area: ', r$current_building_sqft, ' square feet.</p><p>', escape(r$evidence),
    '</p>', if (r$followup_reviewed) paste0('<p><strong>Follow-up:</strong> ', escape(r$followup_evidence),
      '</p><p>Proposed year: ', ifelse(is.na(r$recommended_construction_year), 'unresolved / excluded', r$recommended_construction_year),
      '. Proposed floor area: ', ifelse(is.na(r$recommended_building_sqft), 'withheld', r$recommended_building_sqft),
      ' square feet. <strong>Sample recommendation: ', escape(r$proposed_sample_action), '</strong>.</p>') else '',
    '<p><strong>Limits and remaining questions:</strong> ', escape(r$remaining_issue),
    '</p>', ifelse(is.na(r$other_measurement_concern), '', paste0('<p class="caution"><strong>Reason for the date/floor follow-up:</strong> ',
      escape(r$other_measurement_concern), '.</p>')),
    '<p>', source_links,
    '</p><details><summary>Exact Assessor history and permit evidence</summary><p>Project: ',
    escape(r$project_id), '</p><pre>', escape(r$assessor_history), '</pre><pre>', escape(r$permit_evidence),
    '</pre><p>Assessor source row IDs: ', escape(r$assessor_row_ids), '</p></details></article>')
}
review_section <- paste0('<h2>Review of all ', nrow(count_land), ' count disagreements</h2>',
  '<p>Each record below has conflicting apartment counts in assessments with unchanged floor and lot areas. ',
  'I compared the recorded assessments, permits, previous decisions and available building reports. ',
  'These decisions were approved and entered in the existing recorded_building_changes.csv on September 15. The original values and sample membership shown here describe the dataset before adoption.</p>', html_table(review_counts),
  '<p><strong>Follow-up completed for ', sum(count_land$followup_reviewed), ' of these same records.</strong> ',
  'This includes the original eleven unresolved or provisional cases and the overlapping date/floor questions. ',
  sum(count_land$proposed_sample_action == 'Withhold density'),
  ' still lack a supported measurement or construction year; they are withheld from the common FAR-and-DUPAC sample. ',
  'The two Sawyer properties retain an explicitly qualified recommendation of two apartments each. ',
  'The table describes the adopted treatment under the common FAR-and-DUPAC sample. “Keep” includes the corrections specified below; ',
  'it does not mean all original values were correct. No new cases or unflagged-building sample were added.</p>',
  '<p><strong>Recommended principle:</strong> measure the original completed building. Use the Assessor count when original-building evidence supports it; ',
  'count apartments separately from shops; distinguish later conversions from original construction; and pair a building count with its own reported land. ',
  'Neither the oldest nor the newest assessment wins automatically. An issued permit alone is not proof of the completed plan.</p>',
  '<p><a href="../count_land_review.csv">All adopted decisions and evidence in one CSV</a> · ',
  '<a href="../count_land_review.md">Findings, unresolved cases and adoption details</a></p>',
  '<input id="review-search" placeholder="Find an address, recommendation or evidence"> ',
  '<label><input type="checkbox" id="main-only" style="width:auto"> Original main 500-foot sample only</label>',
  paste(case_sections, collapse = ''),
  '<script>function filterReview(){const q=document.getElementById("review-search").value.toLowerCase();',
  'const main=document.getElementById("main-only").checked;',
  'document.querySelectorAll(".review-case").forEach(r=>r.hidden=!r.textContent.toLowerCase().includes(q)||(main&&r.dataset.main!=="TRUE"));}',
  'document.getElementById("review-search").addEventListener("input",filterReview);',
  'document.getElementById("main-only").addEventListener("change",filterReview);</script>')
rule_table <- rules[, .(`Source route` = route, Projects = projects, `Main sample` = main_sample,
  `Four values reproduced from identified assessment` = fifelse(route == "recorded correction", "Recorded input", as.character(all_four_match)),
  `Values accounted for by stated rules or recorded input` = numbers_follow_rule)]
ordinary_commercial <- rule_checks[route == "commercial_entity_family"]
rule_timing <- data.table(
  Check = c("At least one permit sharing the recorded parcel base", "At least one full residential new-building permit candidate",
    "One completed new-building permit candidate dated after the assigned construction year",
    "A later permit describing residential conversion or a count change"),
  Projects = c(sum(rule_checks$exact_parcel_permits > 0), sum(rule_checks$original_building_permit_candidates > 0),
    sum(rule_checks$single_complete_permit_after_assigned_year), sum(rule_checks$later_conversion_or_count_change_candidates > 0)))
madison <- raw[source_project_id %in% c("commercial_17084440270000", "commercial_17084450160000")][order(address, year),
  .(`Source data row` = source_data_row, Address = address, `Assessment year` = year,
    Apartments = tot_units, `Building sqft` = bldgsf, `Land sqft` = landsf, `Year built` = yearbuilt,
    Studios = studiounits, `1 bedroom` = `_1brunits`, `2 bedrooms` = `_2brunits`, `3 bedrooms` = `_3brunits`, `4 bedrooms` = `_4brunits`)]
findings <- projects[!is.na(status)][order(status, -in_main_500ft_sample, address),
  .(Address = address, `Main sample` = in_main_500ft_sample, `Current units` = dwelling_units,
    Finding = status, Evidence = evidence, Recommendation = recommendation, Sources = source_url)]
flagged <- projects[flagged == TRUE][order(-in_main_500ft_sample, -stable_area_unit_conflict, address),
  .(Address = address, Project = project_id, `Main sample` = in_main_500ft_sample,
    Units = dwelling_units, `2021 units` = earlier_units, `Building sqft` = building_sqft,
    `2021 building sqft` = earlier_building, `Land sqft` = land_sqft, `2021 land sqft` = earlier_land,
    Year = construction_year, `Distance ft` = round(distance_to_boundary_ft, 1),
    Flags = screen_reasons, `Existing recorded decision` = decision_reason,
    `Follow-up status` = status, Evidence = evidence, Recommendation = recommendation)]
influence_cases <- character(nrow(influential))
for (i in seq_len(nrow(influential))) {
  r <- influential[i]
  links <- strsplit(r$source_urls, ";", fixed = TRUE)[[1]]
  stopifnot(all(grepl("^https://", links)), all(!grepl('["<>]', links)))
  influence_cases[i] <- paste0('<article class="review-case"><h3>', r$case_number, '. ', escape(r$address),
    '</h3><p><strong>', escape(r$recommendation), '</strong></p><p>', escape(r$confidence),
    '</p><p>Current data: ', r$current_units, ' homes; ', r$current_building_sqft,
    ' square feet of floor; ', r$current_land_sqft, ' square feet of land; year ', r$current_year,
    '; ', round(r$current_dupac, 2), ' DUPAC. Current alderman pair: ', escape(r$current_pair),
    '.</p><p>', escape(r$evidence), '</p><p><strong>Limits:</strong> ', escape(r$remaining_issue),
    '</p><p>', paste0('<a href="', escape(links), '">Source ', seq_along(links), '</a>', collapse = ' · '),
    '</p><details><summary>Recorded Assessor and permit evidence</summary><p>', escape(r$project_id),
    '</p><pre>', escape(r$assessor_history), '</pre><pre>', escape(r$permit_evidence),
    '</pre></details></article>')
}
writeLines(paste0('<!doctype html><meta charset="utf-8"><title>Construction measurement review</title>',
  '<style>body{font:16px/1.5 system-ui;margin:30px;color:#203543}table{border-collapse:collapse;font-size:13px}td,th{padding:8px;border-bottom:1px solid #ccd6dc;text-align:left;vertical-align:top}th{background:#edf2f5;position:sticky;top:0}section{overflow:auto;margin:20px 0}input{font:inherit;padding:8px;width:50%}h1,h2,p{max-width:1100px}td{min-width:90px}a{color:#176184}</style>',
  '<h1>Do units, floor area and land describe the same buildings?</h1><p>The population contains ', nrow(projects),
  ' retained construction projects citywide. ', sum(projects$in_main_500ft_sample),
  ' enter the main 500-foot regression sample. This report screens measurements without estimating effects or changing production values.</p>',
  '<style>.review-case{max-width:1100px;border-top:1px solid #ccd6dc;padding:12px 0}pre{white-space:pre-wrap;font-size:12px;overflow-wrap:anywhere}.caution{color:#794718}summary{cursor:pointer;font-weight:600}</style>',
  '<h2 id="influential">September 15: review of 13 influential observations</h2>',
  '<p><strong>These are recommendations and open questions, not adopted corrections.</strong> ',
  'The five largest individual influences supporting the negative multifamily DUPAC estimate are reviewed first, followed by eight additional influential observations, including three pushing against that result. ',
  'Selection used the current post-correction estimates. This is not a random sample and cannot estimate the error rate in the full dataset.</p>',
  '<p>None was in the earlier 87-case count-disagreement review. Repeated but incorrect counts, additions and mistaken completion years can pass that particular screen. ',
  'The current multifamily estimate is −11.68%; no estimate incorporating these recommendations has been run. ',
  'A supported count does not independently certify the floor area, lot coverage or exact completion date. ',
  'All source-reported areas are distinguished from independently supported measurements below.</p>',
  '<p><a href="../influential_building_review.csv">All 13 reviews, source records and earlier omission diagnostics</a></p>',
  paste(influence_cases, collapse = ''),
  review_section,
  '<details><summary>Earlier population checks and reconciliation of the original 218 flags</summary><p>The sections below record earlier stages. The count-and-land review above supersedes their case recommendations for the 87 overlapping records; earlier counts have not been relabeled as new findings.</p>',
  '<h2>First stage: check the general source rules</h2><p>The population check reconstructs units, floor area, lot area and construction year from the identified assessment rows, the explicit source-combination rules and the recorded correction file. Reproducing a number establishes where it came from. It does not establish that a current assessment describes the original construction.</p>',
  html_table(rule_table),
  '<p>For ordinary commercial records, ', sum(ordinary_commercial$earlier_area_used | ordinary_commercial$earlier_area_aggregate),
  ' use older area measurements: ', sum(ordinary_commercial$earlier_area_used), ' use an earlier reported value and ', sum(ordinary_commercial$earlier_area_aggregate),
  ' combine earlier component records. In ', sum((ordinary_commercial$earlier_area_used | ordinary_commercial$earlier_area_aggregate) & ordinary_commercial$earlier_area_units_agree %in% FALSE),
  ' of these, the earlier and selected counts differ. This identifies where the same-building assumption needs evidence; it does not make those observations automatically wrong.</p>',
  '<p>The existing residential rule moves ', sum(rule_checks$permit_year_adjustment & !rule_checks$recorded_correction),
  ' construction years forward by one when the Assessor year is one year before the permit application. The application year is not a recorded completion date. Separately, ', sum(!rule_checks$recorded_correction & rule_checks$identified_rows_complete & !rule_checks$source_year_label_matches),
  ' records retain an assessment-year label that differs from the row supplying their measurements after floor-area recovery.</p>',
  '<p>For ', sum(rule_checks$route == "same_pin_multiple_cards" & rule_checks$unselected_same_assessment_cards > 0),
  ' ordinary multiple-card properties, the selected assessment also has positive-area building cards outside the counted construction episode. The numerator includes selected new-construction cards while the denominator uses the parcel lot. Those extra cards could represent existing buildings or obsolete records; their presence shows that a complete selected episode is not necessarily the whole parcel. This has not yet been adjudicated.</p>',
  '<p>Development records use an apartment field that the <a href="https://github.com/ccao-data/model-res-avm#features-used">Assessor defines for class 211 and 212 properties</a>. A positive value in a development record is not enough to establish the completed-building count. Completed condominium records instead count residential units and exclude parking and common-area records.</p>',
  '<h3>Check the chronology without assuming a permit is the same building</h3>', html_table(rule_timing),
  '<p>These matches use the first ten digits of the parcel number and all available dates. Condominium unit parcels can share this base number. The same parcel can host a replacement, another building or a later alteration. Full-building candidates require residential new-construction text and omit revisions, alterations, foundations-only, additions and existing-building descriptions. They are not an exhaustive permit inventory or completed case decisions. Later changes can already be correctly excluded from the construction observation. The 218 original flags and their adjudications remain unchanged; no unflagged-building sample has been selected or reviewed.</p>',
  '<p><a href="source_rule_checks.csv">Population source-rule checks</a> · <a href="source_rule_summary.csv">Source-route summary</a> · <a href="../source_rules.md">What each rule establishes and what it assumes</a></p>',
  '<h2>Reconcile the original 218 flags with earlier decisions</h2>', html_table(reconciliation_counts),
  '<p>These rows partition the original flagged properties. All 64 recorded decisions were read for the specific question raised by the flag. An earlier documented decision can explain a repeated count or a deliberate correction without independently verifying every measurement. Eight old decisions still rely on a source preference rather than settling the count; another addresses a count but not the repeated lot area. The six properties already outside the common FAR and DUPAC sample are retained in this review, not newly excluded.</p>',
  '<p>The earlier automated commercial review is also attached for every matching project. Its “resolved” label is not accepted as proof: 74 flagged properties passed its latest-report preference without permit support for either conflicting vintage. Subsequent documented decisions or case evidence can supersede that old label. Remaining source disagreements and repetition-only clues have not been individually adjudicated by this reconciliation.</p>',
  '<p><a href="flag_reconciliation.csv">Every original flag and its present evidence status</a> · <a href="../prior_decision_review.csv">Review of the 64 earlier decisions</a>. Production measurements and estimates are unchanged. The unflagged-building sample remains deferred.</p>',
  '<h2>Are these all the possible problems?</h2><p>No. The 218 are the output of particular screens, not an exhaustive error list or a new manual-review assignment. An unchanged but incorrect value can pass; repeated site totals below the size or distance cutoffs can pass; changes in building identity can prevent a historical comparison. The screen also starts after project selection, so it cannot establish that no construction was wrongly omitted. Dates and geography have not received a comprehensive new validation in this audit.</p>',
  html_table(coverage),
  '<p>The first two rows partition the flagged records; other rows overlap. Missing comparison evidence and changed areas are coverage findings, not newly established errors. A recorded decision may concern location or year rather than apartment counts, so its subject and supporting evidence must be read before treating the present concern as resolved.</p>',
  '<p>Before more case-by-case research, compare units, floor area, land and construction timing across the relevant source branches, reuse previous evidence for the specific variable it addresses, and independently check a sample of unflagged records selected without reference to the regression results. Catching the eight known errors is not a validation of the screen: some checks were added after seeing those cases. No false-negative rate has been established.</p>',
  '<h2>Why Madison was wrong</h2><p>The preserved 2024 Assessor file gives each building 81 apartments while keeping its individual floor and lot measurements. The earlier review accepted the newer count when no permit count supported the older one. It then called the count resolved simply because the selected number was positive. That did not establish that the apartments and land described the same building.</p>',
  html_table(madison),
  '<p>Independent building reports support 9 apartments at 1100 W Madison and 18 at 1048, within an 81-apartment community. Thus the old 24 at 1048 is also not the recommended correction. The newer 81 is already in the raw source; neither summing bedroom categories nor a join created it.</p>',
  '<h2>Findings with evidence reviewed</h2><p>', sum(projects$status == "confirmed_count_error", na.rm = TRUE),
  ' records have a confirmed count problem; other rows below distinguish supported current counts from unresolved questions. A supported apartment count alone does not certify the building year, floor area or lot coverage.</p><section>', html_table(findings), '</section>',
  '<p>Flags are leads, not verdicts. Repeated counts can describe genuinely separate buildings. Historical disagreements can reflect corrections, additions or a change in which buildings an assessment covers. An existing manual decision is shown as evidence to read, not automatic proof that a case is settled.</p>',
  '<h2>Screen coverage</h2>', html_table(counts),
  '<p>', sum(projects$flagged), ' projects trigger at least one check, including ', sum(projects$flagged & projects$in_main_500ft_sample),
  ' in the main sample. Counts overlap. Residential comparisons require the same card, exact floor and lot measurements, and a construction-year difference no greater than two. Shared or replaced records without those comparable source rows remain outside that historical comparison. Development records are checked against the available permit count. Nearby repeats use a 1,000-foot radius and either at least eight repeated units within a two-year construction window or at least 20,000 repeated square feet of land across any construction years. All multifamily projects are also checked for floor area below 300 or above 5,000 square feet per dwelling. All projects are checked for shared parcel numbers.</p>',
  '<h2>Source conflicts and repeated measurements</h2><input id="search" placeholder="Filter by address, project, flag or review status"><section id="cases">',
  html_table(flagged), '</section><h2>Exact commercial Assessor fields</h2><p>These values are copied from the preserved CSV. Source data rows exclude the header. The assessment year (year) is distinct from its reported construction year (yearbuilt).</p><section>',
  html_table(raw), '</section><p><a href="project_screen.csv">All projects and checks</a> · <a href="measurement_pairs.csv">Nearby repeated measurements</a> · <a href="commercial_source_rows.csv">Exact Assessor rows</a> · <a href="flagged_permits.csv">Recorded permit evidence</a> · <a href="../README.md">Definitions and findings</a></p>',
  '</details><script>document.getElementById("search").addEventListener("input",function(){const q=this.value.toLowerCase();document.querySelectorAll("#cases tbody tr").forEach(r=>r.hidden=!r.textContent.toLowerCase().includes(q));});</script>'),
  "../output/measurement_review.html")
