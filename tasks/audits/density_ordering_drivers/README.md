# Which comparisons drive the density ordering sensitivity?

Run `make` in `code/`; open `output/density_drivers.html`. This audit diagnoses the move from adjusted-score ordering to raw average log processing-time ordering. It does not change production data or propose an outcome-driven exclusion rule.

## Comparisons

The source is the preserved `data_raw/score_robustness/new_construction_analysis_data.csv`. Raw pair orderings come from `score_robustness_review`; baseline and fully reversed estimates come from `raw_log_score_sensitivity` (revision `78239a53`). All construction uses 4,023 projects; multifamily uses 863. FAR and DUPAC share the same observations. The original 500-foot, ten-bin specification, controls, fixed effects and ward-pair clustering remain unchanged. Each original and raw-ordering coefficient, standard error and sample count must match the preceding audit.

`estimate_density_drivers.R` performs four trials for every disagreeing pair and every alderman involved in a disagreement. Switching starts from the original ordering and changes only the named pair's direction, or every disagreeing pair involving the named alderman. Restoring starts from raw ordering and puts those comparisons back in their original direction. The two deletion trials instead remove all observations belonging to that pair or alderman, under original and raw ordering separately. An alderman deletion includes their agreeing comparisons too.

There are 73 disagreeing pairs and 77 alderman groups in all construction, and 52 pairs and 55 alderman groups in multifamily. The audit saves 1,200 and 856 trial estimates, respectively, including both outcomes. The four actions are diagnostic counterfactual fits. Their effects do not add, and the alderman groups overlap. A finding for Burnett–Fioretti cannot be allocated separately to the two men's behavior. Labels, covariates and fitted sample are held constant except for the stated intervention; outcome values never change.

The script also saves all affected projects with their recorded addresses, counts, areas, construction years, assignments and distances, and descriptive summaries by each side within 500 and 100 feet. These descriptions are not adjusted regression estimates. `render_density_drivers.R` shows the five largest upward one-at-a-time switches for each outcome/sample, while retaining all trials in the download. Ranking influential cases uses outcome estimates explicitly and is for diagnosis only.

## Findings

Burnett's disagreements are the main source of the multifamily DUPAC change. Switching those eight pairs (113 projects) alone changes DUPAC from −17.36% to +1.05%. Keeping their original direction while switching all others gives −16.39%, compared with +2.16% under fully raw ordering. Burnett–Fioretti alone (27 projects) changes −17.36% to +0.14%. Nevertheless, deleting all Burnett comparisons (157 projects, including agreements) leaves −14.49% under original ordering. Sensitivity to his labels is different from claiming his observations alone generate the negative result.

Multifamily FAR is sensitive to both Burnett and Hopkins. Switching only Burnett's disagreements changes −12.85% to −3.46%; switching only Hopkins's changes it to −3.54%. The leading individual FAR switches are Burnett–Fioretti, Hopkins–Waguespack and Hopkins–Michele Smith. All-construction changes are more dispersed: Dowell–King leads both measures, followed by different combinations involving Waguespack, Michele Smith, La Spata and Burnett–Fioretti. Removing Burnett leaves original all-construction FAR and DUPAC at −8.93% and −12.96%.

## Unresolved Assessor count conflict found during diagnosis

Two of the four multifamily Burnett–Fioretti projects within 100 feet have conflicting recorded unit counts. In `data_raw/construction_review/commercial_valuation_data.csv`, rows 60951 and 60952 (2024) report 81 units each for 1100 and 1048 W Madison. Rows 83214 and 83215 (2021) report 9 and 24 units. Building/land areas remain exactly 16,094/4,599 and 35,051/10,000 square feet. Current DUPAC is consequently 767.20 and 352.84. The renderer extracts all preserved commercial history for the nearest projects in the leading DUPAC pair, with original source row numbers.

The earlier recorded review, `construction_review_history/records/commercial_unit_adjudication_evidence.csv`, explicitly records these changes but accepts the 2024 report without a unit-review requirement. Production `prepare_new_construction/code/select_assessor_buildings.R` calculates apartment-unit sums correctly and selects the most recent assessment. This is an unresolved source-selection/measurement-coverage problem, not an arithmetic error newly introduced in the robustness code.

Recorded new-building permits 100445820 and 100444350 identify 4 and 3 N Aberdeen, respectively, and describe four-story residential buildings with ground-floor retail and garages. They contain no dwelling-unit count. The same raw permit file and earlier review connect those permits to these parcels. The current 2012 construction dates coincide with permit issuance; completion also deserves checking.

Evidence consulted September 15, 2026:

- [YoChicago building profile](https://yochicago.com/madison-aberdeen-place-apartments-3-n-aberdeen-west-loop/) describes four buildings and assigns 18 and 9 apartments to 3 and 4 N Aberdeen, with 54 more at 20 and 22 N Aberdeen. It reports 2013 completion for the first two. Its reported 18 also differs from the older Assessor's 24, so the older vintage should not simply be adopted without reconciliation.
- [Original leasing agent, March 2014](https://www.luxurychicagoapartments.com/blog/pre-leasing-begins-madison-aberdeen-place-west-loop/) identifies two 27-unit buildings at 20 and 22 N Aberdeen and a leasing center at 1100 W Madison.
- [Buyer's acquisition announcement, April 2017](https://www.taylorjohnson.com/tj-news/waterton-acquires-81-unit-apartment-community-in-chicagos-west-loop/) identifies an 81-unit Madison Aberdeen Place community, renamed The Aberdeen West Loop.

Together these suggest that the 81-unit community total has been repeated on individual-building measurements. That is an inference from the source coverage and address links, not a fully adjudicated replacement. No unit or year corrections, exclusions or manuscript changes are made here. Resolve the building-level measurements before interpreting the entire DUPAC ordering sensitivity as evidence about scores.
