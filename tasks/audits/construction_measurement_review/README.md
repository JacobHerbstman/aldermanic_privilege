# Construction measurement review

## September 15: adopted decisions

Jacob approved the recommendations for the same 87 records. Keep 65 after the
specified corrections and two Sawyer records with an explicit qualification;
leave 20 outside the common FAR-and-DUPAC sample. The latter comprise six
non-new-construction records, one partial building, one lodging building, two
pre-period buildings, one duplicate and nine unresolved records. Among the
retained observations, 25 counts, 13 years and one lot change.

The existing `new_construction_cleaning/output/recorded_building_changes.csv`
contains 50 new rows and two revised decisions. The other 35 reviews need no
additional override. All 87 review rows now have `adopted = TRUE`. The ordinary
construction cleaner applies these decisions at its existing single join.
No production scripts or tasks were added.

The audit comparisons use the pre-adoption project ledger, boundary summary and
correction table preserved in `records/`. This keeps the original discrepancies
visible after correction. Analysis estimates and the manuscript have not been
rerun. Earlier sections below record the findings and recommendations at each
earlier stage; their statements about unadopted decisions describe that stage.

## September 15: initial review of all 87 count disagreements

The latest [count-and-land review](count_land_review.md) records a recommendation for every one of the 87 projects with changed counts and unchanged floor/lot measurements. It recommends keeping 45 count-and-land choices, correcting 24, removing six conversions/additions/renovations and one obsolete duplicate, and treating eleven as unresolved or provisional. The corresponding main-sample counts are 15, 6, 4, 0 and 2. Twenty of these same records also have timing questions and three have possible inherited floor areas, explicitly distinguished from count-and-land support.

The manually written [evidence table](count_land_review.csv) preserves assessment row IDs and history, exact permit descriptions, external sources and caveats. All proposals remain unadopted. The existing renderer validates exact coverage and current values, produces its data report, and displays all cases before the earlier audit stages. The earlier findings below are preserved as the research history; their case recommendations are superseded where this 87-case review supplies new evidence. No production measurements, regressions or manuscript were changed.

## September 15: general rules and reconciliation

The population check and its limits are explained in [source_rules.md](source_rules.md). It accounts for the selected values of all 13,677 retained projects without changing them. It also exposes substantive gaps: a complete set of selected construction cards may omit older buildings on the same parcel; commercial sources can mix older areas with later counts; development-record apartment fields and permit application years do not independently establish completed-building counts or years.

The original 218 flags are reconciled with existing evidence in `output/flag_reconciliation.csv`. The reconciliation reads every existing recorded decision for the subject raised by its flag; these 64 readings are preserved in `prior_decision_review.csv`, with the exact earlier record references, evidence and caveats. Earlier row references include the header row. This is an audit record, not a new production correction file.

| Present evidence status | Citywide | Main 500-foot sample |
| --- | ---: | ---: |
| Confirmed count error; correction not applied | 8 | 5 |
| Current count supported by recently reviewed evidence | 10 | 9 |
| Earlier documented decision addresses the flag | 47 | 12 |
| Already outside the common FAR and DUPAC sample | 6 | 0 |
| Case reviewed; measurement question remains | 16 | 9 |
| Earlier decision leaves the flagged question open | 7 | 2 |
| Source disagreement still needs reconciliation | 72 | 6 |
| Repeated values or unusual ratio only; not adjudicated | 52 | 17 |
| Total original flags | 218 | 60 |

These categories partition the original flags. The 47 earlier decisions are reused for their stated subject; they are not 47 independent new certifications of units, floor, lot and year. For example, Belmont's separate 27-home permit revisions explain its repeated counts, and the documented commercial space at 440 N Halsted explains its unusually high gross floor area per dwelling. The old 490-home decision for Optima Signature does not settle the separate shared-lot question.

Eight of the 64 old decisions still choose a source without independently settling the count. One of these, 3300 W Lawrence, also appears among the sixteen recently reviewed open questions; the other seven have their own row above. Expiry of a conflicting permit is not evidence that the selected newer apartment count is correct. The former automated commercial review is attached as context, and its `unit_count_resolved` label is not treated as verification.

This completes the first source-rule check and an evidence reconciliation of the original flags, not the adjudication of every open case. No new errors, exclusions or corrections are inferred solely from a diagnostic comparison. Production data and estimates are unchanged. No unflagged-building sample has been drawn or reviewed.

To rebuild only the reconciliation and HTML against the completed audit checks, run from `code/`:

```sh
make -o ../output/project_screen.csv -o ../output/measurement_pairs.csv \
  -o ../output/commercial_source_rows.csv -o ../output/flagged_permits.csv \
  -o ../output/source_rule_checks.csv -o ../output/source_rule_summary.csv
```

The pins hold the existing screen and population-check outputs fixed for this report-only build. The ordinary dependencies remain declared in the Makefile.

The Madison errors came from accepting apartment counts for a whole development alongside floor and land areas for individual buildings. The current production data have 81 apartments at both 1100 and 1048 W Madison; the evidence supports 9 and 18 respectively. Both observations are in the main 500-foot sample on Burnett's side. This audit investigates similar problems across the citywide construction population before further estimation. It changes no production values, estimates or manuscript text.

## Exactly what the Assessor reports

These are entries in the preserved `data_raw/construction_review/commercial_valuation_data.csv`. Data-row numbers exclude the header. The year of assessment and reported year built are different fields.

| Address | Data row | Assessment | Apartments | Building sqft | Land sqft | Year built |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1100 W Madison | 83214 | 2021 | 9 | 16,094 | 4,599 | 2014 |
| 1100 W Madison | 60951 | 2024 | 81 | 16,094 | 4,599 | 2012 |
| 1048 W Madison | 83215 | 2021 | 24 | 35,051 | 10,000 | 2014 |
| 1048 W Madison | 60952 | 2024 | 81 | 35,051 | 10,000 | 2012 |

At 1100, the bedroom breakdown changes from nine three-bedroom apartments to 81 one-bedroom apartments. At 1048, it changes from 24 one-bedroom apartments to two studios, 19 one-bedroom, 54 two-bedroom and six three-bedroom apartments. Both the bedroom sum and the source's `tot_units` field give 81 in 2024. This rules out our addition of bedroom categories as the explanation for these two count errors.

The buyer describes an [81-apartment community](https://www.taylorjohnson.com/tj-news/waterton-acquires-81-unit-apartment-community-in-chicagos-west-loop/). Building-level reporting distinguishes [9 apartments at 4 N Aberdeen and 18 at 3 N Aberdeen](https://yochicago.com/madison-aberdeen-place-apartments-3-n-aberdeen-west-loop/); exact-parcel construction permits link those corner addresses to 1100 and 1048 W Madison. The other two buildings have 27 each, totaling 54 at 20–22 N Aberdeen. Thus 81 is consistent with the community total, and the old 24 at 1048 is also inconsistent with the building evidence. Simply restoring every old Assessor count would be wrong.

## Why our earlier review missed this

The historical review code in `../construction_review_history/code/build_commercial_unit_adjudication_evidence.R` chooses the newer count when the two vintages disagree and no permit count supports the older one. It does not require evidence supporting the newer count in that branch. It then marks a positive selected count as resolved. Both Madison records passed that branch despite no permit count supporting either vintage.

The active producer, `../../prepare_new_construction/code/select_assessor_buildings.R`, selects the latest commercial assessment, retains the older measurements for comparison, and fills missing floor or land areas from the older assessment under membership checks. It does not reject an apartment-count change merely because floor area, lot area and parcel membership stay identical. No committed correction changes the Madison counts. The weakness is treating a source-selection preference as substantive verification of the building.

A second mechanism affects class-297 development records: the active producer uses the Assessor apartment field when positive and uses a permit count only to fill a missing value. An explicit disagreement between these counts therefore survives. Three retained development records disagree with their available permit count. At 1400 W Monroe, the stored permit count itself is incomplete: it retains 42, whereas a March 2021 revision explicitly reduces the building to 38. That revision agrees with the current developer description.

The residential parser correctly maps the preserved text values Two, Three, Four, Five and Six. The preserved historical file contains decoded text, not numeric category codes. A category-code decoding error was considered and ruled out for this source.

## Findings

`review_notes.csv` records evidence and recommendations for 34 projects. These are audit findings, not adopted manual corrections. The report shows the findings separately from the automated flags and links each to its current project identifier.

Eight records have a supported count error: the two Madison buildings; 1607 W Waveland; 922 W Washington; 2051 N Clifton; 550 N Ogden; 5914 N Lincoln; and 1400 W Monroe. Five of these eight enter the current main 500-foot sample. The errors go in both directions.

- Waveland has six original apartments. An October 2023 permit turns the restaurant into two additional apartments. The selected eight-unit count incorrectly assigns those additional apartments to the original construction. Its currently assigned year also needs reconciliation.
- Circa 922 has 104 newly built apartments plus 45 pre-existing lofts. The selected 149 is the combined community. The [developer](https://workwithfocus.com/portfolio/circa-922/) and original permit agree on that distinction; floor/lot coverage and the selected year still need review.
- Clifton's [original sale](https://lowegroupchicago.com/properties/2051-n-clifton-avenue) and new-building permit report three apartments. The selected assessment says two; later assessments say three with unchanged floor/lot measurements for the new building.
- Ogden's original permit and completed condo listings report eight apartments; the development assessment says Six. Lincoln similarly has six apartments against a development assessment reporting four.
- Monroe's development assessment says Six. The original permit specified 42, but revision 100896420 explicitly reduces the count to 38, matching the [developer](https://jkequities.com/project/1400-west-monroe-street-chicago-il/). The [builder's March 2022 completion announcement](https://summitdb.com/summit-design-build-completes-1400-monroe/) still gives 42. Recommend 38 on the revision evidence; reconcile the selected 2021 construction year and floor-area coverage before adoption.

At 3408 N Milwaukee, six apartments are better supported than the selected two by the original permit and earlier same-building assessments. Its selected 2012 construction year precedes the July 2013 new-building permit. This remains a strong count/year conflict requiring completion evidence, rather than a fully resolved observation.

Ten additional reviewed records have evidence supporting the current count despite the screen. These include 321 W Evergreen (15), 2109 S Halsted (18), 1123 W Van Buren (198), and 1 S Halsted (492). At 3332 W Irving Park the original three apartments are correct even though a later commercial-space conversion creates a fourth. A count finding does not certify every other variable.

The remaining notes identify seven other unresolved measurement cases and eight records whose large land areas repeat on a nearby project. None is automatically excluded. Nearby repeated land may reflect a shared development site even when the individual records have different tax parcels. Mixed-use buildings can legitimately have large floor area per apartment.

## Population and screens

The audit starts from all 13,677 retained citywide construction projects dated 2006–2022: 810 commercial-source and 12,867 residential-source projects. The main sample contains 4,023 projects within 500 feet satisfying the existing common FAR/DUPAC and control requirements. Screen definitions use measurement evidence, not alderman identity, scores, estimated effects, or statistical significance. Sample membership and ward information are attached for reporting after the screens are defined.

1. Any commercial apartment-count change between the selected observation and the 2021 comparison assessment; separately identify unchanged floor/lot measurements and stable parcel membership.
2. Historical reviews that cleared conflicting counts without permit support for either vintage.
3. Residential count changes on the same parcel/card with identical floor and lot measurements and construction years within two years. Single-family assessment classes use the production one-dwelling definition. This compares component cards; it does not reinterpret tied or replaced cards as new buildings.
4. Class-297 development counts that disagree with an available positive permit count in the current producer.
5. Nearby equal apartment counts of at least eight, within 1,000 feet and a two-year construction window; nearby repeated land of at least 20,000 square feet within 1,000 feet across all construction years.
6. Multifamily floor area below 300 or above 5,000 square feet per dwelling; shared component parcel identifiers between retained projects.

These round thresholds identify leads for review, not new cleaning rules. The updated screen flags 218 projects, 60 in the main sample. Flags overlap and are not counts of errors. Of the residential projects, 12,054 have a comparable history without a unit conflict, 35 have a conflict, and 778 have no comparable history under the exact measurement/card criteria. Neither an unflagged record nor missing comparable history proves its measurements correct. This audit does not establish that all excluded candidates, dates, land areas or buildings have been independently verified.

## Files and reproduction

The subsequent [general-rule review](source_rules.md) is the first stage Jacob requested before flag reconciliation. Its population tables are `output/source_rule_checks.csv` and `output/source_rule_summary.csv`. They reconstruct the source arithmetic and record historical/permit comparison coverage. They do not change the 218 flags, adopt corrections, or select an unflagged-building sample.

### Coverage assessment before further manual review

The 218 are not an exhaustive universe of potential problems. Of these, 126 have a source disagreement or an earlier review cleared without supporting unit evidence; the other 92 are flagged only by nearby repeats or unusual floor area per dwelling. Those two groups contain 32 and 28 main-sample projects respectively. A repeated measurement or unusual ratio alone does not justify reopening a building.

There are 64 flagged projects with existing recorded decisions (19 in the main sample), and 146 with an earlier commercial count review (40 in the main sample). These groups overlap. A full row of recorded measurements is not proof that every field received independent adjudication: the reason and evidence must address the particular new concern. None of the eight confirmed count-error projects has a case-specific entry in the current correction table. These errors therefore point to gaps in ordinary source selection and its earlier automated review; they do not invalidate all previous manual research.

Concrete limits of the current screens include:

| Coverage issue | Citywide | Main 500-ft sample |
| --- | ---: | ---: |
| Unflagged residential projects without a comparable historical assessment | 771 | 254 |
| Unflagged commercial projects without a positive older comparison count | 113 | 30 |
| Unflagged commercial projects with changing floor or lot measurements | 62 | 20 |
| Development records without an available positive permit count | 50 | 16 |

These counts overlap and do not add new errors to the review queue. The 62 area changes may be legitimate or previously corrected. Missing historical comparisons may result from replaced source identities, including finished condo records held in another source. Stable-but-wrong measurements can also pass, as can shared-site totals below the current thresholds. The screen starts from retained projects, so it cannot validate omitted construction. It is not a comprehensive new check of construction dates or geography.

The appropriate next stage is an assessment of the checks themselves: compare all required measurements across each relevant source branch, distinguish original construction from later changes, reconcile existing decisions with the evidence for each variable, and independently review a sample of unflagged records chosen without reference to regression results. The screen has not been validated on such a sample. Catching all eight currently known count errors is not evidence of a low miss rate, because some checks were added after those cases were identified. Do not adopt a blanket old-source/new-source preference or launch 218 fresh manual adjudications from this list.

### Reproduction details

- `code/screen_measurements.R` reads existing production outputs and recorded sources and writes four audit datasets.
- `code/check_source_rules.R` checks the general measurement rules across the retained population and writes the two source-rule datasets.
- `output/project_screen.csv`: one row per retained project, including each check, main-sample membership and existing decision reason.
- `output/measurement_pairs.csv`: nearby repeated measurements, keyed by the two project identifiers.
- `output/commercial_source_rows.csv`: original commercial field values for flagged source records, retaining source data-row numbers.
- `output/flagged_permits.csv`: all available permit dates for flagged projects' exact component parcels and addresses associated with their recorded permit links. `exact_component_pin` identifies exact-parcel evidence. An address candidate is not automatically the same building or construction episode.
- `code/render_measurement_review.R` produces the searchable HTML report from these tables and the reviewed evidence notes. The `report/` files describe keys, missingness and reproducibility fingerprints and are generated with SaveData/ReportData.

The saved analysis snapshot and current analysis output have the same SHA-256: `ab32f0f635f8a117d728aa38530894e9fa0b9bb982980d2810241645e0cbed36`. Every analysis observation matches the current citywide ledger exactly on units, floor area, land and construction year. The code was written against branch `score_robustness`, following commit `32c094b8`.

Ordinary reproduction uses `make` in `code/`, following the explicit source edges. For this review, the existing production outputs were held fixed because recursive freshness checks would otherwise rebuild production while the data review is pending. The audit itself was rebuilt through Make:

```sh
cd tasks/audits/construction_measurement_review/code
make \
  -o ../../../prepare_new_construction/output/preferred_new_construction_project_ledger.csv \
  -o ../../../prepare_new_construction/output/assessor_buildings.csv \
  -o ../../../prepare_new_construction/output/assessor_measurement_records.csv \
  -o ../../../prepare_new_construction/output/preferred_new_construction_boundary_scope.csv \
  -o ../../../prepare_new_construction/output/building_permit_evidence.csv \
  -o ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv \
  -o ../../../prepare_construction_assessor_history/output/residential_assessor_history.parquet \
  -o ../../../download_construction_assessor_history/output/construction_condominium_history.csv \
  -o ../../../new_construction_cleaning/output/recorded_building_changes.csv
```

During the first-stage source-rule review, the four existing screening outputs were also held fixed with `-o`: `../output/project_screen.csv`, `../output/measurement_pairs.csv`, `../output/commercial_source_rows.csv`, and `../output/flagged_permits.csv`. This preserved the earlier flag definition while the new source-rule checks and report were built.

No regressions or paper rebuilds are part of this audit. The outstanding work is to resolve the consequential count and coverage conflicts, then implement approved corrections once in the ordinary cleaning path and verify their effect on the dataset before re-estimation.

### Adoption verification

The construction Make build using preserved inputs passed. All 67 retained
review records have the specified units, years, reported areas and usable
locations. The other 20 are outside the common FAR-and-DUPAC sample: thirteen
leave the construction ledger and seven remain with limited eligibility.
Citywide, the ledger changes from 13,677 to 13,664 rows and common density
eligibility from 13,635 to 13,615. Every unreviewed building and boundary row
is unchanged. The 740 unrelated earlier correction rows are unchanged.
The ordinary producer, the audit rebuilt from its before-adoption sources,
and the manual-decision task all passed unchanged second Make checks.
Estimation inputs, regressions and the manuscript have not been rerun.

### Follow-up: 13 influential observations

After the approved corrections and density rerun at commit `35135702`, Jacob
requested a check of the five most influential observations supporting the
negative multifamily DUPAC result and several other influential observations.
`influential_building_review.csv` records 13 reviews, including three observations
whose removal makes the multifamily estimate more negative and one influential
single-family observation. None belongs to the earlier 87-case count-disagreement
review. These are recommendations, not adopted production changes.

The review finds additions or conversions, residential counts apparently including
commercial space, construction-start years used as completion years, and unresolved
building/parcel groupings. It also finds support for the current 500 W 66th and
1164 W Madison measurements. Exact counts, dates, reported areas, confidence,
remaining questions and sources are recorded separately for each observation.
An area repeated by the Assessor is not labeled independently verified. Selection
by statistical influence cannot establish the population error rate or justify
exclusion without substantive evidence.

The CSV preserves current values from
`tasks/new_construction_analysis_data/output/new_construction_analysis_data.csv`
(SHA-256 `d78388a3713ed8224cd94bbdf5413d038902a080deb1038ceff49ccc6f2edebf`)
and the already-produced omission results in `density_ordering_drivers/output/`.
It includes source-row identifiers and excerpts from the preserved commercial,
residential and permit histories. Linked web evidence was reviewed September 15,
2026. Production files and estimates are unchanged by this follow-up.

Only the report needs rebuilding to display these recorded judgments. Run `make`
in `code/`, holding the six existing audit datasets fixed with `-o`:
`../output/project_screen.csv`, `../output/measurement_pairs.csv`,
`../output/commercial_source_rows.csv`, `../output/flagged_permits.csv`,
`../output/source_rule_checks.csv`, and `../output/source_rule_summary.csv`.
The same renderer generates the deterministic report for the new CSV. No new
production task or script was introduced.
