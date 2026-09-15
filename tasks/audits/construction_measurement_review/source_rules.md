# Do the four measurements describe the original construction?

This is the first stage Jacob requested on September 15: examine the general production rules across the retained population before reconciling the flags. The production code and data are unchanged. No unflagged-building sample has been drawn or reviewed.

The audit separates **where a number comes from** from **whether it measures the original building**. The first question can usually be answered by reconstructing the code's calculation. The second also requires the correct building identity and timing. Agreement with the Assessor is evidence of provenance, not a guarantee that the reported count or area has the intended coverage.

## What the production rules do

| Source route, excluding recorded corrections | How the four values are obtained | What the rule establishes | What it does not establish |
| --- | --- | --- | --- |
| Ordinary residential building | Select a reported card under the 2022/2025/later priority. Keep its units, floor, land and year, subject to the explicit floor-recovery and permit-year rules. Single-family classes count as one dwelling. | Measurements trace to one recorded card; recovery sources can be identified. | A later assessment can include an addition or conversion. An unchanged parcel number or year does not prove unchanged construction. |
| Multiple cards on one parcel | Select an assessment containing the selected construction episode's cards; sum their units and floor area, take the repeated parcel land once, require one construction year. | Selected components are contemporaneous; land is not added once per selected card. | The selected episode need not comprise every building on the parcel. Card identity and any buildings outside the episode need checking before calling the lot exclusive to the new construction. |
| One building across tied parcels | Require one assessment and complete recorded shares; use the repeated building count/floor once and add each parcel's reported land once. | Internal agreement and accounting of the declared component parcels. | Tax shares summing to one do not independently prove that every land value is a separate lot rather than a repeated shared site. |
| Completed condominium building | Require complete unit membership, remove parking/common-area records, count remaining dwellings, and use agreeing building-wide area/year fields once. | Counts and measurements are from the completed condo source and one selected assessment. | The source's building-wide fields can still contain errors or later changes. |
| Development record (class 297) | Prefer its positive apartment field; otherwise fill from one available permit-chain count. | Reproduces the selected fields and the stated count fallback. | The apartment field is documented for small multifamily classes, not as a general completed-development count. Conflicting counts and revisions are not reliably resolved by positivity or precedence. |
| Commercial assessment | Select the latest assessment. Recover missing floor or land from older records when parcel membership agrees; sometimes add earlier component measurements. | Identifies the selected assessment and numerical recovery source. | Latest counts can cover a larger community or later conversion. Stable parcel membership alone does not establish that older land and newer building measurements describe the same original construction. |

The active rules are in `tasks/prepare_new_construction/code/select_assessor_buildings.R`; the common correction, completed-condo selection and density calculations are in `build_construction_data.R` in that task. The **494 retained projects with recorded corrections** are treated separately in this audit. Checking that their stored numbers were applied is not a new review of the evidence behind those decisions.

The Assessor's [field dictionary](https://github.com/ccao-data/model-res-avm#features-used) defines `char_apts` for classes 211/212, building area as exterior-measured building square footage, and land as property land area. Its [residential source description](https://catalog.data.gov/dataset/assessor-single-and-multi-family-improvement-characteristics) describes improvement-level data primarily collected for valuation and reporting. These definitions support using the fields as reported measurements; they do not turn a current assessment into a certificate of original completion.

## Population findings

The check uses the same 13,677 retained projects as the earlier screen. It reconstructs selected rows separately for ordinary residential, commercial and completed-condo sources. It checks documented corrections against the current output. The source-route summary is generated in `output/source_rule_summary.csv`, and `output/source_rule_checks.csv` records the result for every project.

For the 614 ordinary commercial observations without recorded corrections, 452 reproduce all four measurements from the identified selected assessment. The other 162 use earlier area data: 158 take earlier reported values and four aggregate earlier component records. Twenty of these 162 have a different unit count in the earlier area source. The numbers reproduce the stated rules; the changed count shows why parcel membership alone is not independent same-building evidence. This is not a new adjudication of those twenty cases.

The existing one-year residential correction affects 636 retained projects, including 201 in the main sample: when the Assessor year is exactly one year before the first matched permit application, the code substitutes the application year. This is a construction-year proxy, not verified completion. The audit has not established that those dates are wrong or authorized changing them.

Five ordinary records recover floor area from a 2025 row while retaining a 2022 assessment-year label. Their other values are compatible under the implemented recovery rule, so this is a provenance-label defect, not evidence that those five density measurements are wrong. The audit uses the supplying row's year when measuring assessment lag.

The 64 development-route observations without recorded correction or completed-condo replacement include 62 that use a positive development-record apartment value and two that fill a missing apartment value from a permit count. The available field's documented scope and the known Ogden/Monroe contradictions undermine treating all positive development values as verified completed-building counts. No blanket exclusion or substitution is applied.

All 339 ordinary multiple-card observations reproduce the selected cards' unit/floor sums and the repeated parcel land. However, **40 have additional positive-area cards in that same assessment that are not counted in the selected episode**, including 13 main-sample properties. These additional cards mostly have construction dates before the study period. This exposes a distinction between covering the selected episode and covering the full parcel. The extra cards may represent older buildings or obsolete records; this stage has not adjudicated them. The audit's `complete_components` field refers to the selected component checks, not proof that the parcel contains no other buildings.

All 321 ordinary tied-parcel observations reproduce their selected unit/floor value and parcel-land sum, and all 37 completed-condo observations without recorded overrides reproduce the completed-source count and area/year fields. All 494 retained correction records reproduce their explicit current numerical values. Across all 13,677 observations, the measurements are accounted for by the stated source rules or correction inputs. That is a successful accounting check, not a claim that all measurements are correct.

## What the chronological check can establish

The script also retrieves permit numbers sharing each retained project's recorded parcel base (the first ten digits) from the full preserved permit file. It keeps all dates rather than limiting the search to a window around the selected construction year. Residential new-building descriptions are distinguished from text describing later conversions or count changes. A single completed new-building permit issued after the selected year is recorded as a timing lead, not automatically the correct building or proof of a wrong year.

These are candidate links. One parcel may contain several buildings or successive construction episodes. Some later conversions are already correctly omitted from the original construction count. Missing permit matches can reflect changed parcel identifiers or incomplete PIN information. No permit count or date is automatically substituted. No new cases are added to the 218 flags by this audit.

The full-date search finds permits sharing the parcel base for 7,295 projects and at least one full residential new-building candidate for 4,002. There are 505 projects with one completed candidate issued after the assigned construction year, and 62 with later residential conversion/count-change text. These are **candidate chronology comparisons, not 505 wrong years or 62 inflated counts**. Matching the original building and distinguishing amendments from later construction is required before either interpretation. The conservative text screen also misses permits whose descriptions mix new construction with additions or existing buildings.

Likewise, the historical comparisons now allow counts, floor, lot and year to change rather than requiring exact areas and a two-year window before recording any disagreement. A replaced building can legitimately change every field. These comparisons reveal the reach of the general checks and where the original-construction assumption needs evidence; they are not a list of independently identified errors.

## Implication for the next stage

The remaining weakness is substantive: **one source record, positive measurements, or an agreed parcel list does not establish one original construction episode**. We have specific source rules to examine when reconciling the existing flags: latest commercial selection, older-area recovery, development-record counts, permit revision interpretation, and the application-year substitution. The established numerical aggregation and once-only correction step should be preserved unless their actual behavior is contradicted by this check.

The first reconciliation of the 218 flags is now recorded in `output/flag_reconciliation.csv`, using the specific subjects and evidence of earlier decisions. It does not adjudicate every remaining property. An independent sample of unflagged buildings remains deferred until Jacob discusses it.

## Reproduction

`code/check_source_rules.R` is an audit producer, not a new production cleaning stage. Its Make rule declares the preserved residential history, completed-condo source, recorded correction file, existing construction outputs and full permit file. The two CSVs are saved with SaveData reports. The existing report renderer shows their summaries.

This review holds production inputs fixed, using Make's `-o` arguments documented in the README plus the residential-history, condominium-history and recorded-correction upstream outputs. The ordinary Make dependency graph remains intact. No production source priority, manual decision, estimate or manuscript is changed by the audit.
