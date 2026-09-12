# Remaining dependence on manual construction decisions

## September 11 update: manuscript description

Following the literal prerequisites of `preferred_new_construction_project_ledger.csv`
and `preferred_new_construction_boundary_scope.csv` in the construction-cleaning
Makefile identifies **26 active adjudication CSVs containing 809 rows**. All 26
files are tracked in Git. This count covers the construction identity,
measurement, eligibility and location inputs to those two outputs. It excludes
separate downstream building-type and zoning reviews.

These are recorded entries, not 809 distinct projects or independent corrections.
A project can occur in more than one table; entries also record source selection,
confirmation, exclusion, component membership and location. For example, the
construction-year table contains 191 rows, the residential building-correction
table contains 55 source-to-building rows identifying 51 final buildings, and the
commercial measurement-correction table contains 60 rows.

The manuscript's statement that 52 citywide projects, including 26 within 500 feet,
required manual resolution is not a verified total for the current cleaning
process. The decisions are now stored in multiple committed tables. Neither the
809 entry count nor a count from a single commercial decision category should
replace 52 as a count of unique manually corrected projects. A defensible paper
description should explain the decisions and their supporting records without
claiming a distinct-project total that this inventory does not establish.

The September 9 counts below describe the earlier state and are preserved as
history.

## September 9 update: current building workflow

The current audit starts from the combined candidate Make targets, rather than
counting every historical file in the adjudication directory. Before cleanup,
the active path required 30 decision CSVs with 685 rows. Consolidation leaves
26 active CSVs with 666 rows. These are recorded inputs, not 666 distinct
buildings or independent numeric corrections. Source links, retained-source
confirmations, exclusions and location decisions all contribute rows.

The three residential retained-building inputs now have one owner:
`apply_residential_building_corrections.R` reads
`residential_building_corrections.csv` after `residential_selected_assessments.csv`.
Its 53 source-to-building rows identify 49 final buildings. Two old source records
are absent from the current candidate population and are not copied into the new
active table. Thirteen exclusions or commercial replacement links from the same
older files are in `residential_source_decisions.csv`; commercial replacement
measurements are validated from the finished commercial output, not copied into
that residential table.

Commercial field and unit-definition corrections are consolidated from 57 rows
in two inputs to 52 applying rows in one table. Eight whole-project decisions
already superseded by component decisions are preserved separately. The commercial
producer rejects overlapping source coverage among its component, measurement
and whole-project decisions. The empty condo-link override reader and the two
early commercial unit corrections were removed. Final commercial cleaning owns
the unit definition; preliminary source records preserve reported values.

The preliminary commercial land heuristic that used low implied density was
removed from parcel discovery. The existing final reported-land selection remains
the owner of density land. Forty-seven obsolete map-derived numeric land values
were cleared from active decision tables; the approved source-reported selections
and external land decisions remain. This removes the sequence of applying a map
value and then replacing it with a reported value.

`calculate_construction_density.R` now calculates both density measures from
`new_construction_measurements.csv`'s selected building inputs, before final
geography. Final geography and combined assembly assert that construction year,
units, building area and land area agree with that saved measurement dataset.
Identity-related year corrections remain earlier because they help distinguish
old combined records from their individual successor homes. They are not applied
again to final measurements. The final residential correction producer rejects
an attempt to overwrite a field already reviewed during identity construction.

The completed local comparison preserves all 13,764 final project IDs and every
pre-existing project-ledger field, all 15,206 parcel membership rows, and the
complete boundary-distance table. The added `far` and `dupac` columns are calculated
from the same final measurements. Within 500 feet, eligibility remains 4,139 for
at least one density outcome and 4,128 for both. The preliminary commercial source
file changes two raw unit values and three raw land values because those early
adjustments have been removed; none changes the final reviewed measurements.
This is an output-preserving cleanup of the candidate building dataset, not a
new regression run or a claim that a fresh clone already reproduces the paper.

Verification completed September 10 on GNU Make 3.81. The full task build passed;
an unchanged second build reported nothing to do. A parallel build restored a
missing measurement CSV and a missing secondary residential-component CSV with
identical saved bytes. All 268 fixed review cases remain closed and the current
question table has zero rows. The 53 consolidated residential and 52 commercial
measurement-decision rows were checked against their original recorded values
and evidence, allowing only the explicitly cleared obsolete map denominators.

Earlier counts and proposed reductions below describe the September 7 recovery
and are preserved as history; they are not the current active-input inventory.

---


September 7, 2026. Agent-drafted audit, not a replacement cleaning specification.
Jacob requested general rules wherever possible and case-specific ledgers only
as a last resort. No decisions or production observations were changed here.

## What is currently recorded

The restored adjudication directory contains 51 CSVs. Three are preserved
source/derived snapshots rather than decision ledgers. The other 48 contain
1,088 review or decision rows. These are not 1,088 distinct projects, new
judgments to request, or proven exceptions: the same project can appear at
multiple stages, some entries merely repeat selected source measurements, and
some record supporting evidence. Forty of the 48 files (1,041 rows) have literal
read references in the currently restored R/Python scripts; eight (47 rows)
await restored consumers or a decision about their role. A missing current
consumer is not evidence that the input can be deleted.

## Two measured opportunities

The 273-row external-review layer agrees with its preceding building-type rule
on 250 rows and differs on 23. It changes units or building area for 32 rows
(28 unit changes and 12 area changes, overlapping). Taking the union, 51 rows
need an explanation for an incremental classification/value decision and 222
agree with the preceding adjudicated input. This is not a raw-data comparison:
the preceding input already includes other manual decisions. Agreement therefore
identifies candidates for removing redundant external overrides, not 222 proven
manual-free projects. The two already-suppressed rows also count as agreement.
Completion, membership and land validity require their own checks.

The 42-row manual episode ledger contains 39 `retain_card_inventory` decisions.
For all 39, the preserved final disposition, units and building area already
equal the corresponding computed rule values. Thirty-eight request suppression
of matched successors and one requests none. The other three episode entries
rely on separate value overrides. These checks identify a common-rule candidate,
but do not establish that all matches or suppression choices are valid. The
expanded two-year graph must be reconciled before that candidate is adopted.

The audit CSVs `manual_review_increment.csv` (273 rows) and
`manual_episode_increment.csv` (42 rows) preserve the row-level comparisons.
Their Make targets produce keyed standard reports. Unchanged second builds run
no producers. These two populations overlap and their counts must not be added.

## General rules to investigate before another case ledger

| Repeated decision | Evidence of potential reduction | What must be established before replacing it |
|---|---|---|
| Retain contemporaneous cards and suppress matched successors | 39 of 42 episode rows already use computed disposition and physical values | Consistent unit definitions, unique valid matches, unmatched buildings kept separately, and nonduplicated land |
| Reapply external type/value confirmations | 222 of 273 rows add no different type or numeric value at this layer | Independent class/permit/physical-building evidence and eligibility checks, without inheriting an earlier unsupported decision |
| Copy named Assessor vintages into final fields | In the main 80-row commercial ledger, 61 rows retain/split/merge projects; 59 of these cite the 2021 or 2024 commercial source for building area | Programmatic source-row linkage and a consistent rule for choosing vintages and project membership; source labels alone do not validate the chosen vintage |
| Exclude construction outside the study period | 17 of 44 unresolved residential source dispositions cite no supported 2006–2022 construction year | Reconstruct the supported-year evidence programmatically, then apply the study-period restriction to the full population |
| Recalculate zoning after a corrected construction year | 12 explicit corrected-year zoning assignments | Reconstructed ordinance links and dates plus a single date-selection rule; ambiguous source interpretation remains separate |
| Resolve transient units and construction dates | Five current graph nodes disagree with the later unit rule; several ledgers describe transient or stable later Assessor reports | One consistent temporal rule, checked on all eligible histories, with redevelopment and additions distinguished from reporting changes |

These are candidates, not counts of successfully removed manual decisions.
No new numeric override or project-ID exception was added for them.

## What remains uncertain

The 51 external-review differences include repeated, potentially programmatic
problems: old-building/accessory cards, expired/unbuilt projects, mixed-use unit
counts, duplicate cards, and counts copied from a larger development. Some
notes explicitly say the larger development count does not identify the parent
phase, yet the external-review application writes a nonmissing external count.
For example, the intermediate stage substitutes 70 for a 43-card parent and 47
for a 39-card parent. The inspected frozen final file instead contains 43 and 39.
This is a scope/ordering issue to resolve generally; it is not evidence that
those larger counts are currently in the paper. A support flag or a broad
listing count alone should not be used as an automatic correction.

The cases most likely to need further source evidence concern completion dates,
which buildings a source actually describes, shared land across phases, and
ambiguous ordinance geography. A general unresolved-status rule may handle
some without a case-specific value. We have not yet demonstrated which remaining
cases cannot be resolved programmatically. Thus the count of **unavoidable new
manual judgments is not established**; neither 51 nor 1,088 is that count.

The six approved density-exclusion records remain intact. No conclusion here
reinstates Lenox or the three unresolved land-denominator projects.

## File inventory

Rows are counted from the complete CSVs. All links point to the restored inputs.

| Decision/review input | Rows |
|---|---:|
| [assessor_default_project_exceptions.csv](../audits/construction_review_history/records/assessor_default_project_exceptions.csv) | 6 |
| [assessor_default_site_reviews.csv](../audits/construction_review_history/records/assessor_default_site_reviews.csv) | 56 |
| [commercial_component_overrides.csv](../new_construction_cleaning/output/commercial_component_overrides.csv) | 47 |
| [commercial_cross_family_decisions.csv](../new_construction_cleaning/output/commercial_cross_family_decisions.csv) | 21 |
| [commercial_field_decisions.csv](../audits/construction_review_history/records/commercial_field_decisions.csv) | 18 |
| [commercial_manual_decisions.csv](../new_construction_cleaning/output/commercial_manual_decisions.csv) | 80 |
| [commercial_parcel_year_overrides.csv](../audits/construction_review_history/records/commercial_parcel_year_overrides.csv) | 2 |
| [commercial_semantic_decisions.csv](../audits/construction_review_history/records/commercial_semantic_decisions.csv) | 29 |
| [commercial_verified_case_review.csv](../new_construction_cleaning/output/commercial_verified_case_review.csv) | 10 |
| [corrected_year_zoning_decisions.csv](../new_construction_cleaning/output/corrected_year_zoning_decisions.csv) | 12 |
| [density_denominator_decisions.csv](../audits/construction_review_history/records/density_denominator_decisions.csv) | 6 |
| [early_multicard_manual_review.csv](../audits/construction_review_history/records/early_multicard_manual_review.csv) | 13 |
| [eligibility_manual_exceptions.csv](../new_construction_cleaning/output/eligibility_manual_exceptions.csv) | 1 |
| [final_project_overrides.csv](../audits/construction_review_history/records/final_project_overrides.csv) | 12 |
| [historical_address_matches.csv](../new_construction_cleaning/output/historical_address_matches.csv) | 4 |
| [historical_coordinate_year_corrections.csv](../new_construction_cleaning/output/historical_coordinate_year_corrections.csv) | 2 |
| [historical_zoning_exact_preconstruction_support.csv](../audits/construction_review_history/records/historical_zoning_exact_preconstruction_support.csv) | 5 |
| [historical_zoning_recovered_ordinance_dates.csv](../audits/construction_review_history/records/historical_zoning_recovered_ordinance_dates.csv) | 1 |
| [historical_zoning_reviewed_events.csv](../audits/construction_review_history/records/historical_zoning_reviewed_events.csv) | 14 |
| [initial_commercial_unit_overrides.csv](../audits/construction_review_history/records/initial_commercial_unit_overrides.csv) | 2 |
| [multicard_cross_pair_decisions.csv](../audits/construction_review_history/records/multicard_cross_pair_decisions.csv) | 6 |
| [multicard_cross_project_suppressions.csv](../audits/construction_review_history/records/multicard_cross_project_suppressions.csv) | 2 |
| [multicard_external_web_reviews.csv](../audits/construction_review_history/records/multicard_external_web_reviews.csv) | 273 |
| [multicard_manual_episode_decisions.csv](../audits/construction_review_history/records/multicard_manual_episode_decisions.csv) | 42 |
| [multicard_manual_overrides.csv](../audits/construction_review_history/records/multicard_manual_overrides.csv) | 6 |
| [multicard_parent_pair_decisions.csv](../audits/construction_review_history/records/multicard_parent_pair_decisions.csv) | 12 |
| [multicard_year_overrides.csv](../audits/construction_review_history/records/multicard_year_overrides.csv) | 5 |
| [preferred_project_duplicate_overrides.csv](../audits/construction_review_history/records/preferred_project_duplicate_overrides.csv) | 17 |
| [project_manual_reviews.csv](../audits/construction_review_history/records/project_manual_reviews.csv) | 179 |
| [recovered_project_duplicate_pair_decisions.csv](../audits/construction_review_history/records/recovered_project_duplicate_pair_decisions.csv) | 9 |
| [recovered_project_zoning_overrides.csv](../audits/construction_review_history/records/recovered_project_zoning_overrides.csv) | 1 |
| [residential_additional_candidate_decisions.csv](../new_construction_cleaning/output/residential_additional_candidate_decisions.csv) | 10 |
| [residential_candidate_suppressions.csv](../audits/construction_review_history/records/residential_candidate_suppressions.csv) | 13 |
| [residential_class297_component_overrides.csv](../new_construction_cleaning/output/residential_class297_component_overrides.csv) | 2 |
| [residential_class297_exceptions.csv](../audits/construction_review_history/records/residential_class297_exceptions.csv) | 11 |
| [residential_overlap_decisions.csv](../new_construction_cleaning/output/residential_overlap_decisions.csv) | 28 |
| [residential_remaining_case_decisions.csv](../audits/construction_review_history/records/residential_remaining_case_decisions.csv) | 7 |
| [residential_successor_condo_overrides.csv](../audits/construction_review_history/records/residential_successor_condo_overrides.csv) | 1 |
| [residential_tieback_episode_exceptions.csv](../audits/construction_review_history/records/residential_tieback_episode_exceptions.csv) | 1 |
| [residential_tieback_no_snapshot_decisions.csv](../audits/construction_review_history/records/residential_tieback_no_snapshot_decisions.csv) | 23 |
| [residential_unresolved_final_projects.csv](../audits/construction_review_history/records/residential_unresolved_final_projects.csv) | 14 |
| [residential_unresolved_predecessor_reference_overrides.csv](../audits/construction_review_history/records/residential_unresolved_predecessor_reference_overrides.csv) | 1 |
| [residential_unresolved_predecessor_selections.csv](../audits/construction_review_history/records/residential_unresolved_predecessor_selections.csv) | 22 |
| [residential_unresolved_source_dispositions.csv](../new_construction_cleaning/output/residential_unresolved_source_dispositions.csv) | 44 |
| [residential_unresolved_successor_condo_overrides.csv](../audits/construction_review_history/records/residential_unresolved_successor_condo_overrides.csv) | 2 |
| [residual_footprint_candidate_overrides.csv](../audits/construction_review_history/records/residual_footprint_candidate_overrides.csv) | 5 |
| [residual_historical_candidate_overrides.csv](../audits/construction_review_history/records/residual_historical_candidate_overrides.csv) | 6 |
| [threshold_sensitive_coordinate_decisions.csv](../audits/construction_review_history/records/threshold_sensitive_coordinate_decisions.csv) | 5 |


## September 7 implementation update

The candidate producer replaces 17 pre-period disposition entries with a general
rule. Five episode decisions are reproduced by the corrected unit/matching
pipeline, leaving 37 episode decisions. This removes 22 active ledger rows
from the original inventory; it does not identify the final unavoidable count.
The web ledger retains 273 evidence records but now has only 22 explicit
classification/suppression exceptions and the existing pending lineage.
It no longer copies 208 unit values or 147 area values already computed upstream.
Recorded URLs and notes remain inputs to construction verification.

Partial Make replays preserve residential project membership and the boundary
table, and preserve the external-review model's values on identical inputs.
The 2% automatic match screen is implemented; a broader 75%–135% area band
would resolve Finsbury, Cleveland, and four additional cases, pending Jacob's
choice. He requested further investigation before exclusions. No new density
exclusions have been applied. Remaining work includes the meaning of related
permit applications, development-wide external values, two noncontemporaneous
card inventories, and source-vintage/zoning reconstruction.
