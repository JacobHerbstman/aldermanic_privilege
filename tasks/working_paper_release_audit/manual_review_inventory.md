# Remaining dependence on manual construction decisions

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
| [assessor_default_project_exceptions.csv](../new_construction_cleaning/adjudication/assessor_default_project_exceptions.csv) | 6 |
| [assessor_default_site_reviews.csv](../new_construction_cleaning/adjudication/assessor_default_site_reviews.csv) | 56 |
| [commercial_component_overrides.csv](../new_construction_cleaning/adjudication/commercial_component_overrides.csv) | 47 |
| [commercial_cross_family_decisions.csv](../new_construction_cleaning/adjudication/commercial_cross_family_decisions.csv) | 21 |
| [commercial_field_decisions.csv](../new_construction_cleaning/adjudication/commercial_field_decisions.csv) | 18 |
| [commercial_manual_decisions.csv](../new_construction_cleaning/adjudication/commercial_manual_decisions.csv) | 80 |
| [commercial_parcel_year_overrides.csv](../new_construction_cleaning/adjudication/commercial_parcel_year_overrides.csv) | 2 |
| [commercial_semantic_decisions.csv](../new_construction_cleaning/adjudication/commercial_semantic_decisions.csv) | 29 |
| [commercial_verified_case_review.csv](../new_construction_cleaning/adjudication/commercial_verified_case_review.csv) | 10 |
| [corrected_year_zoning_decisions.csv](../new_construction_cleaning/adjudication/corrected_year_zoning_decisions.csv) | 12 |
| [density_denominator_decisions.csv](../new_construction_cleaning/adjudication/density_denominator_decisions.csv) | 6 |
| [early_multicard_manual_review.csv](../new_construction_cleaning/adjudication/early_multicard_manual_review.csv) | 13 |
| [eligibility_manual_exceptions.csv](../new_construction_cleaning/adjudication/eligibility_manual_exceptions.csv) | 1 |
| [final_project_overrides.csv](../new_construction_cleaning/adjudication/final_project_overrides.csv) | 12 |
| [historical_address_matches.csv](../new_construction_cleaning/adjudication/historical_address_matches.csv) | 4 |
| [historical_coordinate_year_corrections.csv](../new_construction_cleaning/adjudication/historical_coordinate_year_corrections.csv) | 2 |
| [historical_zoning_exact_preconstruction_support.csv](../new_construction_cleaning/adjudication/historical_zoning_exact_preconstruction_support.csv) | 5 |
| [historical_zoning_recovered_ordinance_dates.csv](../new_construction_cleaning/adjudication/historical_zoning_recovered_ordinance_dates.csv) | 1 |
| [historical_zoning_reviewed_events.csv](../new_construction_cleaning/adjudication/historical_zoning_reviewed_events.csv) | 14 |
| [initial_commercial_unit_overrides.csv](../new_construction_cleaning/adjudication/initial_commercial_unit_overrides.csv) | 2 |
| [multicard_cross_pair_decisions.csv](../new_construction_cleaning/adjudication/multicard_cross_pair_decisions.csv) | 6 |
| [multicard_cross_project_suppressions.csv](../new_construction_cleaning/adjudication/multicard_cross_project_suppressions.csv) | 2 |
| [multicard_external_web_reviews.csv](../new_construction_cleaning/adjudication/multicard_external_web_reviews.csv) | 273 |
| [multicard_manual_episode_decisions.csv](../new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv) | 42 |
| [multicard_manual_overrides.csv](../new_construction_cleaning/adjudication/multicard_manual_overrides.csv) | 6 |
| [multicard_parent_pair_decisions.csv](../new_construction_cleaning/adjudication/multicard_parent_pair_decisions.csv) | 12 |
| [multicard_year_overrides.csv](../new_construction_cleaning/adjudication/multicard_year_overrides.csv) | 5 |
| [preferred_project_duplicate_overrides.csv](../new_construction_cleaning/adjudication/preferred_project_duplicate_overrides.csv) | 17 |
| [project_manual_reviews.csv](../new_construction_cleaning/adjudication/project_manual_reviews.csv) | 179 |
| [recovered_project_duplicate_pair_decisions.csv](../new_construction_cleaning/adjudication/recovered_project_duplicate_pair_decisions.csv) | 9 |
| [recovered_project_zoning_overrides.csv](../new_construction_cleaning/adjudication/recovered_project_zoning_overrides.csv) | 1 |
| [residential_additional_candidate_decisions.csv](../new_construction_cleaning/adjudication/residential_additional_candidate_decisions.csv) | 10 |
| [residential_candidate_suppressions.csv](../new_construction_cleaning/adjudication/residential_candidate_suppressions.csv) | 13 |
| [residential_class297_component_overrides.csv](../new_construction_cleaning/adjudication/residential_class297_component_overrides.csv) | 2 |
| [residential_class297_exceptions.csv](../new_construction_cleaning/adjudication/residential_class297_exceptions.csv) | 11 |
| [residential_overlap_decisions.csv](../new_construction_cleaning/adjudication/residential_overlap_decisions.csv) | 28 |
| [residential_remaining_case_decisions.csv](../new_construction_cleaning/adjudication/residential_remaining_case_decisions.csv) | 7 |
| [residential_successor_condo_overrides.csv](../new_construction_cleaning/adjudication/residential_successor_condo_overrides.csv) | 1 |
| [residential_tieback_episode_exceptions.csv](../new_construction_cleaning/adjudication/residential_tieback_episode_exceptions.csv) | 1 |
| [residential_tieback_no_snapshot_decisions.csv](../new_construction_cleaning/adjudication/residential_tieback_no_snapshot_decisions.csv) | 23 |
| [residential_unresolved_final_projects.csv](../new_construction_cleaning/adjudication/residential_unresolved_final_projects.csv) | 14 |
| [residential_unresolved_predecessor_reference_overrides.csv](../new_construction_cleaning/adjudication/residential_unresolved_predecessor_reference_overrides.csv) | 1 |
| [residential_unresolved_predecessor_selections.csv](../new_construction_cleaning/adjudication/residential_unresolved_predecessor_selections.csv) | 22 |
| [residential_unresolved_source_dispositions.csv](../new_construction_cleaning/adjudication/residential_unresolved_source_dispositions.csv) | 44 |
| [residential_unresolved_successor_condo_overrides.csv](../new_construction_cleaning/adjudication/residential_unresolved_successor_condo_overrides.csv) | 2 |
| [residual_footprint_candidate_overrides.csv](../new_construction_cleaning/adjudication/residual_footprint_candidate_overrides.csv) | 5 |
| [residual_historical_candidate_overrides.csv](../new_construction_cleaning/adjudication/residual_historical_candidate_overrides.csv) | 6 |
| [threshold_sensitive_coordinate_decisions.csv](../new_construction_cleaning/adjudication/threshold_sensitive_coordinate_decisions.csv) | 5 |


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
