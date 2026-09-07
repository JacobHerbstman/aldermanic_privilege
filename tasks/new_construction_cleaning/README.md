# Construction cleaning

The intended product is one dataset of new residential construction projects,
with floor-area ratio, dwelling units per acre, and distance to ward boundaries.

This task reconstructs residential construction projects from Assessor records,
permits, historical parcels, building footprints, zoning evidence, and recorded
human decisions. A project can contain several parcels or building cards. Parcel
records, buildings, and whole developments must not be treated as interchangeable.

**The reconstruction is unfinished and is not the paper's input.** The paper
still reads the committed 8,648-row file owned by
`../new_construction_analysis_data/`. Its Makefile checks that snapshot; it does
not rebuild the construction history. Do not replace it until the restored chain
and the resulting sample and estimates have been reconciled.

## Agreed construction definition and pending grouping decisions

On September 7, Jacob confirmed that wholly new residential buildings qualify
even when they replace demolished buildings. Renovations, additions, and
conversions do not themselves qualify. Parcel subdivision is an administrative
change to trace for physical identity, not a construction event.

Jacob confirmed combining the two 42-unit buildings at 4400 Grove into one
84-unit observation. This agrees with the existing commercial decision; no
measurement or sample change follows from this confirmation alone.

For Natchez, Jacob requested investigating one combined observation dated to
completion of the whole included group. This is conditional on establishing
the group's component buildings, matching units/floor area/land, and actual full
completion timing. The existing 84-unit record uses 2020, but that year has not
been newly verified as full completion. A permit's issue date and its current
completed status do not establish its completion date. Other Natchez records
exist; a common development name does not establish shared membership.

Roosevelt Square grouping remains unresolved. The existing review supports a
separately measured six-unit building, but that does not decide whether the
intended observation should cover a larger construction group. Establish the
relevant component set and measurement scope before retaining or aggregating it.
Jacob requested seeing questionable evidence before consequential judgments.
These decisions do not authorize combining every phase of a named development.

## Read the research in chronological order

The table distinguishes the intended research sequence from the current code.
It is a guide to consolidation, not a claim that the task already has six stages.
The concrete dependencies remain in [`code/Makefile`](code/Makefile).

| Research question | Current implementation | Simplification required |
|---|---|---|
| What did each source report, and when? | `../prepare_construction_assessor_history/`, `build_residential_cross_section.R`, `build_residential_discovery_cross_section.R`, `build_commercial_cross_section.R`, `build_construction_permit_history.R` | Five residential readers now share one normalized full-history source. Extend this to the remaining readers. Keep observation year separate from reported construction year. The two residential selections use different rules; do not silently replace one with the other. |
| Which records describe the same construction project? | `build_residential_project_candidates.R`, `build_residential_tieback_temporal_evidence.R`, commercial candidate scripts, historical recovery, and later multicard matching | Resolve membership and predecessor/successor relationships before choosing project totals. Keep parcel/card membership in keyed tables rather than repeatedly assembling and splitting identifier strings. Related permits are not necessarily revisions of the same building. |
| When was that project built, and how much housing and land belong to it? | Preferred candidate/ledger scripts, `build_final_multicard_adjudication.R`, external reviews, classification, and final verification | Give each resolved year, unit count, floor area, and land area one owner and a source of the same scope. Eliminate the sequence that aggregates cards, restores a selected card, and later reconstructs totals. Preserve unresolved evidence and committed exceptions. |
| Is it eligible residential new construction? | `validate_new_construction_eligibility.R`, `validate_multifamily_classification_rules.R`, and the project-verification/review scripts | Assemble corroborating evidence before one inclusion/classification decision. Preserve evidence checks; eliminate repeated overwrites and copies of unchanged confirmations. Geography can support matching and review scope before final assignment. |
| Where was it, and what zoning applied when built? | Historical geometry, several boundary/zoning stages, and corrected-year reassignment in the final exporter | Select construction-year geometry and zoning from resolved project facts. Keep preliminary spatial evidence distinct from final assignment, so a later year correction cannot leave an earlier zoning or ward assignment behind. |
| What enters the regression? | `build_final_verified_density_input.R`, then the existing density-estimation tasks | Assign aldermen, scores, controls, and analytical variables after project verification. Keep estimator-specific score choices and bandwidths in estimation. |

There is no target number of scripts. Separate source preparation, project
construction, and analysis because they own different data products. Keep a
coherent transformation in one readable script; do not create one task per step
or hide a long script inside a large helper function.

## Residential Assessor branch: history, membership, measurements

The first three pieces now build together. Run from `code/`:

```sh
make ../report/residential_assessor_project_candidates.csv.log
```

1. `../prepare_construction_assessor_history/` preserves all 12,320,011 source
   reports in typed Parquet. Five readers use this shared history instead of
   separately parsing the full CSV. No source vintage was refreshed.
2. `build_residential_cross_section.R` selects candidate PINs;
   `build_residential_project_candidates.R` assembles card and parcel history;
   `build_residential_tieback_temporal_evidence.R` checks membership and measurements
   within the same assessment snapshot. The 2022/2025/later preference is now one
   ranking operation in each applicable reader, with both cutoffs exposed in Make.
   Discovery still uses its existing earlier-report rule.
3. `build_residential_assessor_projects.R` assembles the ordinary, tied-parcel,
   and multiple-card project forms in one table. It records component PINs, source
   rows, year, units, building area, land area, and measurement provenance. It now
   owns those initial Assessor measurements; `build_preferred_residential_candidates.R`
   consumes them before applying the existing permit evidence and adding other
   residential project forms.

The initial consolidation (`36094f8`) produced 27,831 candidates: 25,610 ordinary parcels,
1,045 tied-parcel groups, and 1,176 multiple-card parcels. This inventory includes
out-of-period records. Existing rules label 12,904 mechanically retained, 14,728
outside the period, 190 requiring further evidence, and 9 deferred to commercial
reconciliation. These labels are intermediate rules, not final verification or
an estimate of the unavoidable manual workload. Later duplicate reconciliation
can affect mechanically retained candidates too. Another 255 class-297 candidates
and 45 residential/commercial overlaps enter the existing downstream resolver.

The resulting candidate table agrees exactly with the controlled replay, including
its preliminary distances. Across the fifteen existing CSV products compared before
and after this consolidation, fourteen are byte-identical. The 6,520-row card table
has a different row order after the ranking simplification; its keyed records and
all downstream candidate measurements are unchanged. These comparisons use the
same currently pinned full source on both sides; they do not establish equivalence
to the missing original source vintage or to the paper's final sample.

This is a working candidate branch, not the finished density dataset. The carried
`current_distance_m` is preliminary spatial evidence. Final distance must use the
resolved construction year and project geometry. Card/successor duplication,
completion-year evidence, and measurement scope remain to be resolved before
calculating final density ratios and making one final geographic assignment.
No new case-specific decisions or exclusions were introduced here.

## Complete assessment snapshots for multiple-card measurements

Jacob approved choosing one complete assessment snapshot under the existing
2022/2025/later priority. `build_residential_project_candidates.R` now does this
before exporting the card table. Within each proposed construction-year episode,
a qualifying snapshot must contain exactly the selected component cards, with
positive unit and building-area measurements and one positive parcel-land value.
It cannot silently omit missing cards or add unselected cards. The selected source
row IDs and assessment years remain in `residential_multicard_cards.csv`, together
with `complete_episode_snapshot`.

The project producer sums only complete component measurements supported by one
assessment year. Unsupported totals remain missing and the candidate requires
reconciliation. The later multicard evidence reader uses the producer's snapshot
flag and chosen assessment year; it no longer reconstructs its own selection.
No project or source-history row is deleted by this change.

All 27,831 candidate identities, construction years, land values, and preliminary
distances are unchanged. Three additional candidates require reconciliation,
bringing that intermediate count to 193; mechanically retained candidates number
12,901. Twelve unit totals and twelve building-area totals become missing because
they lack complete support. Three other building-area totals now use complete
snapshots, including one previously partial six-card sum. This is not a claim
that those Assessor totals identify distinct physical buildings.

The remaining distinction matters. Two cards can coexist and still duplicate one
house: the recorded review for 42 E 90th Street provides that counterexample.
Conversely, the Ingleside candidate omits a rebuilt card because its older report
wins the historical selection rule; the full snapshot contains an extra component.
The general snapshot rule exposes that mismatch rather than silently choosing a
subset. Existing physical-identity reviews still enter downstream, and the final
dataset remains unfinished. The next membership work must distinguish card
renumbering, replacement buildings, and parcel subdivision before those records
can contribute density measurements.

## First consolidation: analytical assignment

`build_preferred_density_input.R`, `build_final_density_input.R`, and
`build_final_multicard_adjudication.R` now carry physical project records without
assigning aldermen, treatment scores, or ward controls. Their existing output
names are retained while consumers are simplified. The names containing
`model_input` describe historical stages, not finished analysis datasets.

`build_final_verified_density_input.R` explicitly reads the project fields it
needs and is the sole owner of these assignments within construction cleaning.
It uses recorded alderman terms at an imputed June 15 construction date. June 15
is an existing convention for year-only records, not observed completion timing.
The exporter checks serving-alderman score coverage, term overlap, main-bandwidth
controls, and unique project IDs. Early geographic checks remain because matching
and review use locations; this first consolidation does not yet eliminate all
geographic reassignment or repeated density calculations.

A ten-script Make replay on identical saved upstream inputs produced the same
8,648 rows and all 41 final export columns byte for byte before and after this
change. Intermediate project membership and research decisions were unchanged.
The comparison held the original episode inventory and its recorded decisions
fixed, so it does not resolve the two pending duplicate groups under the updated
matching rules. Sparse review fields also received explicit read types after
inspection found that automatic type guessing discarded numeric evidence and
classification-source labels. These corrections preserve the final export.

The verification record is in
[`../working_paper_release_audit/restored_stage_checks.csv`](../working_paper_release_audit/restored_stage_checks.csv)
and the September 7 chronology entry in [`../../logbook/`](../../logbook/).

The year-rule implementation is also simpler. The temporal-evidence producer now
counts distinct usable years and distinct in-period years while those years are
still numeric. The candidate producer reads those counts directly; it no longer
parses a slash-separated display string twice. The five repeated component-table
blocks are one transformation of the assembled project table. On identical inputs,
all 27,947 candidates, all 29,125 project/parcel memberships, and all 270 review
queue rows are byte-identical. The temporal output gains three numeric summary
fields; its 10,202 assessment snapshots remain byte-identical.

## Rules and decisions still requiring scrutiny

- **Assessment-year selection.** Ordinary cards prefer reports through 2022,
  then through 2025, then later reports when necessary. Multicard and discovery
  selections differ. These rules need an explicit justification tied to the
  construction object before consolidation changes selected observations.
- **Completion timing.** The preferred candidate script moves a reported year
  to a permit application year under a particular one-year conflict. Applications,
  issuance, reinstatement, assessment, and completion are different dates. This
  existing choice is preserved pending review, not newly endorsed.
- **Measurement scope.** External development totals cannot automatically replace
  values for a constituent building or phase. Parent/successor matching should
  establish identity before choosing the measurement source.
- **Permit relationships.** The current permit-reference components can connect
  distinct buildings through related applications. Their unit counts cannot be
  interpreted as same-building revisions without distinguishing link types.
- **Review coverage.** The last verification pass targets initially uncorroborated
  projects within 500 feet. Its absence of a review is not universal affirmative
  verification. A more consistent evidence rule must make that scope explicit.
- **Historical benchmark assertions.** Several restored scripts still require
  exact old counts such as 795 reviews or 101 corrected-year zoning rows. Replace
  those with universe coverage, key, and evidence assertions when their producers
  are reconciled. Old counts remain useful comparisons in the audit.

The approved two-year episode window and all-date permit history remain in force.
The six approved density exclusions remain recorded in
`adjudication/density_denominator_decisions.csv`. The broader automatic card-area
matching band is still a proposal; the active setting remains 0.02. No additional
exclusions or measurement-source choices were adopted in this consolidation.

## Sources, builds, and release limits

The derivation was recovered from `research-archive` at
`010a1f8497c1f32e2c79b5933d1c5baf9af44be3`. Pinned source snapshots and their hashes
are under `data_raw/construction_review/`; human inputs and acquisition notes are
listed in [`adjudication/README.md`](adjudication/README.md). Preserve source bytes.
Public acquisition belongs in explicit Make rules; documented manual/restricted
acquisition remains an input boundary. Do not fetch a changing vintage inside a
cleaning script.

Run producers through Make from `code/`. Fixed paths are literal at read/write
calls. Make owns directories, concrete input links, shared execution rules, and
standard reports in `report/`. Shared code lives in `tasks/shared/code/`. Reports
and meaningful key/evidence assertions complement each other. An unchanged build
must run no producer; a deleted member of a multiple-output product must regenerate.

The declared final-output dependency closure currently reaches 99 local scripts
and 188 internal output paths; 38 of those output paths still lack producer rules.
This is a static count of declared prerequisites, not a successful build. The
missing work includes historical parcels, duplicate reconciliation, condominium
successors, and ordinance reconstruction. The exact original residential download
remains unavailable, and the pinned replacement vintage is not fully reconciled.

Consequently, `make` for this whole task cannot yet certify raw-to-final replication.
Preserved-input replays establish only the stages actually compared. Detailed
restoration evidence and source limitations remain in
[`../working_paper_release_audit/construction_provenance_followup.md`](../working_paper_release_audit/construction_provenance_followup.md)
and the logbook. The paper dependency graph remains generated from actual Make
prerequisites with `make -C task_graph`; the unconnected reconstruction does not
appear as though it already feeds the paper.
