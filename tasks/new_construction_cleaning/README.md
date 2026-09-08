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

Jacob finalized the following choices on September 7, 2026:

- Combine the two 42-unit buildings at 4400 Grove. Use the 97,215-square-foot
  Assessor land denominator for their combined 84 units, retaining the existing
  114,070-square-foot architect building area and 2020 completion year. The
  commercial manual ledger records this decision and the two parcel identifiers.
- Keep Natchez A/B/C as three separate construction groups, containing respectively
  12, 14, and 8 buildings and 72, 84, and 39 units. Use their existing Assessor-year
  proxies of 2017, 2020, and 2020. All three already occur separately in the frozen
  dataset; A and B enter the main 500-foot sample, while C is outside it. This
  settles grouping and timing. Subsequent footprint review established that C's
  strip-only land denominator is incomplete; Jacob approved excluding C from
  density pending a reliable site boundary. The existing density-exclusion ledger
  records that decision and retains the construction observation. B's focused
  follow-up supports retaining its 210,101-square-foot Assessor site: all fourteen
  residences are inside it, and the original 2016 approval independently specifies
  84 units on 207,228 square feet. The 1.39% area difference and later C overlaps
  remain documented measurement uncertainty, not a pending Natchez coding choice.
- Use the later Assessor year 2006 for 1217 W Arthington at Roosevelt Square.
  The project review ledger labels this an accepted Assessor proxy. The earlier
  2005 source value remains evidence; the accepted date is not independently
  verified building-level completion. The existing six-unit count is unchanged;
  its conflict with the 2022 renovation description remains unresolved.

A group may contain multiple buildings. These choices do not authorize splitting
site totals into individual-building observations or merging every phase of a
named development. The accepted Grove area and Roosevelt year do not require
further approval. A controlled replay of the commercial decision producer now
verifies A/B retention, C's two density flags set to false, and Grove's approved
land area. It preserves 815 project identities. This replay holds earlier project
and evidence inputs fixed; these changes have not propagated to the paper's
frozen dataset. The next commercial build gap is historical parcel coverage.

## Local City footprint evidence

`../download_city_building_footprints/` now acquires the complete August 2015 City
shapefile from its recorded public attachment URL. A fresh download reproduced the
preserved ZIP's SHA-256 exactly. The same input supplies the existing 2015
verification extract and `build_commercial_city_building_footprints.R`.

The commercial producer selects every source footprint intersecting an in-period
commercial project polygon, then standardizes the address, year, units, and area
fields used by its evidence consumer. It validates identifiers and geometry in
that scope. The consumer retains its existing matching and classification rules.
This replaces the missing producer's project-by-project circular API queries.
It does not assign project land, construction years, or density eligibility.

On the preserved 793 project geometries, the local producer selects 1,241
footprints. Eleven were missing from the old API extract; five of those pass the
existing overlap rule and change footprint summaries for three projects. With
the same current permit inputs on both sides, all 800 candidate evidence
classifications and review flags agree. Other area differences are below
0.000002 square feet and arise from the old longitude/latitude round trip.
This is a controlled comparison on fixed geometry and candidates, not a complete
rebuild of those inputs or a claim about final regression results.

The saved reports for the footprints, commercial evidence, and commercial
decisions record these controlled replay baselines. The standard reporter now
supports a single-layer GeoPackage's attributes; the spatial producer separately
validates geometry. The final-source chain still stops at
`historical_project_parcel_coverage.csv`, whose producer has not been restored.

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
The seven approved density exclusions remain recorded in
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

The earlier declared final-output dependency inventory reached 99 local scripts
and 188 internal output paths, with 38 missing producer rules. The City footprint
producer and initial historical parcel coverage have since been restored. Coverage
now builds from the current candidate components through pinned administrative
polygons and recorded query scope. It first matches the exact annual parcel
identifier, then a unique polygon sharing the ten-digit base identifier; other
cases remain ambiguous or missing. It fails if a requested parcel-year has never
been queried. No row-specific exceptions enter this rule.

The updated coverage has 11,108 component-year requests: 8,508 exact matches,
seven unique base-identifier matches, and 2,593 missing matches. These are candidate
component-years, not final projects or density exclusions. All 11,079 rows shared
with the archived coverage retain their match fields exactly. A controlled replay
with original candidates reproduced the archived coverage bytes and all 9,505
project-year geometries, holding predecessor evidence fixed.

Initial predecessor recovery is now restored as well. It reads the complete pinned
Parcel Universe history, checks uniqueness before selecting records, and chooses
the closest assessment year for each exact PIN (preferring the earlier year in a
tie). All 2,593 current requests have that history. The old unused current-parcel
and permit-coordinate fallbacks are not carried into this stage; absent history
remains explicitly unresolved. Annual point-polygon intersections retain every
candidate and accept only a unique polygon. Boundary points are not silently
assigned to one of several parcels.

With original candidates, this recomputes all 2,623 predecessor-resolution rows
byte for byte and reproduces all 9,505 project-year geometries exactly, without
holding accepted predecessor decisions fixed. Current predecessor recovery has
2,563 uniquely matched requests, 24 with multiple candidates, and six without a
polygon. These unresolved requests are preserved for later evidence and identity
reconciliation; they are not new density exclusions.

**Required checkpoint before the full rerun:** revisit all 30 unresolved initial
predecessor requests (24 with multiple polygons; six with none). Trace each through
the later pipeline and record whether it is resolved, remains ineligible, or needs
Jacob's judgment. Identify them by source family, project, component PIN, and target
year in `historical_project_predecessor_resolution.csv`; filter
`predecessor_status` to `multiple_predecessor_polygons` or `no_predecessor_polygon`
and count distinct requests rather than candidate-polygon rows. Do not treat this
checkpoint as completed merely because the later code builds.

Address geocoding now separates request eligibility, full source responses, and
match selection. `build_preferred_address_geocode_requests.R` requests a selected
historical address only when both historical and current exact-PIN coordinates
are unavailable. The two geocode scripts replay pinned responses from
`download_construction_address_geocodes`; they make no network calls. Current
requests number 216: all 211 original requests are unchanged, and five additional
requests have no selected historical address. Both services reproduce the old
chosen-match records exactly on the shared 211 requests.

**Geocode acceptance remains under review before the full rerun.** The preserved
Census rule checks one match, house-number agreement, and Chicago coordinate
bounds. It accepts four responses that change the requested direction or street
type. Three of those responses entered the old reference-point calculation; a
City point match superseded the fourth. Do not infer a corrected location or a
density exclusion from this comparison alone: the source historical address can
also be wrong. The proposed general full-street agreement rule, including how to
handle omitted unit labels, still needs Jacob's decision. The reconstruction
currently retains the old rules as a comparison baseline, not a completed
location validation.

The current initial geography builds 9,530 complete candidate project-year shapes.
All 9,501 shapes shared with the original output retain identical attributes and
geometry; 29 candidate project-years enter and four leave following the upstream
candidate changes. Seventeen additions are annual shapes for one parcel whose
conflicting source years still need later resolution. These are not 29 new projects
or finalized density observations.

The dependency inventory was a static count, not a successful build. The remaining
work includes later address-based geography, duplicate reconciliation, condominium
successors, and ordinance reconstruction. The exact original residential download
remains unavailable, and the pinned replacement vintage is not fully reconciled.

Consequently, `make` for this whole task cannot yet certify raw-to-final replication.
Preserved-input replays establish only the stages actually compared. Detailed
restoration evidence and source limitations remain in
[`../working_paper_release_audit/construction_provenance_followup.md`](../working_paper_release_audit/construction_provenance_followup.md)
and the logbook. The paper dependency graph remains generated from actual Make
prerequisites with `make -C task_graph`; the unconnected reconstruction does not
appear as though it already feeds the paper.
