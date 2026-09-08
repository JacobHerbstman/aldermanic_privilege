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

**September 7 carry-forward decision (superseded by the September 8 rule below):**
Jacob asked to retain the following
two cases in the unresolved review. The exact-PIN coordinates from one year after
construction are the leading candidates, not approved final locations or grounds
for exclusion. Check their construction-year parcel matches as the candidate
geography is assembled. The evidence is preserved in the release audit's
`geocoding_parcel_history_review.csv` and its pinned Parcel Universe source.

| Address needing resolution | Exact PIN | Construction year | Candidate coordinate year |
| --- | --- | --- | --- |
| 763 W 15th Place / Street | 17211330050000 | 2006 | 2007 |
| 3609 W 50th Street / Place | 19111230100000 | 2007 | 2008 |

Reconcile these with the earlier 30-case checkpoint by identifier; do not assume
they are two additional distinct projects without checking overlap.

Address geocoding now separates request eligibility, full source responses, and
match selection. `build_preferred_address_geocode_requests.R` requests a selected
historical address only when both historical and current exact-PIN coordinates
are unavailable. The two geocode scripts replay pinned responses from
`download_construction_address_geocodes`; they make no network calls. Current
requests number 216: all 211 original requests are unchanged, and five additional
requests have no selected historical address. Before the acceptance change below,
both services reproduced the old chosen-match records exactly on the shared 211
requests.

Both geocoders require agreement on the full street address after standard
abbreviation normalization; an omitted trailing unit label is allowed. Direction
and street type must agree. The first strict-street review flagged four Census
responses, leaving 190 accepted requests and all 114 Chicago acceptances unchanged.
Three reference points became unresolved; the fourth had an accepted City point.
The pinned permits supported the alternative labels. These were not density
exclusions. The release audit records evidence for the remaining mismatches.

Jacob subsequently confirmed that 1236 N Troy Street is an Assessor error and the
address is 1236 S Troy Street. `adjudication/historical_address_corrections.csv`
records the parcel, original address, correction, evidence, and decision date.
`build_historical_address_history.R` applies that table before geocoding and keeps
the original selected address alongside the corrected one. The correction must
match the selected source address exactly; stale or unmatched decisions stop the
build. No address or parcel identifier is hardcoded in the correction code.

For every construction request needing an address geocode, an accepted Chicago
point takes priority over Census. This ordering already existed and is retained.
Historical and current exact-parcel coordinates take precedence over both address
services. The broader proposal to add newly recovered historical exact-PIN points
is still pending. Both services now have pinned responses for the corrected South
Troy address, and a pinned query covers its Chicago point in the 2008 parcel layer.

The preferred predecessor lookup reads a pinned source union from its acquisition
task. It preserves all 1,693 original features, adds one 2007 polygon for a newly
queried point, and adds the 2008 South Troy polygon. The first restored lookup had
3,768 shapes spatially equal to the archived selections, two removed selections
under the strict street rule, and one new selection. The subsequent Troy correction
preserves all 3,769 selections from that build and adds Troy, yielding 3,770.
The lookup and its reports rebuild through Make. The September 8 candidate
geography build below now carries these results through boundary assignment.

The release audit also examines complete 1999–2025 exact-PIN location history for
all 216 address requests: 447 source rows provide finite coordinates for 191
requests. The nearest-year choice follows the existing initial predecessor rule,
including earlier-year tie breaking. Applying these points before address
geocoding remains a recommendation awaiting Jacob's decision; production does
not consume the audit's selected points.

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

## Candidate geography and boundary distances: September 8

The existing preferred-project geography and boundary producers now build together
from the restored parcel inputs. `preferred_project_boundary_scope.csv` accounts
for all 14,017 requested candidate project-years in 2006–2022: 13,903 have complete
parcel geometry and boundary distances; 114 remain explicitly unresolved, with
missing distances. Another 23 review candidates have no selected construction year
and remain in `preferred_adjudication_scope.csv`. These are candidate groups, not
the final density sample or final project identities.

`build_preferred_project_geography.R` unions the matched component parcels only
when all requested components are located. It saves component shapes, project
shapes, centroids, and complete coverage accounting. `build_preferred_boundary_scope.R`
uses June 15 of the recorded year, as stated in the paper, to choose the ward map.
It measures distance from the project centroid to the nearest shared boundary of
its ward in EPSG:3435. Unresolved geometry receives no assigned ward or distance.
Assertions reject duplicate source keys, unmatched accepted components, nonpositive
areas, and centroids that lie in zero or multiple ward polygons. All six outputs
have standard Make-generated reports.

The release audit's `candidate_geography_checks.csv` independently computes
distances to every boundary of the assigned ward. All 13,903 distances and assigned
pairs agree to within 0.000001 feet. It also identifies 41 centroids outside their
project polygons (nine within 500 feet of a ward boundary). Such shapes need review;
they are not automatically excluded or moved to a different point. The largest
case is a review-required Assessor tieback linking parcels 20174140320000 and
29174140310000, approximately 12.6 miles apart. A calculated centroid is not evidence
that those two parcels form one building.

`candidate_geography_review_queue.csv` in the release audit consolidates 194 review
items across 188 project identifiers: unresolved component locations, the 23 unknown
years, the 41 centroid flags, and all 30 initial predecessor checkpoints. These
categories overlap. It preserves historical exact-PIN coordinate candidates as
evidence, including the one-year-ahead locations for 763 W 15th and 3609 W 50th;
those coordinates are not adopted by this build. No identifiers are hardcoded in
the audit rules. Final identity, measurements, zoning, and density eligibility
remain downstream work.

The same audit measures the existing 2015 timing convention: using the old ward
map instead changes the ward for 179 of 754 located 2015 candidates and changes
500-foot membership for 189. This is a sensitivity calculation, not an adopted
date or map change. The paper's June 15 convention remains in force.

## Consolidated geography review: September 8 follow-up

Jacob approved a general fallback after reviewing the new parcel evidence. If the
original predecessor lookup has no reference point or finds no polygon,
`recover_preferred_historical_predecessors.R` looks for the exact PIN's coordinate
in the following year and queries the construction-year parcel map. It accepts
a unique polygon; the existing exact-equivalence rule handles duplicate shapes.
Unequal overlapping polygons remain unresolved. It does not change construction
years, adopt later-year parcel geometry, or replace previously accepted matches.

The producer reads both pinned coordinate-history extracts, verifies agreement
on overlapping PIN/year records, and preserves the original attempt alongside
the selected coordinate year and source row identifier. New coordinates must be
inside the recorded spatial-query scope. The tolerance for comparing query
coordinates is 0.000001 feet, solely for decimal serialization; it does not buffer
points when matching parcels. The new parcel snapshot is a downloaded source
input, not a manual decision ledger. No case-specific correction was added.

The source review finds 41 requests with a next-year coordinate: 34 yield one
construction-year polygon, six have no polygon, and one has multiple unequal
polygons. Both 763 W 15th and 3609 W 50th now have unique historical parcel
matches. The six empty results concern the 2016 map; the next-year coordinates
agree closely with the original exact-PIN reference locations. These need
historical-map coverage evidence, not another address-direction correction.

The release audit now records the responsible production script, candidate
measurement/year reason, source years, competing land areas, and historical
coordinate parcel matches for the remaining review items. It traces changed
project identifiers by a unique component/year match. Ten original checkpoint
requests are resolved by the existing equivalent-shape rule; their records stay
in the audit with `initial_checkpoint_resolved`. The other 20 remain pending.
A centroid outside one concave polygon is distinguished from a project assembled
from multiple polygon parts. Neither flag automatically changes the centroid or
certifies that the parcel scope belongs to one construction episode.

Choices about project membership or construction episodes belong in the upstream
Assessor/commercial candidate producers. Parcel-match rules belong in the
predecessor producer. Only an irreducible project-specific judgment should become
a committed adjudication input, with identifiers, evidence, reason, and a
producer check that it still matches the source. The audit output is not an input
to production, and the final dataset must not be edited to implement decisions.

After the actual rebuild, 13,937 candidate project-years are located and 80 remain
unresolved. All 13,903 previously located records are unchanged; seven of the 34
newly located candidates lie within 500 feet, bringing that count to 4,221. The
independent distance check passes for all 13,937. The consolidated audit retains
159 records, including ten resolved checkpoints: 149 open items across 144 current
projects. Their routes are 39 older-coordinate candidates, 21 overlapping-parcel
cases, 19 missing-location cases, six empty-map cases, 23 year conflicts, and 41
centroid flags. One duplicate audit entry under an older project identifier has
been consolidated; `initial_project_ids` preserves the checkpoint identity.

## September 8: Empty assessment records

Before selecting a residential assessment, omit rows where both building square
feet and apartment count are missing. A reported zero is not treated as missing,
and a row with either field present still proceeds to the existing substantive
checks. Apply this before construction-year discovery and report ranking in
`build_residential_cross_section.R`, and before card selection and complete
assessment selection in `build_residential_project_candidates.R`. The pinned
history and the history used for tracing parcel relationships remain intact.

This removes empty records as evidence of additional buildings; it does not
establish that every property with only empty records lacks new construction.
Other source branches remain available. No case identifiers enter this rule.

## September 8: Prefer individual homes to duplicate combined records

`build_residential_assessor_projects.R` now identifies complete replacements
before preferred candidate selection. It uses pinned historical polygons for the
exact old PIN and exact-PIN current coordinates. The old PIN must have disappeared
from the current parcel source. A single assessment must describe its study-period
house cards with positive areas. Within the latest available old-parcel map no
later than that assessment, separately accepted one-unit homes must account for
all those cards. Restrict possible replacements to the existing two-year window;
compare their sorted individual areas and associated years using the existing
2% area and two-year tolerances. Conflicting shapes, incomplete counts, measurement
conflicts, or homes claimed by multiple old parcels prevent automatic replacement.
This is evidence of complete replacement, not a universal parcel genealogy.

The project table keeps the old row marked
`exclude_source_duplicate_keep_successors`, with replacement project IDs and a
reason for each unsuccessful check. Preferred candidate selection carries those
fields forward. No individual fields are overwritten and no case IDs enter the
rule. Older manual-resolution stages remain unfinished and must respect these
source dispositions before the final dataset can be rebuilt.

Jacob confirmed keeping separately recorded townhomes separately. Specific
review decisions are: Eddy's three individual homes retain 2022; Bell's eleven
retain their individual Assessor years; 2403--2419 W 32nd Place's nine individual
homes retain 2007; Cullerton/Prairie's twelve remain individual with their own
Assessor years. These decisions do not waive unresolved duplicate matching.
Fletcher's announced development size is not an observation count or a reason
to add homes. Its six recorded cards still require reconciliation with individual
properties. No news report creates observations.

## September 8: Approved building identities

Jacob approved the preferred resolutions for Eddy, Bell, Cullerton/Prairie,
31st Street, Dearborn, Washington, and Ingleside. Two small source ledgers record
these exceptions instead of placing property identifiers in executable code.
`adjudication/residential_reviewed_card_selections.csv` selects three existing
Assessor rows: Dearborn 2022 card 1; Washington 2025 card 1; Ingleside 2025 card 1.
The existing card producer applies these choices before counting components or
adding their areas. It rejects absent source rows, duplicate cards, mixed
assessment snapshots, and unusable measurements. No numerical field is invented.
Washington's three units remain an Assessor proxy from 2025, not an independently
verified original unit count.

`adjudication/residential_reviewed_home_replacements.csv` identifies six old
combined records and their thirty individually retained homes. The existing
project producer applies the reviewed correspondences alongside the automatic
rule, validates that the individual homes are present and eligible, and rejects
competing claims on the same home. The six cases preserve their original values
for inspection but are marked excluded as duplicates. Individual home fields do
not change. These are explicit reviewed exceptions; the general 2% area and
two-year automatic tolerances have not been loosened.

The accepted fixes resolve nine of the fifteen remaining records from the
original review. Fletcher, 32nd Place, Vernon, and three Broad/Pitney records
remain under review. This does not remove independently measured individual
homes near those unresolved records. The paper's frozen input remains unchanged.

## September 8: Final decisions for the remaining townhome records

Vernon is one reviewed five-home site dated 2020, as approved by Jacob. The
component ledger identifies five actual 2022 Assessor rows across PINs
20034000310000, 20034000320000, and 20034000330000. The existing project producer
reads those rows, requires one assessment snapshot, sums building areas and units,
and counts each parcel's land once. It produces 11,891 square feet of building
and 9,015 square feet of land. The three prior parcel candidates are absorbed
into that one site; their membership remains explicit in the source ledger.
The reviewed year is distinguished from the original card years in year_source.
The preferred candidate consumer retains this reviewed project and all three
component PINs for construction-year geography.

Fletcher's six individual homes (PINs 13252030460000 through 13252030510000)
are retained under the approved reviewed correspondence. The sixth point lies
1.55 feet outside the old polygon; this is not grounds for dropping the house.
The combined record is suppressed, and no individual year or area is overwritten.

The 32nd Place combined record is excluded as unreliable rather than treated as
additional construction. Its complete set of eighteen building areas matches
individual 2008 assessments on PINs 16362010530000 through 16362010700000, all
reporting construction in 2007. This covers both the odd- and even-numbered rows
of nine homes. The parent record's later 2011 dates and its conflicting parcel
location do not justify adding eighteen more homes. All individual records retain
their own fields, including later revisions to individual building measurements.

The three Broad/Pitney combined records are likewise excluded as unreliable.
The five newer Broad Street homes retain 2022 individually. The eight other
individually recorded Broad/Pitney homes retain their Assessor 2006/2007 years;
the decision about the 2022 cohort is not an unsupported redating of older homes.
These source exclusions are recorded in residential_reviewed_source_exclusions.csv
and applied by the existing project producer. The paper's frozen input is not
replaced by this reconstruction.

## Reviewed individual locations above a shared garage

The two Deming homes have approved individual lot areas but overlapping,
development-wide tax polygons. `residential_reviewed_permit_locations.csv`
records their completed new-house permits and expected addresses.
`build_preferred_project_geography.R` validates those permit identities and
addresses, then carries their recorded points into boundary assignment. It does
not fabricate individual lot polygons or assign the garage's area to either home.
These points have `location_source = reviewed_completed_permit_point`; their
mapped land area remains missing and `complete_project_geometry` remains false.
The boundary scope identifies them as `reviewed_permit_location`. Their density
land areas come from the separately approved land-area input in the Assessor
project producer. The geography audit checks their ward assignments and distances
while restricting polygon-area checks to observations with actual parcel maps.

### Reconciliation of earlier year-case recommendations

The reviewed building-component ledger now identifies the candidate each selected
Assessor row replaces. The producer validates that each row belongs to that source,
uses its recorded assessment measurements, and retains the superseded candidate as
an excluded source record. Only the reviewed buildings enter component membership.
This preserves Vernon while restoring Geneva's pre-addition residence and treating
the two Maud houses separately. The reviewed identity ledger also supports one
obsolete record being replaced by several retained homes, as at Springfield and
Nashville. It verifies that every replacement exists and remains eligible.

The earlier Seeley exclusion is withdrawn: its cited permit was on Orchard Street.
The correct Seeley permits include both an addition and a new house. Its floor-area
identity remains unresolved. The older final-ledger script now rejects a pending
source decision rather than silently treating it as an exclusion. That older branch
still has missing source dependencies and has not been rebuilt. The ten-case review
in `working_paper_release_audit` distinguishes applied resolutions from open evidence
questions; a historical recommendation is not proof that a current row is resolved.

### Recorded measurements and a stopping rule for case review

For density, use land area reported by the Assessor or another accessible source.
Do not construct replacement land measurements by calculating parcel polygon areas.
Maps can check identities, coverage, and locations; they are not a license to invent
missing density fields. Apply general cleaning rules first and existing approved
source decisions next. If required measurements remain unavailable or unreliable,
retain the record and exclusion reason in the inventory and leave it out of the
relevant density analysis. Do not spend hours reconstructing an individual property
just to retain it. Jacob clarified this stopping rule on September 8, 2026.
