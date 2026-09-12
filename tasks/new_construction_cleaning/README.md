# New construction: buildings, density and ward-boundary distances

The observation is a newly constructed residential building or a coherently
measured group of buildings. Wholly new replacement buildings qualify;
renovations, additions, conversions and parcel-number changes do not themselves
create new construction. Separately measured townhomes remain separate projects.
Units, floor area, land area and construction year must describe the same object.

Run `make` from `tasks/new_construction_cleaning/code/`. The default build follows
the six steps below. Source downloads belong to the upstream acquisition tasks;
cleaning reads their recorded local files. The resulting project and boundary
files feed `tasks/new_construction_analysis_data/` and the paper. Run `make` in
`paper/` to rebuild downstream analyses and PDFs after a cleaning change.

## Read the cleaning in this order

| Step | Substantive work | Principal code and handoff |
| --- | --- | --- |
| 1. Read sources and remove empty cards | Prepare the recorded residential and commercial Assessor histories. Remove residential cards with neither usable floor area nor units before selecting an assessment. Preliminary commercial records preserve source values; they do not receive final unit or land corrections. | `build_residential_cross_section.R`, `build_commercial_cross_section.R` |
| 2. Identify buildings | Reconcile cards, tied parcels, parcel renumbering and old combined records with individual successors. Preserve individual homes. Apply identity decisions and completion-year decisions needed to distinguish construction episodes here, once. | `build_residential_project_candidates.R`, `build_residential_tieback_temporal_evidence.R`, `build_residential_assessor_projects.R`; commercial families in `build_commercial_project_candidates.R` |
| 3. Select assessment measurements | Choose a complete report for the building, retaining source-row identifiers. Match completed condominium records when an earlier development record is incomplete. Permit and parcel evidence may be needed to establish that the report describes the same building. | `select_residential_assessments.R` produces `residential_selected_assessments.csv`; `build_residential_class297_resolution.R` selects completed condo assessments; `build_preferred_commercial_candidates.R` selects commercial reports |
| 4. Apply remaining recorded corrections | Apply reviewed building measurements, final years, source exclusions and density holds once. A separate table records which source parcels form a reviewed building. Commercial measurement corrections have disjoint source coverage from component and whole-project decisions. | `apply_residential_building_corrections.R`, `build_preferred_commercial_ledger.R`; `select_residential_buildings.R` collects the finished residential buildings in `residential_selected_buildings.csv` |
| 5. Calculate density | Combine the finished residential and commercial measurements. Calculate floor-area ratio as building square feet divided by land square feet, and DUPAC as units times 43,560 divided by land square feet. Missing or withheld measurements remain missing for the affected outcome. | `calculate_construction_density.R` produces `new_construction_measurements.csv` |
| 6. Attach final geography | Locate each selected building and assign its construction-year ward boundary. Geography may withhold an unlocated observation; it cannot alter its year, units, floor area or land area. | `build_preferred_project_geography.R`, `build_residential_final_geography.R`, `build_preferred_commercial_final_geography.R`; `build_preferred_new_construction_ledger.R` checks the combined handoff |

These are six research steps, not six scripts. Preliminary locations are needed
for some identity and permit matches. Likewise, the completed-condo selector runs
only for buildings not already settled by a recorded building decision; it does
not apply the same year exception again. The final measurement file precedes the
combined geography handoff. Building-type classification and its recorded
exceptions are assigned once in the measurement producer and carried to the
regressions and Table 1. The [exact execution order](../../task_graph/construction_steps.md)
and [script graph](../../task_graph/construction_scripts.svg) are generated from
Make prerequisites, including those evidence dependencies.

## Correction ownership

The [recorded-decision task](../new_construction_corrections/README.md) names the current
owners. Use a general source rule whenever the evidence supports one. Preserve
unavoidable judgments as committed inputs with identifiers, evidence and reasons.
Do not create another correction layer in geography, density or regression code.

- Identity and card-selection exceptions belong to step 2. Completion-year
  exceptions needed by the duplicate matcher are applied there and carried
  forward. Geography may compare with the original reported year to find the
  same assessment, but does not change the corrected construction year.
- Final residential building measurements belong to
  `construction_modifications.csv` (residential measurement rows). Old condo/no-snapshot review tables are
  historical records, not additional active measurement instructions.
- Commercial field and unit-definition corrections belong to
  `construction_modifications.csv` (commercial measurement rows). Its source projects cannot also
  receive component or whole-project corrections. Superseded instructions are
  preserved separately and never applied by the default build.
- A correction to a complete building cannot overwrite a year or measurement
  already corrected during residential identity construction. The producer
  rejects such overlapping instructions.
- The final combined producer compares all four physical fields with the saved
  measurement dataset. A downstream change fails the build.

## Source-selection rules that remain substantive

Multiple-card residential measurements must occur together in one assessment,
using the existing 2022, then 2025, then later priority. A card is a tax record,
not necessarily another building. Do not combine unrelated years of card values.
An independent later assessment may resolve a single-PIN historical self-reference
only when class, construction year, floor area and units agree, there were never
concurrent multiple cards, and the complete later report has no tied-property key,
full allocation and positive reported areas. Its whole report supplies the values.

Automatic old/new matching uses the approved two-year window, complete membership,
unit and floor-area agreement, and location/land checks. An exact parcel partition
can also establish that already selected individual homes replace an old combined
record: the lots must cover its site within the existing 0.5% tolerance, the complete
card list and home counts must agree, and the construction episodes must match.
Polygon areas establish identity and location only; they never supply density land.

Completed condo assessments exclude parking and common-area records from home
counts and use repeated whole-building areas once. All selected measurements must
agree within one assessment. A uniquely matched completed new-building permit can
confirm the finished home count. Ordinary tied-parcel buildings additionally need
that permit confirmation. The completed Assessor year is the default completion
proxy unless a recorded stronger completion decision has already settled the
building. Separately numbered completed-building permits can corroborate a site
count only with a complete consecutive list and unambiguous residential counts;
amenity buildings contribute no homes. Permit history retains all available years.

Commercial source selection preserves the approved rules for stable parcel
membership and compatible earlier reported measurements. Land must describe every
selected building. A recorded external reported measurement may replace an Assessor
placeholder; otherwise unsupported density remains withheld. Historical map-based
land values are not accepted as density denominators.

Chicago spatial work uses EPSG:3435. Final location priorities include exact
construction-year parcels, an exact parcel's point from one year later, and a later
exact-PIN assessment matching the same building's reported year, units and areas.
Individual-home points must fall within the historical site; a former site's center
is not automatically an individual-home location. Accepted Chicago address points
require a unique score-100 point-address match with street number and direction.
Chicago geocoding precedes Census where both are used. Recorded parcel and permit
location exceptions apply only in the geography producers.

## Products and historical work

The principal products are `new_construction_measurements.csv` and
`preferred_new_construction_project_ledger.csv`, accompanied by the component,
centroid and boundary-distance files and their standard reports. The complete
candidate and source-disposition files preserve exclusions and source replacements.
All 268 cases in the fixed construction review were closed before this cleanup;
that count is not a claim of perfect source accuracy or clean-clone replication.

The current rules are in `code/Makefile`, with settings declared there. A recipe
that writes several files uses one multiple-target pattern rule, supported by
GNU Make 3.81; requesting any missing member reruns its producer once. The
pattern's stem is the `output` directory. Standard reports are written when the
data are saved; they are not Make targets.

Fifty superseded R scripts, one old Python zoning script, and the old
`legacy_construction.make` rules were removed from the active checkout during
September 11 cleanup. Their unchanged source is preserved at commit `1b5dae29`.
The original footprint downloader is retained with its acquisition task as
historical documentation. Current decision inputs live in `../new_construction_corrections/output/`,
a source task with no R scripts. The historical zoning files live in
`../construction_zoning_history/output/`.
Superseded files and research notes are preserved in the
[construction review archive](../audits/construction_review_history/README.md),
which has no Makefile. The logbook preserves the dated research decisions.

`select_commercial_evidence_rules.R` applies the existing Assessor-count and
parcel-coverage rules directly. Three former scripts that assembled unit and
land review tables are no longer production dependencies. Across all 1,128
source records, the rule flags and resulting decisions agree with the earlier
workflow; the 810 commercial-building records are byte-identical. The four
replaced scripts remain in the archive. Automatic decisions remain general rules,
not new entries in a manual table.

There are currently 46 production R scripts in this task. The five linked
historical-parcel preparation steps now run chronologically in
`build_historical_project_geography.R`. `read_address_geocodes.R` reads the two
recorded geocoder responses. These changes preserve the existing products and
source priorities. Consolidation of the remaining identity and historical-evidence
stages remains unfinished.

The residential cross-section producer reads the history once for its two
selections. `select_residential_buildings.R` combines ordinary and resolved
buildings directly and records the source dispositions; two unused intermediate
tables have been removed. The longer commercial completion summaries now belong
to the [commercial construction review audit](../audits/commercial_construction_review/README.md).
Their production consumer calculates only the permit condition it uses. These
changes preserve all selected buildings and do not turn automatic selections
into manual decisions.

### September 10 handoff check

The fixed 268-case review did not cover every older recorded decision. Comparing all retained project IDs with the historical review inputs found instructions affecting 109 projects that had not carried forward. Those existing decisions now enter the chronological cleaning stages above, with newer explicit decisions preserved. The resulting dataset removes 50 previously excluded records and changes 51 construction years and five home counts; it changes no retained building or land area. Regression preparation attaches covariates only. The September 10 estimation handoff uses the preserved zoning history, as approved by Jacob. Reconstructing that history from the original ordinances remains outstanding for raw-source replication.
