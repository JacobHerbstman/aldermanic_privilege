# New construction: buildings, density and ward-boundary distances

The observation is a newly constructed residential building or a coherently
measured group of buildings. Wholly new replacement buildings qualify;
renovations, additions, conversions and parcel-number changes do not themselves
create new construction. Separately measured townhomes remain separate projects.
Units, floor area, land area and construction year must describe the same object.

Run `make` from `tasks/new_construction_cleaning/code/`. The default build follows
the six steps below. Source downloads belong to the upstream acquisition tasks;
cleaning reads their recorded local files. The paper still uses the frozen input
in `tasks/new_construction_analysis_data/`; this build does not rerun estimation.

## Read the cleaning in this order

| Step | Substantive work | Principal code and handoff |
| --- | --- | --- |
| 1. Read sources and remove empty cards | Prepare the recorded residential and commercial Assessor histories. Remove residential cards with neither usable floor area nor units before selecting an assessment. Preliminary commercial records preserve source values; they do not receive final unit or land corrections. | `build_residential_cross_section.R`, `build_residential_discovery_cross_section.R`, `build_commercial_cross_section.R` |
| 2. Identify buildings | Reconcile cards, tied parcels, parcel renumbering and old combined records with individual successors. Preserve individual homes. Apply identity decisions and completion-year decisions needed to distinguish construction episodes here, once. | `build_residential_project_candidates.R`, `build_residential_tieback_temporal_evidence.R`, `build_residential_assessor_projects.R`; commercial families in `build_commercial_project_candidates.R` |
| 3. Select assessment measurements | Choose a complete report for the building, retaining source-row identifiers. Match completed condominium records when an earlier development record is incomplete. Permit and parcel evidence may be needed to establish that the report describes the same building. | `select_residential_assessments.R` produces `residential_selected_assessments.csv`; `build_residential_class297_resolution.R` selects completed condo assessments; `build_preferred_commercial_candidates.R` selects commercial reports |
| 4. Apply remaining recorded corrections | Apply reviewed building measurements, final years, source exclusions and density holds once. A separate table records which source parcels form a reviewed building. Commercial measurement corrections have disjoint source coverage from component and whole-project decisions. | `apply_residential_building_corrections.R`, `build_preferred_commercial_ledger.R`; `select_residential_buildings.R` collects the finished residential buildings in `residential_selected_buildings.csv` |
| 5. Calculate density | Combine the finished residential and commercial measurements. Calculate floor-area ratio as building square feet divided by land square feet, and homes per acre as units times 43,560 divided by land square feet. Missing or withheld measurements remain missing for the affected outcome. | `calculate_construction_density.R` produces `new_construction_measurements.csv` |
| 6. Attach final geography | Locate each selected building and assign its construction-year ward boundary. Geography may withhold an unlocated observation; it cannot alter its year, units, floor area or land area. | `build_preferred_project_geography.R`, `build_residential_unresolved_final_ledger.R`, `validate_preferred_residential_ledger.R`, `build_preferred_commercial_final_geography.R`; `build_preferred_new_construction_ledger.R` checks the combined handoff |

These are six research steps, not six scripts. Preliminary locations are needed
for some identity and permit matches. Likewise, the completed-condo selector runs
only for buildings not already settled by a recorded building decision; it does
not apply the same year exception again. The final measurement file precedes the
combined geography handoff. The [exact execution order](../../task_graph/construction_steps.md)
and [script graph](../../task_graph/construction_scripts.svg) are generated from
Make prerequisites, including those evidence dependencies.

## Correction ownership

The [decision-input documentation](adjudication/README.md) names the current
owners. Use a general source rule whenever the evidence supports one. Preserve
unavoidable judgments as committed inputs with identifiers, evidence and reasons.
Do not create another correction layer in geography, density or regression code.

- Identity and card-selection exceptions belong to step 2. Completion-year
  exceptions needed by the duplicate matcher are applied there and carried
  forward. Geography may compare with the original reported year to find the
  same assessment, but does not change the corrected construction year.
- Final residential building measurements belong to
  `residential_building_corrections.csv`. Old condo/no-snapshot review tables are
  historical records, not additional active measurement instructions.
- Commercial field and unit-definition corrections belong to
  `commercial_measurement_corrections.csv`. Its source projects cannot also
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

Active final-building rules are in the main Makefile. `legacy_construction.make`
preserves explicitly callable older reconstruction and zoning targets; it is not
another stage of the default cleaning. Earlier evidence is documented in the
[reconstruction history](reconstruction_history.md), decision inputs, release audit
and logbook. Reconnect the paper to the completed new data separately, after
verifying the replication inputs and comparing the estimation sample.
