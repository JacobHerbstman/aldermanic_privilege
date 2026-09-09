# Construction cleaning

The intended observation is a newly constructed residential building or a
coherently measured group of buildings. Its construction year, units, floor area,
and land area must describe the same object. Wholly new replacement buildings
qualify; renovations, additions, conversions, and changes in parcel numbers do
not themselves create new construction. Separately measured homes stay separate.

**This builds candidate projects and their locations. It does not yet rebuild the
paper's final density dataset.** The paper still uses the frozen file in
`../new_construction_analysis_data/`. The older final-assembly rules have missing
inputs and remain outside the default candidate build.

## Read the cleaning in this order

| Step | What the code does | Main scripts in `code/` |
| --- | --- | --- |
| 1. Read the recorded sources | Read the pinned Assessor histories, complete permit history, parcel addresses, and ward maps. Remove Assessor records with neither floor area nor a unit count before selecting a usable assessment. | `build_residential_cross_section.R`, `build_residential_discovery_cross_section.R`, `build_commercial_cross_section.R`, `build_construction_permit_history.R` |
| 2. Find possible new construction | Follow parcel numbers and historical addresses to find candidate buildings and locations. An initial boundary-distance screen helps define the candidate population. | `build_historical_coordinate_requests.R` through `select_historical_coordinates.R`, `geocode_residential_data.R`, `build_construction_parcel_distances.R` |
| 3. Decide which records describe the same buildings | Distinguish individual homes, several cards for one property, and linked parcels. Choose one complete assessment for a multiple-card measurement. Match older combined records to individual successors only when the recorded year, area, location, and land evidence support it. Apply recorded exceptions here. | `build_residential_project_candidates.R`, `build_residential_tieback_temporal_evidence.R`, `build_residential_assessor_projects.R` |
| 4. Check permits and construction years | Match permits to candidates, recover historical parcels where necessary, and link revisions of the same permit. An exact single permit chain can correct an Assessor year one year before its application. Reviewed years retain priority. Ambiguity remains recorded. | `build_new_construction_permit_evidence.R`, `build_historical_project_geography.R`, `build_spatial_permit_evidence.R`, `build_permit_revision_evidence.R`, `build_preferred_residential_candidates.R` |
| 5. Locate the selected projects | Match the construction-year parcel. If it is unavailable, use the recorded parcel/address evidence and the approved fallback rules. Address matches require street agreement; Chicago geocoding takes priority over Census where used. An exact parcel's coordinate from one year later can identify a unique construction-year polygon. | `build_preferred_geography_requests.R` through `recover_preferred_historical_predecessors.R`, `build_preferred_project_geography.R` |
| 6. Measure boundary distances | Use the selected project geometry and the ward boundaries for its construction year, in the Chicago working coordinate system (EPSG:3435, feet). Preserve unresolved locations. | `build_preferred_boundary_scope.R` |
| 7. Finish the commercial-source branch | Use Assessor, permit, footprint, and recorded evidence to distinguish new buildings, construction timing, units, and land. Then locate the selected year and component set; unresolved locations remain ineligible. | `build_preferred_commercial_candidates.R` through `build_preferred_commercial_ledger.R`, then `build_preferred_commercial_final_geography.R` |

This table summarizes the logic; it does not imply seven scripts or perfectly
separate stages. In particular, permit checking needs preliminary project
geography, and final geography uses the subsequently selected projects. The
[exact script order](../../task_graph/construction_steps.md) and
[script dependency diagram](../../task_graph/construction_scripts.svg) are
generated from the actual Make prerequisites. Independent steps can run together.

## Measurement and decision rules

- A card is an Assessor record, not necessarily another building. Empty records
  are removed before assessment selection. Several cards are not automatically
  several projects.
- For a multiple-card project, use one complete assessment snapshot under the
  existing 2022, then 2025, then later priority. Do not assemble measurements from
  incompatible assessments. Initial discovery records are not final measurements.
- Automatic predecessor/successor matching uses the approved two-year window,
  together with measurement and location checks. The current Make options are
  listed in [construction_settings.make](code/construction_settings.make).
  A close construction year alone is not enough to declare a duplicate.
- Use source-reported land and floor areas that cover the buildings being counted.
  Do not replace a missing density denominator with a manually calculated polygon
  area. Parcel maps support identity, location, and boundary distance.
- General rules run in the producing scripts. Exceptions remain in the
  [recorded decision inputs](adjudication/README.md), with source identifiers and
  reasons. Cleanup does not reopen approved decisions or replace them with guesses.
- Missing or conflicting evidence remains explicit. Candidate counts and review
  counts are not final regression sample sizes.

## Build and inspect

Run `make` from `tasks/new_construction_cleaning/code/`. It builds the current
candidate path and the standard data reports. The main products are:

- `output/preferred_residential_project_candidates.csv`: residential project
  identity, measurements, proposed treatment, and reasons.
- `output/preferred_commercial_projects.csv`: reviewed commercial-source projects.
- `output/preferred_project_boundary_scope.csv`: candidate construction-year
  boundary distances and geography status.
- `output/preferred_commercial_project_ledger.csv` and
  `output/preferred_commercial_boundary_scope.csv`: selected commercial records
  and their construction-year distances; missing locations are explicitly ineligible.

The main [Makefile](code/Makefile) contains their current ancestors. Earlier
review and final-assembly targets remain explicitly callable through
[legacy_construction.make](code/legacy_construction.make), preserving existing
callers while excluding them from the default candidate build. Moving these rules
out of `all` does not supply their missing inputs or establish full replication.

Source acquisition belongs to the upstream download tasks. Ordinary cleaning
reads local recorded responses; source refreshes are deliberate. Data reports
are in `report/`, and research checks live in `../working_paper_release_audit/`.
Rebuild the diagrams with `make -C task_graph` from the repository root.

## Before replacing the paper's frozen input

The unfinished final assembly still asks for residential review footprints and
condominium successor-year evidence without complete producing rules. The current
commercial location producer now reads the recorded historical parcels directly;
31 other selected commercial records still lack accepted final locations. Reconcile those old requests against the
current approved project decisions before choosing the final assembly path.
Then verify one final row per included observation, eligibility for each density
measure, and construction-year distances. Compare the new sample and estimates
with the frozen file before switching the paper to it.

The agreed retention list has now been carried through: 30 residential records
and seven named commercial records have retained measurements and supported
construction-year distances. The original 30 parcel requests were not 30 remaining
building decisions. See the [current land review](../working_paper_release_audit/commercial_land_followup.md)
for the completed exclusions/retentions and the 56 older commercial map-denominator
choices still awaiting source-land decisions. The [preserved reconstruction history](reconstruction_history.md)
records earlier evidence and comparisons. A successful candidate build is not a
claim that the final paper input or every reviewed measurement is complete.
