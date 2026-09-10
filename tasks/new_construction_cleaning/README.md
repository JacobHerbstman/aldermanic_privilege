# Construction cleaning

The intended observation is a newly constructed residential building or a
coherently measured group of buildings. Its construction year, units, floor area,
and land area must describe the same object. Wholly new replacement buildings
qualify; renovations, additions, conversions, and changes in parcel numbers do
not themselves create new construction. Separately measured homes stay separate.

**This builds candidate projects, selected residential and commercial records,
and their construction-year boundary distances. It does not yet rebuild the
paper's final density dataset.** The paper still uses the frozen file in
`../new_construction_analysis_data/`. The combined final-assembly rules still have missing
inputs. Unfinished residential records remain explicitly recorded for review;
the selected residential file is not a claim that every candidate is settled.

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

## Current decision reconciliation

The [current unfinished-record list](../working_paper_release_audit/output/current_construction_questions.csv)
compares current records with the existing decisions. The residential candidate
producer now carries through six recorded exclusions for buildings that are not
new construction and one recorded duplicate whose successor still matches its
units and floor area. The remaining 190 review labels are not all new judgments.
The list also includes retained homes whose locations still use the center of an
older larger parcel; a computed distance does not establish an individual location.

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
choices now replaced by reported source areas or density exclusions. The selection script matches the full selected parcel set before using Assessor land; five external reported measurements are explicit reviewed inputs. Maps supply locations, not these density denominators. The [preserved reconstruction history](reconstruction_history.md)
records earlier evidence and comparisons. A successful candidate build is not a
claim that the final paper input or every reviewed measurement is complete.

### Individual residential locations

For an individually recorded home on a former parcel, `build_preferred_project_geography.R` first uses the existing construction-year/next-year parcel point. If unavailable, it uses the latest saved exact-PIN point whose assessment from the same year reports the selected units, floor area, land area and construction year. Existing committed construction-year corrections supply the expected reported year for this comparison. Only assessments with one occupied card qualify, and the point must fall within the historical site (0.001-foot tolerance for coordinate rounding). If that check cannot supply a point, an already accepted exact Chicago address point can locate the individual property, subject to the same site check. The address producer requires a unique score-100 point-address match, including street direction and house number.

These rules retain the source-reported density measurements and the construction-year ward map. A later vacant or exempt classification does not itself invalidate earlier construction, but it does not satisfy the same-assessment check either. Location provenance is retained in the centroid output. Location success does not adjudicate contradictory construction years or prove that a recorded episode was a new building.

## Residential selection after the source checks

The ordinary build now reaches the selected residential file through the existing
selection scripts. `build_residential_class297_resolution.R` counts complete
residential condo records, excluding parking and common-area records, and uses
whole-building areas once. It requires one matched building and agreement in
unit count. A completed new-building permit can also confirm the finished
residential count when the earlier development count is incomplete or includes
commercial space. This requires one linked permit chain, one completed full-building
permit, and one unambiguous residential count across its linked permit mentions.
The completed Assessor record supplies the construction-year proxy;
a recorded stronger completion decision takes precedence. A uniquely matched,
complete building dated outside 2006–2022 is explicitly excluded from this sample,
even when its earlier development record had an in-period year. If the combined candidate lacks units, all
component source records must agree on that count and year. Missing floor area
prevents floor-area-ratio use, while a supported unit count and reported land
still permit homes-per-acre density. Conflicting land areas remain unresolved.

`build_residential_overlap_resolution.R` applies recorded residential/commercial
choices only after checking the current replacement and parcel coverage. A
residential placeholder with no usable floor area can be replaced by a complete
commercial record for the identical parcel set and construction year. An
existing commercial decision to defer to residential data is applied only when
the residential measurements are complete and no retained commercial record
still overlaps them.

`build_residential_tieback_episode_resolution.R` checks whether a combined
property repeats a complete list of single-family house cards across its parcels.
It suppresses that old record only when already retained individual homes inside
the historical site match the number of cards, construction episode and floor
areas. An earlier complete assessment of those same individual homes may establish
identity; their selected measurements remain unchanged. The existing two-year
window and two-percent floor-area tolerance apply. Tax allocation shares alone
do not establish that a card is empty or another building.

`build_residential_review_resolution_ledger.R` records retained projects,
verified replacements and still-unresolved sources. `build_residential_unresolved_final_ledger.R`
combines the supported projects with ordinary candidates, checks unique parcel
membership, and carries forward only accepted locations for the identical
construction year. `validate_preferred_residential_ledger.R` then assigns wards
and distances using that year. These scripts replace the old incomplete manual
review assembly, rather than adding corrections after density calculation.

The selected products are `preferred_residential_project_ledger.csv`,
`preferred_residential_project_components_final.csv`,
`preferred_residential_project_centroids.gpkg`, and
`preferred_residential_boundary_scope.csv`. Every selected record is traceable to
its source project; unresolved records remain in `residential_review_source_dispositions.csv`.
The paper's frozen data are unchanged. Restoring the combined final density,
zoning and analysis handoff is separate unfinished work.

Permit histories retain explicitly linked permits of every type, including
alterations that revise a new-building permit. Only new-construction permits
supply new-building counts and dates in the corresponding completion checks.
Numeric unit mentions include parenthesized numbers and combined wording such as
“residential dwelling units”; mentions remain evidence, not automatic final counts.
For a development with separately numbered buildings, the permit summary may
corroborate its Assessor total by adding one residential count per completed
new-building permit. Building numbers must form a complete consecutive list;
revisions, ambiguous counts and missing buildings prevent this confirmation.
An explicitly identified community-center/amenity permit contributes no homes.

### September 9: complete independent reports after historical self-references

For a historical lineage containing just one PIN, the Assessor project producer may use a later complete independent assessment when the PIN, class, construction year, floor area and residential count agree with the selected record. There must never have been concurrent multiple cards; the later assessment must contain exactly one card, no tied-property key, full parcel allocation and positive source-reported land and floor areas. The whole later report supplies the measurements and row identifier, preferring 2025 among later reports. This does not split a genuinely multi-PIN building or allocate land using tax shares. It resolves seven Calumet townhomes and 1705 W Chicago using their reported individual lots.

Peoria Green is a recorded exception in `adjudication/residential_class297_exceptions.csv`: one two-wing project with 17 homes completed in 2021. Complete permit 100876378 reduces the earlier 20-home plan to 17; contemporary completion reporting agrees. All 17 entries in its 2022 condominium base report 56,295 square feet of whole-building floor and 16,695 square feet of land. Those repeated areas enter once. The approved complete-building decision supersedes the earlier spatial-link-only entry in `residential_successor_condo_overrides.csv`; that input retains its schema but has no remaining rows.

The completed-condominium resolution also checks unresolved single-card residential records with a uniquely matched finished condo base; an intermediate class-297 designation is not required. This applies the same completed-year rule to 1920 W Cullerton, whose three completed unit records report 2023. Tribune Tower at 435 N Michigan is separately recorded as an existing-building conversion, supported by completed City permit 100757281; it is excluded under the established ground-up definition.

### Complete individual lots can identify a retired house group

The house-duplicate producer also recognizes a complete parcel partition: the retained individual house lots must cover the former project site with neither gaps, overlap nor outward extension above the existing 0.5% parcel tolerance. Their count must equal a complete old house-card list and their years must lie within the existing two-year episode window. All old records must share one tied-property key; repeated card numbers must agree on floor area, each parcel must supply its declared card count, and at least one must contain the entire common card list. This allows partial copies on other tied parcels. The old source record is suppressed and the already selected individual homes remain unchanged. Polygon areas are used only to establish identity; density continues to use reported land.

This resolves the old Lansing, Throop and Orchard groups. It does not authorize a wider floor-area tolerance, fill missing measurements, create additional homes, or replace reported land with map calculations. The earlier complete-list/floor-agreement rule remains available for other duplicate groups.

The default build also combines selected residential and commercial projects in
`preferred_new_construction_project_ledger.csv`, with component membership,
construction-year boundary distances, centroids and standard data reports.
Recorded residential-to-commercial replacements are applied before residential
selection is exported; the combined producer rejects overlapping parcel membership.
Records already excluded from both density outcomes can remain without locations
or usable measurements. Only located records enter the centroid file. These
candidate products do not yet replace the frozen paper estimation input.

Completed condominium checks also cover ordinary tied-parcel residential records
with a recorded successor-condominium match. Unlike development-class records,
these require one unambiguous completed new-building permit confirming the finished
home count. Existing membership, one-building identity, assessment consistency and
source-year rules remain in force. The central review collector assigns these
records to condominium review instead of also issuing a townhouse disposition.

## September 9 construction review completion

All 268 cases in the fixed construction review are closed, and the current
unresolved-case list is empty after the final approved decisions. The verified
combined candidate has 13,764 project records, with explicit density exclusions.
This completes the reviewed building/measurement/location questions. The paper
still uses its frozen analysis input; reconnecting and rerunning estimation is
separate work, not implied by this review count.
