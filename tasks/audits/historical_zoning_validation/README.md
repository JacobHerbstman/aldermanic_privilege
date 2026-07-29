# Historical Zoning Validation

This audit reconstructs construction-year broad zoning groups for the
2006--2022 density projects within 1,500 feet of a sampled ward boundary. The
1,500-foot limit covers the 500-foot main design and both 500-foot windows
around the cutoffs shifted 1,000 feet for the placebo figures. It does not
change the production zoning control or the paper.

## Official zoning anchors

The three City of Chicago files used here contain zoning ordinances through:

- October 31, 2012: [Zoning_nov2012](https://data.cityofchicago.org/d/p8va-airx)
- July 30, 2014: [September 2014 zoning](https://data.cityofchicago.org/d/nifi-zqag)
- November 18, 2015: [January 2016 zoning](https://data.cityofchicago.org/d/xfyf-x4kx)

The September 10, 2025 City zoning file in `data_raw/` is used only to check
the completed event replay. It is not used as the construction-year group
when its recorded amendment postdates construction.

The first file is named `Zoning_nov2012`, although its portal description calls
it an August 2012 map. The polygon ordinance dates establish the operative
cutoff used in this audit.

### Search for a 2006 archive

No public citywide 2006 zoning shapefile was found in the City Data Portal,
the public Chicago ArcGIS services, or the zoning-map file directories. The
earliest public citywide vector snapshot located for this audit is the October
2012 file above. The [current zoning service](https://gisapps.chicago.gov/arcgis/rest/services/ExternalApps/Zoning/MapServer/1?f=pjson)
is versioned internally, but its public metadata says that the layer is not
archived and does not support queries at a historical moment. An actual 2006
file would therefore have to come from a retained City GIS backup, most likely
through a direct request or FOIA to the Department of Planning and Development
and the City's GIS/technology staff.

## Parcel polygons

Cook County publishes an official parcel-polygon layer for each tax year. The
audit uses the 2012--2022 layers: [2012](https://www.arcgis.com/home/item.html?id=bd26024e1c6546d6a86ad384a7a31765),
[2013](https://www.arcgis.com/home/item.html?id=ea846a11e7a64c6eb7eafcd132c88484),
[2014](https://www.arcgis.com/home/item.html?id=f2d470e08ab441229f6d310fb8c625ab),
[2015](https://www.arcgis.com/home/item.html?id=cb0a110357284b1ab23dedc6d0a34c57),
[2016](https://www.arcgis.com/home/item.html?id=0b86fc37d99c413a8b70a1c2bfc895ba),
[2017](https://www.arcgis.com/home/item.html?id=a45722101ed8491fb71930fd4c2c64ab),
[2018](https://www.arcgis.com/home/item.html?id=9539568a52124b99addb042efd0f83b1),
[2019](https://www.arcgis.com/home/item.html?id=3d3375ac11d147308815d5cf4bb43f4e),
[2020](https://www.arcgis.com/home/item.html?id=577d80fcbf0441a780ecdfd9e1b6b5c2),
[2021](https://www.arcgis.com/home/item.html?id=34021b4f3b834a69bf737e6c3344888e),
and [2022](https://www.arcgis.com/home/item.html?id=d7f375b66b314dfd9f13ead523f615c1).
The cached ArcGIS export for 2020 is unavailable, so that year comes from the
official File Geodatabase linked in its metadata.

For a PIN that exists in the amendment year, the audit uses that exact annual
polygon. Otherwise it finds the same target PIN in the 2012--2022 annual files
and selects the year closest to construction, preferring the later year in a
tie. It then intersects that target polygon with every parcel polygon in the
amendment year. Intersections of one square foot or less are discarded. No
centroid is used to establish parcel lineage.

All 440 date-consistent changed projects have a target polygon and at least one
overlapping event-year parcel. Of these, 181 use the exact event-year PIN and
259 use the same PIN from a later annual file.

## Amendment method

The strict pass starts with the earlier zoning map and applies passed
amendments in date order. Ordinance text supplies the origin and destination
zoning groups. For two-stage ordinances, such as `M1-2 -> B3-3 -> PD`, the
parser follows the sequence to its terminal group. Project-event links
primarily use an exact PIN, official zoning-polygon identifier, or exact
ordinance address. One strict 2012--2014 link uses the existing nearby-parcel
fallback.

The later zoning map is used only to measure agreement. The reported rates are
limited to polygons whose ordinance date falls inside the validation window;
retrospective or undated differences between snapshots are reported
separately.

The polygon pass serves two purposes. First, every strict exact assignment must
have a target polygon that overlaps an event-year parcel. Second, strict
mismatches are reconsidered using only matters passed on the later polygon's
ordinance date whose origin group matches the earlier map. Matter evidence can
come from an exact event PIN, an event geocode inside an overlapping historical
parcel, or an ordinance address covered by overlapping historical parcels. A
two-parcel address bracket is accepted only when two distinct predecessor
parcels on the same street lie on opposite ends of the ordinance range. The
destination is compared with the later map only after the event and polygon
lineage are selected.

The nonstandard map amendments were checked against the official Council
journals, including [A-7914](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public/document_uploads/journals-proceedings/2013/07_24_13.pdf),
[A-7916](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2013_12_11_VI_VII_VIII.pdf),
and [MA-181](https://chicityclerk.s3.us-west-2.amazonaws.com/s3fs-public-1/reports/2014_05_28_VI_VII.pdf).

## Results

| Validation | Sample | Changes | Strict exact | Polygon recovered | Final exact |
|---|---|---:|---:|---:|---:|
| 2012 to 2014 | All density PINs | 231 | 216 | 13 | 229 (99.1%) |
| 2012 to 2014 | Within 500ft | 76 | 74 | 2 | 76 (100%) |
| 2012 to 2014 | Multifamily within 500ft | 26 | 24 | 2 | 26 (100%) |
| 2014 to 2016 | All density PINs | 209 | 206 | 0 | 206 (98.6%) |
| 2014 to 2016 | Within 500ft | 50 | 50 | 0 | 50 (100%) |
| 2014 to 2016 | Multifamily within 500ft | 31 | 31 | 0 | 31 (100%) |

Every strict exact assignment is backed by event-year polygon lineage. The
polygon pass recovers 13 of the 15 strict mismatches in 2012--2014, compared
with five under the earlier centroid-based reconciliation. It leaves the
already exact 2014--2016 assignments unchanged.

The two recovered 2012--2014 projects inside 500 feet are:

- `13354100350000`, 1802 N Sawyer: MA-181 changes RS-3 to POS-2.
- `20114080610000`, 1328 E 53rd: predecessor parcels at 1322 and 1332 E
  53rd overlap the later project parcel and bracket the ordinance address,
  1330 E 53rd. Ordinance `SO2013-796` changes B3-2 to PD 1218.

No date-consistent broad-group change within the paper's 500-foot density
sample remains unexplained in either validation window.

## Remaining cases

Five citywide changes remain unresolved, all outside 500 feet:

- `17294190470000` and `17294190480000`, 1,510 and 1,518 feet from pair
  11--25, are part of map amendment A-7916. The later zoning map applies the
  amendment, but neither polygon overlap nor the ordinance title identifies
  these two parcels independently.
- `14313220560000`, 571 feet from pair 2--32, has two ordinances on the same
  date and the independently selected matter has the wrong destination group.
- `17082070020000`, 1,235 feet from pair 1--27, and `17084290010000`, 829 feet
  from pair 25--27, are multi-parcel sites for which the ordinance geocode lies
  on an adjacent parcel and the title does not enumerate the target parcel.

One exact 2014 parcel, `17092240150000`, overlaps a neighboring official base
parcel by about 3,852 square feet. Its own event-year PIN covers the full target
polygon, its strict assignment is exact, and the extra overlap does not affect
the selected amendment or any reported rate. This is an official parcel-layer
topology issue rather than centroid-lineage error.

## Assessment

The forward exercise now validates the broad zoning reconstruction for the
paper's 500-foot density sample using polygon rather than centroid lineage. It
also raises citywide agreement to 99.1% in 2012--2014 while preserving 98.6%
agreement in 2014--2016.

## Backward reconstruction to 2006

The audit also reads all 118 available Council journal PDFs from January 2006
through October 2012. It identifies 2,832 zoning-ordinance blocks and parses a
broad-group transition for 2,745. The parser keeps the ordinance date,
application number, zoning map sheet, source and destination groups, legal
description, journal page, and source URL.

The candidate map starts with the official October 2012 polygons. Polygons
whose recorded ordinance predates 2006 retain their 2012 broad group. For a
polygon whose recorded ordinance was passed from 2006 through October 2012,
the audit matches the ordinance by date and application number, checks the map
sheet and destination group, and reverses the ordinance only when it has one
unambiguous immediate prior broad group. The modern ordinance extract is used
only when the full journal transition is unavailable. Multi-origin ordinances
are left unresolved because one later polygon cannot be assigned wholesale to
several earlier districts.

The resulting audit GeoPackage contains 11,294 polygons in EPSG:3435:

- 11,070 polygons are assigned a candidate 2006 broad group, covering 98.95%
  of the official 2012 zoning-map area.
- 1,565 post-2005 polygons have one recoverable immediate prior group; 1,050
  of them change broad group and 515 change only within a broad group.
- 224 polygons remain unresolved. Their reason codes distinguish missing,
  ambiguous, multi-origin, and unparsed ordinances.
- The City file contains 104 invalid source geometries. `st_make_valid()`
  changes their combined area by less than 0.00005 square feet. The output
  stores the original validity flag and writes repaired geometries.

The polygon map resolves 13,568 of 13,758 density-project PINs. Coverage is
4,091 of 4,159 within 500 feet and 890 of 922 multifamily PINs within 500 feet.
For the 13,563 projects also resolved by the independent point-level method,
the two implementations agree in every case.

The manual audit reads four ordinances per year, split evenly between
multifamily and other construction. All 28 source groups and all 28
destination groups agree with the journal text. Ordinance map-sheet headers
agree in 26 cases; in the two remaining cases, the written street bounds match
the project but the journal and current City map index use different sheet
labels. A second screen identifies earlier ordinances on the same map sheet
that share at least two boundary streets. Manual review finds three earlier
same-site ordinances, all preserving the same broad group, and no conflicting
earlier broad-group change in the 28-case sample.

### Expanded repeat-event check

The repeat-event screen covers every post-2005 target application in the
1,500-foot reconstruction. Of 235 target applications, 81 have 192 plausible
earlier ordinances based on map sheet, street bounds, or common address. All 81
applications and all 192 candidates were reviewed. Twelve applications have a
same-site earlier amendment, five require a correction to the initially
recovered January 2006 group, and no application remains unreviewed.

The reconstruction provides an exact January 2006 broad group for 9,468 of
9,609 projects in the geographic audit universe. Another 137 have an official
state reached after January 2006 but before construction. Four projects at
714--732 W 25th Street use the stable B2 classification in the 2012, 2014,
2016, and 2025 maps because the application number attached to the 2012 polygon
belongs to an unrelated Webster Avenue ordinance. Every project therefore has
a supported state dated no later than its construction proxy.

| Sample | Projects | Exact January 2006 group | Later preconstruction state | Supported history |
|---|---:|---:|---:|---:|
| All construction within 1,500ft | 9,609 | 9,468 (98.5%) | 137 | 9,609 (100%) |
| Multifamily within 1,500ft | 1,993 | 1,927 (96.7%) | 66 | 1,993 (100%) |

The official [PD 896 file](https://gisapps.chicago.gov/gisimages/zoning_pds/PD896.pdf)
and [PD 447 file](https://gisapps.chicago.gov/gisimages/zoning_pds/PD447.pdf)
resolve the two large planned-development histories whose later amendments
otherwise appear to have multiple origins. The row-level source and review
note for every manual history seed are in
`historical_zoning_paper_sample_seed_reviews.csv`.

This does not establish an exact January 2006 citywide zoning map. It
establishes a supported broad zoning history for the projects needed by the
paper. It does not recover exact district codes or project-specific PD bulk
limits.

## Project-level zoning at construction

The project date is June 15 of the reported construction year, matching the
paper's existing timing rule. Each project starts from the latest state known
before that date:

- reviewed January 2006 history for construction through 2012;
- the official October 2012 map for 2013 and 2014 construction;
- the official July 2014 map for 2015 construction; and
- the official November 2015 map for construction from 2016 through 2022.

Passed amendments are applied in date order only when the current replay state
matches an ordinance origin and the ordinance has one terminal broad group.
The September 2025 group is then used as a check on the completed replay.

Extending the reconstruction beyond 500 feet initially left nine post-2016
histories unresolved. Direct review of the official ordinances resolved all
nine:

- 3130 N Spaulding was legally RT-3.5 at construction, although the ordinance
  narrative describes the new house as single-family and the current map has
  returned to RS-3.
- 728--738 N Milwaukee became PD 1396 in 2018. The automated parser had stopped
  at the ordinance's intermediate DX-5 step.
- 369 W Grand and 9329--9429 S Stony Island entered planned developments before
  construction.
- 2316 W 35th remained RS-3; the range-based link had incorrectly applied the
  neighboring 2312--2314 RT-4 change.
- The Western Avenue corridor and the three 41st Street projects changed after
  construction, so their pre-amendment groups are retained.

The reviewed matter IDs, PDFs, and notes are in
`historical_zoning_project_construction_reviewed_events.csv`. That file also
retains the five ordinance histories reviewed for the original 500-foot audit.

### Validation at 1,500 feet

The construction replay contains 9,609 projects. Six belong to an older
geographic source but no longer appear in the current density file after the
construction-year cleanup. The three-column lookup is therefore restricted to
the exact current production universe: 9,603 projects within 1,500 feet,
including 1,991 multifamily projects. It covers every eligible production PIN.

The extension beyond 500 feet adds 5,448 production projects. Of these, 227
have a construction-year group different from September 2025, 40 use a
manually reviewed history seed, and nine have a directly reviewed modern
ordinance. The union contains 261 assignments. All are retained with their
evidence in `historical_zoning_paper_sample_validation.csv`; none is
unsupported.

The anchor holdouts provide a separate check. Within the exact 1,500-foot
production universe, the 2016-to-2014 backcast reproduces all 151 projects with
a unique reversal. The 2016-to-2012 backcast reproduces 255 of 258. The three
misses are the same Armitage Avenue site, where two same-day ordinances make the
single-reversal test inappropriate. All three projects were constructed in
2017 or 2020 and use the observed November 2015 map directly, so the holdout
exception does not affect their construction-year assignment.

The original deterministic 56-project check remains restricted to the main
500-foot sample. It reads 47 official ordinance PDFs and confirms all 56
assignments. The broader 1,500-foot validation adds the 261 changed or
manually reviewed assignments described above.

## Frozen lookup provenance

`historical_zoning_project_construction_year_lookup.csv` is the source for the
frozen production file in `tasks/density_construction_zoning`. It has exactly
three columns:

- `pin`: the original whole-building assessor PIN;
- `construction_year`: the production construction year; and
- `construction_zone_group`: the broad zoning group in force at the June 15
  construction proxy.

The current candidate's SHA-256 checksum is
`50c84dbf114bc5f3467a269354bdf6052557d9eb35a1ca54fc51eae3d275215e`.

Its lineage is:

1. The current density file supplies the exact 2006--2022 PIN universe within
   1,500 feet (`merge_in_scores/output/parcels_with_ward_distances.csv`).
2. Official Chicago zoning snapshots provide the October 2012, July 2014, and
   November 2015 anchors. The September 2025 map is a replay check.
3. Cook County's 2012--2022 annual parcel polygons connect changing historical
   PINs by polygon overlap rather than centroid proximity.
4. The City Council journal archive supplies 2006--2012 ordinance text. ELMS
   matter metadata and official ordinance PDFs supply later amendments.
5. Official planned-development files resolve PD 896, PD 447, PD 15, and PD
   1145 histories that cannot be read from a single later zoning polygon.
6. The reviewed January 2006 state and later passed amendments are replayed to
   the construction proxy. The lookup is then intersected with the current
   production PIN universe and checked for one row per PIN, matching
   construction years, complete zoning groups, and full 1,500-foot coverage.

The production copy is fixed at the checksum above. Changes to the audit
reconstruction do not alter the paper unless the production file is reviewed
and deliberately replaced.

## Main outputs

- `historical_zoning_project_construction_year_lookup.csv`: the three-column,
  9,603-row candidate lookup for production.
- `historical_zoning_paper_sample_validation.csv`: the 261 added assignments
  that differ from 2025 zoning or required direct review.
- `historical_zoning_paper_sample_validation_summary.csv`: coverage and
  holdout counts.
- `historical_zoning_project_construction_year.csv`: the full 9,609-row audit
  replay with status and event fields.
- `historical_zoning_project_construction_event_log.csv`: every linked and
  applied event in the construction and current-map replays.
- `historical_zoning_project_construction_reviewed_events.csv`: official PDF
  evidence for directly reviewed modern ordinances.
- `historical_zoning_paper_sample_history_seeds.csv`: the reviewed starting
  state for each project.
- `historical_zoning_paper_sample_seed_reviews.csv`: source links and notes for
  all manual history seeds.
- `historical_zoning_2006_candidate.gpkg`: the broader candidate January 2006
  polygon reconstruction. It should not be described as an exact citywide map.

Run `make` from `code/`. In the current worktree the recursive check of the
upstream rezoning task stops on its pre-existing missing `O2018-3281` correction;
the outputs above were rebuilt directly from the already linked audit inputs.
