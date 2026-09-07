# Construction grouping: 4400 Grove, Natchez, and Roosevelt Square

September 7, 2026. Evidence review on `sales_sample_exploration`, against the
frozen paper input. No production measurement, eligibility flag, or estimate was
changed. Recommendations below are not adopted decisions.

Jacob has confirmed that new buildings replacing demolished buildings qualify,
while renovations, additions, and conversions do not themselves qualify. He has
approved combining the two 4400 Grove buildings, requested investigating a
combined Natchez observation dated to full completion, and left Roosevelt Square
grouping open.

## Current paper observations

| Observation | Year | Units | Building sq ft | Land sq ft | FAR | Units/acre | Boundary distance, ft |
|---|---:|---:|---:|---:|---:|---:|---:|
| 4400 Grove, commercial_20034130510000 | 2020 | 84 | 114,070 | 196,020 | 0.582 | 18.67 | 319.0 |
| Natchez B, commercial_13312140010000 | 2020 | 84 | 124,620 | 210,101 | 0.593 | 17.42 | 419.6 |
| Roosevelt, residential_17173230250000 | 2006 | 6 | 4,202 | 3,633 | 1.157 | 71.94 | 208.5 |

All three pass the frozen main density sample's year, 500-foot, positive outcome,
eligibility, control-availability, and geographic-key filters. These are existing
observations, not hypothetical future candidates. Estimates have not been rerun.

## 4400 Grove: combine the buildings, correct the scope of land

The pinned City source contains permits 100795428 and 100795429, both issued
December 3, 2018. Each authorizes a four-story, 42-unit building with ground-floor
retail/support space. The recorded addresses are 4424 and 4434 S Cottage Grove;
both use predecessor PIN 2003413047. Both now have completed status and an
occupancy-certificate milestone. Issue dates are not completion dates.

The architect's [opening announcement](https://www.pappageorgehaymes.com/4400-grove-new-mixed-use-mixed-income-housing-development-complete-in-bronzeville/)
reports an October 1, 2020 opening for the two-building first phase, and identifies
a later phase on the west part of the site. Its
[project page](https://www.pappageorgehaymes.com/projects/4400-grove/) supplies
84 units, 114,070 gross square feet, and 4.5 acres. These are the origins of the
current manual measurements, including 196,020 = 4.5 × 43,560 square feet of land.

The primary planning evidence now distinguishes the scopes. The City's
[PD 1395 compilation](https://gisapps.chicago.gov/gisimages/zoning_pds/PD1395.pdf)
contains the original January 17, 2018 approval. PDF page 12 assigns 102,637 net
square feet and 84 units to Subarea A. The whole development has 194,189 net square
feet, including Subareas B and C. PDF page 16 maps the separate areas. The February
3, 2023 letter on PDF page 2 explicitly identifies the completed 84-unit Phase I
as Subarea A and discusses later construction in B and C.

The Assessor provides another independent comparison:

| Source | Year built | Units | Building sq ft | Land sq ft | Component PINs |
|---|---:|---:|---:|---:|---|
| Commercial assessment 2021 | 2020 | missing in tot_units | 118,806 | 97,215 | 20034130510000 / 20034130550000 |
| Commercial assessment 2024 | 2021 | 42 | 59,403 | 0 | same two PINs |
| Current manual decision | 2020 | 84 | 114,070 | 196,020 | combined project |

The later commercial record halves the earlier floor area and reports 42 units,
despite retaining both component PINs. It cannot simply replace the complete
project measurement. Archived 2021 parcel polygons for the two PINs measure
74,911.421 and 22,304.692 square feet in EPSG:3435, totaling approximately
97,216.113. That independently reproduces the 97,215 assessed area, allowing for
geometry precision. These are 2021 polygons, not yet a verified 2020 union.

**Finding:** the current 4.5-acre denominator covers the wider development and is
too broad for the completed first-phase numerator. Agreement on combining the
two buildings does not validate this denominator.

**Still unresolved:** the roughly 5,422-square-foot difference between the
approved Subarea A and the two assessed parcels. We must reconcile boundaries,
plaza/open-space ownership, and the construction-year parcel configuration.
Also, 118,806 Assessor square feet versus 114,070 architect gross square feet
remains a 4.2% difference relative to the architect measure; neither should be
silently substituted. Both include a mixed-use project, so this review does not
authorize changing the paper's existing gross-building-area definition.

Holding 84 units and the current 114,070 building square feet fixed:

| Land choice | FAR | Units/acre | Interpretation |
|---|---:|---:|---|
| Current 196,020 | 0.582 | 18.67 | Wider-development denominator; unsupported for these two buildings |
| Approved Subarea A: 102,637 | 1.111 | 35.65 | Phase-specific planning area; legal parcel difference unresolved |
| Assessor: 97,215 | 1.173 | 37.64 | Supported by two 2021 parcel polygons; 2020/common-space scope unresolved |

These are arithmetic scenarios, not new regression results. Keeping the current
project location would leave its 500-foot status unchanged; a change in project
geometry requires a new distance assignment.

**Recommendation:** preserve the approved combined 84-unit observation and 2020
completion year. Replace the whole-development denominator only after resolving
the narrow 97,215-versus-102,637 scope question. Do not split the buildings to
work around the land problem or accept the incomplete 2024 record.

## Natchez: a defined 84-unit area, with uncertain completion year

The preserved [2018 City submission](../../data_raw/construction_review/SO2018-4452.pdf),
PDF pages 10 and 15, distinguishes three areas:

| Area | Plan's physical scope | Plan's construction status | Current paper representation |
|---|---|---|---|
| A, northern area | 12 six-unit buildings, 72 units | Completed | PIN 13312050690000, 2017, within 500 feet |
| B, southern area | 14 six-unit buildings, 84 units | In progress | PIN 13312140010000, 2020, within 500 feet |
| C, western expansion | Five six-unit and three three-unit buildings, 39 units | Proposed | Separate 39-unit record, outside 500 feet; its membership has not been validated by this review |

The B polygon in the archived 2020 parcel source has approximately 210,101.234
square feet, matching the 210,101 Assessor denominator. Its shape corresponds to
the southern area on the plan. This supports a single measured group rather than
arbitrary aggregation under a common development name. It does not independently
validate every component's floor area or completion date.

Fourteen distinct 2017 new-construction permits describe six units each, for 84
units. Their addresses lie along the private Dickens/Shakespeare roads shown in
the B site plan. Shared predecessor PINs also appear on later permits for C, so a
PIN match alone would incorrectly add the later expansion.

| Issue date | Permits and recorded addresses |
|---|---|
| 2017-09-26 | 100716815: 6535 W Shakespeare |
| 2017-09-27 | 100716830: 6536 W Dickens |
| 2017-10-17 | 100716743: 6531 W Dickens; 100716755: 6533 W Dickens; 100716809: 6539 W Shakespeare |
| 2017-10-18 | 100716645: 6541 W Dickens; 100716652: 6543 W Dickens; 100716661: 6545 W Dickens; 100716672: 6547 W Dickens; 100716726: 6544 W Dickens; 100716766: 6535 W Dickens; 100716776: 6537 W Dickens; 100716791: 6539 W Dickens; 100716823: 6542 W Dickens |

All fourteen have occupancy-certificate milestones in the pinned source. Some
descriptions preserve previous addresses. Prototype permit references describe
reused plans and do not establish that two different permitted buildings are one
building. The 2019 Nashville permits instead sum to 39 units plus a community
center, matching the separately proposed C expansion.

The timeline is less settled than the old review note implied:

1. In assessment years 2018 and 2019, the residential Assessor source contains
   only six cards for B, each reporting construction year 2017, six units, and
   7,874 square feet. Their 36 units and 47,244 square feet are an incomplete
   source representation, not proof that B should be defined as a 36-unit phase.
2. The 2018 City plan labels B construction in progress.
3. The commercial assessment for 2021 reports year built 2017, 124,620 building
   square feet, and 210,101 land square feet.
4. The commercial assessment for 2024 reports year built 2020 and 84 units, with
   exactly the same 124,620 building and 210,101 land square feet.

The old ledger says the 84 units span two construction phases but does not name
their buildings or document their completion dates. This review reproduces the
84-unit building-permit total, not that asserted phase chronology. The developer's
[Phase IV page](https://www.noahchicago.com/properties/north-oak-lofts-phase-iv)
identifies 6537 W Dickens, while its
[Phase III page](https://www.noahchicago.com/properties/north-oak-lofts-phase-iii)
identifies 2116 N Natchez, a different frontage. Marketing phase numbers and City
subarea letters should not be treated as equivalent without a crosswalk.

Secondary leasing material suggests at least some B apartments were available
before 2020. For example, a [broker listing](https://www.exprealty.com/chicago-il-real-estate/apt-3w-6536-dickens-ave)
reports 2018 construction for 6536 W Dickens. This is a lead for corroboration,
not evidence that every B building was complete in 2018. No exact final
completion date for all fourteen buildings has been established here.

**Recommendation:** keep B combined as 84 units on its measured site. Do not merge
A, B, and C merely to manufacture one final completion date. The choice still
needed is how to treat the unresolved full-completion year: obtain building-level
occupancy dates, or explicitly accept an Assessor-year proxy with sensitivity
analysis. Calling 2020 verified full completion would overstate the evidence.
Changing only the year leaves the density arithmetic unchanged, but requires
reassigning year-dependent geography, zoning, aldermen, and scores. No such
reassignment or sensitivity estimation was performed in this review.

## Roosevelt Square: physical grouping, unit count, and entry year are separate questions

The specific building is 1217 W Arthington Street, PIN 17173230250000. It is part
of Roosevelt Square Phase I, but a whole-neighborhood development is not the same
physical object as this building. [Brook Architecture's account](https://brookarchitecture.com/roosevelt-square)
describes its Phase I contribution as 28 residential buildings and one mixed-use
building, 137 units, with 2006 completion. It is an architect's subset, not the
complete phase roster. The [developer's current account](https://www.related.com/our-company/properties/roosevelt-square)
describes an ongoing 67-acre development with later new buildings and renovation
of existing Phase I apartments. The neighborhood should not become one observation
dated to the latest phase.

The exact-parcel evidence is conflicting:

| Evidence | Year | Units | Building sq ft | Land sq ft |
|---|---:|---:|---:|---:|
| Residential assessments 2009–2020 | 2005 | 6 | 4,202 | 3,633 |
| Residential assessments 2021–2026 | 2006 | 6 | 4,202 | 3,633 |
| Renovation permit 100959844, issued 2022-08-31 | Existing building | 4 | not stated | not stated |

The renovation permit matches both the address and exact PIN. Its description
concerns replacement of doors, plumbing fixtures, kitchen equipment, and finishes;
it does not explicitly report a conversion from six to four apartments. Thus it
could reflect a source error, an earlier undocumented change, or a problem with
the Assessor count. It is not sufficient evidence to overwrite six with four.
The 2023 contractor-change permit references the same 2022 permit and is not
independent evidence of the count.

The 2006 historical parcel polygon measures approximately 3,644.371 square feet,
close to the reported 3,633. The main unresolved denominator concern is whether
other shared land belongs to the building, not evidence that this small parcel
contains the entire Roosevelt Square development.

The year change is consequential: 2005 would place this building outside the
paper's 2006–2022 construction window; 2006 retains it. The pinned permit source's
earliest issue date is January 3, 2006. Failure to find its original construction
permit in this source cannot rule out 2004–2005 construction.

At unchanged floor and land areas, four rather than six units would reduce units
per acre from 71.94 to 47.96, leaving FAR 1.157. That is an illustrative scenario,
not an accepted correction. Aggregating Phase I would also change observation
weights and locations, and could bring together buildings on opposite sides of
the sample's distance cutoff. A complete component roster and land union are
needed to calculate that alternative; this review has not constructed them.

Five other Roosevelt Square commercial records explicitly mentioned together in
the existing cross-family ledger are dated 2008. Four are outside 500 feet
(PINs 17173320070000, 17173320160000, 17201020620000, 17201020640000); one,
17201030600000, is inside. They are examples of spatially distinct observations,
not a complete list of Phase I membership or a reason to merge them with Arthington.

**Recommendation:** retain building-level identity for Arthington while checking
whether it shares project land with adjacent buildings. Resolve the four-versus-six
units and 2005-versus-2006 timing before final density eligibility. A broad
development-level year or unit total cannot settle those building-specific facts.

## Evidence preservation and reproduction

The source extracts remain unchanged. The ordinary Make target
`../report/phased_project_permits.csv.log` in this audit's `code/` folder produces
181 permit search records (43 Grove, 132 Natchez and vicinity, 6 Arthington).
These are search results, not accepted memberships. Unique source IDs are asserted.
The output retains permit status, milestone, dates, descriptions, and source PINs.
No production task consumes this audit output.

Assessor comparisons use `data_raw/construction_review/commercial_valuation_data.csv`
filtered on normalized `keypin`, and the normalized full residential source in
`tasks/prepare_construction_assessor_history/output/residential_assessor_history.parquet`
filtered on `pin`, ordered by `tax_year, card_num`. Frozen sample values come from
`tasks/new_construction_analysis_data/output/new_construction_analysis_data.csv`.
Parcel areas were computed with `sf::st_area` in EPSG:3435 on the matching
`pin14, target_year` rows of
`data_raw/construction_review/preferred_historical_parcel_source.gpkg`.

The Natchez plan was already preserved as `SO2018-4452.pdf`. The newly acquired
Grove plan is owned by `tasks/download_construction_development_records/` with an
explicit Make download rule and recorded checksum. Docling was unavailable;
PDF text was extracted with `pdftotext -layout`, with visual checks of the Natchez
master plan and Grove numeric land table. Underlying web review pages were read
September 7, 2026; a later web page may change. No manual override has been changed
on the basis of this review.
