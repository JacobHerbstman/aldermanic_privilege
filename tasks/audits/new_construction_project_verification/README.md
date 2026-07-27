# Project-level new-construction verification

This audit reviews all 795 projects within 500ft that the current eligibility
rule initially retained principally because the Assessor designates them as new
construction. It does not alter the production sample or paper.

The audit compares each project site with three official building-footprint
snapshots:

- Cook County's 2008 building-footprint layer;
- the City of Chicago's archived August 2015 building-footprint file; and
- Cook County's 2022 building-footprint layer.

For projects after 2008, a structure that appears between snapshots or a
replacement footprint with little overlap provides independent physical
evidence. A footprint year within one year of the Assessor's construction year
is recorded separately as corroboration because that field may share an
administrative source with the Assessor data.

The audit also incorporates the already adjudicated multicard permit links.
An explicit new-building permit counts as independent evidence only when its
address or point is tied to the project site and its year is within one year
of the selected construction year. Completed external property reviews are
recorded as corroboration of project identity, not independent verification
of construction timing.

The same rule is applied to the full issued-permit file. A permit must be
classified by the City as new construction, fall within 25ft of the historical
project site, have an application year within one year of the selected
construction year, and not describe work on an existing building or an
accessory structure.

`project_verification_ledger.csv` contains exactly one row per project and
records the source, measurements, rule, and evidence. The rule is deliberately
conservative. Projects not resolved by the snapshots remain in
`project_verification_review_queue.csv`; they are not silently counted as
verified. `snapshot_rule_sensitivity.csv` reports how the unresolved count
changes under nearby appearance and replacement thresholds.

The review queue separately flags an older reported structure in the 2008
footprint file and a substantially unchanged footprint across the relevant
before-and-after snapshots. These flags identify cases requiring scrutiny;
they do not automatically exclude a project because a replacement building
can occupy much the same footprint as its predecessor.

The manual-review queue uses the nearest-year historical parcel address when
available, then the 2025 parcel address for a surviving PIN. Every row also
contains a coordinate-based Google Maps link, including projects whose
historical address is unavailable.

The official services are:

- <https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2008/MapServer/1>
- <https://data.cityofchicago.org/Buildings/Building-Footprints/syp8-uezg>
- <https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0>

## Final review

Every candidate received a final disposition. The review retains 740 projects
and excludes 55. Among the retained projects, 205 are multifamily, 737 have the
fields required for FAR, and all 740 have the fields required for DUPAC.
Fifty-one selected construction years changed during the review.

The final integration preserves the dwelling units, building area, land area,
and multifamily classification from the previously validated analysis file.
Those fields change only when the manual review supplies an explicit
replacement. This applies six reviewed unit-count corrections and three
reviewed multifamily-classification changes; it does not infer building type
from an aggregated project-level unit count.

The evidence fields distinguish independent project records, corroborating
records, manual project review, and manual site review. All 795 candidates were
reviewed, but the audit does not claim that every historical project has an
independent second-source record. That distinction is unavoidable for some
older small projects and is preserved in
`final_project_verification_ledger.csv`.

`final_project_verification_checks.csv` records the final counts.
`final_project_verification_unresolved.csv` is empty. The checks require unique
project IDs and component PINs, complete dispositions, at least one valid
density outcome for every retained project, locations within 500ft, explicit
resolution of permit conflicts, and an empty duplicate-review queue.

## Final analysis file

`final_verified_density_input.csv` applies the approved decisions to the
provisional 1,500ft analysis file. It then rebuilds construction dates, ward
and boundary assignments, distances, boundary segments, aldermen, ward-year
controls, zoning groups, and score variables. Corrected years use an official
zoning snapshot when the broad zoning group is stable across the relevant
interval. Twelve projects whose interval is not stable have explicit decisions
in `adjudication/corrected_year_zoning_decisions.csv`.

The resulting file contains 8,648 projects within 1,500ft and 3,710 within
500ft. Two retained projects move beyond 1,500ft and three additional projects
move beyond 500ft when their corrected construction years place them under a
different ward map. `final_verified_density_geography_changes.csv` records
these changes.

In the common FAR-DUPAC sample with ward-pair clustering, the binary
more-stringent-side estimates are:

| Sample | Log(FAR) | Log(DUPAC) |
|---|---:|---:|
| All construction | -0.119 (0.047) | -0.118 (0.065) |
| Multifamily | -0.168 (0.081) | -0.207 (0.093) |

`final_verified_density_checks.csv` verifies the final counts, exclusions,
component-PIN uniqueness, field replacements, geography changes, and model
coverage. The production task should not reproduce the footprint downloads,
permit searches, web review, or diagnostics in this audit. It can consume a
small frozen analysis file with documented provenance.
