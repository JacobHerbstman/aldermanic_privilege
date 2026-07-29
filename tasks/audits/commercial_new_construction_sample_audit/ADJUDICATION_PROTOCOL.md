# New-construction project adjudication protocol

## Scope

The preferred file contains one observation per residential construction project
completed from 2006 through 2022. A project is ordinarily one building. It can
contain several PINs when those PINs describe one building or a coordinated
development on common land. Several buildings on one PIN remain one site-level
project unless permits or assessor records identify separate construction
episodes and defensible project-specific land.

The same rules apply citywide. Every ambiguous source record that could enter a
500-, 1,000-, or 1,500-foot boundary sample is reviewed before estimation.
Adjudication files do not contain alderman scores, treatment assignments,
regression outcomes, or influence measures.

## Evidence

Evidence is considered in this order:

1. Issued new-construction permits and final revisions, read together with
   post-construction assessor records.
2. Consistent assessor reports across tax-year vintages.
3. Official construction-year parcel polygons and parcel lineage.
4. Cook County parcel addresses and assessor permit records.
5. Official housing-agency, institutional, or developer records.
6. Contemporaneous public reporting or archived property records.

An authorized permit count does not automatically replace a stable
post-construction assessor count. Conversely, a later assessor rollup does not
override a permit and building-level assessor record that agree on the completed
project. Conflicts are recorded and resolved at the field level. A project is
excluded when the available evidence cannot support a consistent definition.

## Required provenance

Each retained project records the source of:

- project membership and component PINs;
- construction year;
- dwelling units;
- building square feet;
- land square feet; and
- project coordinates.

Fields may come from different rows only when those rows have been shown to
describe the same physical project. A missing building area is not imputed.
Such a project may enter units per acre when units and land are verified, but it
does not enter FAR.

## Residential source

### Ordinary records

- A single-card, single-PIN record uses the latest assessor report available by
  2022. A PIN first reported later uses the latest report available by 2025.
- When one exact-PIN permit chain is available and the assessor year is exactly
  one year before the earliest permit application, the permit application year
  is used.
- Revisions to physical fields across reports do not create new projects.
- Successor condo PINs are evidence about the original building. They are never
  expanded into separate construction observations.

### Tiebacks

- PINs and tieback keys form one connected lineage. Completeness is tested
  within a tax year, never by combining rows from different years.
- A complete snapshot requires PIN shares that sum to one and consistent
  repeated building fields.
- Building area and units are counted once. Land is summed across distinct
  component parcels and checked against historical parcel polygons.
- Incomplete or internally inconsistent lineages are reviewed. They are
  excluded when the physical project cannot be reconstructed.

### Multiple cards

- Cards with one construction year on the same PIN are aggregated to one
  site-level project. Building area and units are summed and land is counted
  once.
- Classes 211 and 212 use reported apartment counts. Other ordinary residential
  classes count one dwelling per card.
- Cards with different construction years remain separate only when permits or
  other evidence identify separate construction episodes. Otherwise the site
  is unresolved.

### Class 2-97

- An explicit multifamily classification is never recoded to one unit from
  residence form.
- Records are linked to permits and successor condo evidence by PIN, historical
  PIN, address, date, and distance.
- Project membership and units must be supported by the combined evidence.
  Building area may remain unavailable, in which case the project enters only
  units-per-acre specifications.

## Commercial source

- Every component PIN is parsed from every 2021 and 2024 entity row.
- Entity rows are connected across vintages by component PINs, addresses, and
  new-construction permits.
- The 2024 row is primary. The 2021 row is used when no 2024 row exists or as a
  field fallback when project membership is stable.
- A later rollup and one of its component buildings cannot both survive.
- Rollups are disaggregated only when permits or assessor rows provide separate
  building fields.
- Bedroom or bed counts are not dwelling-unit counts.
- Missing building area can be recovered from another vintage only when entity
  membership and the remaining physical fields are stable.
- Land must describe the retained project. When all component PINs exist in the
  selected historical parcel layer, the union of those polygons is the legal
  site. A component parcel's land cannot stand in for a multi-PIN entity.
- Construction-year conflicts require permit or completion evidence. A
  reassessment recode is not evidence of construction.

## Permit chains

City record IDs and permit numbers are different fields. Revision chains use
the nine-digit permit number cited in permit descriptions. Application and
issue dates establish chronology because a current description can mention an
earlier or later revision. Every unit mention and its context are retained;
conflicting counts are not resolved by choosing the largest, smallest, earliest,
or latest value mechanically.

## Geography

Historical parcel polygons come from Cook County's annual parcel service:

`https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer`

All spatial work uses Illinois StatePlane East, EPSG:3435.

For a multi-PIN project, available construction-year polygons are dissolved and
the union centroid locates the project. The commercial audit stores the official
polygon and known-absence responses in `adjudication/` so a rebuild does not
repeat external queries. The live service is consulted only for a new
year-PIN10 key. The parcel snapshot and its hash are recorded in
`commercial_historical_parcel_snapshot_manifest.csv`.

The 1218-20 S Washtenaw project is dated to its 2010 permit. Its exact 2009
parcels are used because they are the official site immediately before
construction; the available 2011 PIN15 polygon is unchanged. This exception is
recorded in `commercial_parcel_year_overrides.csv`.

When no historical commercial polygon is available, an exact current component
PIN centroid is a documented fallback. One 14-PIN commercial project lacks a
location for one component but is located from its other 13 components.

Some residential projects inherit the same historical predecessor parcel. For
these projects, an exact successor-PIN centroid or accepted address point
replaces the shared site centroid only when the point lies inside the selected
historical polygon. Projects without an individual point retain the historical
site centroid. For every such project inside 500 feet, the validator confirms
that the site polygon does not intersect the ward boundary.

## Cross-source reconciliation

A component PIN can belong to only one retained project across both sources.
When residential and commercial records describe the same construction, one
project is retained and the other source record names that replacement.

The final cross-source case is 1218-20 S Washtenaw. The commercial project
retains both component PINs. Permit 100349959 and the post-construction
residential assessor row support five completed units; stable commercial rows
supply whole-site building and land area. The residential record is suppressed
as a duplicate.

## Validation

Before estimation:

1. Project IDs are unique.
2. Component PINs are unique across both source families.
3. Every source record has one retained or excluded disposition.
4. Every retained project has positive land, a valid year, and a documented
   location.
5. FAR and units-per-acre eligibility agree with field availability and the
   adjudicated sample rule.
6. Project component lists reproduce the component ledger exactly.
7. Project, component, centroid, and boundary files contain the same projects.
8. All joins assert one-to-one, many-to-one, or one-to-many cardinality.
9. Citywide, 1,500-foot, and 500-foot counts reconcile.
10. Every exclusion and manual replacement has a reason and evidence identifier.
11. No treatment, score, regression outcome, or influence field appears in an
    adjudication input.

The combined validator must pass before any regression is run.
