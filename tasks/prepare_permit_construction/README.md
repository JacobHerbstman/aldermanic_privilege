# Permit-based new construction

One row per new residential building authorized by a City of Chicago building permit issued 2006–2022,
measured with the first Cook County Assessor record that shows the new building. This task does not
feed the paper yet; `tasks/audits/permit_density_comparison` compares it with the paper's current data.

## Rules

`select_permits.R` (every `PERMIT - NEW CONSTRUCTION` issued 2006–2022, one `scope` per permit)
- Braced notes about later permits (`{ALSO SEE PERMIT ...}`) are ignored.
- Dropped: revisions and reinstatements, tent/event structures, additions and conversions, garages.
  Foundation and superstructure phases stay; they are resolved by the Assessor claims below.
- Dwelling count: the first count stated in the description (`35 DWELLING UNITS`, `3 D.U.`, `6 UNIT`,
  `2-FLAT`, `(7) 3-STORY ROWHOMES`); otherwise 2 for a duplex and 1 for a single house.
- Repeated permits at the same address with the same count within `REPEAT_PERMIT_YEARS` are one building.

`measure_buildings.R`
- Parcels: the permit's PIN list; the 2025 parcels at the permit's house number and street are used only
  when the listed PINs show no new building.
- A new building is an Assessor record with year built no earlier than the issue year minus
  `ASSESSOR_YEAR_LEAD`, first appearing after the permit. A parcel already showing it before the permit
  year holds an earlier building. The Assessor codes many 2013 and 2016 permits' buildings two years
  before issue, which sets the lead at 2.
- Units, floor area and land come from one source and one first-appearance year, never mixed:
  condominium records first, then residential cards, then commercial apartment valuations (2021 onward).
  Prorated buildings count once; land is summed once per parcel.
- Several permits at one address reaching the same building: the latest permit issued before the building
  appears keeps it. Permits at different addresses reaching the same building are one development.
- Flags withhold density: Assessor units differ from the permit (exactly for single houses, beyond
  `UNIT_TOLERANCE` for multifamily), floor area per unit below `MIN_SQFT_PER_UNIT` (a shop-only record),
  land per unit above `MAX_LAND_SQFT_PER_UNIT` (development-wide land), or an older building on the parcel.
- `adjudication/manual_decisions.csv` is the only place for hand research: one row per permit number and
  field (`exclude`, `accept`, `dwelling_units`, `building_sqft`, `land_sqft`) with a source and note.

`build_ledger.R` (`permit_issue`, `assessor_year`)
- Location: centroid of the measured parcels in the 2025 parcel universe (permit coordinates are geocoded
  at the street frontage, a median 56 ft from the parcel centroid); the permit point only when no parcel remains.
- Ward and nearest ward-pair boundary from the map in effect on the permit issue date, or on June 15 of the
  Assessor's year built.
