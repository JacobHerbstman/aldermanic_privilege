# Permit-based new construction

`output/construction_buildings.csv` has one row per new residential building: every building authorized by a
City of Chicago new-construction permit issued 2006–2022 (`route = permit`), and every new building in Cook County
Assessor records that no such permit reaches (`route = assessor_only`). The permit file starts on January 3, 2006,
so buildings completed in 2006–2007 are often on earlier permits. This task does not feed the paper yet;
`tasks/audits/permit_density_comparison` compares it with the paper's current data.

## Rules

`select_permits.R` (every `PERMIT - NEW CONSTRUCTION` issued 2006–2022, one `scope` per permit)
- Braced notes about later permits (`{ALSO SEE PERMIT ...}`) are ignored.
- Dropped: revisions and reinstatements, tent/event structures, additions and conversions, garages, and work
  at, for or serving an existing building. Foundation and superstructure phases stay; they are resolved by the
  Assessor claims below. Residential wording includes `S.F.R.`, mixed use and ground-floor commercial.
- Dwelling count: the first count stated in the description (`35 DWELLING UNITS`, `3 D.U.`, `6 UNIT`,
  `2-FLAT`, `(7) 3-STORY ROWHOMES`); otherwise 2 for a duplex and 1 for a single house.
- Repeated permits at the same address with the same count within `REPEAT_PERMIT_YEARS` are one building.

`measure_buildings.R`
- Parcels: the permit's PIN list; the 2025 parcels at the permit's house number and street are used only
  when the listed PINs show no new building.
- A new building is an Assessor record with year built from the issue year minus `ASSESSOR_YEAR_LEAD` to the
  issue year plus `MAX_BUILD_LAG_YEARS` (99.5% of measured buildings), first appearing after the permit. A parcel already showing it before the permit
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

`build_construction_buildings.R`
- Assessor-only buildings: a condominium building, residential card or commercial apartment valuation first
  reported built from `FIRST_YEAR_BUILT` on, persisting the following year, not measured by a permit and on no
  parcel of a new residential permit issued in the preceding years. Measured as in the permit arm.
- Those built through `LAST_UNPERMITTED_YEAR_BUILT` (2007) are accepted (`measured_without_permit`). Among
  permitted buildings, the Assessor year built is at least one year after the permit for about a third and at
  least two years after it for an eighth, so many 2006 and some 2007 buildings had pre-2006 permits; the paper's
  no-permit share falls to its later level by 2008. Later ones are `no_permit_found` and wait for research;
  many sit beside a permitted development (other lots of a townhome row, for example).
- A residential year built that changes while the parcel keeps last year's floor area is a revised record.
- Lot rule: a permit that reaches no new building on its own parcels takes the unclaimed new building within
  `LOT_DISTANCE_FT` of its geocoded point that first appears after it, within the construction lag, with a
  matching dwelling count, when each is the other's only such match (`match_basis = nearby_lot`). Most are corner
  buildings addressed on the cross street, one building on several lots, or parcel addresses without a direction.
  Not candidates: lots on the opposite side of the permit's street, lots at an address with its own
  new-construction permit, and permits whose own parcel still holds a building of changed floor area
  (`parcel_changed_after_permit`, possibly the new building under an old year built). Other qualifying pairs are
  listed in `lot_rule_candidates` for review. Hand checks are in `tasks/audits/construction_hand_checks`.
- Location: centroid of the measured parcels in the 2025 parcel universe (permit coordinates are geocoded
  at the street frontage, a median 56 ft from the parcel centroid); the permit point only when no parcel remains.

`build_ledger.R` (`permit_issue`, `assessor_year`)
- Ward and nearest ward-pair boundary from the map in effect on the permit issue date, or on June 15 of the
  Assessor's year built (always the latter for Assessor-only buildings).
