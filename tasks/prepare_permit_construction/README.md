# Permit-based new construction

`output/construction_buildings.csv` has one row per new residential building: every building authorized by a
City of Chicago new-construction permit issued 2006–2022 (`route = permit`), and every new building in Cook County
Assessor records on a Chicago parcel that no such permit reaches (`route = assessor_only`). The permit file starts on
January 3, 2006, so buildings completed in 2006–2007 are often on earlier permits. This task does not feed the paper
yet; `tasks/audits/permit_density_comparison` compares it with the paper's current data.

## Rules

`select_permits.R` (every `PERMIT - NEW CONSTRUCTION` issued 2006–2022, one `scope` per permit)
- Renovation, easy and express permits are included when their first sentence erects or constructs a new building of
  stated height with dwellings (`ERECT NEW 2 STORY 6 DU 3B BUILDING`) and describes no work on or next to an existing
  building (additions, dormers, porches, build-outs, trade work).
- House numbers the description lists on the permit's street (`329, 335, 337, 339 EAST 25TH PLACE`, `1626-46 SOUTH
  PRAIRIE`) are kept (`house_numbers`); prototype references name other buildings.
- Notes about later permits (`{ALSO SEE PERMIT ...}`, `[SEE PERMIT #... TO CONVERT ...]`, `SEE REVISION #...`) and
  review-program notes (`***SELF CERT PROJECT***`) are ignored. Common misspellings and abbreviations (`DWELING`,
  `APARMENT`, `6 UNITBUILDING`, `4(DU)`, `EXIST.`, `SRF`, `STRY`) are corrected.
- Dropped: revisions and reinstatements, tent/event structures, additions, conversions and rehabs, accessory
  structures (a garage, carport, deck, porch, stair, fence, pergola or shed named in the first clause, before any
  dwelling or building: `ERECT A 33X24 FRAME GARAGE PER PLANS, TO AN EXISTING ...`), and work at, for or serving an
  existing building. Foundation and superstructure phases stay; they are resolved by the Assessor claims below.
- `new_residential`: a stated dwelling count or dwelling wording (including `S.F.R.`, mixed use and ground-floor
  commercial). `building_use_not_stated`: a new building, structure or story count with no dwelling wording and no
  non-residential use (`ERECT A NEW 3 STORY MASONRY BUILDING AS PER PLANS`); the Assessor decides these.
- Dwelling count: the first count stated in the description (`35 DWELLING UNITS`, `3 D.U.`, `6 UNIT`,
  `2-FLAT`, `(7) 3-STORY ROWHOMES`); otherwise 2 for a duplex and 1 for a single house.
- Repeated permits at the same address with the same count within `REPEAT_PERMIT_YEARS` are one building.

`measure_buildings.R` (one row per building: each new Assessor record belongs to one permit)
- Parcels: the permit's PIN list with the parcels that later succeeded them; the 2025 parcels at the permit's address
  are used only when these show no new building. Hand-named lots and homes are added to either. A parcel address without a
  direction matches by house number and street name within `ADDRESS_MATCH_FT` of the permit's geocoded point. A permit
  for several dwellings also covers its listed house numbers, at most one per dwelling.
- Parcel succession: a condominium declaration or subdivision retires a parcel number and creates new ones. Each new
  parcel descends from the nearest older parcel on its tax block last assessed in the year before, or the year of, its
  first assessment (from `tasks/download_parcel_centroids`, every assessment year 1999–2025). A successor parcel listed
  on another permit, or at another permit's house number and street issued within the construction lag, belongs to
  that permit; when some successors face the permit's street, those on other streets are other buildings of the site.
  A permit reaching records on current parcels measures those, not records on parcels later retired
  (`match_basis = parcel_successor`).
- A new building is an Assessor record with year built from the issue year minus `ASSESSOR_YEAR_LEAD` to the
  issue year plus `MAX_BUILD_LAG_YEARS` (99.5% of measured buildings), first appearing after the permit.
  A parcel already showing it before the permit year holds an earlier building. The Assessor codes many 2013 and 2016
  permits' buildings two years before issue, which sets the lead at 2.
- Some new buildings keep the old reported year built. A parcel without a new record whose floor area first rises by
  `REBUILT_AREA_GROWTH` within the construction lag, and keeps it, is measured in that year
  (`match_basis = floor_area_change`).
- A building is dated by its first assessment and measured on the record it holds most often in its first
  `MEASUREMENT_YEARS` assessment years (the earliest when tied; years without a complete measurement do not count):
  first-year records are often partial (a building still under construction, duplicate cards, a record before a
  condominium declaration). A building on prorated parcels keeps its proration as first assessed.
- Units, floor area and land come from one source and one measurement year, never mixed:
  condominium records first, then residential cards, then commercial apartment valuations (2021 onward; hotels,
  care facilities and parking valuations are not dwellings). Prorated buildings count once; land is summed once
  per parcel.
- One Assessor record reported built within the construction lag for several permits is one building: a later permit
  on the parcel, or a revised year built, does not make it new again.
- A record reached by permits at several addresses belongs to the permit on the record's street whose house number is
  the nearest at or below the record's on the same side, or else the nearest on that street, or, for a record on none
  of their streets, the permit geocoded nearest to it (street names one letter apart match). A hand-named record
  belongs to its named permit. Several permits at one address reaching the same building: the latest permit issued
  before the building appears keeps it (`superseded_permit_numbers`). A permit whose records all went to buildings at
  other addresses (a foundation or phase permit) is listed in `member_permit_numbers` of the building holding most of
  them.
- A `building_use_not_stated` permit measures only a building that no residential permit reaches; one reaching no
  building leaves the data.
- A permit reaching no building, followed by a measured permit on one of its parcels or at its address, was not built
  under this permit (`later_permit_built`).
- Flags withhold density: Assessor units differ from the permit (exactly for single houses, beyond
  `UNIT_TOLERANCE` for multifamily), floor area per unit below `MIN_SQFT_PER_UNIT` (a shop-only record),
  land per unit above `MAX_LAND_SQFT_PER_UNIT` (development-wide land), or an older building on the parcel.
- `adjudication/manual_decisions.csv` is the only place for hand research: one row per permit number and
  field, with a source and note. Named lots and homes (`assign_lot`, `add_homes`, `replace_homes`) join their permit's
  parcels here (`match_basis = hand_checked`), as do the measurement fields (`exclude`, `accept`, `dwelling_units`,
  `building_sqft`, `land_sqft`); `same_building` (another phase of a named permit's building; the building takes the
  earlier issue date) and `no_match` (no candidate is this permit's building) apply in `build_construction_buildings.R`.
  The rows come from `tasks/audits/construction_hand_checks/queue_first_pass.csv`; two decisions that assigned a whole
  divided site to one permit (Medill/Belden 100553156, Campbell/Homer 100645761) were retired when rows became
  buildings.

`build_construction_buildings.R`
- Assessor-only buildings: a condominium building, residential card or commercial apartment valuation on a Chicago
  parcel, first reported built from `FIRST_YEAR_BUILT` on, persisting the following year, not measured by a permit and
  on no parcel of a new residential permit issued in the preceding years. Measured as in the permit arm.
- Those built through `LAST_UNPERMITTED_YEAR_BUILT` (2007) are accepted (`measured_without_permit`). Among
  permitted buildings, the Assessor year built is at least one year after the permit for about a third and at
  least two years after it for an eighth, so many 2006 and some 2007 buildings had pre-2006 permits; the paper's
  no-permit share falls to its later level by 2008. Later ones are `no_permit_found` and wait for research.
- A residential year built that changes while the parcel keeps last year's floor area is a revised record.
- Location: centroid of the measured parcels, from every assessment year 1999–2025
  (`tasks/download_parcel_centroids`), so parcels retired by later condominium declarations or subdivisions keep a
  location. Permit coordinates are geocoded at the street frontage, a median 56 ft from the parcel centroid; the
  permit point is used only when no parcel is found.
- An Assessor-only record on a parcel whose successor parcels hold a new building reported built within its lead and
  lag is that building counted twice (before and after a condominium declaration or subdivision), and is removed.
- Lot rule: a permit that reaches no new building on its own parcels takes the unclaimed new building within
  `LOT_DISTANCE_FT` of its geocoded point (`LARGE_LOT_DISTANCE_FT` for `LARGE_BUILDING_UNITS` or more dwellings, whose
  lots reach farther from the street frontage) that first appears after it, within the construction lag, with a
  matching dwelling count, when each is the other's only such match (`match_basis = nearby_lot`). Most are corner
  buildings addressed on the cross street, one building on several lots, or parcel addresses without a direction.
  Not candidates: lots on the opposite side of the permit's street, lots at an address with its own
  new-construction permit, and permits whose own parcel still holds a building of changed floor area
  (`parcel_changed_after_permit`, possibly the new building under an old year built). Other qualifying pairs are
  listed in `lot_rule_candidates` for review. Hand checks are in `tasks/audits/construction_hand_checks`.
- Townhouse rule: a permit for several townhouses or houses, or a group of single-house permits, reaching fewer
  homes than it authorizes takes the unclaimed new single-family parcels that complete one run of consecutive
  parcel numbers on a block, first assessed within a year of its measured homes, exactly matching its count; a
  permit without a measured home takes the one such run within `TOWNHOUSE_DISTANCE_FT`
  (`match_basis` includes `townhouse_lots`). Parcels of other permits in the construction window and homes two
  permits would take are excluded; other qualifying homes are listed in `townhouse_candidates` for review.
  Buildings reported built after `LAST_YEAR_BUILT` are kept only as possible matches for late permits.
- Every hand-named lot and home must be measured in the row holding its permit, and no Assessor record may measure
  two rows first assessed in the same year; the build stops otherwise.

`build_ledger.R`
- Ward and nearest ward-pair boundary from the map in effect on the first permit's issue date, or on June 15 of
  the Assessor's year built for Assessor-only buildings (`date_source`).
