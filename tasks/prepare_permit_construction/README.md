# Permit-based new construction

`output/construction_buildings.csv` has one row per new residential building: every building authorized by a
City of Chicago new-construction permit issued 2006–2022 (`route = permit`), and every new building in Cook County
Assessor records on a Chicago parcel that no such permit reaches (`route = assessor_only`). The permit file starts on
January 3, 2006, so buildings completed in 2006–2007 are often on earlier permits. This task does not feed the paper
yet; `tasks/audits/permit_density_comparison` compares it with the paper's current data.

## Rules

Settings named below are set at the top of the script that applies them; those used by both
`measure_buildings.R` and `build_construction_buildings.R` are at the top of `construction_rules.R`.

`select_permits.R` (every `PERMIT - NEW CONSTRUCTION` issued 2006–2022, one `scope` per permit)
- Renovation, easy and express permits are included when their first sentence (after a label or a clause removing the
  existing building) erects a dwelling named directly as its object, of stated height or described as new (`ERECT NEW
  2 1/2 STORY SINGLE FAMILY FRAME RESIDENCE WITH REAR OPEN DECK AND DETACHED FRAME GARAGE`, `REMOVE EXISTING BUILDING
  AND ERECT NEW SINGLE FAMILY RESIDENCE`), with no accessory structure, trade work or place named before the dwelling
  (`ERECT NEW PARTITIONS IN BASEMENT OF SFR`), and describes no work on an existing building or trade work for a new
  one (`NEW TWO STORY SINGLE FAMILY HOUSE WITH 200A SERVICE`).
- House numbers the description lists on the permit's street (`329, 335, 337, 339 EAST 25TH PLACE`, `1626-46 SOUTH
  PRAIRIE`) are kept (`house_numbers`); prototype references name other buildings.
- Notes about later permits (`{ALSO SEE PERMIT ...}`, `[SEE PERMIT #... TO CONVERT ...]`, `SEE REVISION #...`) and
  review-program notes (`***SELF CERT PROJECT***`) are ignored. Common misspellings and abbreviations (`DWELING`,
  `APARMENT`, `TWELEVE`, `6 UNITBUILDING`, `4(DU)`, `EXIST.`, `SRF`, `STRY`) are corrected.
- Dropped: revisions and reinstatements, tent/event structures, additions (also misspelled: `NEW FRAME ONE STORY
  ADDTION`), conversions and rehabs (unless the first clause erects dwellings: `ERECT EIGHT TOWNHOUSE, AN ADDITION TO A
  FOUR EXISTING ... TOWNHOUSES`), accessory structures (a garage, carport, deck, porch, stair, fence, pergola or shed
  named in the first clause, before any dwelling or building other than a building code: `ERECT A 33X24 FRAME GARAGE
  PER PLANS, TO AN EXISTING ...`, `NEW DETACHED GARAGE-PER 2019 CHICAGO BUILDING CODE`), and work at, for or serving an
  existing building. Foundation and superstructure phases stay; they are resolved by the Assessor claims below.
- `new_residential`: a stated dwelling count or dwelling wording (including `S.F.R.`, mixed use and ground-floor
  commercial). `building_use_not_stated`: a new building, structure or story count with no dwelling wording and no
  non-residential use (`ERECT A NEW 3 STORY MASONRY BUILDING AS PER PLANS`); the Assessor decides these.
- Dwelling count: the first count stated in the description (`35 DWELLING UNITS`, `3 D.U.`, `6 UNIT`,
  `2-FLAT`, `(7) 3-STORY ROWHOMES`); otherwise 2 for a duplex and 1 for a single house.
- Repeated permits at the same address with the same count within `repeat_permit_years` are one building.

`measure_buildings.R` (one row per building: each new Assessor record belongs to one permit)
- Parcels: the permit's PIN list with the parcels that later succeeded them; the 2025 parcels at the permit's address
  are used only when these show no new building. Hand-named lots and homes are added to either. A parcel address without a
  direction matches by house number and street name within `address_match_ft` of the permit's geocoded point. A permit
  for several dwellings also covers its listed house numbers, at most one per dwelling.
- Parcel succession: a condominium declaration or subdivision retires a parcel number and creates new ones. Each new
  parcel descends from the nearest older parcel on its tax block last assessed in the year before, or the year of, its
  first assessment (from `tasks/download_parcel_centroids`, every assessment year 1999–2025). A successor parcel listed
  on another permit, or at another permit's house number and street issued within the construction lag, belongs to
  that permit; when some successors face the permit's street, those on other streets are other buildings of the site.
  A permit reaching records on current parcels measures those, not records on parcels later retired
  (`match_basis = parcel_successor`).
- A new building is an Assessor record with year built from the issue year minus `assessor_year_lead` to the
  issue year plus `max_build_lag_years` (99.5% of measured buildings), first appearing after the permit.
  A parcel already showing it before the permit year holds an earlier building. The Assessor codes many 2013 and 2016
  permits' buildings two years before issue, which sets the lead at 2.
- Some new buildings keep the old reported year built. A parcel without a new record whose floor area first rises by
  `rebuilt_area_growth` within the construction lag, and keeps it, is measured in that year
  (`match_basis = floor_area_change`). Floor area also rises with additions, corrected records and work on another
  parcel, so this applies only to permits for new residential buildings, on parcels within `rebuilt_distance_ft` of
  the permit's geocoded point (reviewers found all 4 farther matches wrong).
- A building is dated by its first assessment and measured on the record it holds most often in its first
  `measurement_years` assessment years (each year votes once, however many residential cards it has; the earliest
  when tied; years without a complete measurement do not count, and a condominium building's measurement is its unit
  count, floor area and land, or its unit count and land when the Assessor records no floor area):
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
  `unit_tolerance` for multifamily), floor area per unit below `min_sqft_per_unit` (a shop-only record),
  land per unit above `max_land_sqft_per_unit` (development-wide land) or land below `min_land_sqft` (a recording
  error such as 1 sq ft), or an older building on the parcel. A permit's building left without a measurement when a
  neighboring permit takes its record by address and then keeps other evidence is a known case (one 6-unit condominium,
  1419100038).
- `adjudication/manual_decisions.csv` is the only place for hand research: one row per `subject` (a permit number, or
  an Assessor-only `building_id`) and field, with a source and note. Named lots and homes (`assign_lot`, `add_homes`,
  `replace_homes`) join their permit's parcels here (`match_basis = hand_checked`), as do the measurement fields
  (`exclude`, `accept`, `dwelling_units`, `building_sqft`, `land_sqft`); `same_building` (another phase of a named
  permit's building; the building takes the earlier issue date) and `no_match` (no candidate is this permit's
  building) apply in `build_construction_buildings.R`. The rows come from
  `tasks/audits/construction_hand_checks/queue_first_pass.csv`, the condominium site reviews, and high-confidence
  verdicts of `validation_reviews.csv` that still applied to the data (not new construction, duplicates, wrong
  buildings; verdicts on superseded rows or measurements were not carried over); two decisions that assigned a whole
  divided site to one permit (Medill/Belden 100553156, Campbell/Homer 100645761) were retired when rows became buildings.

`build_construction_buildings.R`
- Assessor-only buildings: a condominium building, residential card or commercial apartment valuation on a Chicago
  parcel, first reported built from `first_year_built` on, persisting the following year, not measured by a permit and
  on no parcel of a new residential permit issued in the preceding years. Measured as in the permit arm.
- Those built through `last_unpermitted_year_built` (2007) are accepted (`measured_without_permit`). Among
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
- New-parcel rule: a permit that reaches no new building takes the Assessor-only building on a parcel it lists that was
  created after the permit was issued and first assessed within the construction lag, with a matching dwelling count,
  when each is the other's only such match (`match_basis = new_parcel`). The parcel did not exist before the permit, so
  the building is the permit's even when the Assessor reports it built years earlier (Lake Park Crescent: twelve 2012
  permits, parcels from 2014, `assessor_year_built` 2006, kept as reported).
- Lot rule: a permit that reaches no new building on its own parcels takes the unclaimed new building within
  `lot_distance_ft` of its geocoded point (`large_lot_distance_ft` for `large_building_units` or more dwellings, whose
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
  permit without a measured home takes the one such run within `townhouse_distance_ft`
  (`match_basis` includes `townhouse_lots`). Parcels of other permits in the construction window and homes two
  permits would take are excluded; other qualifying homes are listed in `townhouse_candidates` for review.
  Buildings reported built after `last_year_built` are kept only as possible matches for late permits.
- One row per home: a permit row measured on several single-family Assessor records (a permit for several
  townhouses or houses, a group of single-house permits, and homes the townhouse rule added) becomes one row per
  record, measured on that record and located at its parcel centroid (the row's location when the parcel has none
  yet), with `building_id` the permit row's id and the record id joined by `_`. Each home keeps the row's permits,
  dates and matching flags; per-dwelling plausibility is checked on the home. Assessor-only townhouses are already
  one row per home.
- Two flags withhold density for buildings left Assessor-only (reviewers found 3 of 15 and 1 of 12 such buildings
  right): a permit of any type at the address or parcels, issued by the reported year built, for work on an existing
  building, with no new-construction or wrecking permit through the lead after it (`existing_building_permit`, mostly
  condominium conversions and rehabs given a new year built), and a residential or condominium record first assessed
  more than `max_assessment_lag_years` after its reported year built (`assessed_long_after_year_built`: an older
  building, or one built years later). Permit-linked buildings are dated by their permits and are not checked.
- Floor area from 2022 footprints: the Assessor records no floor area for condominium buildings of 20 or more units.
  For those of `footprint_min_units` to `footprint_max_units` units reported built by `footprint_last_year_built`,
  floor area is the volume of the footprints containing the building's parcels
  (`tasks/download_building_footprints_2022`; structures below `footprint_min_height_ft` ignored, footprints reached
  by two buildings not used) times the median floor area per cubic foot of rentals of the same size with Assessor
  floor area. Only buildings `footprint_min_building_height_ft` to `footprint_max_building_height_ft` tall whose
  footprints cover `footprint_min_lot_coverage` to `footprint_max_lot_coverage` of their land are filled; the error
  checks are in `tasks/audits/footprint_floor_area_check`. `floor_area_source` is `assessor` or `footprint`; drop
  `footprint` rows for an Assessor-only FAR sample.
- Condominium buildings the Assessor re-declared under several records as units sold, researched one by one
  (`tasks/audits/construction_hand_checks/condominium_site_reviews.csv`): `measure_record` measures a row on the record
  describing the whole building, keeping its date; `drop_record` removes a row duplicating another building or a
  placeholder record; `keep_record` keeps a separate building the duplicate rule would remove.
- Shared land: the Assessor often records a development's whole site on each of its towers (Wolf Point West and East
  both report 178,133 sq ft). For buildings of `large_building_units` or more, land is development-wide
  (`land_shared_development`, density withheld) when another such building within `shared_land_distance_ft` reports
  the same land, or when a building at least `shared_land_min_height_ft` tall covers less than
  `shared_land_max_lot_coverage` of its land with its 2022 footprints (Lakeshore East). Buildings finished after the
  2022 imagery (Cirrus, 211 N Harbor) cannot be checked by footprint.
- Every hand-named lot and home must be measured in the row holding its permit, and no Assessor record may measure
  two rows first assessed in the same year; the build stops otherwise.

`build_ledger.R`
- Ward and nearest ward-pair boundary from the map in effect on the first permit's issue date, or on June 15 of
  the Assessor's year built for Assessor-only buildings (`date_source`).
