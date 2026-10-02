# New buildings from the Assessor's records, 2000–2025

Chicago's building permits begin in 2006, so before then the only record of a new
building is the Cook County Assessor's. This task finds new buildings in the
Assessor's records for every year 2000–2025 with one set of rules, and
`output/new_buildings.csv` lists each building once: its year, type, parcels and
units. The rules were set and checked against the permits of 2006–2022
(`tasks/audits/assessor_new_building_checks`) and are applied unchanged to
2000–2005.

Steps (`code/`, in Make order):

- `build_parcel_lineage.R` → `parcel_lineage.csv`: for every 10-digit parcel first
  assessed in 2000 or later, the parcels its polygon overlaps in the Clerk's parcel
  maps (`tasks/download_parcel_history`), with the share of the new parcel each
  covers (`new_share`) and the share of each under it (`old_share`).
- `detect_house_events.R` → `house_events.csv`: houses and 2–6 flats from the
  residential record cards (`tasks/prepare_construction_assessor_history`): a card
  first assessed in 2000 or later whose year built is recent (at most five years
  before), or a card whose year built jumps by ten years or more to a recent year.
- `detect_condominium_buildings.R` → `condominium_buildings.csv`: every condominium
  building (10-digit parcel) with the first year its units appear and their median
  year built; a building is new if its units were built at most five years before
  they appear, and a conversion if earlier.
- `detect_apartment_events.R` → `apartment_events.csv`: apartment-class buildings
  (classes 3xx and 9xx except 399, 390 and 990) from the assessed values
  (`tasks/download_assessed_values`), by three routes: a PIN becoming improved the
  year after a vacant or exempt year; a new parcel on land that was cleared, or
  carried little building value, in the three years before; and a PIN entering an
  apartment class when the Assessor's 2021+ commercial valuations date its building
  within two years. The valuations also set aside existing buildings (dated more
  than five years before the event; fifteen on exempt land), date buildings built
  while their land was exempt, and give units. Every event is kept with the reason it
  is counted or not (`apartment_building`).
- `combine_new_buildings.R` → `new_buildings.csv`: the three rules' events, each
  building once. A building first assessed on a parcel retired within three years is
  counted on the parcels that replace it, with the earlier date; events on one parcel
  (or one house record card) within three years are one building; apartment-class
  parcels improved in the same year that touch, or share a valuation record, form one
  development (`development_id`), usually one building on assembled lots.

Apartment-class buildings should be counted by `development_id`; the other types
have one building per development. The 2021+ valuations describe only buildings
standing in 2021, so the routes that rely on them find fewer early buildings. Run
`make` in `code/`.
