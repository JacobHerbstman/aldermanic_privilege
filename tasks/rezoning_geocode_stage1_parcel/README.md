# Parcel Geocoding

Matches the Nov. 2010-2020 rezoning addresses to Cook County parcel coordinates.

## Inputs

- `tasks/rezoning_far_pre_geocode/output/zoning_matters_far_20101101_20201231.csv`
- `tasks/cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv`
- `tasks/download_parcel_universe_data/output/parcel_universe_2025_city.csv`

## Output

- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_20101101_20201231.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_parcel_matches_20101101_20201231.csv`

The unmatched file is retained because the frozen external-geocoder handoff uses
it to identify rows eligible for a fallback coordinate.

The parcel-match file contains every exact-address parcel and every same-side
parcel whose house number lies inside an ordinance address range. Nearest-address
fallbacks are not treated as affected parcels.
