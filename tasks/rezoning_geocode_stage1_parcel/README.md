# Parcel Geocoding

Matches the Nov. 2010-2020 rezoning addresses to Cook County parcel coordinates.

## Inputs

- `tasks/rezoning_far_pre_geocode/output/zoning_matters_far_20101101_20201231.csv`
- `tasks/cook_parcel_addresses_download/output/parcel_addresses_3723-97qp.csv`
- `tasks/cook_parcel_addresses_download/output/parcel_universe_pabr-t5kh.csv`

## Output

- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_20101101_20201231.csv`

The unmatched file and upload mapping are operational handoffs for the external
geocoder, not analytical QC outputs.
