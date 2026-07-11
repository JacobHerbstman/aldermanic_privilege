# Merge Rezoning Geocodes

Preserves live Cook County parcel matches and fills remaining rows from the
committed external-geocoder table.

## Inputs

- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_20101101_20201231.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_20101101_20201231.csv`
- `tasks/rezoning_geocode_external_merge_ready/output/rezoning_external_geocodes_20101101_20201231.csv`

## Output

- `tasks/rezoning_geocode_external_merge/output/rezoning_geocode_with_external_20101101_20201231.csv`

Source priority is deterministic: parcel coordinates are preserved, Chicago
GIS Geocoder coordinates fill remaining rows first, and Census Geocoder
coordinates fill the remaining reviewed addresses.
