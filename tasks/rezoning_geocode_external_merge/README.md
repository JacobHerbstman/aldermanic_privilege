# rezoning_geocode_external_merge

## Purpose
Merges Chicago/Census geocoder results back into Stage 1 geocoded rezonings.

## Inputs
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/chicago_geocoder_upload_mapping_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_external_merge_ready/output/chicago_geocoder_results_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_external_merge_ready/output/census_geocoder_results_<DATE_TAG>.csv`
- Raw/audit handoff files in `tasks/rezoning_geocode_external_merge_ready/output/`

## Outputs
- `tasks/rezoning_geocode_external_merge/output/rezoning_geocode_with_external_<DATE_TAG>.csv`

## Run
```bash
cd tasks/rezoning_geocode_external_merge/code
make link-inputs
make
```

## Approx Runtime
- <2 minutes

## Notes
- Source priority is deterministic: existing parcel matches are preserved, Chicago geocoder results fill remaining unmatched rows first, and Census geocoder results fill any still-unmatched rows second.
- Chicago bulk results come from the City of Chicago GIS Geocoder, `https://gisapps.chicago.gov/geocoder/bulkgeo/single`.
- If Chicago geocoder results do not include an ID column, rows are reconciled by upload order using `chicago_geocoder_upload_mapping_<DATE_TAG>.csv`.
- Do not store manual geocoder output in `temp/`; publish canonical external geocoder handoff files through `tasks/rezoning_geocode_external_merge_ready/output/`.
- The canonical event-study period is `20101101_20201231`.
- `census_geocoder_results_19990101_20260212.csv` supplements the 2011-2020 rows that had resolved FAR but missing latitude/longitude after parcel and Chicago geocoding. It was produced from the Census Geocoder one-line address endpoint, `https://geocoding.geo.census.gov/geocoder/locations/onelineaddress`, with `benchmark=Public_AR_Current`.
- Broad or underspecified locations are kept in the Census audit file with `NO_QUERY` or `No_Match` rather than converted to an invented point.
