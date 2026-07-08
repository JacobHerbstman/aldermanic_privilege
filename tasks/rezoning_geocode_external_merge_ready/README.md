# Rezoning External Geocoder Merge-Ready Handoff

This task publishes committed, merge-ready external geocoder results for the
canonical rezoning dataset.

The current handoff files are:

- `output/chicago_geocoder_results_19990101_20260212.csv`
- `output/chicago_geocoder_results_19990101_20260212.xlsx`
- `output/chicago_geocoder_results_19990101_20260212_import_meta.json`
- `output/census_geocoder_results_19990101_20260212.csv`
- `output/census_geocoder_queries_19990101_20260212.csv`

The Chicago files come from the City of Chicago GIS Geocoder bulk single-column
address workflow. The Census files supplement 2011-2020 rows with resolved FAR
but missing latitude/longitude after parcel and Chicago geocoding, using the
Census Geocoder one-line address endpoint with `benchmark=Public_AR_Current`.

The Census result file has 44 reviewed rows: 38 matched geocodes and 6
documented unresolved rows. Broad or underspecified non-point locations remain
unmatched rather than being assigned invented coordinates.

Downstream tasks should read this committed handoff instead of local geocoder
downloads in `tasks/rezoning_geocode_external_merge/input/`.
