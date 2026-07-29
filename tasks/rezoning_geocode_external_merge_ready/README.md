# Frozen Rezoning Geocodes

This task publishes the committed external-geocoder coordinates used after the
live Cook County parcel-address match.

## Output

- `output/rezoning_external_geocodes_20101101_20201231.csv`

The file is unique by `external_row_id` and contains 233 Chicago GIS Geocoder
matches, 38 earlier Census Geocoder matches, 118 additional reviewed Census
address matches, 9 reviewed intersection matches, 2 title-direction corrections,
and 2 parcel-supported manual matches. Raw uploads, downloads, row mappings, and
reconciliation material are retained in
`tasks/audits/rezoning_external_geocoder_handoff/`.

The Makefile intentionally only verifies that the committed hand-adjudicated
output is present.
