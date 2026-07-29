# Rezoning External Geocoder Handoff Audit

This audit folder retains the raw Chicago GIS Geocoder and Census Geocoder
handoffs used to construct the committed, ID-keyed external geocode table in
`tasks/rezoning_geocode_external_merge_ready/output/`.

The production pipeline does not read these raw sidecar files. They are kept
only to document the frozen external matches and their row reconciliation.
