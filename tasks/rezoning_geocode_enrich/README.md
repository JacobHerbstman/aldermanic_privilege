# rezoning_geocode_enrich

## Purpose
Adds ward assignment, boundary distance, and date-aware FAR parsing to geocoded rezonings.

## Inputs
- `tasks/rezoning_geocode_external_merge/output/rezoning_geocode_with_external_<DATE_TAG>.csv`
- `tasks/ward_panel_create/output/ward_panel.gpkg`
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`

## Outputs
- `tasks/rezoning_geocode_enrich/output/rezoning_geocoded_enriched_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_enrich/output/ward_rezoning_counts_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_enrich/output/sponsor_ward_validation_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_enrich/output/map_geocode_source_<DATE_TAG>.pdf`
- `tasks/rezoning_geocode_enrich/output/map_upzone_downzone_<DATE_TAG>.pdf`

## Run
```bash
cd tasks/rezoning_geocode_enrich/code
make link-inputs
make
```

## Approx Runtime
- 1-5 minutes

## Notes
- FAR lookup is selected by `zone_code + matter_intro_date`, with the cutoff at `2004-11-01`.
