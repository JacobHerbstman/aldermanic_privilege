# rezoning_geocode_enrich_diagnostics

## Purpose
Builds review summaries and maps for the enriched rezoning geocode table. These outputs are not downstream production inputs.

## Input
- `tasks/rezoning_geocode_enrich/output/rezoning_geocoded_enriched_<DATE_TAG>.csv`

## Outputs
- `tasks/audits/rezoning_geocode_enrich_diagnostics/output/ward_rezoning_counts_<DATE_TAG>.csv`
- `tasks/audits/rezoning_geocode_enrich_diagnostics/output/sponsor_ward_validation_<DATE_TAG>.csv`
- `tasks/audits/rezoning_geocode_enrich_diagnostics/output/map_geocode_source_<DATE_TAG>.pdf`
- `tasks/audits/rezoning_geocode_enrich_diagnostics/output/map_upzone_downzone_<DATE_TAG>.pdf`

## Run
```bash
cd tasks/audits/rezoning_geocode_enrich_diagnostics/code
make link-inputs
make
```
