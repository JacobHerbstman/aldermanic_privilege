# zoning_data_cleaning

## Purpose
Builds the cleaned zoning FAR lookup used for rezoning FAR parsing, including a date-aware split between pre-2004 and post-2004 zoning code eras.

## Inputs
- `data_raw/Boundaries_-_Zoning_Districts_20250910.geojson`
- `data_raw/zoning-code-summary-district-types.csv`
- `tasks/old_zoning_data/output/old_zoning_bulk_density_1957_2004.csv`

## Outputs
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`
- `tasks/zoning_data_cleaning/output/zoning_data_clean.gpkg`

## Run
```bash
cd tasks/zoning_data_cleaning/code
make link-inputs
make
```

## Approx Runtime
- 1-3 minutes

## Notes
- FAR lookup output includes `effective_start_date`, `effective_end_date`, and `zoning_code_version`.
- The GeoPackage output preserves the existing zoning-boundary input used by distance/balance tasks.
