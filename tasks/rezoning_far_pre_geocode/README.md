# rezoning_far_pre_geocode

## Purpose
Assigns date-aware FAR values to the `1999-2026` harmonized rezoning table before geocoding.

## Inputs
- `tasks/rezoning_dataset/output/rezoning_dataset_harmonized_19990101_20260212.csv`
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`

## Outputs
- `tasks/rezoning_far_pre_geocode/output/zoning_matters_far_19990101_20260212.csv`
- `tasks/rezoning_far_pre_geocode/output/rezoning_far_pre_geocode_summary_19990101_20260212.json`

## Diagnostic Output
- `tasks/rezoning_far_pre_geocode/output/far_unresolved_rows_19990101_20260212.csv`

## Run
```bash
cd tasks/rezoning_far_pre_geocode/code
make link-inputs
make
```

## Approx Runtime
- <2 minutes

## Notes
- Geocode gate metric is `parseable_non_structural_missing_count == 0`.
- Parseable non-structural means at least one side has a parsed non-`PD`/`POS` code; these rows must have both `from_far` and `to_far` non-null.
- `both_sides_far_share_all_rows` and `both_sides_far_share_non_structural_rows` are retained as diagnostics.
- Structural FAR NA classes (`PD`, `POS`) are preserved and reported separately in diagnostics.
- The unresolved-row table supports the production gate summary; `make diagnostics` exposes it as the review surface.
