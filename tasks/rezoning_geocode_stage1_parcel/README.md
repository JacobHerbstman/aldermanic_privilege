# rezoning_geocode_stage1_parcel

## Purpose
Matches rezoning addresses to Cook parcel coordinates and produces Stage 1 geocodes plus Chicago geocoder upload artifacts.

## Inputs
- `tasks/<REZONING_INPUT_TASK>/output/<REZONING_INPUT_BASENAME>_<DATE_TAG>.csv`
  - default: `REZONING_INPUT_TASK=build_zoning_dataset`, `REZONING_INPUT_BASENAME=zoning_matters`
  - FAR-first: `REZONING_INPUT_TASK=rezoning_far_pre_geocode`, `REZONING_INPUT_BASENAME=zoning_matters_far`
- `tasks/rezoning_far_pre_geocode/output/rezoning_far_pre_geocode_summary_<DATE_TAG>.json` (required only when `REZONING_INPUT_TASK=rezoning_far_pre_geocode`)
- `tasks/cook_parcel_addresses_download/output/parcel_addresses_3723-97qp.csv`
- `tasks/cook_parcel_addresses_download/output/parcel_universe_pabr-t5kh.csv`

## Outputs
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_<DATE_TAG>.csv`
- `tasks/rezoning_geocode_stage1_parcel/output/chicago_geocoder_upload_<DATE_TAG>.xlsx`
- `tasks/rezoning_geocode_stage1_parcel/output/chicago_geocoder_upload_mapping_<DATE_TAG>.csv`

## Run
```bash
cd tasks/rezoning_geocode_stage1_parcel/code
make link-inputs
make
```

## Approx Runtime
- 2-15 minutes

## Notes
- Optional Census fallback template: `make census-template`.
- Canonical harmonized full-period source is `tasks/rezoning_dataset/output/rezoning_dataset_harmonized_19990101_20260212.csv`.
- FAR-first canonical full-period input uses `make REZONING_INPUT_TASK=rezoning_far_pre_geocode REZONING_DATE_TAG=19990101_20260212`.
- When `REZONING_INPUT_TASK=rezoning_far_pre_geocode`, Stage 1 runs a FAR gate preflight and fails unless `parseable_non_structural_missing_count == 0` in the FAR summary.
- Legacy historical combiner (`rezoning_dataset_combine_historical`) remains available for reproducibility checks.
- Chicago upload workbook includes only uploadable addresses; `chicago_geocoder_upload_mapping_<DATE_TAG>.csv` remains the canonical row-level mapping for all unmatched matters.
- Address candidate source order is `address_raw`, then `address_raw_loose` (if present), then title-derived extraction from `matter_title`.
