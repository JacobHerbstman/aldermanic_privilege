# build_zoning_dataset

## Purpose
Builds the cleaned rezoning matter table used by downstream geocoding and enrichment tasks.

## Inputs
- `tasks/elms_fetch_ordinances/output/matters_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/matter_sponsors_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/matter_histories_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/matter_attachments_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/candidate_final_ids_<DATE_TAG>.csv`
- `tasks/elms_pdf_processing/output/pdf_text_<DATE_TAG>.csv`
- `tasks/elms_pdf_processing/output/pdf_zoning_fields_<DATE_TAG>.csv`
- `tasks/build_zoning_councilmatic_fallback/output/councilmatic_fallback_<DATE_TAG>.csv`

## Outputs
- `tasks/build_zoning_dataset/output/zoning_matters_<DATE_TAG>.csv`

## Run
```bash
cd tasks/build_zoning_dataset/code
make link-inputs
make
```

## Approx Runtime
- Fast after inputs are linked. Councilmatic network calls run in the upstream fallback task.

## Notes
- The assembler reads a Make-managed Councilmatic fallback table and fails if any required fallback row is absent.
- Modern source is ELMS-backed through upstream tasks.
- Output is restricted to map-reclassification rezonings using `rezoning_detection_method` (`title_map`, `pdf_from_to`, `both`).
