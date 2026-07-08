# elms_pdf_processing

## Purpose
Downloads candidate ordinance PDFs and extracts page text plus zoning fields for downstream parsing.

## Inputs
- `tasks/elms_enrich_matters/output/matter_attachments_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/candidate_final_ids_<DATE_TAG>.csv`

## Outputs
- `tasks/elms_pdf_processing/output/pdf_text_<DATE_TAG>.csv`
- `tasks/elms_pdf_processing/output/pdf_zoning_fields_<DATE_TAG>.csv`

## Run
```bash
cd tasks/elms_pdf_processing/code
make link-inputs
make
```

## Approx Runtime
- 20-180 minutes

## Notes
- Downloads PDFs into `tasks/elms_pdf_processing/output/pdfs_<DATE_TAG>/`.
- Supports string `matter_id` values from ELMS (`recordNumber`).
