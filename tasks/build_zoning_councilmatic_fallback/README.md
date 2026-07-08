# build_zoning_councilmatic_fallback

## Purpose
Builds the Councilmatic fallback table used by `build_zoning_dataset` when ELMS PDF parsing leaves an incomplete zoning pair.

## Inputs
- `tasks/elms_fetch_ordinances/output/matters_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/candidate_final_ids_<DATE_TAG>.csv`
- `tasks/elms_pdf_processing/output/pdf_text_<DATE_TAG>.csv`
- `tasks/elms_pdf_processing/output/pdf_zoning_fields_<DATE_TAG>.csv`

## Outputs
- `tasks/build_zoning_councilmatic_fallback/output/councilmatic_fallback_<DATE_TAG>.csv`

## Run
```bash
cd tasks/build_zoning_councilmatic_fallback/code
make link-inputs
make
```

## Notes
- The output has one row per matter whose pre-Councilmatic zoning pair is incomplete.
- The table records the pre-Councilmatic parse state, Councilmatic fetch status, Councilmatic text source, and parsed Councilmatic zoning pair.
- `build_zoning_dataset` fails if this table does not cover every fallback row it needs.
