# rezoning_dataset

## Purpose
Builds the canonical 1999-2026 rezoning dataset in one task: upstream harmonization (modern ELMS + historical Clerk first-pass) and terminal final analysis tables.

## Inputs
- `tasks/build_zoning_dataset/output/zoning_matters_20101201_20260212.csv`
- `tasks/clerk_journals_first_pass_features/output/journal_rezonings_first_pass_1999_2010.csv`
- `tasks/rezoning_geocode_enrich/output/rezoning_geocoded_enriched_19990101_20260212.csv`
- `tasks/create_alderman_data/output/chicago_alderman_panel.csv` (finalize mode)

## Outputs
- `tasks/rezoning_dataset/output/rezoning_dataset_harmonized_19990101_20260212.csv`
- `tasks/rezoning_dataset/output/rezoning_dataset_all_19990101_20260212.csv`
- `tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv`

## Run
```bash
cd tasks/rezoning_dataset/code
make link-inputs-harmonize
make harmonize

make link-inputs-finalize
make finalize
```

Optional combined run:
```bash
cd tasks/rezoning_dataset/code
make link-inputs-harmonize
make link-inputs-finalize
make both
```

For the final analysis dataset, prefer `make both` or `make finalize`. Plain `make`
only builds the harmonized pre-geocode file.

## Approx Runtime
- `harmonize`: <1 minute (excluding upstream regeneration)
- `finalize`: <1 minute
- `both`: <2 minutes (excluding upstream regeneration)

## Notes
- `make` defaults to `harmonize`.
- `is_usable_analysis` is `TRUE` only when `far_change` is numeric and both `latitude` and `longitude` are present.
- Finalize mode assigns `assigned_alderman` by `ward + matter_intro_date` month only for rows with non-null `ward`, `latitude`, and `longitude`.
- New finalize columns: `assigned_alderman`, `assigned_alderman_month`, `assigned_alderman_date_source`.

## Reproducibility notes
- Local raw inputs required under `data_raw/`: `Boundaries_-_Zoning_Districts_20250910.geojson`, `zoning-code-summary-district-types.csv`, `chicago_zoning_code1957.txt`, and `zoning_conversion_2004_crosswalk.csv`.
- Live-download stages: ELMS matters/attachments, ordinance PDFs, Clerk journal PDFs, Cook County parcel/address Socrata files, and Councilmatic fallback text.
- Static committed handoffs: `tasks/rezoning_geocode_external_merge_ready/output/` and `tasks/rezoning_hand_adjudications/output/`.
- Python dependencies used by the rezoning tasks include `pandas`, `requests`, `PyMuPDF` (`fitz`), `pypdf`, `openpyxl`, and optionally `pypdfium2`, `pytesseract`, `Pillow`, and `geopandas` for OCR/map/geometry paths.
