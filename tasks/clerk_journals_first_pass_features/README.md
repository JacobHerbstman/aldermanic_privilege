# clerk_journals_first_pass_features

## Purpose
Runs a first-pass, no-OCR feature extraction on Clerk journal PDFs to capture rezoning signals: from/to zoning text, address candidates, FAR mentions, and basic density metrics (units, stories, height).

## Inputs
- `tasks/clerk_journals_download/output/journal_manifest_<YEAR_TAG>.csv`
- `tasks/clerk_journals_download/output/journals/`
- `tasks/zoning_data_cleaning/output/zoning_far_lookup_clean.csv`

## Outputs
- `tasks/clerk_journals_first_pass_features/output/journal_rezonings_first_pass_<YEAR_TAG>.csv`

## Run
```bash
cd tasks/clerk_journals_first_pass_features/code
make YEAR_START=1999 YEAR_END=2010 link-inputs
make YEAR_START=1999 YEAR_END=2010
```

## Approx Runtime
- 10-40 minutes for 1999-2010, depending on PDF size and page-window settings.

## Notes
- This is intentionally a first-pass parser (high recall, moderate precision) and is kept separate from the modern ELMS pipeline.
- Defaults use zoning-section page windows from the manifest to avoid scanning entire PDFs.
- No OCR is used in this task.
- FAR is included two ways: explicit text mentions (`far_values`) and lookup-derived values from parsed zoning codes (`from_far_lookup`, `to_far_lookup`, `far_change_lookup`).
- Lookup-derived FAR uses the date-aware pre/post-2004 table from `zoning_data_cleaning`.
- Address extraction now outputs `addresses_strict` and `addresses_loose`; `addresses` is retained as a compatibility alias to `addresses_strict`.
- `addresses_strict` is conservative and drops boundary-description fragments (e.g., "feet north of..."); `addresses_loose` is a broader fallback for geocoder handoff workflows.
