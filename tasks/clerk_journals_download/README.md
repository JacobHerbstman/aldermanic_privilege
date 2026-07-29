# clerk_journals_download

## Purpose
Scrapes City Clerk journal manifests and downloads/validates journal PDFs for historical rezoning extraction.

## Inputs
- Live City Clerk journals site

## Outputs
- `tasks/clerk_journals_download/output/journal_manifest_<YEAR_START>_<YEAR_END>.csv`

## Run
```bash
cd tasks/clerk_journals_download/code
make
```

## Approx Runtime
- 1-12 hours (network + PDF download volume)

## Notes
- Intermediate scrape artifacts are written to `tasks/clerk_journals_download/temp/`.
