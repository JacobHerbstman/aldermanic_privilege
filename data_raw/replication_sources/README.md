# Recorded inputs to price and rent analysis

These files preserve the task outputs present at the September 7, 2026 audit.
The dates in the filenames are the original local modification dates, not
independently verified download timestamps. The owning task's
`code/source_snapshot.sha256` identifies the exact preserved bytes.

- `fred_cpi_cuura207sa0_20260731.csv`: Chicago CPI-U, FRED series CUURA207SA0,
  https://fred.stlouisfed.org/graph/fredgraph.csv?id=CUURA207SA0. This is the
  original downloader's normalized two-column CSV, not the HTTP response bytes.
- `parcel_sales_city_2006_2022_20260826.csv`: Cook County Assessor parcel sales,
  https://datacatalog.cookcountyil.gov/resource/wvhk-k5uv.csv, township codes
  70–77 and years 2006–2022; selected/renamed columns are explicit in the owning
  task's download script. It is the assembled ordered API extract.
- `residential_improvements_2006_2022_20260731.csv`: Cook County Assessor
  residential improvement records, https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv,
  township codes 70–77 and years 2006–2022; assembled ordered API extract.
  This is distinct from the longer construction-review history snapshot.

Ordinary task Make builds verify and restore these files without contacting the
mutable APIs. The source records must be distributed with the replication data
bundle; they are not committed as large Git objects. Bundle distribution remains
unfinished. A current API request cannot be claimed to reproduce their vintage.

Each owning task has a separate `download_recipes.make`. Running
`make -f download_recipes.make` from that task's code directory requests a
candidate refresh ending in `_current.csv`. It never replaces the recorded
input. Adoption requires comparing reports, documenting the data change, and
updating the recorded snapshot and checksum deliberately.

These files are distributed in the September 10, 2026 recorded-source release.
Run `make -C replication` at the repository root to retrieve and verify them.
