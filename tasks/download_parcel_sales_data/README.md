# Parcel sales

This task supplies Cook County Assessor parcel sales, Chicago township codes 70–77, years 2006–2022.
An ordinary `make` in `code/` verifies the recorded source checksum and writes
`output/parcel_sales_city.csv` and its keyed data report. It does not refresh the source.

The preserved file, source URL, and limits of its recorded vintage are described
in [`data_raw/replication_sources/README.md`](../../data_raw/replication_sources/README.md).
The checksum is in `code/source_snapshot.sha256`. The file is restored from the
recorded source archive by `make -C replication`, which the root `make` runs first.

To request a deliberate candidate refresh, run `make download-current`
in `code/`. The existing source-specific download script writes a separate
`_current.csv`; the normal pipeline continues to use the recorded snapshot.
Query choices belong to that refresh recipe. Compare the candidate with the
recorded input before adopting it; a new download is a source-vintage change.
