# Chicago CPI

This task supplies FRED Chicago all-items CPI-U series CUURA207SA0.
An ordinary `make` in `code/` verifies the recorded source checksum and writes
`output/fred_cpi_cuura207sa0.csv` and its keyed data report. It does not refresh the source.

The preserved file, source URL, and limits of its recorded vintage are described
in [`data_raw/replication_sources/README.md`](../../data_raw/replication_sources/README.md).
The checksum is in `code/source_snapshot.sha256`. These preserved files still
need inclusion in the distributed replication data bundle.

To request a deliberate candidate refresh, run `make download-current`
in `code/`. The existing source-specific download script writes a separate
`_current.csv`; the normal pipeline continues to use the recorded snapshot.
Query choices belong to that refresh recipe. Compare the candidate with the
recorded input before adopting it; a new download is a source-vintage change.
