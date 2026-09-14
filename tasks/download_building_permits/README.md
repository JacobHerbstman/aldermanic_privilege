# Building permits

Ordinary `make` restores the exact City of Chicago permit extract used by this
checkout. The file contains 721,958 records and 116 columns from public dataset
[`ydr8-5enu`](https://data.cityofchicago.org/resource/ydr8-5enu.csv), selected by
application date from January 1, 2006 through December 31, 2022 and ordered by ID.
It includes the City's public permit descriptions, fees, locations and contact
fields; no research estimates or manual corrections are added to the source.

The snapshot was preserved on September 12, 2026. That is the preservation date,
not a claim about the original download date. Its uncompressed SHA-256 is
`768736629c461e6a04d9345e91c930d99dfe9b7b6ffbc0d31c2ae3accb487a5a`.
The Makefile verifies the compressed snapshot before extracting the unchanged
CSV, then writes a standard data report. The separate compressed asset is
`building_permits_2006_2022_preserved_20260912.csv.gz` in the repository's recorded
source release. The asset was uploaded with approval on September 12, 2026.
Retrieval through the Makefile's public URL reproduces both the compressed
snapshot and the original CSV exactly, verified by their SHA-256 checksums.
The original September 10 archive is unchanged.

For a deliberate refresh, `make download-current` retrieves the Makefile's
`START_YEAR`–`END_YEAR` period into `temp/`. Ordered batches, CSV structure and
start/end row counts are checked before publishing that comparison file.
It does not replace the recorded production input. Adopting a refresh requires
preserving and comparing the new source, and updating its URL and checksum.
