# Recorded replication sources

The root `make` runs `make` in this directory before building the paper. It
retrieves the September 10, 2026 source archive from the repository's GitHub
release, verifies its SHA-256 checksum, and restores the files listed in
`source_files.txt` under `data_raw/`. A missing member is restored through the
same extraction rule; unchanged builds reuse the archive and extracted files.

The archive also holds sources used only by the construction audits under
`tasks/audits/`; the paper build reads the ward maps, OpenStreetMap layers,
`data_raw/replication_sources/`, and these files in `data_raw/construction_review/`:
the full building-permit extract, commercial valuations, residential improvement
history, 2025 parcel addresses and parcel universe, and the 2012, 2014, 2016 and
2025 zoning maps.

The building-permit processing-time task separately restores the exact
721,958-record extract preserved on September 12, 2026, an additional asset of
the same release. Its filename and checksum are documented in
[`tasks/download_building_permits/README.md`](../tasks/download_building_permits/README.md).

Source descriptions and qualifications belong to the owning task READMEs. Other
services used for source acquisition, including Census and Dewey, retain their
documented access and credential requirements.

For maintainers deliberately assembling a new vintage, `make build-source-archive`
writes `source-archive-current.tar.gz` from the explicitly listed sources. It does
not overwrite the downloaded archive or publish a release. A new vintage needs
its own release name, checksum, source comparison and documentation.
