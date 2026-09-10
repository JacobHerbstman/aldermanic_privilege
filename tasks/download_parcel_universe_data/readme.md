# Parcel universe source

Ordinary `make` in `code/` verifies and replays the recorded 2025 City native
snapshot supplied in `data_raw/construction_review/`, then rewrites its column
names to the existing consumer schema. `parcel_universe_snapshot.sha256` records
the required source bytes. Changing shared Make paths cannot refresh this source.
The snapshot must be included in the replication data archive; if absent, the
build fails. It does not substitute a current API vintage.

The recorded native file was preserved from the existing local acquisition on
September 6, 2026. Its July 31 modification date is not proof of the exact retrieval
date. Source: Cook County Assessor Parcel Universe, Socrata `nj4t-kc8j`, query
`year=2025 and triad_name='City'`, ordered by PIN and partitioned by PIN prefix.
The script checks each partition's record count and schema, and checks the total
before and after retrieval. The schema rewrite preserves source values.

An intentional API refresh uses `make -f download_recipes.make` from `code/`.
Its separate output is `parcel_universe_2025_city_native_current.csv`. An unchanged
refresh build reuses that file; `make -B -f download_recipes.make` explicitly requests
a new retrieval. Transfers are validated in a temporary directory before replacing
that refresh output. Neither command changes the recorded snapshot or the paper's
source selection. Adopting a new vintage requires preserving the new source,
updating the snapshot checksum, comparing affected data reports, and recording the
research consequences in the logbook.
