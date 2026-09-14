# Assessor source records for construction cleaning

Construction cleaning uses the full Chicago residential assessment history,
including reports before 2006 and after 2022. Later reports identify successor
parcels and clarify buildings completed during the 2006–2022 construction
period. Assessment-report years and construction years are different filters.

The task reuses the existing Cook County download script with an explicit
1999–2026 assessment-year window. These years cover the assessment vintages
present in the surviving full-history source copies used during the original
construction work. The sales download retains its separate 2006–2022 window.

Run `make download-current` from `code/` for new residential and commercial extracts, saved with `_current` filenames. Ordinary `make` restores the recorded condominium extract. The residential source is Cook County's
[residential improvement characteristics dataset](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds).
The downloader orders records by PIN, assessment year, card, and row ID and
checks CSV structure, batch counts, and the total source count before replacing
the output. Public-source revisions can still change historical records; a
fresh download must be reconciled with the reference construction sample.

The task also downloads the separate
[commercial valuation records](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Commercial-Valuation-Data/csik-bsws).
Its downloader is restored from
`research-archive:tasks/download_commercial_valuation_data/code/download_commercial_valuation_data.sh`
at commit `010a1f8497c1f32e2c79b5933d1c5baf9af44be3`. This source retains the
record ordering used by the archived commercial adjudications. The first
fresh download and restored commercial cross-section both matched their
archived SHA-256 hashes exactly.

## September 8, 2026 reconciliation sources

The condominium extract contains 4,560 parcel-year records for the requested successor condominium bases, assessment years 2021–2025, from Cook County dataset `3r7i-mrz4`. The source repeats whole-building measurements on individual unit records: never sum these repeated areas. Parking and common-area records must be distinguished from homes. The extract is a pinned, mutable public-source snapshot; a deliberate refresh requires comparison before use.

- `construction_condominium_history.csv` SHA-256: `be6833fbf1e4dcb122ddf24e37b63550282928824e0be7f81f42e3b837b8e612`.

`code/construction_condominium_queries.csv` pins the 66 condominium bases identified in the initial 268-record reconciliation. These are acquisition parameters, not building measurements or inclusion decisions. Resolving a project does not remove its source records from this extract. Further acquisition requires an explicit addition to this query list and a reviewed snapshot refresh.

The ordinary condominium build now restores the committed September 9 source
snapshot and verifies `code/condominium_snapshot.sha256`. This makes the recorded
vintage available to a fresh clone instead of downloading mutable records again.
Run `make download-current` for a separate current extract; adopting it
requires a comparison and deliberate source update.
