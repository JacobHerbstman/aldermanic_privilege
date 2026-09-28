# Zoning map amendments, 2010–2026

`clean_zoning_map_amendments.R` builds one row per zoning map amendment in the City Clerk's eLMS records (6,750
matters introduced from 2010 to September 2026; `tasks/download_elms_matters`), in `output/zoning_map_amendments.csv`.
Run `make` in `code/`.

- **Filing and outcome.** Whether an alderman filed the amendment (filing office a ward number) or it is an
  application transmitted by the Zoning Administrator; introduction and passage dates and the days between; whether
  it was held in committee; and its outcome from the record's sub-status: passed (including as a substitute),
  withdrawn, placed on file, failed, stalled (never passed and introduced before the council term that began May 15,
  2023, so lapsed with its term) or pending (introduced in the current term and undecided).
- **Title.** Application number, zoning map number and address.
- **Districts before and after**, from the legislation text (`tasks/extract_zoning_legislation_text`), using the
  substitute ordinance where one exists and the introduced one otherwise. The district before is the application's
  "Present Zoning District", or else the first ordinance sentence's ("changing all the ... District symbols and
  indications ... to those of a ... District"); the district after is the last such sentence's, or else the
  application's "Proposed Zoning District". OCR confusions in district codes are repaired and only codes of the zoning
  ordinance are kept. `from_source` and `to_source` record where each came from, `in_steps` marks changes made in
  several steps (for example to a district and then to a planned development), and the two `*_sources_disagree`
  columns flag amendments where the ordinance and the application name different districts.
- **Direction.** Up or down by the highest allowed floor-area ratio before and after (Second City Zoning's district
  table, `tasks/download_second_city_zoning_districts`), the same ratio, to a planned development, or unknown.

About 80 percent of amendments have districts at each end. Amendments without them are mostly changes to planned
developments' text and pages the OCR could not read; they keep their filing, dates and outcome.
