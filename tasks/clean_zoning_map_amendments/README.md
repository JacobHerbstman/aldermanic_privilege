# Zoning map amendments, 2010–2026

`clean_zoning_map_amendments.R` builds one row per zoning map amendment in the City Clerk's eLMS records (6,568
amendments from 6,716 ordinance records introduced from 2010 to September 2026, leaving aside 34 communications and
resolutions filed under the zoning category; `tasks/download_elms_matters`), in `output/zoning_map_amendments.csv`.
Run `make` in `code/`.

- **One row per amendment.** When the Clerk moved to its new system in 2023, amendments still pending were given a
  second record (numbered O2023-000xxxx) with the original introduction date, on which later actions were recorded;
  the original record stayed at "Council Introduction". A few ordinances were also entered twice on one day.
  Records introduced the same day with the same title or the same numeric application number are one amendment
  (148 amendments with two records), represented by the record on which it was resolved: passed, then withdrawn,
  placed on file, failed, pending, stalled. `record_numbers` lists all its records and `records` counts them. Of
  the 148, 67 passed, 4 are pending and 77 stalled; counted record by record, the stalled original records of
  amendments that passed later looked like stalls, and the 77 stalls were counted twice.
- **Filing and outcome.** Whether an alderman filed the amendment (filing office a ward number) or it is an
  application transmitted by the Zoning Administrator; introduction and passage dates and the days between; whether
  it was held in committee; and its outcome from the record's sub-status: passed (including as a substitute),
  withdrawn, placed on file, failed, stalled (never passed and introduced before the council term that began May 15,
  2023, so lapsed with its term) or pending (introduced in the current term and undecided).
- **Title.** Numeric application number ("App No. 17212", "App 20897"; the "A" numbers of aldermen's amendments
  name a series shared by several ordinances and are not kept), zoning map number and address.
- **Districts before and after**, from the legislation text (`tasks/extract_zoning_legislation_text`), using the
  substitute ordinance where one exists and the introduced one otherwise, from the files of all the amendment's
  records. The district before is the application's "Present Zoning District", or else the first ordinance sentence's
  ("changing all the ... District symbols and indications ... to those of a ... District"); the district after is the
  last such sentence's, or else the application's "Proposed Zoning District". The sentence is read in the wordings and
  OCR misreadings found in the text ("changing the", "changingill", "District, as shown on Map", "10 those of",
  "tiiose", "vvhich"), and district codes are repaired where OCR misreads them ("Ml-2", "83-2", "RMS", "R M 5", "B l -
  l"); only codes of the zoning ordinance are kept. `from_source` and `to_source` record where each came from,
  `in_steps` marks changes made in several steps (for example to a district and then to a planned development), and
  the two `*_sources_disagree` columns flag amendments where the ordinance and the application name different
  districts. `introduced_to_districts`, `substitute_to_districts` and whether each version is to a planned development
  keep the district after as each version states it, for `tasks/compare_substitute_ordinances`. An ordinance changing
  several separate areas (SO2017-5225) is read as its first area's district before and its last area's after.
- **Direction.** To a planned development if any step names one or the ordinance attaches a planned development's
  statements ("PLANNED DEVELOPMENT STATEMENTS", "PLAN OF DEVELOPMENT STATEMENTS"); otherwise up or down by the
  highest allowed floor-area ratio before and after (Second City Zoning's district table,
  `tasks/download_second_city_zoning_districts`), the same ratio, or unknown. A planned development or park district
  has no ratio, so a change from one is unknown, and one from a planned development and a district together takes the
  district's ratio.

83 percent of amendments have districts at each end and the direction of 94 percent is known (966 down, 1,286 the
same ratio, 2,996 up, 915 to a planned development): 96 percent of applications and 84 percent of aldermen's
amendments. Of the 405 unknown, 154 change a planned development (or an unread district) to a district, 107 name no
readable district after, 77 none at either end (mostly planned developments' text and pages the OCR could not read),
47 involve a park or other district without a ratio and 20 have no legislation text; they keep their filing, dates and
outcome.

Checked against the City's current zoning map, which names the ordinance record that set each district
(`tasks/audits/zoning_record_validation`, `output/elms_districts_vs_zoning_map.csv`): of 4,316 passed amendments found
there, the district after agrees with the map for 98 percent.
