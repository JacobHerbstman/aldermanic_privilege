# How substitute ordinances changed zoning map amendments

`compare_substitute_ordinances.R` compares, for each eLMS zoning map amendment with both an introduced version and a
substitute that replaced it, what the two versions state, in `output/substitute_changes.csv`: one row per amendment
and field, 5,148 rows for 2,570 amendments. Run `make` in `code/`.

- **Versions.** A substitute ordinance's file is named SO (or S0); from 2023 eLMS also attaches the application's
  narrative or statements, the version passed named "Final" ("Final Narrative and Plans.pdf"). Every other file is
  the introduced version. A file is identified by its address, since a record may hold two different files of one
  name.
- **Fields.** The district after, by its highest allowed floor-area ratio, as each version's ordinance sentences state
  it (`tasks/clean_zoning_map_amendments`, `introduced_to_districts` and `substitute_to_districts`); and the project's
  floor-area ratio, dwelling units and building height as each version's Type 1 narrative or planned development's
  bulk table states them, from the narrative and data-table pages re-read by tesseract
  (`tasks/extract_zoning_application_forms`). Each version's value is the first found in its files by the first of the
  field's rules that matches, a labelled table entry ("Maximum Number of Dwelling Units: 64", "FAR: 2.47", "Building
  Height: 45 feet 6 inches") before a sentence ("a total of thirty-four (34) dwelling units"). `introduced_text` and
  `substitute_text` hold the text each value was read from.
- **Comparison.** A pair is compared only where both versions state the field by the same rule, the Clerk's text layer
  and tesseract read the value alike where a page has both, neither value is nine or more times the other (a dropped
  foot mark or decimal point, "674'" for 67'4"), and, outside planned developments, neither stated floor-area ratio is
  more than twice the district's maximum (bonuses take it to about one and a half times); `not_compared` gives the
  reason otherwise. `change` is down, same or up, values within 0.05 of a floor-area ratio or a foot of height being
  the same (restated or rounded, 4.07 and 4.08). A district change counts only between two districts; a version to a
  planned development has no ratio.

Of 2,237 passed applications with both versions, 1,145 are compared on at least one field:

| Field | Compared | Substitute lower | Same | Substitute higher |
| --- | ---: | ---: | ---: | ---: |
| District after (highest floor-area ratio) | 852 | 48 | 772 | 32 |
| Stated floor-area ratio | 416 | 32 | 347 | 37 |
| Dwelling units | 329 | 17 | 296 | 16 |
| Height | 277 | 21 | 223 | 33 |

Most pairs are not compared because one version states nothing: substitutes are often the ordinance alone, without
the narrative, and only the first three pages of the narratives attached from 2023 were read, so the bulk tables of
recent planned developments are mostly missed. A planned development whose bulk table lists subareas is read at its
first listed or labelled value, and a pair read by different rules (one version's overall total, the other's first
subarea) is not compared.

Checked against hand reads of 30 passed applications drawn at random, 20 with a change and 10 without
(`tasks/audits/zoning_record_validation`, section 10): of the 26 pairs the comparison finds changed, readers of the
page images find the same change for 25, and of the 28 it finds the same, the same for 26. The rules that read a
ratio printed before "FAR" and a total without "of", and the requirement that a number not run on into further
digits ("FAR 127 dwelling units" read as a ratio of 12), were written after the first scoring of that sample, which
found the change confirmed for 26 of 29 pairs.

Consumers: `tasks/audits/zoning_stalls_and_delays` (substitute changes by alderman) and
`tasks/audits/zoning_record_validation` (the hand-read check).
