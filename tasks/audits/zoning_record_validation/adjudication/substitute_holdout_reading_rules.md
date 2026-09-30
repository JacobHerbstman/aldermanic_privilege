# Reading rules: introduced and substitute zoning ordinances

Each Chicago zoning map amendment below has page images of two versions of its project description: the ordinance
or narrative as introduced (`<record>_introduced_p<page>.png`) and the substitute or final version that replaced it
(`<record>_substitute_p<page>.png`). The pages are a Type 1 application's narrative ("Narrative and Plans") or a
planned development's "Bulk Regulations and Data Table" or statements.

For each amendment and each version, read from its images only:

- `far`: the project's floor-area ratio (FAR) as printed ("FAR: 2.47", "Floor Area Ratio: 1.63", "Maximum Floor Area
  Ratio: 5.0", "(2.4 FAR)"). Not a floor area in square feet.
- `dwelling_units`: the number of dwelling units ("Dwelling Units: 16", "Maximum Number of Dwelling Units: 64",
  "sixteen residential dwelling units" = 16). 0 if the page says there are none.
- `height_ft`: the building height in feet, inches as twelfths (45 feet 6 inches = 45.5; 102'-4" = 102.333).

Where a page gives several values for one field (subareas, several buildings, existing and proposed), record the
overall or total value if one is printed, otherwise the first listed, and say so in `note`. Leave a field blank if
the version's images do not state it. Record the value as printed even if it looks wrong. `clear` is TRUE if every
value you recorded for that version is legible and unambiguous, FALSE otherwise (explain in `note`).

Write one CSV row per amendment and version, with this header:

record_number,version,far,dwelling_units,height_ft,clear,note

`version` is `introduced` or `substitute`. Quote any note containing a comma.
