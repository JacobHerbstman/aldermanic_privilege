# Alderman zoning measures (exploratory, branch `explore-alderman-measures`)

`estimate_alderman_zoning_measures.R` measures each alderman's handling of zoning map amendments in 2010–2026
(`tasks/clean_zoning_map_amendments`, with wards and aldermen from `tasks/assign_zoning_amendment_wards` and
refilings from `tasks/link_zoning_refilings`). Run `make` in `code/`.

- **Stall:** whether a decided application in the alderman's ward (one not filed by an alderman) stalled, lapsing
  without a vote. Only applications introduced before the current council term (May 15, 2023) count, since whether
  later ones will stall is not yet known. Stalls are split into **terminal stalls**, never filed again, and
  **delays by refiling**, filed again and then passed.
- **Days to passage:** log days from introduction to passage of passed applications.
- **Downzonings:** downzoning amendments the alderman filed, per year in office within the data window.

The application measures are adjusted for introduction year, the kind of change (up, down, same floor-area ratio, to
a planned development, unknown) and the months left in the council term at introduction; applications introduced
in a term's last six months often lapse with it. The adjusted values are averaged by alderman and shrunk toward zero
by empirical Bayes, and the shrunk values are reported only for aldermen with at least 20 decided applications (56
aldermen). Downzoning rates are shrunk toward the mean rate. `alderman_zoning_measures_by_half.csv` repeats the stall,
terminal-stall and days measures for the earlier and later halves of each alderman's applications.

## Findings (September 29, 2026, after the geocoding correction in `tasks/assign_zoning_amendment_wards`)

Of the 4,105 decided applications with an alderman introduced before May 2023, 319 stalled: 79 were filed again and
passed, 5 were filed again without passing, and 235 were never filed again. Among the 56 aldermen with enough
applications, shrunk stall rates spread with a standard deviation of 3.3 percentage points (terminal stalls 3.0) and
days to passage by 0.086 log points. The earlier and later halves of aldermen's applications agree only modestly (0.20
for stalls and terminal stalls, 0.35 for days, across aldermen with at least 10 applications in each half), so much of
the difference is specific to a period. Days to passage still reflect the projects a ward receives: Walter Burnett,
Jr., whose Near West Side ward files many large projects, has the longest adjusted time, which controls for project
size (not yet in these measures) should address.
