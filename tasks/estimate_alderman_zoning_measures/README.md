# Alderman zoning measures (exploratory, branch `zoning-records`)

`estimate_alderman_zoning_measures.R` measures each alderman's handling of zoning map amendments in 2010–2026
(`tasks/clean_zoning_map_amendments`, with wards and aldermen from `tasks/assign_zoning_amendment_wards`). Run `make`
in `code/`.

- **Stall:** whether a decided application in the alderman's ward (one not filed by an alderman) stalled, lapsing
  without a vote. Only applications introduced before the current council term (May 15, 2023) count, since whether
  later ones will stall is not yet known.
- **Days to passage:** log days from introduction to passage of passed applications.
- **Downzonings:** downzoning amendments the alderman filed, per year in office within the data window.

The first two are adjusted for introduction year, the kind of change (up, down, same floor-area ratio, to a planned
development, unknown) and the months left in the council term at introduction; applications introduced in a term's
last six months stall 19 percent of the time, against 8 to 10 percent otherwise, because they lapse with the term. The
adjusted values are averaged by alderman and shrunk toward zero by empirical Bayes; downzoning rates are shrunk
toward the mean rate. `alderman_zoning_measures_by_half.csv` repeats the first two for the earlier and later halves
of each alderman's applications.

## Findings (September 27, 2026)

Beyond sampling noise, stall rates differ across aldermen by 9 percentage points (standard deviation) and days to
passage by 0.12 log points. The earlier and later halves of aldermen's applications agree only modestly
(correlation 0.19 for stalls across 63 aldermen, 0.29 for days to passage across 82), so a good part of the
difference is specific to a period. The three measures are nearly unrelated to one another (Spearman 0.24 between
stalls and days, 0.08 and −0.04 with the downzoning rate). They have not yet been compared with the processing-time
index or with alderman characteristics, which live on the main line of the project.
