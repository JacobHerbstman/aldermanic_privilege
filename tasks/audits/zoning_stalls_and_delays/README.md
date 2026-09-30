# Stalls and days to passage of zoning applications, 2000--2026 (audit)

Exploratory summaries of how often zoning map amendment applications (amendments not filed by an alderman) stalled
and how long those that passed took, by year of introduction and by the alderman of the ward they lie in. Run `make`
in `code/`.

- `combine_amendments.R` joins the City Council Journals' zoning map amendments of 2000--2011
  (`tasks/follow_journal_zoning_amendments`, aldermen from `tasks/place_journal_zoning_amendments`) and eLMS's from
  2011 to the end of its download, September 23, 2026 (`tasks/clean_zoning_map_amendments`,
  `tasks/assign_zoning_amendment_wards`, `tasks/link_zoning_refilings`), in `output/zoning_amendments.csv`, one row
  per amendment and source, with aldermen's own amendments (`filer`) alongside the applications (4,475 applications
  from the Journals, 166 without an alderman, and 5,193 from eLMS, 28 without). An application stalled if it lapsed
  with a council term without passing, failing or being withdrawn, which is known only for applications introduced
  before the current term (May 15, 2023): 4,174 of eLMS's. It stalled for good if no refiling of it passed.
  `not_passed_in_window` (90 days) is known for every application with 90 days of follow-up, 5,125 of eLMS's,
  including the current term's. Days to passage are kept for applications with a year of follow-up, since only the
  quick passages of later ones are seen (2.4 percent of passages take longer); eLMS's 28 passages dated on or before
  their introduction are left without one. Each application's kind of change (`direction`) is defined as in
  `tasks/clean_zoning_map_amendments`: to a planned development, else up, down or the same by the highest allowed
  floor-area ratio of the districts before and after, else unknown. For the Journals the districts are the parser's
  (`tasks/parse_journal_zoning_amendments`); a district of the 1957 ordinance, in force until November 1, 2004, takes
  the ratio of the 2004 district the city converted it to outside downtown, or downtown where the conversion table
  (`data_raw/zoning_conversion_2004_crosswalk.csv`) gives only that. The kind is unknown for 2 percent of Journal
  applications and 4 percent of eLMS's. Under the 1957 ordinance 16 percent of applications lowered the floor-area
  ratio, against 8 percent under the 2004 ordinance and 7 percent in eLMS: most of these rezoned manufacturing or
  commercial land for housing (M1-2, 2.2, to R4, 1.2, is the commonest).
- `summarize_by_year.R` and `plot_by_year.R` give, by source and year, the stall rates (through 2022), the share not
  passed within 90 days (through 2025 in the figure) and days to passage (through 2025) in
  `output/stalls_and_delays_by_year.csv` and `output/stalls_and_delays_by_year.png`. The sources overlap in 2011 and
  agree: 8.4 percent of applications stalled in each, and the median passage took 63 days.
- `summarize_by_alderman.R` gives, in `output/stalls_and_delays_by_alderman.csv`, one row per alderman and period (the
  Journals for 2000--2010, eLMS for 2011--2026, and the two combined): applications, the raw stall rate and its
  standard error, the stall rate less the citywide rate of the same years, the rate stalled for good, the share not
  passed within 90 days, the median days to passage and the days relative to the citywide days of the same years. For
  the Journals it also counts applications and stalls by the alderman of the 2003 ward map, in which aldermen filed
  from December 2001. 74 aldermen have Journal applications (53 with at least 20), 106 eLMS applications (72) and 136
  either (106).
- The same file adjusts the stall, the share not passed within 90 days and log days to passage for composition, as
  `tasks/estimate_alderman_zoning_measures` does for eLMS, within each period: each application's outcome is adjusted
  for its year of introduction, kind of change (interacted with the source, since the kind is unknown far more often
  in eLMS) and months left in the council term (0--6, 6--12, 12--24, more than 24); the adjusted values are averaged
  by alderman, and each average is shrunk toward zero by empirical Bayes, for aldermen with at least 20 applications.
  For eLMS the adjusted stall measure reproduces the production one (correlation 0.996 over 58 aldermen). The
  shrinkage differs: the variance of the aldermen's true effects is estimated here only from the aldermen given shrunk
  measures. Aldermen with a few applications at the edge of a period, often lapsing with a term, otherwise inflate it:
  for eLMS stalls it is 7.8 points (standard deviation) with them, as in production, and near zero without them.
- `match_large_projects.R` compares each large project (an eLMS application whose form states at least 100
  dwelling units or a building of at least 150 feet; 326 with a located site and an alderman, 202 of them within 2
  miles of the Loop) with its 5 nearest large projects in other aldermen's wards, of the same kind (planned
  development or change of district), inside or outside 2 miles of the Loop alike, and nearest in log units (or
  height) and year, in `output/large_project_matches.csv`: days to passage, whether it passed within a year, and its
  log days less its matches' mean.
- `summarize_substitute_changes.R` gives, in `output/substitute_changes_by_alderman.csv`, one row per alderman, how the
  substitute ordinances of passed eLMS applications changed the projects introduced
  (`tasks/compare_substitute_ordinances`): an application was made smaller if some compared field (the district
  after's highest floor-area ratio, the stated floor-area ratio, dwelling units, height) fell and none rose, larger if
  some rose and none fell, and mixed if both; with the shares made smaller and larger, their standard errors, and the
  mean log changes of units, height and floor-area ratio.
- `test_substitute_changes.R` tests whether the aldermen with at least 10 compared applications differ in the shares
  made smaller, made larger and changed more than reassigning applications among them at random, within kind
  (planned development or not) and period of introduction, produces, in `output/substitute_change_tests.csv`.

Stalls carry almost no alderman signal: the aldermen's stall rates, adjusted for composition, vary no more than
sampling error allows in eLMS and in the combined period, so their shrunk measures are all zero (in the Journals they
span 2.6 points), and an F-test of equal alderman means, with the same adjustment, gives p = 0.05 in the Journals,
0.21 in eLMS and 0.02 combined. Delays carry more: the share not passed within 90 days and log days to passage differ
across aldermen beyond sampling error. Much of that difference is the kind and place of project. Natarus's passages
took 80 percent longer than the city's, but 4 percent longer than those of the same kind; Reilly's took 48 percent
longer in 2011--2026, but 23 percent shorter than those of the same kind, most of them planned developments. Burnett's
and Hopkins's took 21 percent longer than those of the same kind in 2011--2026, and Conway's 96 percent longer (from
14 passages of 2023--2026, nearly all large downtown planned developments passed as substitutes).

Among downtown large projects, Reilly's pass faster than their matches (mean log days 0.69 below, standard error
0.11, from 49 passed) and Burnett's slower (0.36 above, 0.08, from 76). Conway's 9 all passed, in a median of 182
days, 0.80 above their matches (standard error 0.31): two of them, 30 N LaSalle (889 days) and 37 S Sangamon (798),
took 7 and 13 times as long as theirs, and the other 7 took about half again as long (0.39 above on average).

Substitutes do not show aldermen cutting projects more than one another. Of 1,137 passed applications with an
alderman compared on at least one field, the substitute made 94 smaller (8 percent), 90 larger and 7 mixed, and left
946 the same. Planned developments change more often (31 percent, against 15 percent of other applications). Across
the 31 aldermen with at least 10 compared, the shares made smaller vary no more than reassigning applications at
random within kind and period of introduction produces (permutation p = 0.34; larger, p = 0.12; any change, p =
0.33). Hopkins's substitutes made 5 of his 44 smaller and 1 larger; Conway has 7 compared, since the bulk tables of
his planned developments lie beyond the pages read. The share made smaller falls from 18 percent of those introduced
in 2010--2014 to 2 percent of those from 2023, when the versions come from the attached narratives instead.

Consumer: none (exploratory).
