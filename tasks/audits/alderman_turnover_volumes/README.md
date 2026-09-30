# Zoning amendments and permits when a ward's alderman changes (audit)

Exploratory tests of whether aldermen shape what is filed and built in their wards, not only what happens to
applications once filed. Run `make` in `code/`.

- `count_ward_terms.R` counts, for each ward and council term of 2003--2023, applications (by the ward their site lies
  in), the alderman's own amendments and own downzonings (by the ward they were filed from), high-discretion permits,
  new construction permits, and new residential buildings and their dwelling units (the permit- and Assessor-linked
  buildings of `tasks/prepare_permit_construction`), with the alderman who held the ward for most of the term and the
  ward's side of the city, in `output/ward_term_counts.csv` (250 rows). Amendments come from
  `tasks/audits/zoning_stalls_and_delays` (the Journals before 2011, eLMS from 2011), permits from
  `tasks/data_for_alderman_uncertainty_index` (2006--2022, so the first and last terms are covered in part), terms
  from `tasks/create_alderman_data/adjudication/alderman_terms.csv`. The sides are the conventional nine groups of
  community areas, entered by hand in `sources/community_area_sides.csv`; a ward's side is that of the community area
  most of its permits lie in.
- `test_turnover_changes.R` compares the terms before and after the elections of 2007 and 2011 (2003 ward map) and
  2019 (2015 map), when wards kept their boundaries: 150 ward-elections, 43 of them with a new alderman. For each ward
  and measure it compares the count after with what the ward's counts before and after would split into under the
  citywide split (or its side's), in `output/ward_election_changes.csv`, and tests whether wards with a new alderman
  depart from that split more than wards whose alderman stayed, by reassigning the changes at random among the wards
  of each election (and side), in `output/turnover_change_tests.csv`. Dwelling units arrive a building at a time, so
  their counting noise uses the sum of each building's units squared rather than the count. `plot_turnover_changes.R`
  draws each ward-election's departure, for new construction permits and for dwelling units, in
  `output/turnover_new_construction.png` and `output/turnover_new_units.png`.
- `compare_current_term_permits.R` places the new construction permits of both permit periods
  (`tasks/clean_building_permits`, 2006--2022 and 2023 to September 2026) in the wards of the 2024 map and compares,
  by issue month, the current term (May 15, 2023 through August 2026) with the same territory in the previous term
  (May 20, 2019 to May 14, 2023), in `output/current_term_permits_by_ward.csv`: permits and reported cost per year
  in each term, and each ward's change relative to the city's.
- `summarize_own_amendments.R` gives each alderman's own amendments by kind of change and per year in office, for
  the Journals (2000--2010) and eLMS (2011 to September 2026), in `output/own_amendments_by_alderman.csv`.

## Results (September 30, 2026)

New construction shifts when the alderman changes. Measured as the squared departure from the side's split, in units
of counting noise, wards with a new alderman average 12.6, against 4.7 for wards whose alderman stayed (permutation p
= 0.011; 15.3 against 7.6 against the citywide split); without the two largest departures of each group, 9.3 against
3.9. The largest are both falls and rises: after 2011 the 24th ward (Dixon to Chandler) had 29 new construction
permits where 95 would have kept its share of the South Side, the 6th (Lyle to Sawyer) 11 for 39 and the 29th
(Carothers to Graham) 8 for 28, while the 1st (Flores to Moreno) had 427 for 335 and the 26th (Ocasio to Maldonado)
237 for 179; after 2019 the 1st (Moreno to La Spata) had 222 for 329. The permit- and Assessor-linked buildings agree:
new residential buildings depart from their side's split by 14.6 at turnovers against 6.5 (p = 0.038). Their dwelling
units do not (3.1 against 3.3, p = 0.84). Units are concentrated in a few large buildings (those of 50 or more units
hold 63 percent of the 113,270 units, while 82 percent of buildings have one to three), whose timing is lumpy whoever
the alderman and whose counting noise is large, so the test has little power for them: turnover moves the number of
new buildings, most of them small, not measurably the number of units. Applications point the same way but not clearly
(2.6 against 1.6 by side, p = 0.14), and all high-discretion permits, mostly renovations, do not (p = 0.22).

Aldermen's own amendments vary far more across aldermen than counting noise allows. Among those with two or more years
in office, own downzonings average 2.15 a year in 2000--2010 with a standard deviation across aldermen of 2.36,
against 0.57 from counting alone, and 0.79 a year in 2011--2026 with 0.98 against 0.36. The most frequent in the 2000s
were Matlak (32nd, 9.9 a year), Ocasio (26th), Flores (1st), Shiller (46th), Balcer (11th) and Waguespack (32nd). But
the rates do not shift at turnovers more than between two terms of one alderman (p = 0.34 to 0.89): they come in
bursts, and in the same wards whoever holds them. Own downzonings fell from 540 in the term of 2003--2007 to 120 in
that of 2019--2023.

In the current term the territory of the 34th ward (Conway) was issued 16 new construction permits a year, against 32
in the previous term: half the city's pace of change (0.53; reported cost 0.39). Neighboring downtown wards held up
(the 42nd 1.20, the 27th 0.93). The comparison is early for a new alderman: large projects receive building permits a
year or more after their rezoning, and the 34th's planned developments passed in 2024--2025, so the permits of the
current window are mostly of projects entitled before it.

Consumer: none (exploratory).
