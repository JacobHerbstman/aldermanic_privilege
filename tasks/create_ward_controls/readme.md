# Ward Characteristics

This task creates the 2006--2022 ward-year characteristics used to estimate the
alderman stringency score.

Controls for 2006--2009 use Census 2000 SF3 counts. Bachelor's-or-higher counts
sum the male and female bachelor's, master's, professional, and doctorate
categories in table P037. The 2010--2012 controls combine 2010 SF1 population
and tenure counts with socioeconomic measures from the 2009--2013 ACS
five-year estimates, the earliest ACS block-group data currently available
through the Census API. Controls from 2013 onward use the corresponding ACS
five-year release.

Race and ethnicity are mutually exclusive: non-Hispanic White, non-Hispanic
Black, and Hispanic of any race. Homeownership is owner-occupied housing
divided by occupied housing. Census counts are allocated across intersecting
wards in proportion to block-group area under the map in effect that year.
Ward shares are calculated from the allocated counts. Median household income
is the household-weighted average of block-group medians after households are
allocated by area.

Historical ward polygons contain small overlapping slivers. Ward-specific
intersections are scaled to the block group's area within the union of all ward
polygons so that no area is counted twice.

The output is `output/ward_controls_2006_2022.csv`.

The Makefile restores the recorded Census inputs from [`sources/`](sources/README.md).
`create_ward_controls.R` reads those counts and polygons, allocates the counts,
and calculates ward-year controls. It writes a report when saving the data.
`make download-current` runs `download_census_controls.R` with `CENSUS_API_KEY`
and saves separate current responses for review before adoption.
