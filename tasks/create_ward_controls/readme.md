# create_ward_controls

Purpose: Creates ward-year demographic controls from Census block-group data.

The 2000 controls use Census 2000 SF3 counts and apply to 2000--2009. Bachelor's-or-higher counts sum the male and female bachelor's, master's, professional, and doctorate categories in table P037. The 2010--2012 controls combine 2010 SF1 population and tenure counts with socioeconomic measures from the 2009--2013 ACS five-year estimates, the earliest ACS block-group data currently available through the Census API. Controls from 2013 onward use the corresponding ACS five-year release.

Race and ethnicity are mutually exclusive: non-Hispanic White, non-Hispanic Black, and Hispanic of any race. Homeownership is owner-occupied housing divided by occupied housing. Census counts are allocated across intersecting wards in proportion to block-group area under the map in effect that year. Ward shares are calculated from the allocated counts. Median household income is the household-weighted average of block-group medians after households are allocated by area.

Historical ward polygons contain small overlapping slivers. Ward-specific intersections are scaled to the block group's area within the union of all ward polygons so that no area is counted twice.

Produces: `output/ward_controls_2000_2023.csv`.

Approx. runtime: ~1-10 minutes.
