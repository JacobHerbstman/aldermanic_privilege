# Main Density Results

The main figure estimates the paper's 100-foot distance-bin specification for
FAR and DUPAC using the analysis data and current through-2022 alderman scores.
Both outcomes use the common `density_eligible` field from the analysis-data
producer, requiring usable, positive FAR and DUPAC for the same projects.

Table 1 is a separate citywide descriptive summary. It reads the full cleaned
project ledger and construction-year boundary distances, retaining 2006–2022
construction with both density measures usable and positive units. It has no
boundary-distance, score-availability or regression-control restriction. The
analysis data alone cannot supply this table because that file is limited to
1,500 feet. The table retains two columns: all construction and multifamily.
Its building-type classification is carried from the same construction
measurement file as the regressions and checked on overlapping observations. The ward-pair/boundary-segment row counts
distinct nearest ward pairs and boundary segments over this citywide sample,
using the construction-year map. Segment assignments are checked against the
analysis data where available. The summary CSV has a standard data report.

Run Make in code/. No data cleaning or regression definitions are changed by
the descriptive table. The older citywide table's scope is restored; its obsolete
measurement values are not reused.

The paper labels the table "New Residential Construction in Chicago, 2006–2022."
Its note defines FAR, DUPAC, and multifamily buildings for readers. Multifamily
refers to buildings with two or more dwellings; the reviewed building-type
classification above supplies the subgroup, rather than a cutoff based on the
total units in a project containing several separate houses.
