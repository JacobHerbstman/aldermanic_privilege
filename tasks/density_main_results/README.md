# Main Density Results

`build_density_main_results.R` produces the paper's main density figure,
`density_rd.pdf`. It regresses log dwelling units per acre (DUPAC) on ten 100-ft
distance bands within 500 ft of the ward boundary, with the band just inside the
less-stringent ward omitted. It also regresses log DUPAC on an indicator for the
more-stringent side of the 500-ft window, which gives the average difference.
The specification and samples are set in
`tasks/shared/code/density_boundary_helpers.R` and shared with the appendix
density tasks: 2006–2022 buildings with a usable DUPAC; ward demographic
controls; boundary-segment-by-joint-service and zoning-group fixed effects;
standard errors clustered by ward pair. The three panels cover all
construction, multifamily buildings (two or more units, not single-family or
townhouse homes), and multifamily buildings with five or more units.

`build_density_summary.R` produces Table 1 (`density_sample_summary.tex`) and
its data (`density_sample_summary.csv`). The table describes the citywide sample
of 2006–2022 buildings with a usable DUPAC and positive dwelling count, with no
distance limit. Each building's nearest boundary segment on its ward pair is
counted, and those assignments are checked against the analysis data.

Run `make` in `code/`.
