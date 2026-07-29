# New-Construction Counts at Ward Boundaries

This audit tests whether fewer residential projects are completed on the
more-stringent side of ward boundaries.

The analysis counts projects in 100-foot distance bands from 2006 through
2022. Poisson models compare bands within the same boundary segment and
construction year, so the comparison holds fixed local construction activity
and the length of the boundary segment. Standard errors are clustered by ward
pair.

The exercise is descriptive of completed projects. It does not observe sites
where development was considered but never entered the Assessor data.

Run `make` from `code/`. The main outputs are:

- `project_count_models.csv`
- `project_counts_by_band.csv`
- `project_count_by_band.pdf`
