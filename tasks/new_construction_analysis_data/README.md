# New-construction regression inputs

This task connects the finished construction dataset to the density regressions.
It does not choose buildings or change construction years, units, building area,
land area, density permissions, or the two saved density measures.

1. `attach_construction_regressors.R` reads the finished building and boundary
   files, retains projects within 1,500 feet, and attaches boundary segments,
   daily alderman terms, the existing through-2022 score, and ward-year controls.
   It saves `construction_regressors.csv`.
2. `build_new_construction_analysis_data.R` attaches construction-year zoning
   and saves `new_construction_analysis_data.csv`, which the density tasks read.

The construction-date proxy remains June 15 of the recorded completion year.
A vacancy or an alderman without a score remains explicitly recorded and cannot
supply a signed boundary distance. Segment matching is constrained to the
building's ward pair and uses the existing 1,320-foot boundary segments. Chicago
geometry is in EPSG:3435. Ward controls retain their existing definitions and
2006–2022 producer; the score estimator is unchanged.

Multifamily classification follows the existing Assessor-class rule and recorded
building-type reviews. Groups of individually classified houses or townhomes
are not automatically apartment buildings. These reviews supply classification
only; their old measurements never overwrite the finished building data here.

Each outcome uses its own density permission and positive, finite measurement.
A missing floor area therefore does not remove a building from homes-per-acre
analysis. Missing regression covariates are never filled with fabricated values.

The prior 8,648-row paper input is preserved for comparison in
`tasks/working_paper_release_audit/reference/new_construction_analysis_data_before_reconnection.csv`.
Its SHA-256 is
`9dc7953e91bdf21a909224d2d68697a8440b56b66f137c7d784bea6137bf8ea4`.
The comparison task reports additions, removals and changed measurements; project
ID changes must be interpreted using the recorded source-project links.

## Current integration status

Jacob approved using the preserved zoning history on September 10. The zoning
producer reads the recorded 2006 map and construction-year history from the
construction task's adjudication directory, alongside the pinned official
snapshots. It applies existing corrected-year zoning decisions once, keyed to
both project and year. Those inputs define the zoning replication boundary for
this run; reconstruction from the underlying ordinances remains outstanding.

Boundary characteristics and project-permit matches now have producers that read
this dataset. Fresh-checkout verification and distribution of the recorded source
archive are tracked separately in `replication/README.md`.
