# Density score checks

This task checks two concerns about how aldermanic stringency classifies the two sides of a ward boundary.

First, it recalculates the two alderman scores for each construction project after removing permit records matched to that project. The permit-level adjustment is estimated once, and the alderman scores are then recalculated without the project's own permit outcomes. This prevents a project's processing time from directly affecting the score used to classify its boundary.

Second, it excludes boundaries where the two alderman scores differ by less than 0.25 or 0.50 standard deviations. These restrictions test whether close rankings drive the main result.

The recorded matches in `adjudication/project_permit_matches.csv` remain inputs.
`build_project_permit_matches.R` retains matches for surviving project IDs and
adds exact component-parcel matches or permits inside a saved parcel polygon
with the same project ID and construction year. Nearby permits alone do not
qualify. New matches use the existing window: application from six years before
through two years after construction, and issue from four years before through
two years after. These broad windows support the leave-project-out check; they
do not determine a building's completion year. Jacob approved this rule on
September 10, 2026. The output records each match's source, with one row per
project/permit pair. A permit may relate to more than one building on a site;
the score calculation removes its ID only once for each project's check.

Running `make` from `code/` creates `output/density_score_robustness.tex`, which appears in Appendix D.
