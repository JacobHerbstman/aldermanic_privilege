# Density score checks

`build_density_score_robustness.R` checks two concerns about how the alderman
stringency score classifies the two sides of a boundary, using the main density
specification (`tasks/shared/code/density_boundary_helpers.R`).

First, it re-estimates the two alderman scores for each building after removing
the building's own permits (the permit ids carried in the analysis data). The
permit-level first stage is estimated once; only the alderman scores are
recalculated. The script first checks that its full-sample scores reproduce the
published through-2022 index exactly.

Second, it keeps only boundaries where the two alderman scores differ by at
least 0.25 or 0.50 standard deviations.

Running `make` in `code/` creates `output/density_score_robustness.tex`, which
appears in Appendix D.
