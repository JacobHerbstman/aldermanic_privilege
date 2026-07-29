# Density Results Without Zoning Controls

This audit re-estimates the short paper's current 100-foot distance-band
comparisons after removing construction-year zoning-group fixed effects. It
does not change the paper.

The output distinguishes:

- the current model with zoning-group fixed effects;
- the same observations without zoning controls; and
- all otherwise eligible projects without zoning controls.

All models retain the alderman-pair average score, neighborhood
characteristics, boundary-segment and construction-year fixed effects, and
ward-pair clustering.

Run `make` from `code/`. Estimates are in
`output/current_density_no_zoning_estimates.csv`; the figure reproduces the
main density plot on the common sample without zoning controls.
