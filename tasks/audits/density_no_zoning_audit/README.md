# Density Results Without Zoning Fixed Effects

This audit reproduces every density result shown in the short paper after removing zoning-group fixed effects. It does not change production outputs or the paper.

The main regression comparison separates two changes:

- `No zoning FE, common sample` removes the zoning fixed effects while retaining only observations with a production zoning code.
- `No zoning FE, full sample` also retains otherwise eligible observations whose zoning code is missing.

All versions retain the current alderman score, 500ft bandwidth, side-specific linear distance controls, own-side demographic controls, pair-average score control, segment and construction-year fixed effects, and ward-pair clustering.

Run `make` from `code/`. The complete comparison is in `output/density_no_zoning_report.pdf`, with machine-readable estimates in `output/density_no_zoning_estimates.csv`.
