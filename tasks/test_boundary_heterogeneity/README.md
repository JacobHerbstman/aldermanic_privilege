# Do outcomes jump at ward boundaries more than at placebo lines? (exploratory, branch `alderman-effects`)

A test that uses no stringency measure. `test_boundary_jumps.R` labels each side of a boundary by ward number and
estimates the jump in log DUPAC (all construction and multifamily), log listed rent and log sale price within each
cell: a boundary segment during one pair of facing aldermen (segment by joint service for density, segment by the two
aldermen for prices). It uses the main specifications' controls and fixed effects, within 500 ft, and requires a
minimum number of observations on each side of each cell. The same cells are estimated at placebo lines 1,000 ft
inside each ward, where both sides share an alderman. Each cell's jump is identified within one ward pair, so
standard errors are clustered by building (rents: a building's repeated listings) or property (sales); clustering by
ward pair makes them degenerate. Before estimating anything new, the script reproduces the paper's rent and sales
average differences exactly. Run `make` in `code/`.

Outputs: `boundary_cell_jumps.csv` (every cell's jump) and `boundary_jump_heterogeneity.csv`: the variance of jumps
across cells beyond sampling noise, mean(estimate² − se²), and of the change in a segment's jump between consecutive
alderman pairs, mean(difference² − se₁² − se₂²)/2, each with its standard error, at the real boundary and at the two
placebo lines.

## Findings (September 27, 2026)

Rents jump at real boundaries about as much as at placebo lines: the variance of cell jumps is 0.0127 (SE 0.0016),
against 0.0104 and 0.0108 (0.0016). The change in a segment's jump when the facing aldermen change is 0.0069
(0.0019), against 0.0024 (0.0009) and 0.0046 (0.0023) at placebo lines, larger by one to two standard errors. Sales
jumps are indistinguishable from placebo jumps (0.0061, SE 0.0051, against 0.0030 and 0.0104). The density samples
are too small for this test: with three buildings a side, cell standard errors are unreliable (the multifamily
variance estimate is negative), and only 17 segments change aldermen with buildings on both sides.
