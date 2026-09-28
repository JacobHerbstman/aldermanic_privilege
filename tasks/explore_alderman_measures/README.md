# Comparing measures of alderman stringency (exploratory, branch `explore-alderman-measures`)

Two scripts, run with `make` in `code/`:

- `compare_alderman_measures.R` puts the alderman-level measures side by side, each oriented so that higher means
  stricter: the processing-time index (2006–2022 and 2006–2014 permits), fewer high- or low-discretion permit
  applications than the ward's blocks otherwise receive (`tasks/estimate_alderman_permit_effects`), and the zoning
  stall rate, days to passage and own downzonings per year (`tasks/estimate_alderman_zoning_measures`). It writes
  their rank correlations, differences by characteristic and stated position (`tasks/alderman_characteristics`),
  correlations with ward demographics, and principal components.
- `explore_zoning_patterns.R` looks at the 6,716 zoning map amendments directly: citywide trends by year, outcomes by
  who filed and the kind of change, outcomes by ward demographics, outcomes by distance to the nearest other ward,
  and whether the serving alderman explains outcomes beyond the ward (ward-on-map fixed effects, with and without
  alderman fixed effects, identified by the 29–31 changes of alderman within a ward on one map).

## Findings (September 27, 2026)

- **The measures do not share a common factor.** The processing-time index is unrelated to every other measure
  (Spearman between −0.06 and 0.16 across 90–110 aldermen). The permit and zoning measures are also nearly unrelated
  to one another; the one clear link is between stalls and days to passage (0.24). The first principal component
  explains 22 percent of the variance, about what six unrelated measures across 89 aldermen would give by chance
  (median 23 percent in simulations).
- **Stated positions line up with the index, and only with the index.** Aldermen on record wanting to keep prerogative
  score 0.55 standard deviations stricter on the processing-time index (p = 0.04, 22 against 33 aldermen), and
  those on record for reforming or abolishing it score 0.78 lower (p = 0.008). No other measure differs by stated
  position. Other characteristics give about as many differences at p < 0.1 as chance would (11 of 98 tests).
- **Ward demographics.** Aldermen in Blacker, less white wards see fewer high-discretion permits than their blocks
  otherwise would (0.23 and −0.30). Passage takes longer in whiter, richer, lower-homeownership wards nearer the
  Loop, and aldermen nearer the Loop downzone more. Stall rates do not vary with demographics.
- **Trends.** Application stall rates rose from 5–8 percent in 2010–2018 to 11–15 percent in 2019–2022, and median
  days to passage fell from about 56 to 35 in 2021–2022. Aldermen's own downzonings fell from 23–50 a year to 8 in
  2018 and zero in 2019, returning in 2023 and 2025.
- **Direction.** Applications to a planned development stall most (17 percent) and take longest (median 135 days);
  downzoning applications almost always pass. A quarter of alderman-filed downzonings stall.
- **No boundary pattern.** Stall rates, days and upzoning shares are flat in distance to the nearest other ward.
- **The alderman matters beyond the ward for stalls and upzonings.** When a ward's alderman changes, its stall rate
  and upzoning share shift more than chance would allow (F = 1.9, p = 0.003 and 0.002), though the aldermen add only
  one percentage point of explained variance. Days to passage and floor-area increases do not shift (p = 0.18 and
  0.35).
