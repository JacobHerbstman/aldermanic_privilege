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
- `boundary_counts.R` asks whether building and filing sort toward the more lenient side of ward boundaries: for each
  boundary and pair of aldermen serving on its two sides at once, it relates each side's count of new buildings,
  dwelling units, zoning applications, upzoning applications and aldermen's own downzonings within 500 and 250 ft to
  how much stricter that side's alderman is on each measure (Poisson with boundary-by-aldermen fixed effects). The
  same comparison is made at placebo lines 500, 750 and 1,000 ft inside either ward, and with a stall rate estimated
  only from applications more than 500 ft from any other ward (`stall_rate_away_from_boundaries.csv`).
- `density_by_zoning_measures.R` runs the paper's density design with each boundary's more-stringent side decided by
  the zoning and permit measures instead of the processing-time index.

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
- **Upzoning applications sort away from aldermen who stall.** Within 500 ft of a boundary, the side whose alderman
  stalls more (by one standard deviation) receives 12 percent fewer upzoning applications (−0.131 log points,
  SE 0.046), and within 250 ft 21 percent fewer (−0.237, SE 0.062). Dropping any one boundary or alderman leaves
  it between −0.10 and −0.26. At the six placebo lines the estimates run from −0.10 to +0.08 (500 ft) and −0.14 to
  +0.13 (250 ft), so the real boundary gives the most negative in both. With the stall rate estimated only from
  applications away from boundaries (Spearman 0.81 with the full measure) the estimates are about half as large
  (−0.071 and −0.106, p < 0.1) and still the most negative against the placebos. Dwelling units built show the same
  sign at the boundary (−0.14 and −0.18, p < 0.1), but the placebo lines give estimates as large, so that is noise.
  The processing-time index shows no pattern for any count.
- **Stall differences are concentrated near boundaries.** The variance of stall rates across aldermen beyond
  sampling noise falls as applications near boundaries are left out (0.0083 with all, 0.0075 beyond 500 ft, 0.0044
  beyond 750 ft, none beyond 1,000 ft).
- **The density design does not work with the zoning measures.** Ordered by stall rates, days to passage or the
  permit effects, the more-stringent side's multifamily buildings are no less dense, and by days to passage and the
  stall rate away from boundaries they are denser (0.079 and 0.085, p < 0.05). These orderings agree with the
  processing-time index for only 26 to 62 percent of buildings. Applications stall and take longer where
  development is denser, and on the stricter side only projects worth the trouble may be built, so the measures
  may follow density rather than cause it.
