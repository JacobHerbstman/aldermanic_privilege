# Density boundary design checks

This audit asks whether the density results depend on curved or physically divided ward
boundaries and whether observed location characteristics change at the boundary. It does
not alter the construction sample or any paper files.

## Boundary restrictions

The straight-boundary rule follows Kulka, Sood, and Chiumenti's adaptation of Turner,
Haughwout, and van der Klaauw (2014). For each project, the script draws the shortest
line to its assigned ward boundary. It then draws a 100-meter line through that boundary
point, perpendicular to the first line. A project passes when both endpoints are within
15 meters of the actual ward boundary.

The feature restriction uses overlap shares instead of dropping a segment after any
contact with a road, waterway, park, or cemetery. The audit reports a simple 50-percent
expressway-or-water rule and a broader rule that drops segments with at least 50 percent
park, water, or cemetery overlap, at least 40 percent expressway overlap, or at least
75 percent arterial-road overlap.

The simple overlap rule leaves the estimates essentially unchanged. It retains 3,443 of
3,692 all-construction observations and 799 of 822 multifamily observations. The
all-construction discontinuities are -0.126 for log FAR and -0.167 for log DUPAC; the
multifamily discontinuities are -0.150 and -0.195.

The straight-boundary restriction retains 2,877 all-construction observations and 592
multifamily observations. All four estimates remain negative, but they are smaller and
less precise: -0.082 and -0.119 for all construction, and -0.096 and -0.174 for
multifamily. The all-construction DUPAC estimate remains significant at five percent;
the multifamily DUPAC estimate has a p-value of 0.089. The combined straight and broad
feature restriction is demanding enough that the multifamily sample falls to 454
projects and both multifamily estimates become imprecise.

## Continuity checks

The continuity regressions follow the distance-bin approach in Kulka et al. Each
characteristic is the dependent variable in a regression with 100-foot distance-bin
indicators, boundary-segment effects, and construction-year effects. The reported
coefficient compares the 0--100-foot bin on the more-stringent side with the 0--100-foot
bin on the less-stringent side. Standard errors for exact location characteristics are
clustered by ward pair.

In the current sample, none of the five exact location measures differs significantly
at the boundary for either all construction or multifamily construction. These measures
are distances to downtown, the nearest school, the nearest park, the nearest major road,
and Lake Michigan. In the straight-boundary multifamily sample, distance to a major road
is 0.013 miles higher on the more-stringent side (p = 0.017); the other nine
sample-by-characteristic comparisons remain insignificant.

The paper reports the other four measures and omits distance to a major road. Major
roads often form ward boundaries, so that distance is partly a description of the
boundary itself rather than a useful continuity test.

The ACS table is a coarser descriptive check. Projects are assigned to 2010-vintage
block-group geography, then matched to the 2014 five-year ACS for construction through
2014 and the 2019 five-year ACS thereafter. Both ACS releases use 2010 Census
block-group boundaries. Every project has exactly one geographic match.

Many projects nevertheless share the same block-group estimate. In the all-construction
sample, 66 percent of projects are in block groups represented on both sides of a ward
boundary; the corresponding multifamily share is 41 percent. The regressions therefore
cluster by both ward pair and block group, and a separate output excludes block groups
observed on both sides. The results are broadly continuous, but the current multifamily
sample has a marginal difference in Black population share (p = 0.089). Results using
only block groups observed on one side are also largely insignificant.

The smaller straight-boundary multifamily sample shows more evidence of neighborhood
composition differences. White share, Black share, and median household income have
p-values between 0.057 and 0.086. After excluding block groups observed on both sides,
those three differences weaken, but median age differs by 4.8 years (p = 0.023). These
patterns make the ACS table a caution against describing the straight-boundary
multifamily subset as fully balanced.

These ACS variables can reflect neighborhood sorting and later development, so they are
not as clean a falsification test as distances to fixed amenities. The existing
population-density field divides population by total polygon area rather than Census
land area and is omitted from the continuity tables.

## Outputs

- `output/density_straight_boundary_classification.csv`
- `output/density_straight_boundary_results.csv`
- `output/density_amenity_continuity.csv`
- `output/density_acs_continuity.csv`
- `output/density_acs_assignment_diagnostics.csv`
- `output/density_covariate_continuity.tex`
- `output/density_covariate_continuity_straight.tex`

## References

- Kulka, Sood, and Chiumenti, *How to Build the City: The Effect of Land Use
  Regulations*: https://aradhyasood.github.io/Kulka_Sood_Chiumenti_Zoning_Oct2023.pdf
- Turner, Haughwout, and van der Klaauw (2014), *Land Use Regulation and
  Welfare*: https://matthewturner.org/papers/published/Turner_Haughwout_vanderKlaauw_ECT_2014.pdf
- Bayer, Ferreira, and McMillan (2007), *A Unified Framework for Measuring
  Preferences for Schools and Neighborhoods*:
  https://matthewturner.org/ec1410/readings/Bayer_Ferreira_Mcmillan_JPE_2007.pdf
