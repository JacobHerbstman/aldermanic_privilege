# Model-adjusted density figure

This audit plots the four density estimates from the main 500-foot
specification. The points remove the model's demographic controls, zoning
groups, boundary-segment effects, construction-year effects, and pair-average
score while retaining the estimated discontinuity and separate distance trends
on each side of the boundary.

The labels use the same ward-pair-clustered standard errors as the regression
table. The figure omits confidence bands because the lenient-side fitted value
at the boundary is normalized to zero, which makes pointwise bands visually
awkward. No paper files use this figure.

The audit also reports a flat 500-foot comparison matching the rent and sales
figures. That model omits the two distance slopes, so its coefficient is the
adjusted difference in average levels across the two 500-foot strips rather
than a fitted discontinuity at the boundary.

The triangular-kernel figures estimate a local-linear discontinuity while
retaining the density model's controls and fixed effects. The first uses the
paper's 500-foot window and gives greater weight to projects closer to the
boundary. The second uses outcome-specific bandwidths selected by `rdbwselect`
from the corresponding no-controls regression, following the bandwidth choice
used for the reference figure. In both figures, the fitted jump and the
reported coefficient come from the same regression. Circle size records the
number of projects in each distance bin.

The paper-style version of the fixed 500-foot figure presents the same
triangular-weighted estimates with equal-sized binned dots and no confidence
bands. It changes only the display.

The error-bar version adds 95% confidence intervals to those binned means.
Each bin mean uses the triangular project weights, and its standard error is
clustered by ward pair. The fitted lines and regression estimates are
unchanged.

The nonparametric-bin figures follow the display in Kulka, Sood, and
Chiumenti more directly. A single regression replaces the linear distance
terms with indicators for every distance bin and omits the bin immediately
inside the lenient side of the boundary. Each plotted coefficient is therefore
a difference from that reference bin. Confidence intervals are clustered by
ward pair. The 50-foot version provides more detail; the 100-foot version is
close to the reference paper's 0.02-mile bins. These regressions use uniform
weights, as in the reference specification, and retain the controls and fixed
effects from the main density model.

The ribbon version of the 100-foot figure replaces the individual error bars
with shaded 95% confidence regions connecting adjacent bin coefficients. The
ribbons are drawn separately on each side of the boundary and do not alter the
estimates.

The score-comparison figures repeat that 100-foot ribbon graph under four
otherwise identical stringency scores: neither income nor bachelor's share,
bachelor's share only, income only, and both controls. The construction sample,
outcomes, controls, fixed effects, and standard-error calculation remain fixed.
The script also verifies that the estimation sample does not change across the
four scores.

The shifted-cutoff figures apply the same saturated 100-foot-bin specification
to artificial cutoffs 1,000 feet inside either ward. Each model uses the
500-foot window around the artificial cutoff, so all projects on both sides of
a placebo cutoff remain under the same alderman. The legend therefore describes
positions below and above the artificial cutoff rather than aldermanic
stringency. Before estimating these wider-window checks, the script replaces an
assessor land area of one square foot or less with the complete
construction-year parcel area when that area is available. This corrects one
Taylor Street project outside the main 500-foot sample whose assessor land
field equals one square foot.

The donut figures estimate that same 100-foot-bin model at the true ward
boundary after excluding projects within 25 or 50 feet. They test whether the
nearest-bin comparison is driven by projects whose coordinates or boundary
assignment place them almost exactly on the ward line.

The boundary-feature table repeats the nearest-bin comparison after excluding
segments that run near expressways or water for at least 25, 50, or 75 percent
of their length. It also reports the old any-contact rule and a severe version
that drops arterial-road segments. These restrictions follow the
physical-barrier checks used in related boundary studies; the arterial
restriction is intentionally severe because many Chicago ward lines follow
ordinary major streets.

The segment-classifier review replaces the old any-overlap label with the
share of each boundary segment that lies near an expressway or water feature.
It compares the City and OpenStreetMap expressway layers, records how many
projects each segment contributes, and maps the highest-impact candidates.

The 250-foot ribbon figure uses five equal 50-foot bins on each side. It keeps
the same saturated-bin specification but estimates it only with projects
within 250 feet of the boundary.

The distance-band figure estimates separate adjusted side differences for
projects 0–100, 100–200, 200–300, 300–400, and 400–500 feet from a ward
boundary. One regression estimates the five side differences jointly while
holding the controls and fixed effects common across bands. The dashed line
marks zero, while each subtitle reports the full Table 2 estimate. These band
estimates are descriptive checks of where the side difference appears, not
replacements for the RD estimator.

The residualized binned-means figure displays the same band model in running-
variable space. Dots are means of the adjusted project-level outcome in 50-foot
bins, with their size proportional to the number of projects. Horizontal lines
are the adjusted means for each 100-foot band and side. The difference between
the two horizontal lines at a given absolute distance is the corresponding
coefficient in the distance-band figure. The adjustment removes the model's
demographic controls and fixed effects while retaining the distance-band and
side differences.

The descriptive figures use a simpler adjustment. They remove zoning groups,
construction years, boundary-segment effects, pair-average stringency, and
neighborhood characteristics without removing the side difference or distance
patterns. One version adds separate linear summaries on each side and one shows
only the 50-foot binned means. A third figure shows the corresponding raw
binned outcomes and linear summaries. None of these figures reports a visual
discontinuity; the regression estimates remain in Table 2.

The 250-foot version of the residualized points-only figure repeats the
nuisance adjustment using only projects within 250 feet of a boundary. It is
not a cropped version of the 500-foot graph.

The triangular-weighted 250-foot figure gives greater weight to projects closer
to the boundary in both the nuisance adjustment and the 50-foot binned means.

## Boundary and segment checks

The segment geometry is reliable for the density sample. An independent
recalculation found no ward, ward-pair, distance, or segment-assignment
discrepancies among the 4,155 projects within 500 feet. All 4,114
all-construction observations and all 881 multifamily observations used by the
corresponding density samples have valid segment assignments.

The old one-word physical-feature label is not reliable enough to determine
sample exclusions. It assigns a feature when a segment has any buffered
contact, which can mistake a perpendicular crossing for a boundary that
follows an expressway or waterway. The overlap-share fields are more
informative. Among segments used by projects within 500 feet, 111 have any
expressway or water contact, 76 have at least 25 percent overlap, 51 have at
least 50 percent overlap, and 37 have at least 75 percent overlap.

The City and OpenStreetMap expressway layers agree closely for substantial
overlaps. At the 50 percent threshold, the City layer flags 22 segments, the
OpenStreetMap layer flags 32, and all 22 City segments are also identified by
OpenStreetMap. Maps of the highest-impact segments confirm that a 50 percent
threshold removes genuine along-boundary cases much more cleanly than the old
any-contact rule, although complicated interchanges remain judgment calls.
The eight saved overlap measures were also recalculated from the raw road,
water, park, and cemetery maps for all 858 segments used by projects in the
500-foot sample. Polygon measures and OpenStreetMap line measures reproduce to
numerical precision. The largest difference for a City road measure is 0.23
feet, far too small to change any overlap threshold.

The existing pruning rule also excludes a segment for any waterway or cemetery
contact. Seventeen segments, containing 36 projects and 11 multifamily
projects, are removed only by that provision. Their maps mostly show endpoint
or partial contact rather than a boundary that follows the feature. Replacing
that provision with a 50 percent share rule barely changes the estimates.

The nearest-bin estimates are stable after dropping segments with at least 50
percent expressway or water overlap. They are also stable after excluding
projects within either 25 or 50 feet of the boundary. The narrower 250-foot
sample is less precise, especially for multifamily DUPAC, and should be
reported as a weaker bandwidth check rather than as a separate headline
estimate.
