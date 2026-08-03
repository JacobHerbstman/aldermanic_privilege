# Score Vintage and Mechanical Reflection Audit

This audit asks whether using permit records through 2022 to measure aldermanic
stringency mechanically affects the boundary estimates that also use outcomes
observed through 2022. It leaves the paper and the main analysis tasks unchanged.

The checks use the score specification in the paper: permit and review-type fixed
effects, lagged ward-month permit volume, permit-count weights in the second stage,
and all listed ward characteristics except bachelor's-degree share.

The audit compares:

1. the current score estimated from permits filed in 2006--2022;
2. a construction-project leave-out score, which removes permit records linked to
   each project after the common first-stage residualization;
3. a broader leave-out score that removes permits from both boundary wards in the
   observation year;
4. a score estimated only from 2006--2014 permits, on the sample for which both
   endpoint aldermen have scores in that period; and
5. the current score after excluding boundaries with small score differences;
6. a second score estimated from January 2015 through June 2019, before the
   July 2019 change in Chicago's formal review process; and
7. a project-specific classification that uses the 2006--2014 score for earlier
   projects and the 2015--June 2019 score for later projects.

The project-level check uses the permit-chain links retained by the construction
sample audit. Projects without a linked permit in the score receive the unchanged
score. The ward-year check is available for construction, rents, and sales.

`adjudication/final_project_permit_links.csv` is the two-column subset of
`commercial_new_construction_sample_audit/output/project_permit_chain_links.csv`
for projects in the final construction file. Its SHA-256 hash is
`7de81866eb4fd1f017117e8f8ce33cb4a6e5a152277a1c631565514254d41975`.

Run the audit from `code/` with `make`.

## Findings

The direct mechanical link between a construction project and its alderman's
score is negligible. Removing each project's linked permits changes the side
classification for 2 of 3,692 projects and leaves all four nearest-boundary
density estimates unchanged to three decimal places. Removing all score permits
from both endpoint wards in the observation year is much more conservative. It
changes 7.0% of density ward-year comparisons, 4.3% of rental comparisons, and
5.7% of sales comparisons. The nearest-boundary estimates remain similar under
that check:

| Outcome | Current score | Ward-year leave-out |
| --- | ---: | ---: |
| All construction, log FAR | -.127 (.048) | -.102 (.046) |
| All construction, log DUPAC | -.167 (.067) | -.154 (.062) |
| Multifamily, log FAR | -.150 (.068) | -.137 (.068) |
| Multifamily, log DUPAC | -.194 (.098) | -.188 (.098) |
| Rent | .017 (.013) | .019 (.013) |
| Sale price | .045 (.015) | .045 (.015) |

The score is not stable across the two halves of the permit history. Among the
49 aldermen observed in both periods, the 2006--2014 and 2015--2022 scores have a
Pearson correlation of .089 and a rank correlation of .031. Restricting the
comparison to the 29 aldermen with at least 250 permits in each period gives
correlations of .084 and .088. Because the current score contains both periods,
its correlation is higher with each component: .791 with the 2006--2014 score
and .549 with the 2015--2022 score.

This instability matters for the density comparisons. The table below first
holds the score fixed and restricts the sample to boundaries for which both
aldermen have a 2006--2014 score. The last column changes only the score used to
classify the two sides.

| Nearest 100ft comparison | Current score, full sample | Current score, common sample | 2006--2014 score, common sample |
| --- | ---: | ---: | ---: |
| All construction, log FAR | -.127 (.048) | -.107 (.054) | -.027 (.057) |
| All construction, log DUPAC | -.167 (.067) | -.132 (.088) | .014 (.082) |
| Multifamily, log FAR | -.150 (.068) | -.182 (.088) | -.063 (.072) |
| Multifamily, log DUPAC | -.194 (.098) | -.298 (.138) | .043 (.116) |

The common density sample contains 2,774 projects, including 560 multifamily
projects. About 19% of its project classifications change between the earlier
and current scores. Dropping boundaries with small current-score gaps does not
produce a consistent multifamily dose-response pattern; estimates become noisy
and eventually change sign at the one-standard-deviation threshold.

The price results are less sensitive. On the common sample, replacing the
current score with the earlier score changes the nearest-boundary rental
estimate from .011 (.022) to .018 (.022) and the sale-price estimate from .043
(.021) to .056 (.021).

The audit therefore separates two issues. A project's own permits do not
mechanically generate the reported boundary estimates. But the proposed defense
that the pooled score is a more precise measure of a persistent alderman trait is
not supported by the split-period comparison, and the density estimates are not
robust to the earlier score. No paper files have been changed.

## Intact-Regime Split

The July 2019 reform and the pandemic do not explain the weak split-period
correlation. Replacing the 2015--2022 window with January 2015--June 2019 leaves
the correlation near zero:

| Score construction | Aldermen | Pearson | Rank correlation |
| --- | ---: | ---: | ---: |
| Separate first-stage adjustment, all common aldermen | 49 | .057 | -.062 |
| Separate first-stage adjustment, at least 250 permits in each period | 29 | .058 | -.014 |
| Common first-stage adjustment, at least 250 permits in each period | 29 | -.161 | -.058 |

The precommitted standard was a Pearson correlation of at least .40 among
aldermen with 250 permits in both periods. The estimate does not approach that
threshold. The first window contains 71,074 permits used in the score regression;
the second contains 29,481. Mean processing time rises from 46.5 to 65.1 days,
and the median rises from 24 to 37 days.

Low permit counts explain some extreme changes, but not the general result.
Among aldermen with at least 250 permits in both windows, Toni Foulkes moves from
-2.45 to .16, Ed Burke from .30 to 2.72, Carrie Austin from -.71 to 1.22, George
Cardenas from 1.06 to -.15, and Roberto Maldonado from -.51 to .55. Their raw
mean processing times also change substantially. The archived ward maps confirm
that Sposato's geography changed sharply in 2015: only 19.3% of the old 36th
Ward's land area remained in the new ward, while 40.3% of the new ward came from
the old one. That explains why his before-after comparison is especially hard to
interpret, but it cannot account for the other reversals.

## Project Classifications

The classification exercise uses the exact density regression sample. It assigns
projects built through 2014 with the 2006--2014 score and later projects with the
January 2015--June 2019 score. The latter is the most recent pre-reform measure
for projects built after June 2019; aldermen who entered after that date therefore
have no period-specific score.

Coverage is complete for the 2006--2014 projects. It is 85.7% for all construction
and 77.7% for multifamily in 2015--2019. It falls to 36.6% and 37.6%, respectively,
for 2020--2022 because many endpoint aldermen entered after June 2019. The common
sample contains 3,225 projects, including 671 multifamily projects, compared with
3,692 and 822 in the pooled-score samples.

Among common-sample projects in segments represented on both physical ward
sides, 26.7% of all-construction classifications and 29.7% of multifamily
classifications change. In the nearest 100 feet, the corresponding shares are
23.8% and 25.9%. The flip rate is about 18% in 2006--2014 and 46--48% in
2015--2019. These changes are concentrated in substantively important boundary
comparisons, not only observations with nearly tied pooled scores.

Changing the side classification is what changes the result. The table holds the
sample fixed and shows the nearest 100-foot comparison:

| Outcome | Pooled score, common sample | Period-specific side, pooled average | Period-specific side and average |
| --- | ---: | ---: | ---: |
| All construction, log FAR | -.120 (.047) | -.012 (.047) | -.010 (.047) |
| All construction, log DUPAC | -.168 (.073) | .026 (.065) | .029 (.065) |
| Multifamily, log FAR | -.219 (.089) | -.057 (.093) | -.057 (.093) |
| Multifamily, log DUPAC | -.305 (.115) | .066 (.111) | .068 (.111) |

Restricting the analysis to projects built by June 2019 leads to the same
conclusion. On that common sample, the pooled-score estimates are -.100, -.126,
-.164, and -.236 in the order shown above. The period-specific estimates are
.006, .053, -.036, and .093. The full 500-foot comparisons also lose their
negative multifamily pattern.

The direct leave-out and minimum-gap checks remain ready for use: they show that
a project's own permits do not mechanically determine its classification and
that the pooled-score results are not confined to nearly tied aldermen. They do
not resolve the separate vintage problem. The intact-regime scores are not stable,
and using them to classify the same projects materially changes the density
results. No paper files or main analysis tasks have been changed.
