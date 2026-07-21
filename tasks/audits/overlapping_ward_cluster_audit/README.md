# Overlapping Ward-Cluster Audit

This audit asks whether treating each ward boundary pair as an independent
cluster understates uncertainty when different pairs share a ward.

The script reproduces the current density, permit event-study, rental, and
sales models. The density models use the current locally centered score
difference and pair-average score. The audit then reports:

- the production clustering rule;
- ward-pair clustering;
- clustering separately by the ward on each observation's own and neighboring
  sides;
- directional two-way endpoint clustering;
- a dyadic covariance estimator that permits dependence whenever two ward
  pairs share either ward; and
- leave-one-ward-out estimates for the headline multifamily density and permit
  models.

The dyadic calculation follows the shared-node covariance structure in
[Aronow, Samii, and Assenova
(2017)](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/D43E12BF35240100C7A4ED3C28912C95/S1047198700011955a.pdf).
The directional two-way estimates are included as a sensitivity check, not as
a substitute for the dyadic estimator, because a ward can appear in either
endpoint position. Leave-one-ward-out estimates are influence checks rather
than corrected standard errors.

Nothing in this task is used by production or either paper.

## Findings

Ward pairs overlap substantially. The density sample has 125 pairs across 49
wards; the median ward appears in five pairs and the maximum is nine. The
permit panel has 107 pairs across 49 wards; the corresponding counts are four
and eight.

The permit result is not sensitive to allowing pairs that share a ward to be
correlated. The high-discretion pooled estimate is -0.099. Its standard error
is 0.032 with production ward-pair clustering, 0.027 with directional two-way
endpoint clustering, and 0.030 with the shared-ward dyadic calculation. The
low-discretion estimate remains statistically insignificant under every rule.

For multifamily density, the FAR estimates and both binary estimates remain
significant under directional two-way endpoint clustering. Continuous FAR is
-0.235, with an SE of 0.069 under ward-pair clustering and 0.089 under endpoint
clustering (p = 0.011). Binary FAR is -0.259 (endpoint-clustered SE 0.098,
p = 0.012), and binary DUPAC is -0.319 (endpoint-clustered SE 0.095,
p = 0.002). Continuous DUPAC is the exception: its estimate is -0.176, and its
SE rises from 0.057 to 0.091 (p = 0.060). None of the six headline density and
permit estimates changes sign when any single ward is omitted.

Rental precision is more sensitive. Its estimate is 0.0169, with an SE of
0.0083 under production segment clustering, 0.0095 under directional endpoint
clustering, and 0.0115 under the shared-ward dyadic calculation. The sales
estimate is already imprecise and remains so.

The dyadic covariance calculation is informative but not a proposed
replacement for production inference. Some of its full finite-sample
covariance matrices are not positive semidefinite even though the variance of
the target coefficient is positive. The directional endpoint and
leave-one-ward-out results are therefore the cleaner robustness checks.
