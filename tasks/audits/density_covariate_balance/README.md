# Density Covariate Balance Audit

This audit tests whether observed neighborhood characteristics change at the
ward boundaries used by the density analysis. Nothing in this task is used by
production or either paper.

## Sample and specifications

The task starts from the current density regression file and applies the same
2006--2022, 500-foot, valid-zoning, valid-segment, and construction-sample
restrictions. Results are reported separately for all residential construction
and multifamily construction.

Each characteristic is standardized over the relevant complete-case sample.
The binary balance regression uses the stringent-side indicator, pair-average
stringency, separate distance slopes, boundary-segment fixed effects, and
construction-year fixed effects. The continuous regression replaces the side
indicator with the current locally centered score difference. Standard errors
are clustered by ward pair.

The audit does not condition on the September 2025 zoning group. That zoning
measure can postdate construction and is not a predetermined balance variable.
It also does not describe the block-group controls as predetermined. They are
the period-specific ACS measures already attached to the density file: the
2014 five-year estimates for the earlier period and the 2019 five-year
estimates for the later period. They are useful local continuity checks, but
they are not a fixed pre-2006 baseline.

## Findings

The multifamily binary sample is jointly balanced on both sets of observed
characteristics. The joint p-value is 0.549 for the five ward-level controls
used in the density regression and 0.403 for the eleven block-group
characteristics. None of the individual block-group differences is significant
at 5 percent. These tests use 881 multifamily projects for the ward controls and
827 for the block-group controls.

The continuous multifamily treatment is also jointly balanced on the ward-level
controls (p = 0.573), but the block-group joint test is marginal (p = 0.048).
Average household size is the only individual block-group characteristic that
is significant at 5 percent in that specification.

The all-construction sample is less reassuring. The joint tests reject balance
for both the binary treatment (p = 0.019 for ward controls and p = 0.006 for
block-group characteristics) and the continuous treatment (p < 0.001 for both
sets). The imbalance is spread across covariates rather than driven by one
large binary-side discontinuity. This is a real qualification for any broad
all-construction interpretation; it does not overturn the more relevant
multifamily binary comparison.

## System tests and spatial covariance

The SUR exercise estimates the same covariate equations as the individual
balance regressions and stacks their treatment-coefficient score contributions.
Because every equation has the same right-hand side, system estimation leaves
the point estimates unchanged. A production check verifies that every
ward-pair standard error also matches the corresponding individual regression
to numerical tolerance.

The primary system test uses the full cross-equation covariance matrix clustered
by ward pair and an $F$ reference distribution with the number of ward-pair
clusters minus one as the denominator degrees of freedom. This follows the
score-stacking logic of seemingly unrelated estimation while retaining the
paper's clustering level. The multifamily binary joint p-value is 0.507 for the
five ward controls and 0.154 for the eleven block-group characteristics. The
multifamily continuous p-values are 0.786 and 0.018, respectively.

Spatial-HAC sensitivity checks use exact project coordinates, Bartlett kernels,
and 1,000-foot, 2,640-foot, and 5,280-foot cutoffs. The multifamily binary
p-values range from 0.818 to 0.878 for the ward controls and from 0.844 to 0.924
for the block-group characteristics. The multifamily continuous block-group
p-values range from 0.147 to 0.275. Thus the earlier continuous block-group
rejection is not robust to allowing residual covariance among nearby projects.

Directional endpoint and shared-ward dyadic covariance matrices are also
computed. Several eleven-outcome matrices are not positive definite, so the
audit reports no joint Wald statistic for those cases rather than repairing the
matrix and presenting an arbitrary p-value. The shared-ward calculation follows
[Aronow, Samii, and Assenova
(2017)](https://arxiv.org/abs/1312.3398). The spatial covariance follows the
Conley HAC framework; reporting several cutoffs reflects the absence of a
uniquely determined spatial bandwidth.

## Outputs

- `density_covariate_balance.csv` contains raw side means and the binary and
  continuous standardized differences.
- `density_covariate_joint_tests.csv` contains the joint tests and sample sizes.
- `density_covariate_balance.tex` is a compact binary-specification table for
  possible later use in the appendix.
- `density_covariate_balance_report.pdf` shows every individual and joint
  balance result for both construction samples.
- `density_balance_sur_tests.csv` reports cross-equation Wald tests using
  ward-pair, endpoint, shared-ward dyadic, and spatial-HAC covariance matrices.
- `density_balance_sur_coefficients.csv` reports the corresponding individual
  coefficient standard errors.
