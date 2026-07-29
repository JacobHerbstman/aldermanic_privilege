# New-construction event-study audit

This audit asks whether the 2015 ward remap changed completed residential
construction. It uses the same census-block treatment panel, 500-foot
block-centroid restriction, 2010--2020 window, frozen 2006--2014 alderman
scores, fixed effects, and ward-pair clustering as the permit event study.

The count outcome is the number of projects completed in each block and
construction year. The audit compares three specifications: controls for
the block's 2010--2014 construction count, the permit study's original
controls for 2010--2014 high-discretion permit volume, and no pre-period
controls. Each control and its zero-volume indicator are interacted with
calendar year.

The FAR and units-per-acre models are descriptive conditional-outcome
comparisons. They use the mean log density among projects completed in a
block-year. Because construction itself can respond to the remap, these
models do not identify the effect on the density of a fixed set of projects.
They also have much less within-block support than the count model.

The project year records completion rather than the application, design, or
entitlement decision. Effects may therefore appear with a lag, and projects
completed shortly after the remap were likely initiated earlier.

## Results

The permit-style sample contains 1,451 projects completed from 2010 through
2020 in 1,054 block-years. The fixed-effect Poisson models retain 688 blocks
in 66 ward pairs.

The pooled count estimate is sensitive to the pre-period adjustment:

| Controls | Log coefficient | SE | p-value |
| --- | ---: | ---: | ---: |
| Pre-2015 construction | -0.376 | 0.148 | 0.014 |
| Pre-2015 high-discretion permits | 0.043 | 0.194 | 0.824 |
| None | -0.093 | 0.195 | 0.634 |

None of the three count specifications rejects the joint pre-trend test. The
negative estimate with construction controls emerges mainly in years three
through five, but it is not present with the permit controls or without
pre-period controls.

Conditional density does not change detectably. The pooled estimates are
0.036 (SE 0.073) for log FAR and -0.033 (SE 0.083) for log units per acre.
The corresponding estimates remain small under the other control choices.
Their pre-trend tests reject strongly, so the conditional-density paths
should not be given a causal interpretation.

The count exercise is therefore useful as a robustness check but does not
provide stable additional evidence that the remap reduced completed
construction. The conditional-density event studies are not suitable for
the paper.
