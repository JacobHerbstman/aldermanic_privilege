# rezoning_alderman_event_study

## Purpose
Tests whether blocks reassigned to more stringent aldermen in 2015 subsequently
experience fewer or smaller rezonings. This uses the same 2015 block sample,
treatment, 1,000-foot bandwidth, fixed effects, and block clustering as the
paper's permit event study.

The rezoning data begin in November 2010, so the event window starts in 2011
(relative year -4) rather than treating the two observed months of 2010 as a
complete pre-period.

## Outcomes
- `any_rezoning`: probability that the block has any passed rezoning that year.
- `any_upzone`: probability of a classified upzoning. A rezoning year is missing
  when upzoning status cannot be determined.
- `far_change_total`: total classified FAR change in the block-year. A rezoning
  year is missing when any component is unclassified.
- `mean_far_change`: mean FAR change conditional on a fully classified rezoning
  block-year.
- `upzone_share`: share of matters that are upzonings conditional on a fully
  classified rezoning block-year.

The first three outcomes use block and ward-pair-by-year fixed effects. The two
conditional outcomes use ward-pair-by-year fixed effects because most blocks
have only one rezoning and therefore provide no within-block conditional-margin
variation.

## Outputs
- `output/rezoning_alderman_block_year_panel.parquet`
- One event-study PDF, coefficient CSV, and summary CSV per outcome.
- `output/rezoning_alderman_robustness_estimates.csv`: all event-time estimates
  across boundary bandwidths, the citywide sample, pre-rezoning history, and
  pre-2014 permit-demand controls.
- `output/rezoning_alderman_robustness_year0.pdf`: reassignment-year estimates
  across those specifications.
- `output/rezoning_alderman_pooled_did.csv` and `.pdf`: pooled post-2015 effects
  using the full 2011--2014 pre-period rather than only 2014 as the reference.
- `output/rezoning_alderman_upzone_bounds.csv`: sensitivity that codes every
  unresolved rezoning block-year as either no upzoning or an upzoning.

This is an audit task until the outcome definitions and conditional-margin
specification are accepted.

Parallel `pre2015` event studies use the unchanged stringency-index model
estimated only on permits from January 2006 through December 2014. These frozen
scores are mapped to the June 2014 origin and destination aldermen on both sides
of each ward reassignment.

## Preliminary Findings
- The 1,000-foot sample contains 14,438 blocks and 1,780 block-years with a
  rezoning. The pre-2015 probability of any rezoning is 0.0115 per block-year.
- The reassignment-year probability of any rezoning is negative at every
  boundary bandwidth, but this is largely a 2014 normalization result. Pooled
  DiD estimates using the full 2011--2014 pre-period range from -0.0005 to
  0.0003 across 500 feet through the full 800-meter panel and are never
  statistically distinguishable from zero. Omitting the 2015 transition year
  yields estimates from 0.0001 to 0.0008.
- Pre-2014 rezoning and permit-demand controls have almost no effect on the
  estimates. Only 797 of 36,044 citywide blocks had a rezoning in 2011--2013,
  so rezoning-history controls are sparse.
- The pooled boundary estimates for any classified upzoning are small and
  positive rather than negative. Coding all unresolved rezoning years as either
  zero or one upzoning produces estimates between 0.0001 and 0.0011, with every
  confidence interval containing zero.
- Total classified FAR change is weakly positive in the controlled boundary
  specifications. At 1,000 feet and wider, pooled estimates are about 0.0013 to
  0.0019 and several are statistically significant. This is opposite the
  prediction that stricter aldermen grant smaller zoning increases.
- The citywide probability of any rezoning is negative, but the citywide event
  study strongly rejects parallel pretrends. It is less credible than the local
  boundary comparisons.
- Only 1,274 rezoning block-years in the 1,000-foot sample have fully classified
  FAR transitions. Conditional FAR-change and upzoning-share estimates are
  imprecise and do not show that stricter aldermen grant smaller rezonings.

A true pre-2015 zoned-FAR control is not available. The project's polygon zoning
snapshot is from 2025, so using it as baseline FAR would introduce post-treatment
information. The audit therefore uses only outcomes and permit activity observed
through 2013 as predetermined controls.
