# Annual and pooled price-per-square-foot trimming

This is an audit, not production cleaning. It does not change the paper sample.
The approved baseline has no additional upper-tail price trimming; annual
trimming is retained only as a sensitivity check. The rooms rule lives in the
existing `prep_sales_border_data` producer, not a separate cleaning task.
Run `make` from `code/`. Inputs reuse the existing sales-quality audit, final
property-data pipeline, and frozen county sales and improvement files.

## Definitions

The reference population is all 233,699 Chicago sales remaining after the
approved bedrooms-greater-than-rooms exclusion, before boundary proximity,
stringency-score availability, or regression-control restrictions. It retains
the project's existing residential classes and transaction restrictions; it
is not the universe of all raw Chicago deeds.

Price per square foot is the existing monthly-CPI-adjusted sale price in 2022
dollars divided by sale-year recorded building area. The pooled rule uses
the 99.9th percentile of this whole population. The annual rule computes that
percentile separately within sale year. R's type-7 quantile is used, and only
values strictly above the cutoff are flagged. Ties at the cutoff are retained.
No prices are capped, no lower tail is removed, and no later property
characteristics are substituted. Integer sample sizes mean the annual rule
flags 243 rather than exactly 233.699 sales.

Neither rule conditions on ward, boundary side, bandwidth, or the estimated
price discontinuity. The same flagged row IDs are applied at 500, 1,000, and
1,500 feet. These regressions preserve 100-foot bins, property-class fixed
effects, segment-by-quarter fixed effects, the existing property and amenity
controls, and ward-pair clustering. The untrimmed 500-foot coefficient, standard
error, and sample size are checked against the production result.

## Results checked September 3, 2026

| Rule | Citywide exclusions | 500-foot exclusions | 500-foot coefficient | Standard error |
|---|---:|---:|---:|---:|
| No price trimming | 0 | 0 | 0.029961 | 0.013388 |
| Pooled real-price percentile | 234 | 65 | 0.029009 | 0.013400 |
| Annual real-price percentile | 243 | 68 | 0.029244 | 0.013363 |

The annual and pooled rules agree on 201 exclusions; 42 are annual-only and
33 pooled-only. Their union contains 276 transactions. In the preferred RD
sample, 58 are excluded by both, ten only annually, and seven only by the pooled
rule. Annual exclusions are 33 on the more-stringent side and 35 on the
less-stringent side. These counts do not establish that trimming is innocuous
for identification.

The annual cutoffs range from $1,119 to $1,553 in 2022 dollars, compared with
$1,240 for the pooled rule. For example, annual trimming removes 26 rather
than 43 sales in 2006, but 15 rather than seven in 2017. Nominal and real annual
ranking disagree on six row IDs, because the CPI
adjustment varies within year. All 24 sales above $2,000 nominal per square
foot are caught by both rules.

The annual exclusions are not predominantly giant houses: 135 of 243 are
classes 202 or 203. Among the 68 main-RD exclusions, median nominal price is
$1,774,950 and median recorded area is 1,247 square feet.

## What the record review establishes

All 277 union cases have been matched back to their original sale rows and
sale-year property cards. Every flagged price equals the raw recorded price,
and every sale-year area equals the sole source card's area. Thus the extreme
ratio was not introduced by these joins. This does not authenticate the deed
consideration or prove that the source description matched the sold building.

Across the 243 annual exclusions, 105 have a subsequent single-card,
non-prorated record with **more than** 50% greater area. Sixty first cross
that threshold in the next tax year. In 87 of the 105 cases, that later
record's construction year is no later than the sale year (19 equal the sale
year). This is consistent with delayed updates in some cases, not proof that
all 106 sale-year records were wrong. Later renovations, reassessments, and
changed measurement can also explain area changes. These histories end in
2022, and 22 flagged annual sales have no later comparable record at all.

I read complete available sale and improvement histories for 24 reproducibly
sampled transactions: four from each combination of annual-only, pooled-only,
or both, and inside or outside the main RD sample. This deliberately oversamples
disagreement cases, so it cannot estimate the share of invalid transactions.
The case-by-case observations are in `review_notes.md`; the selected row IDs
are marked `sampled_for_reading` in `output/tail_review.csv`. I also inspected
the summary evidence for all 68 annual main-RD exclusions.

The sample includes severe unresolved price anomalies, likely delayed or
inconsistent building descriptions, plausible high-value repeat sales, and
possible redevelopment purchases. Therefore annual trimming is useful as a
transparent sensitivity check, but is not a verified-error filter and does
not eliminate the need to distinguish source mistakes from real high prices.
The annual rule also retains some suspicious early-year cases removed by the
pooled rule. No original deeds, permit histories, or listing photographs were
obtained for this review, and no claim of verified transaction validity is made.

## Outputs

- `output/tail_review.csv`: all 276 cases, rule membership, prices, cutoffs,
  characteristics, RD eligibility, and compact historical evidence. Filter
  `flag_annual == TRUE` for the 243 annual exclusions.
- `output/tail_cutoffs.csv`: yearly reference counts, real and nominal cutoffs,
  exclusions, and differences caused by monthly inflation adjustment.
- `output/tail_counts.csv`: pooled/annual overlap and preferred-RD side counts.
- `output/tail_sale_history.csv`: all available same-PIN raw transactions,
  including transactions excluded from production. Repeat prices alone do not
  establish arms-length status. Buyer/seller names are local review material.
- `output/tail_improvement_history.csv`: complete available same-PIN source
  cards, including multicard and prorated years not used in the compact
  comparable-area summary.
- `output/tail_rd_bw500.csv`, `tail_rd_bw1000.csv`, `tail_rd_bw1500.csv`:
  sensitivity results with the same citywide exclusion IDs at every bandwidth.

`AREA_CHANGE_RATIO=1.5` is a descriptive history-review threshold only, not a
sales exclusion. `REVIEW_SEED=42` and `REVIEW_PER_GROUP=4` reproduce the reading
sample. All substantive trimming choices are exposed in the task Makefile.
