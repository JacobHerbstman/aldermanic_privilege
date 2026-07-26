# ACS structure-age validation

This audit resolves the small-multifamily coverage gap identified in
`tasks/audits/census_bps_coverage_validation`. It does not alter production data
or paper results.

That audit found 2-4 unit buildings covered at roughly 41% of Census Building
Permits Survey authorizations while 5+ unit buildings were covered at 74%, and
left three explanations undistinguished: authorized small buildings never built,
BPS overcounting authorizations, or the ledger genuinely missing them.

American Community Survey table B25127 separates these, because it counts units
that **exist and are occupied** rather than units that were authorized.

## Design

Three independent measures of Chicago housing production in the 2010s:

| Source | Counts | Vintage |
| --- | --- | --- |
| BPS | units authorized by permit | 2009-2018, lagged one year to the completion window |
| ACS B25127 | occupied units reporting built 2010 or later | 2015-2019 five-year |
| Ledger | units completed and recorded by the Assessor | construction year 2010-2019 |

The 2015-2019 ACS vintage is required because it reports "Built 2010 or later" as
its own category; later vintages collapse 2000-2019 into one bin. The download
asserts this rather than trusting the variable numbers.

Two features make ACS a **lower bound** on units built: the universe is occupied
units, so vacant new units are excluded, and a five-year estimate averages over
2015-2019, so units finished late in the decade are under-represented. Any gap
measured against ACS is therefore conservative.

## Result

| Structure size | BPS authorized | ACS occupied | Ledger completed | ACS ÷ BPS | Ledger ÷ BPS | Ledger ÷ ACS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 unit | 3,888 | 5,013 | 5,239 | 1.29 | 1.35 | **1.05** |
| **2-4 units** | 3,391 | 3,254 | 1,272 | **0.96** | 0.38 | **0.39** |
| 5+ units | 39,468 | 18,887 | 39,845 | 0.48 | **1.01** | 2.11 |
| All | 46,747 | 27,154 | 46,356 | 0.58 | **0.99** | 1.71 |

Each bucket has a different and internally consistent explanation.

**2-4 units: the ledger undercounts.** ACS finds 3,254 occupied units in 2-4 unit
structures built in the 2010s against 3,391 authorized, so 96% of authorized
units exist and are lived in. The ledger records 1,272. Neither abandonment nor
BPS overcounting can explain this: the units are standing and occupied. About
2,000 units of small multifamily housing built in the 2010s are absent from the
ledger's 2-4 unit bucket.

**5+ units: the ledger is right and ACS is biased.** The ledger matches BPS to
within 1%. ACS is 48% of BPS here because large buildings lease up slowly and
because the 2015-2019 average under-represents the late-decade tower boom, which
is exactly where ACS's two known biases bite hardest.

**1 unit: BPS undercounts.** The ledger and ACS agree to within 5%, and both
exceed BPS by about a third. BPS is known to miss small-builder single-family
permits. Here the ledger is corroborated by the independent source.

**The aggregate agreement is a coincidence.** Ledger and BPS totals match to
within 1%, but this is BPS undercounting single-family by roughly 1,350 units
offsetting the ledger undercounting 2-4 unit housing by roughly 2,100. ACS
identifies which error belongs to which source. An aggregate coverage statistic
should not be reported without the composition table.

## Where the missing 2-4 unit housing is

The companion BPS audit attributes part of the shortfall to project aggregation:
a site of three 3-unit buildings is booked at nine units and lands in the 5+
bucket. That is consistent with the ledger's 5+ count exceeding BPS by 377 units
here. It does not account for the full 2,100.

The ledger's 1-unit count cannot absorb the remainder either, because ACS
independently validates it to within 226 units.

So most of the 2-4 unit shortfall is not misfiled inside the ledger; it is
missing from it. The residual candidates, both consistent with the eligibility
audit's findings, are that a new two- to four-flat replacing an older building on
the same lot retains a pre-2006 assessor year built and falls outside the
new-construction universe, or that its dwelling count is recorded in a way that
places it elsewhere. Neither is testable from records that omit the project.

## Implication for the paper

The paper's multifamily sample is concentrated in the 2-6 unit range, where two
independent external sources agree that the ledger captures roughly 40% of units
built. This does not invalidate the boundary design, which requires missingness
to be balanced across ward boundaries rather than complete, and the companion
attrition test finds no imbalance with respect to relative stringency. It does
mean the sample must not be described as a census of small multifamily
construction, and that the multifamily estimates are identified from a minority
of the small buildings actually built.

## Outputs

- `acs_b25127_chicago_built_2010_or_later.csv` — ACS cells with margins of error
- `acs_ledger_bps_comparison.csv` — three-way comparison by structure size
- `acs_ledger_bps_summary.csv` — comparison with totals and a per-bucket verdict
