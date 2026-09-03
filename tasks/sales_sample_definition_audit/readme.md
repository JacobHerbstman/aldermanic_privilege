# Sales sample definition audit

This task compares six transaction-cleaning rules and records the evidence used to choose the canonical production rule.

- **Legacy names-required:** Warranty or Trustee deed, valid non-identical buyer and seller names.
- **Warranty/Trustee, names optional:** the production deed restriction, but missing party names are allowed and normalized same-party transactions are excluded.
- **Production current:** the preceding sample with Cook County's three published quality flags required to be false.
- **Official flags, inclusive:** Cook County's three published quality flags must be false; there is no deed or party-name restriction.
- **Official flags, exclude nonmarket labels:** the inclusive sample excluding clearly nonmarket MyDec labels and normalized same-party transactions.
- **Official flags, market deed types:** Warranty, Trustee, Special Warranty, or Limited Warranty deeds passing the official flags, with normalized same-party transactions excluded.

All samples also require a 2006--2022 sale, residential class 202--211, 234, 278, or 295, price above $10,000, a single parcel, and no explicit `LAND` sale type. Class 212 mixed-use properties, cooperatives, condominiums, accessory land, and temporary residential-improvement classes remain outside this comparable improved-residential baseline.

The canonical transaction sample is the Warranty/Trustee, official-flags, names-optional rule. The comparable analysis panel additionally requires a sale-year improvement record for one building, no PIN-level proration, positive building square footage, and no reported bedrooms-greater-than-rooms contradiction. Missing apartment counts remain eligible and no upper-tail price trimming is applied. Same-year new construction remains in the panel; a logged-age specification may drop those observations rather than altering their age.

Before property matching, production excludes an unrefined sale recorded on
the first of the month when an adjoining ward changes aldermen or the ward map
changes during that month. The sample-flow output reports this date rule as a
separate step.

The audit reports citywide composition and hedonic-price diagnostics, downloads historical coordinates for the union sample, and estimates a common 500-foot local-linear boundary specification with segment-by-year-quarter fixed effects and ward-pair clustered standard errors.

The sample-flow output includes the approved rooms rule and validates the final
count against the paper's binned RD. Historical transaction-rule comparisons
use the unfiltered replay from `sales_record_quality_audit`; those local-linear
comparisons are not the paper's preferred estimates.
