# Sales sample definition audit

This task compares six transaction-cleaning rules without changing the production sales pipeline.

- **Production current:** Warranty or Trustee deed, valid non-identical buyer and seller names.
- **Warranty/Trustee, names optional:** the production deed restriction, but missing party names are allowed and normalized same-party transactions are excluded.
- **Warranty/Trustee, official flags, names optional:** the preceding sample with Cook County's three published quality flags required to be false.
- **Official flags, inclusive:** Cook County's three published quality flags must be false; there is no deed or party-name restriction.
- **Official flags, exclude nonmarket labels:** the inclusive sample excluding clearly nonmarket MyDec labels and normalized same-party transactions.
- **Official flags, market deed types:** Warranty, Trustee, Special Warranty, or Limited Warranty deeds passing the official flags, with normalized same-party transactions excluded.

All samples also require a 2006--2022 sale, residential class 202--211, price above $10,000, a single parcel, and no explicit `LAND` sale type.

The audit reports citywide composition and hedonic-price diagnostics, downloads historical coordinates for the union sample, and estimates a common 500-foot local-linear boundary specification with segment-by-year-quarter fixed effects and ward-pair clustered standard errors.
