# Density treatment-timing audit

The density pipeline observes a construction year but not a construction or
entitlement date. Production assigns June 15 of that year, which determines
both the ward-map vintage and the alderman attached to each parcel.

This audit studies the observations for which that convention can matter. It
matches parcels in alderman-turnover ward-years, plus every 2015 parcel, to
new-construction permit projects. Permit records are grouped by PIN10 and
normalized address when both are available, and otherwise by whichever field is
available, with a new project after a two-year gap. Exact PIN10 matches take
priority only when the permit location is also spatially plausible. Otherwise,
the audit reports bounded spatial candidates and retains explicit ambiguity
flags for manual review. Permit application and issue dates are reported
separately; neither is labeled as a construction date.

Regular-election cutoffs use the third Monday in May, when aldermanic terms
begin under 65 ILCS 20/21-22. Off-cycle changes use the first month in which the
new alderman appears in the monthly alderman panel.

The audit also reproduces the main 500-foot density regressions and estimates
four sensitivity samples: dropping regular-election turnover ward-years,
dropping every turnover ward-year, dropping all 2015 construction, and dropping
both every turnover ward-year and all 2015 construction.

As an adverse treatment-assignment bound, the audit keeps the production
geography and full regression sample fixed but assigns April aldermen whenever
either ward along a boundary changes aldermen in a regular election. A second
version does this for every 2015 observation as well. When a ward is vacant in
April, the bound uses the last seated alderman before the vacancy. These are
fixed-sample sensitivity checks, not alternative construction-date estimates.

The candidate dates and robustness results are not connected to production or
either paper.
