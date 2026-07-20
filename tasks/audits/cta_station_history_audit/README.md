# CTA Station-History Audit

The production amenity task now starts from the current CTA station file, adds
opening dates for Oakton-Skokie, Morgan, Cermak-McCormick Place,
Washington/Wabash, and Damen-Lake, and restores the stations that permanently
closed during the analysis period.

This audit adds the three permanent station closures relevant to 2006--2022:

- Washington/State, closed October 23, 2006;
- Madison/Wabash, closed March 16, 2015; and
- Randolph/Wabash, closed September 3, 2017.

The opening dates come from CTA records for
[Oakton-Skokie](https://www.transitchicago.com/oakton-station-on-the-yellow-line-now-open/),
[Morgan](https://www.transitchicago.com/newsprojects/system-improvement-projects/completed-station-projects/),
[Cermak-McCormick Place](https://www.transitchicago.com/mayor-emanuel-announces-opening-of-new-cermak-cta-green-line-station/),
[Washington/Wabash](https://www.transitchicago.com/mayor-emanuel-opens-new-cta-train-station-at-washington-and-wabash/),
and
[Damen-Lake](https://www.transitchicago.com/mayor-brandon-johnson-joins-city-leaders-partners-for-grand-reopening-of-cta-green-line-station-at-damen-and-lake/).
The CTA's
[monthly station-entry data](https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Monthly-Day-Typ/t2rn-p8d7/about_data)
provide an independent check that there are no other permanent station openings
in the 2006--2022 analysis period.

For each rental listing and sale, the audit takes the minimum of the production
CTA distance and the distance to any of these stations that was still open in
the listing month or on the sale date. It then re-estimates the exact 500-foot
rental and sales models, changing no other control, fixed effect, sample
restriction, or clustering choice.

The closure dates come from CTA announcements for
[Washington/State](https://www.transitchicago.com/cta-red-line-washington-subway-station-to-temporarily-close-monday-october-23/),
[Madison/Wabash](https://www.transitchicago.com/reminder-madisonwabash-loop-elevated-station-closes-monday/),
and
[Randolph/Wabash](https://www.transitchicago.com/mayor-emanuel-opens-new-cta-train-station-at-washington-and-wabash/).
Coordinates are fixed at the former station locations reported by the
corresponding station records. The audit addresses permanent changes to the
rail network, not short service interruptions or temporary construction
closures.

The station-history correction has been incorporated into production. The
estimate comparisons and permit-count timing check remain audit-only.

## Findings

The correction changes no observation in either estimation sample. None of
the 240,389 rental observations or 51,224 sales observations is closer to one
of the restored stations than to the station used in production. The rental
estimate remains 0.01685 (SE 0.00829), and the sales estimate remains 0.01140
(SE 0.00780).

The historical omission therefore does not affect the paper's price estimates.
The production layer now supports describing the control as distance to the
permanent CTA station network in place during the month. It deliberately does
not treat short service interruptions as changes in neighborhood transit
access.

The correction changes the 800-meter station count for 8,784 of 224,219
permits used to estimate the stringency score. The corrected and original
scores remain nearly identical: their Pearson correlations are 0.99993 for
the score estimated through 2014 and 0.99998 for the score estimated through
2022. Four alderman pairs reverse order under the 2014 cutoff and five under
the 2022 cutoff, out of 3,403 and 6,328 possible pairwise comparisons. The
production permit file now exactly matches the application-date counts
reconstructed by this audit.

Re-estimating the paper's models with the old and corrected scores confirms
that the downstream changes are small. The multifamily continuous FAR
coefficient moves from -0.15306 to -0.15366, and the continuous DUPAC
coefficient moves from -0.10610 to -0.10687. All four binary density estimates
are unchanged. The pooled high-discretion permit coefficient moves from
-0.09960 to -0.09904, while the low-discretion coefficient moves from -0.04617
to -0.04455. The five alderman rankings that reverse through 2022 do not occur
in the main rent or sales samples, so their binary treatment assignments are
unchanged.
