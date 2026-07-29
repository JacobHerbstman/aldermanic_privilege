# Corrected permit event-study audit

This audit compares census-block permit event studies after assigning both the
origin and destination aldermen from June 2014. The stable specifications keep
only blocks whose origin and destination aldermen remain in office through June
2015. The assigned-change ITT specifications retain all blocks in the existing
2015 panel and use the 2014 assignment regardless of 2015 destination turnover.

Specifications compare issue and application timing, the paper's through-2022
score and a score estimated only through 2014, and a high-minus-low-discretion
triple difference. Every model is estimated with block and ward-pair clustering.
Pooled results report the full post-period and medium-run windows beginning in
years 2 and 3.

The application-date triple difference is excluded because its high-dimensional
PPML demeaning step did not converge after 10,000 iterations.

The audit also reports an exploratory linear 2SLS. Realized treatment is the
2015 destination alderman's through-2022 score minus the 2014 origin score. The
instrument is the corresponding destination-minus-origin difference using the
2014 alderman mapping on both sides. This IV is descriptive because its exclusion
restriction is not established.

At 500 feet, the audit also reports directional indicator and continuous-split
ITT models. Both include stricter and more-lenient treatments jointly, with
stayers as the omitted treatment group and ward-pair-clustered standard errors.
At 500 and 1,000 feet, the alder-influence outputs re-estimate the pooled
continuous-split model after dropping lenient movers associated with each 2014
origin or destination alderman. The Ward 2-42 raw-data figure reports annual
high-discretion permit outcomes for the blocks reassigned from Robert Fioretti
to Brendan Reilly and the nearby blocks represented by Reilly throughout. The
Ward 46-48 figure similarly reports blocks reassigned from Harry Osterman to
James Cappleman against stable blocks within that ward-pair sample.

The frozen-score specifications estimate the unchanged stringency-index model
using permits observed from January 2006 through December 2014. They then use
that score for the corrected assigned-change ITT without using post-reassignment
permit outcomes to construct treatment.

For the frozen assigned-change design, pooled continuous, directional
continuous, and directional indicator event studies are reported at both 500
and 1,000 feet.

Score-comparison outputs report alder-level Pearson and rank correlations,
treatment-direction switches, and leave-one-out diagnostics for the aldermen and
ward pairs that account for the change from the through-2022 coefficient to the
frozen-score coefficient.

The realized frozen-score specifications instead map the June 2015 destination
alderman and compare that score with the June 2014 origin alderman. Blocks
assigned to aldermen first entering office in 2015 are necessarily omitted
because those aldermen have no pre-2015 score.
