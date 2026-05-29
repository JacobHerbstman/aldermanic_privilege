# RD placebo boundaries

This diagnostic task estimates the true 0ft border discontinuity and shifted placebo cutoffs inside each side of the same border. With the default 500ft bandwidth and cutoffs `-500,0,500`, the two placebo tests use observations from `[-1000,0]` and `[0,1000]`, so they do not compare observations across the actual ward boundary.

Rents use the clean-location listed-rent sample, segment-by-month fixed effects, hedonic controls, building-type controls, and amenity controls. Sales use the corrected sales panel, segment-by-quarter fixed effects, residential-improvement hedonics, and amenity controls. Both price outcomes are in 2022 dollars.

The score-gap split outputs divide ward pairs by the median pair-level absolute difference in alderman scores. The median is calculated across ward pairs in each dataset's complete 500ft true-boundary sample, not across rows, so high-volume rental buildings do not set the threshold.
