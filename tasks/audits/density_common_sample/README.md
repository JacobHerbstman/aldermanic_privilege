# Common FAR and DUPAC sample

Compare the previous outcome-specific density samples with a common sample that
requires both FAR and DUPAC to be eligible, finite and positive. Jacob adopted
the common sample on September 11, preserving each regression's existing
distance restriction. This audit retains the before-and-after comparison.

The audit uses the current construction analysis data, through-2022 alderman
scores and recorded boundary characteristics. It reproduces the main 100-foot
comparison, the two placebo cutoffs, the two donut exclusions, the three
boundary-geometry restrictions, and the two minimum score gaps. Other controls,
fixed effects and clustering follow the corresponding production scripts.

Make in code/ writes density_common_sample_estimates.csv and its standard report.
The build log identifies the projects in the main DUPAC regression that would be
excluded by common eligibility. Score re-estimation after own-project exclusion
is not part of this comparison.
