# Permit event-study audit

This task reproduces the 500ft permit event study and compares two binary
versions of the assigned 2015 score change.

- The joint directional model estimates separate effects for blocks assigned
  toward a more stringent alderman and blocks assigned toward a more lenient
  alderman, each relative to unchanged blocks. Its aggregate contrast is
  `(beta_stricter - beta_lenient) / 2`.
- The signed model codes stricter assignments as 1, lenient assignments as -1,
  and unchanged blocks as 0. It imposes equal and opposite directional effects.

Both models use application year, block fixed effects, ward-pair-by-year fixed
effects, the paper's two pre-period high-discretion permit controls, and
ward-pair-clustered standard errors. The sample covers event years -5 through 5
within 500ft of a boundary. Scores are estimated using permit data through
2014.

## Score variants

| Variant | Income | Bachelor's share |
| --- | --- | --- |
| `current_no_income` | No | No |
| `education_added_back` | No | Yes |
| `income_added_back` | Yes | No |
| `all_covariates` | Yes | Yes |

## Pooled years 0-5

Entries are log coefficients with ward-pair-clustered standard errors in
parentheses.

| Outcome and score | Joint contrast | Signed model |
| --- | ---: | ---: |
| High discretion: neither control | -0.066* (0.038) | -0.067* (0.038) |
| High discretion: bachelor's share only | -0.060 (0.039) | -0.061 (0.039) |
| High discretion: income only | -0.086** (0.037) | -0.087** (0.036) |
| High discretion: both controls | -0.086** (0.037) | -0.087** (0.036) |
| Low discretion: neither control | -0.034 (0.029) | -0.042 (0.033) |
| Low discretion: bachelor's share only | -0.036 (0.029) | -0.045 (0.033) |
| Low discretion: income only | -0.034 (0.029) | -0.043 (0.033) |
| Low discretion: both controls | -0.032 (0.029) | -0.042 (0.033) |

`* p < 0.10`, `** p < 0.05`, `*** p < 0.01`.

The high-discretion income-only and all-covariate assignments differ for only
2 of 2,630 reassigned blocks. Their pooled estimates are consequently
identical at the displayed precision. Omitting income reverses assignment for
162 to 197 reassigned blocks across five ward pairs, depending on whether
bachelor's share is retained.

The aggregate pretrend tests do not reject for any score variant. The smallest
high-discretion p-value is 0.174; all low-discretion p-values exceed 0.84.
For high-discretion permits, the directional symmetry test has p = 0.85 or
higher. For low-discretion permits it is marginal, with p-values from 0.074 to
0.081, so the unrestricted joint contrast is preferable for interpreting that
placebo.
