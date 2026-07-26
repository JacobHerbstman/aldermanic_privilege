# Income and education controls in the stringency score

This audit compares four otherwise identical stringency scores: omitting both median household income and bachelor's-degree share, adding back either control separately, and including both. It rebuilds the through-2014 and through-2022 scores from the current permit input, verifies that the full-control rebuild matches production, and re-estimates the density, permit, rental, and sales specifications. It does not modify production data, figures, tables, or manuscript text.

## Results

The full-control and omit-both scores are highly correlated: 0.995 through
2014 and 0.986 through 2022. The full-control score is reproduced exactly
before any outcome model is estimated.

| Current paper specification | Estimate (SE) |
| --- | ---: |
| Multifamily FAR, continuous | -0.140 (0.081) |
| Multifamily DUPAC, continuous | -0.158 (0.079) |
| Multifamily FAR, binary | -0.278 (0.103) |
| Multifamily DUPAC, binary | -0.247 (0.092) |
| High-discretion permit ITT | -0.105 (0.032) |
| Low-discretion permit ITT | -0.043 (0.031) |
| Listed rent, continuous | 0.0229 (0.0063) |
| Home sale price, continuous | 0.0171 (0.0072) |
| Listed rent, binary | 0.0285 (0.0082) |
| Home sale price, binary | 0.0199 (0.0078) |

All four multifamily density estimates remain negative across the four score
constructions. Under the full-control score, continuous DUPAC and both binary
estimates are significant at 5 percent; continuous FAR has p = 0.085.
All-construction estimates remain small. The high-discretion permit estimate
and low-discretion placebo barely move across score constructions. Continuous
rent and sales estimates remain positive and statistically significant.

The continuous rent and sales specifications replace the binary side indicator with the property's alderman score relative to the two-alderman boundary midpoint and control for the pair-average score. Their coefficients are per standard-deviation increase in relative stringency.

## Drivers

For the comparison that restores income while continuing to omit bachelor's-degree share, the largest through-2022 score changes are Daniel La Spata (+0.261 standard deviations), Scott Waguespack (+0.237), Susan Sadlowski Garza (-0.235), Matthew O'Shea (+0.234), William Banks (-0.198), Michele Smith (+0.197), and Patrick Daley Thompson (-0.195).

The sales coefficient changes only because ten close alderman comparisons reverse their more-stringent side. An exact Shapley decomposition assigns 41 percent of the increase to the Moreno--Waguespack boundary, 22 percent to Brookins--O'Shea, 16 percent to Austin--O'Shea, 13 percent to Pawar--Waguespack, and 12 percent to Arena--Laurino. Other reversing boundaries partly offset these increases. Thus boundaries involving Waguespack account for 54 percent of the coefficient change and boundaries involving O'Shea account for 38 percent.

For multifamily density, leave-one-pair-out comparisons identify Moreno--Waguespack as the largest source of FAR attenuation. Removing that pair reduces the binary FAR change from 0.058 to 0.016 and the continuous FAR change from 0.085 to 0.039. Sigcho-Lopez--Thompson and Solis--Thompson are the next most influential binary FAR comparisons. These leave-one-pair-out effects are sensitivity measures and are not additive decompositions.

The detailed alderman movements, exact sales decomposition, and density leave-one-pair-out results are in `current_income_score_movements.csv`, `current_income_sales_pair_drivers.csv`, and `current_income_density_pair_drivers.csv`.

## Continuous price robustness

The score bootstrap resamples 2,000 alderman-by-ward-by-month histories within alderman and refits both score stages, empirical-Bayes shrinkage, and standardization. Each draw is propagated through the boundary-centered continuous rent and sales models, including a draw-specific relative score and pair-average score. The final full-control results use the production-score draws from `generated_score_uncertainty`; the income-only score is retained as a sensitivity comparison.

| Outcome and score | Estimate | Clustered SE | Score-bootstrap SD | Score-only 95% range | Positive draws |
| --- | ---: | ---: | ---: | ---: | ---: |
| Rent, full controls | 0.0232 | 0.0070 | 0.0045 | [0.0146, 0.0322] | 100% |
| Rent, income only | 0.0219 | 0.0067 | 0.0041 | [0.0145, 0.0309] | 100% |
| Sales, full controls | 0.0174 | 0.0072 | 0.0031 | [0.0096, 0.0218] | 100% |
| Sales, income only | 0.0171 | 0.0071 | 0.0030 | [0.0096, 0.0216] | 100% |

Score uncertainty is 61--65 percent as large as the clustered rent standard error and 42--43 percent as large as the clustered sales standard error. These distributions vary the generated score while holding the outcome samples fixed, so they are not conventional confidence intervals.

The leave-one-boundary analysis removes each of 140 ward pairs and re-estimates the full model with segment-clustered standard errors.

| Outcome and score | Full estimate | Leave-one-pair range | Largest change | Significant at 5% |
| --- | ---: | ---: | ---: | ---: |
| Rent, full controls | 0.0232 | [0.0174, 0.0274] | 0.0058 | 140/140 |
| Rent, income only | 0.0219 | [0.0164, 0.0270] | 0.0055 | 140/140 |
| Sales, full controls | 0.0174 | [0.0144, 0.0194] | 0.0030 | 140/140 |
| Sales, income only | 0.0171 | [0.0141, 0.0191] | 0.0030 | 140/140 |

No omission reverses either coefficient. Under the full-control score, rent is most sensitive to ward pair 4--25 and sales to ward pair 16--20. Complete results are in `continuous_price_score_uncertainty_summary_2000draws.csv` and `continuous_price_leave_pair_out.csv`.
