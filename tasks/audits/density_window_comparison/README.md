# Density differences across progressively wider boundary windows

Compare all eligible construction on the more-stringent side with all eligible
construction on the less-stringent side within 100, 200, 300, 400 and 500 feet.
The observation, outcome-specific eligibility, controls and fixed effects follow
`density_main_results`. This is a user-requested exploratory comparison, not a
replacement for the paper's nearest-band coefficient.

Each window estimates log density on a more-stringent-side indicator, the five
neighborhood controls, zoning-group, boundary-segment and construction-year fixed
effects. Standard errors are clustered by ward pair. Observations receive equal
weight. There are no distance-bin indicators or distance slopes: the coefficient
compares the whole two sides within the window. Controls are re-estimated in each
window. Thus the 100-foot estimate need not equal the paper's nearest-band
coefficient estimated with controls fitted on the full 500-foot sample.

Run `make` in `code/`. The estimate CSV contains 20 models, their confidence
intervals and fitted sample counts; the figure transforms log coefficients and
interval endpoints using 100*(exp(beta)-1). Inputs are ordinary production
symlinks. The manuscript and production estimates are unchanged.
