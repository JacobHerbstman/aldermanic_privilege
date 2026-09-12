# Density differences across progressively wider boundary windows

Compare all eligible construction on the more-stringent side with all eligible
construction on the less-stringent side within 100, 200, 300, 400 and 500 feet.
The observation, common FAR/DUPAC eligibility, controls and fixed effects follow
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

The requested levels extension uses the identical fitted samples and controls but
regresses FAR or DUPAC directly, without logs. The `levels` estimate and plot
outputs report FAR units and dwelling units per acre; no winsorization is added.
The log output names are retained. Both scales are built by the default Make
entry point; the estimation script accepts `log` or `levels`, and the plotting
script additionally accepts `pdf` or `png`.

The estimation script also accepts `all` or `straight` for the boundary rule.
The straight-boundary log comparison uses the same recorded local-straightness
classification as the paper's boundary checks, and writes a separate 20-model
CSV and report. This does not alter the manuscript or its estimates.
