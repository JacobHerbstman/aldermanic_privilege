# Density fixed effects

Exploratory comparison requested September 16, 2026. One script estimates the
full 2 spatial × 3 time × 2 zoning grid for FAR and DUPAC, separately for all
construction and multifamily buildings. This task does not change the adopted
specification, construction records, stringency scores or manuscript.

- **Segment:** the existing local boundary-segment fixed effects.
- **Border pair:** one fixed effect for each ward-number pair within each ward-map era.
- **Year:** the existing construction-calendar-year fixed effects.
- **Joint service:** one fixed effect for each uninterrupted overlap of the two
  serving aldermen, within the same ward-map era and ward pair. Reelection alone
  does not split a period. These effects already include border-pair effects.
- **None:** no time fixed effects.
- **Zoning:** include or omit the existing broad zoning-group effects.

Each specification starts with the identical common FAR/DUPAC sample: 2006–2022,
less than 500 feet from the boundary, unequal observed scores, recorded zoning
and segment, and complete values of the existing five demographic controls.
Those controls and ward-pair clustering are unchanged. Zoning remains required
for sample selection even when its controls are omitted. All fitted models retain
4,016 all-construction or 863 multifamily observations; none loses singletons.

The coefficient compares the 0–100-foot bin on the more-stringent side with
−100–0 feet, within the existing ten-bin specification. Percentages equal
100 × (exp(coefficient) − 1); confidence intervals use fixest's clustered inference.
This is not an average across the full 500 feet. Both baseline coefficients and
standard errors are checked against the existing post-correction results.

Daily term records determine joint service using the existing June 15 construction
date proxy. The code verifies each assignment against the aldermen already in the
analysis data. Period effects do not make a wrong construction year harmless:
a different year may change the politician, map, geographic assignment, zoning or
annual demographic covariates. The thirteen newly reviewed influential records
have not been corrected in this input. Model choices are exploratory, not selected
for statistical significance.

Run `make` from `code/`. For this comparison the four recorded upstream outputs
were held fixed so that no production data or earlier analyses were rebuilt:

```sh
make \
  -o ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv \
  -o ../../../create_alderman_data/output/chicago_alderman_terms.csv \
  -o ../../raw_log_score_sensitivity/output/density_all_results.csv \
  -o ../../raw_log_score_sensitivity/output/density_multifamily_results.csv
```

Analysis input SHA-256:
`d78388a3713ed8224cd94bbdf5413d038902a080deb1038ceff49ccc6f2edebf`.
Code baseline: `35135702` on `score_robustness`, with existing uncommitted audits
preserved. The CSV contains all 48 estimates; HTML and LaTeX display the same grid.
The data report is produced with SaveData and is not a Make target.
