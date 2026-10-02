# Density with construction and scores through 2026 (exploratory)

Nothing here feeds the paper or slides. Run `make` in `code/` (about 1.5 hours, mostly `measure_buildings.R` and
`build_construction_buildings.R`). Each script is a copy of the production script named in its header with the
changes listed there:

- Geography: `build_ward_panel_through_2026.R` adds the 2023 ward map for 2024-2026; `build_boundary_segments.R`
  builds ward-pair boundaries and 1,320 ft segments with the post-2023 era (the 2003 and 2015 maps reproduce
  `tasks/border_segment_creation` exactly: 1,142 and 1,293 segments, same ids, positions and validity; the 2023 map
  has 1,239 segments in 121 ward pairs); `create_alderman_panel.R` extends the terms and monthly panel to June 2026
  (identical to production in all 15,000 ward-months through 2022).
- Construction: `select_permits.R` (permits issued through 2026), `measure_buildings.R` and `construction_rules.R`
  (unchanged), `build_construction_buildings.R` (Assessor year built through 2026), `build_ledger.R` (2023-map
  boundaries). The hand decisions are the production file, which covers 2006-2022 cases only.
- `build_ward_controls_2023_map.R`: 2022 ACS block groups allocated to the 2023-map wards with the production method,
  which first reproduces the production 2022 controls on the 2015 map.
- `build_new_construction_analysis_data.R`: the analysis data with the score through June 2026 without
  self-certification permits (`tasks/audits/scores_through_2026`).
- `density_results_through_2026.R`: the paper's density design by period, data version and score.

Rents through April 2026 (1,179 RentHub files on Dewey) are not built yet.

## Findings (October 2, 2026)

- **The extended data reproduce the paper's.** 16,334 of the paper's 16,361 buildings are in the extended ledger, all
  with the same units per acre, and with the published score the 2006-2022 estimates are the paper's (multifamily
  -0.061, 5+ units -0.100).
- **The Assessor has not yet recorded most recent buildings.** Permitted buildings found in the Assessor's records:
  88-93 percent for 2018-2022 permits, 81 percent for 2023, 73 for 2024, 51 for 2025 and 3 for 2026. The density sample
  gains 346 buildings from 2023-2026 (163 multifamily, 32 with 5+ units), too few for the 2023-2026 period alone:
  within segment-by-joint-service groups its estimates are not identified.
- **Adding the new buildings changes nothing; the score decides the result.** Multifamily, average difference within
  500 ft, 2006-2026: -0.060 (0.021) with the published 2006-2022 score, -0.005 (0.025) with the score through 2026
  without self-certification permits (2006-2022 alone: -0.061 and +0.001). 5+ units: -0.100 (0.058) and -0.011
  (0.054).
