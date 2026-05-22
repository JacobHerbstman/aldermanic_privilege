# price_rd_stress_tests

Purpose: Diagnostic stress tests for the listed-rent and home-sales RDs. The task asks whether price gaps are larger on boundary segments where the realized multifamily supply first stage is strongest.

Inputs:
- `input/rental_rd_characteristics_panel_bw500.parquet`: listed-rent RD panel with audited geometry, real rents, hedonics, amenities, and location-quality flags.
- `input/sales_with_hedonics_amenities.parquet`: sales RD panel with audited geometry, real prices, hedonics, and amenities.
- `input/parcels_with_ward_distances.csv`: density/new-construction panel used to build segment-level supply first stages.
- `input/confounded_segment_flags.csv` and `input/confounded_pair_era_flags.csv`: existing pruned-boundary diagnostics.

Main outputs:
- `output/price_rd_supply_first_stage_segments.csv`: one row per boundary segment with the primary multifamily DUPAC first stage and secondary density/count gaps.
- `output/price_rd_stress_test_estimates.csv`: rent and sales RD estimates for supply-first-stage splits, interaction models, demand-pressure splits, old-stock checks, and pruned segments.
- `output/price_rd_supply_split_estimates.pdf`: visual comparison of rent and sales jumps on high- versus low-supply-constraint segments.
- `output/listed_supply_normalization_diagnostics.csv`: explicit feasibility check for normalizing listed-rental supply by segment-side stock.

Notes:
- This is diagnostics-only and is not wired into the paper.
- The primary first stage is based on 2006--2022 multifamily construction within 500ft. It is stored as a stricter-minus-lenient log(DUPAC) gap and as a sign-flipped `supply_constraint_dupac_mf`, where larger values mean lower realized multifamily density on the stricter side.
- The listed-supply normalization diagnostic currently does not create a normalized PPML series unless a reliable segment-side stock denominator exists in upstream data.
