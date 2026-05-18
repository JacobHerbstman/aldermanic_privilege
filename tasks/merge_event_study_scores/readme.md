# merge_event_study_scores

Purpose: Merges event study scores.

Rental diagnostics:
- `output/rent_score_attrition_by_year_pair_side.csv` reports rental rows dropped by the score/sign merge by year, month, ward pair, and score side, including missing alderman and missing/tied score counts.

Produces: Produces files such as `output/block_treatment_panel.csv`, `output/rent_with_ward_distances_full.parquet`, `output/sales_with_ward_distances.csv` and related task-specific outputs in `output/`.

Approx. runtime: ~1-10 minutes.
