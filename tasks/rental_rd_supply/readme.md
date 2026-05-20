# rental_rd_supply

Purpose: Checks whether the RentHub rental RD rent gap is accompanied by differences in advertised rental supply on the stricter side of ward boundaries.

The task treats supply as online RentHub availability, not a count of true vacant housing units. It reports three count definitions:
- `floorplan_month`: cleaned RentHub floorplan-month observations, matching the main rental RD unit.
- `property_month`: distinct RentHub property proxies in a segment-month-side cell.
- `address_stem_month`: distinct nonmissing address stems in a segment-month-side cell.

Outputs:
- `output/rental_rd_supply_side_panel_bw500.parquet`: segment-month-side count panel with zero side cells filled in.
- `output/rental_rd_supply_side_means_bw500.csv` and `.pdf`: raw mean side counts by sample and count definition.
- `output/rental_rd_supply_ppml_bw500.csv` and `.pdf`: stricter-side PPML count gaps with segment-by-month fixed effects.
- `output/rental_rd_supply_bins_bw500.csv` and `.pdf`: binned descriptive floorplan-month availability by distance to the boundary.
- `output/rental_rd_supply_levels_*_bw500.pdf`: main-style binned supply plots for the full, clean-location, pruned-segment, and clean-location-plus-pruned samples.
- `output/rental_rd_supply_sample_summary_bw500.csv`: sample sizes, zero-cell shares, and single-sided segment-month shares.

Robustness samples include the full sample, the clean-location sample, the pruned-segment sample that drops segments flagged by the confound-pruning task, and their intersection.

This task replaces the old rental listing-count diagnostics for the current cleaned RentHub panel and audited rental geometry.
