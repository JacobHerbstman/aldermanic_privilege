# rental_rd

Purpose: Estimates the 2014-2022 RentHub rental RD at ward borders.

Quality gates:
- `output/rd_sample_flag_balance.csv` reports RentHub cleaning/location flags by side of the RD cutoff, ward pair, and segment-month.
- `output/rd_segment_contract_audit.csv` verifies that 500ft rows have valid same-pair segment assignments and that segment-line distance matches the stored boundary distance.
- `output/rd_segment_month_flag_imbalance.csv` summarizes within-segment-month flag imbalance across strict and lenient sides.
- `output/rd_sample_definition_summary.csv` reports full and clean-location robustness sample sizes.

The rental RD should not be interpreted until questionable-location flags are reviewed for differential concentration near the cutoff and the clean-location robustness samples are compared with the full sample.

Produces: Flat no-slope rent discontinuity plots, coefficient CSVs, binned adjusted outcomes, quality-balance diagnostics, and sample diagnostics for the 500ft main bandwidth.
