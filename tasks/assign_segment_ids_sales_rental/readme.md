# assign_segment_ids_sales_rental

Purpose: Assigns boundary-segment IDs to sales and RentHub records using the audited nearest-segment rule.

Outputs:
- `output/sales_pre_scores_with_segments.csv`
- `output/rent_pre_scores_full_with_segments.parquet`

The default segment-assignment radius is 457.2m, or 1500ft. The script hard-checks segment assignment consistency inside 500ft before writing the handoffs.

Approx. runtime: ~1-10 minutes.
