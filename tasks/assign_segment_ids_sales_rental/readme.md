# assign_segment_ids_sales_rental

Purpose: Assigns boundary-segment IDs to sales and RentHub records using the audited nearest-segment rule.

Outputs:
- `output/sales_pre_scores_with_segments.csv`
- `output/rent_pre_scores_full_with_segments.parquet`
- `output/segment_assignment_coverage_summary.csv`
- `output/segment_assignment_spotcheck_queue.csv`
- `output/segment_assignment_reason_summary.csv`

The default segment-assignment radius is 305m, just over 1000ft. That keeps the 500ft rental RD and 1000ft event-study/bandwidth diagnostics inside the assignment radius while avoiding unrestricted global segment assignment.

Approx. runtime: ~1-10 minutes.
