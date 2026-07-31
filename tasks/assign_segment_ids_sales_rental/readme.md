# Assign Rent and Sales to Boundary Segments

This task assigns each sale and RentHub observation to the nearest valid
boundary segment for its ward pair.

It writes:
- `output/sales_pre_scores_with_segments.csv`
- `output/rent_pre_scores_full_with_segments.parquet`

The default search radius is 1,500 feet. Before writing either file, the script
verifies the assignments for every observation within the 500-foot main
sample.
