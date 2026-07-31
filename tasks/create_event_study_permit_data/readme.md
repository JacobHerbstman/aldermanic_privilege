# Permit Event-Study Data

This task creates the 2010--2020 Census-block-year permit panel used for the
2015 ward-remap event study.

The hand-reviewed spreadsheet
`tasks/permit_block_hand_adjudications/output/manual_permit_block_assignments.csv`
records the decision for every permit coordinate that falls outside the
Chicago Census block polygons. The build stops if any such permit lacks a
reviewed decision.

The output is `output/permit_block_year_panel_2015.parquet`.
