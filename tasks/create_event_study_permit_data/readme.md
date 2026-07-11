# create_event_study_permit_data

Purpose: Creates the permit panels used by permit event-study tasks.

Manual block decisions come from `tasks/permit_block_hand_adjudications/output/manual_permit_block_assignments.csv` and record reviewed decisions for permits that do not fall within the Chicago Census block polygons. The build fails if an unmatched permit lacks an explicit review.

Produces:
- `output/permit_block_year_panel.parquet`
- `output/permit_block_year_panel_2015.parquet`
- `output/permit_block_year_panel_2023.parquet`

Approx. runtime: ~1-10 minutes.
