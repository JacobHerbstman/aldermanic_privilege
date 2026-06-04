# create_event_study_permit_data

Purpose: Creates the permit panels used by permit event-study tasks.

Manual block assignments: `input/manual_permit_block_assignments.csv` records reviewed block assignments for permits that fall just outside the Census block polygons.

Produces:
- `output/permit_block_year_panel.parquet`
- `output/permit_block_year_panel_2015.parquet`
- `output/permit_block_year_panel_2023.parquet`

Approx. runtime: ~1-10 minutes.
