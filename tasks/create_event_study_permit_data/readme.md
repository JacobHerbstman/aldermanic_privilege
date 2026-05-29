# create_event_study_permit_data

Purpose: Creates the permit panels used by permit event-study tasks.

Produces:
- `output/permit_block_year_panel.parquet`
- `output/permit_block_year_panel_2015.parquet`
- `output/permit_block_year_panel_2023.parquet`

Event-study regression diagnostics live in `tasks/audits/permit_event_study_audit`.

Approx. runtime: ~1-10 minutes.
