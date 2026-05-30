# process_rent_data

Purpose: Converts the Illinois Dewey/RentHub parquet export into the cleaned Chicago floorplan-month rental panel used by downstream RD tasks.

Inputs: `input/renthub_raw/*.parquet`.

Produces: `output/chicago_rent_panel.parquet`.

Default window: `2014-01-01` through `2022-12-31`.

Output unit: one `analysis_key x month_start` row. Ward, distance, segment, and alderman-score assignments happen downstream.

Approx. runtime: ~1-10 minutes.
