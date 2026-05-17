# event_study_treatment_maps

Purpose: Builds redistricting treatment/control maps for the event-study support.

The maps use the permit block-year panel to select the 1000ft local-support block sample and use `block_treatment_panel.csv` for treatment status. Sales diagnostics live in the sales event-study task outputs.

Produces: files such as `output/treatment_control_map_2015_1000ft.pdf`, `output/treatment_control_maps_combined_1000ft.pdf`, and `output/ward_pair_vertical_13-23_1000ft.pdf`.

Approx. runtime: ~1-10 minutes.
