# process_rent_data

Purpose: Processes the Illinois RentHub download into a cleaned Chicago property/floorplan-month panel for the 2014-2022 rental RD.

The raw Dewey export is Illinois-wide. This task filters to `CITY == "CHICAGO"` for initial loading, cleans address and building-type fields, builds transparent property and floorplan fingerprints, collapses repeated daily scrapes, and then writes one active rent observation per floorplan-month.

`output/chicago_rent_panel.parquet` is the main monthly panel. Same-day multi-rent floorplan groups are retained in the main sample and summarized with diagnostics because they often reflect large multifamily buildings rather than duplicate rows. `output/chicago_rent_panel_drop_multi_rent.parquet` is the conservative robustness panel that drops those days before monthly aggregation. `output/chicago_rent_episode_robustness.parquet` contains episode-start robustness panels for 30, 45, 60, and 90 day gap rules using episode median rent.

The diagnostics report raw-to-clean coverage, building-type recodes, property-day/floorplan-day/floorplan-month repetition patterns, redacted collision examples, city filtering, monthly rent trends, and drop reasons.

Approx. runtime: ~1-10 minutes.
