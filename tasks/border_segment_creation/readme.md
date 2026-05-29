# border_segment_creation

Purpose: Builds canonical ward-pair boundary lines and raw boundary segments.

Produces:
- `output/boundary_segments_1320ft.gpkg`
- `output/segment_classification.csv`
- `output/ward_pair_boundaries.gpkg`

Segment rule:
- Raw segments keep full ward-pair boundary coverage.
- Long connected boundary components are split into roughly 1320ft and 2640ft pieces.
- Short disconnected components are not automatically errors. Ward pairs can share multiple disconnected line components, including real short two-sided boundary pieces near complex ward topology.
- Length alone is not a production exclusion rule.
- Legitimate disconnected short components should remain standalone raw segments unless the segment validity audit flags them.
- Only true touching terminal remainders should be merged into adjacent same-pair analysis segments.
- Topology noise and short pieces that fail two-sided ward-offset checks should be flagged and excluded from analysis segment IDs.

Econometric segment IDs are audited downstream in `tasks/audits/segment_validity_audit`, which creates `raw_segment_id` to `analysis_segment_id` mappings and support diagnostics.

Production segment artifacts include `valid_segment`, `invalid_reason`, `analysis_segment_id`, and `segment_lt500ft`/`segment_lt1000ft` fields. Shared assignment loaders filter `valid_segment == FALSE`, so the two audited artifacts (`9_21_1998_2002_6` and `43_44_post_2023_7`) remain visible in the raw GPKG but are dropped from econometric assignment. The length flags are for robustness checks that drop segments shorter than 500ft or 1000ft; they are not part of the main sample rule.

Approx. runtime: ~1-10 minutes.
