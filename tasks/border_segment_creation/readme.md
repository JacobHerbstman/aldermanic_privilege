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
- Legitimate disconnected short components remain standalone raw segments unless they fail segment-validity rules.
- Only true touching terminal remainders should be merged into adjacent same-pair analysis segments.
- Topology noise and short pieces that fail two-sided ward-offset checks should be flagged and excluded from analysis segment IDs.

Production segment artifacts include `valid_segment`, `invalid_reason`, `analysis_segment_id`, and `segment_lt500ft`/`segment_lt1000ft` fields. Shared assignment loaders keep valid analysis segments and use the length flags only for robustness samples.

Approx. runtime: ~1-10 minutes.
