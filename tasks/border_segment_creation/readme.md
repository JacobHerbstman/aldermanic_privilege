# Ward-Boundary Segments

This task creates one set of shared ward-pair boundary lines and divides them
into the segments used by the rent and sales regressions.

Produces:
- `output/boundary_segments_1320ft.gpkg`
- `output/ward_pair_boundaries.gpkg`

The segment rules are:
- Raw segments keep full ward-pair boundary coverage.
- Long connected boundary components are split into roughly 1320ft and 2640ft
  pieces.
- Short disconnected components are not automatically errors. Ward pairs can
  share multiple disconnected line components, including real short two-sided
  boundary pieces near complex ward topology.
- Length alone is not an exclusion rule.
- Legitimate disconnected short components remain standalone raw segments
  unless they fail segment-validity rules.
- Only true touching terminal remainders are merged into adjacent same-pair
  analysis segments.
- Topology noise and short pieces that fail two-sided ward-offset checks are
  excluded from analysis segment IDs.

The segment file includes `valid_segment`, `invalid_reason`,
`analysis_segment_id`, and `segment_lt500ft`/`segment_lt1000ft`. The rent and
sales tasks keep valid segments and use the length indicators only for the
straight-boundary checks.
