# segment_validity_audit

Purpose: Audits raw ward-border segments and creates an explicit raw-to-analysis segment map.

Produces:
- `output/segment_validity_audit.csv`
- `output/segment_analysis_id_map.csv`
- `output/segment_support_audit.csv`
- `output/short_segment_summary.csv`
- `output/invalid_segment_observation_audit.csv`
- `output/segment_validity_summary.md`

Rule: Keep full raw boundary coverage, keep legitimate disconnected short components, merge only touching terminal remainders, and flag topology noise or wrong-ward offset failures. Length alone is not the production exclusion rule; estimation support is audited separately.

The production segment GPKG carries the same validity fields used here. Shared segment assignment code filters `valid_segment == FALSE`, while the audit reports support under length cutoffs including 500ft, 1000ft, and 1320ft.
