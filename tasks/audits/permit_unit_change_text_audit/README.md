# Permit unit-change text audit

This audit compares the existing permit-text unit-change fields with a
precision-first candidate extraction. The candidate uses the current permit's
work description, not permit conditions that can quote older permits. It keeps
exact unit-count changes separate from direction-only evidence and does not
assign a one-unit magnitude from generic words such as `add`, `remove`, or
`coach house`.

The audit is descriptive. The stringency score and permit outcomes use
substantially overlapping permit data, so the model comparison is not an
external validation exercise.

The audit writes three outputs:

- `permit_unit_change_text_audit_summary.csv` compares source categories and
  unit totals.
- `permit_unit_change_text_review_sample.csv` contains reproducible random
  samples for manual review.
- `permit_unit_change_model_comparison.csv` compares IID and alderman-clustered
  inference for the original and candidate outcomes.

The candidate is not wired into production. The existing text fields also feed
an auxiliary unit-increase outcome in `create_event_study_permit_data`; the main
permit-count event study does not depend on that text classification.
