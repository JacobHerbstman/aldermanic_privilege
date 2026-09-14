# Residential Assessor history for construction

This task parses the pinned residential Assessor source once for construction
cleaning. It produces one row per source record, with parcel, building card,
assessment year, reported construction year, units, and areas. It keeps every
available assessment and construction year in Chicago township codes 70–77.
It does not select a preferred observation, infer construction completion,
aggregate projects, or assign ward boundaries.

The input is the full snapshot already preserved at
`data_raw/construction_review/residential_improvement_characteristics_full.csv`,
SHA-256 `10bbc6c6cf1d6b2c2a57c3df4147f53761bdc8112233b62d7ac34f6637722628`.
Its acquisition and original-vintage limitations are recorded in that source
folder. Ordinary builds reuse these bytes. A source refresh is a separate,
documented acquisition decision. This full-history input differs from the
2006–2022 snapshot used by the sales pipeline.

Run `make` from `code/`. The output is
`output/residential_assessor_history.parquet`, with a report in `report/`.
Numeric text and spelled apartment counts are normalized here. The reported
apartment text is retained alongside its parsed value. Source row order is
retained only to reproduce existing selection tie-breaking; `row_id` identifies
the source record. Neither missing measurements nor conflicting reports are
replaced with invented values. Consumers own and document their selection rules.

Reviewed parcel-reference typos are applied from
`adjudication/parcel_reference_corrections.csv` before consumers form building
relationships. Each correction names an exact source row and expected reported
value; a mismatch stops the build. `reported_proration_key_pin` preserves the
original reference. The September 8 Green Street correction joins the two
neighboring records with identical building fields and complementary shares;
it does not change their year, units, floor area or land area.
