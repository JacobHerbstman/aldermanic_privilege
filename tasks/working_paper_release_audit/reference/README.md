# Recorded construction review baseline

These two CSVs preserve the existing July construction review's card-to-successor
assignments and final multicard adjudication. They were copied without changing
bytes on September 6, 2026 from the corresponding files in
`tasks/audits/commercial_new_construction_sample_audit/output/`. The original files
survived locally; they were not tracked at the recovered code commit. SHA256SUMS
records this audit baseline.

They are computed historical review products, not raw Assessor inputs or newly
recorded human judgments. `review_multicard_land_scope.R` compares their evidence
with the committed paper dataset to identify denominator questions. They are
used only by that audit. Production must regenerate these products through the
restored construction task; this baseline does not satisfy that requirement.
The restored producers reproduced both baseline CSVs byte-for-byte on their
preserved upstream inputs. A complete source-to-final rebuild is still pending.

The denominator-case review also uses an unchanged-row excerpt of the four
projects from `multicard_current_successor_links.csv` in that same historical
output directory. Its source SHA-256 is 20c31f47ee053a5108b3adaeb14efe5c116bfcce57f9eb6b1769bf2ee8083f98.
`denominator_cases.csv` identifies the four reviewed projects and the assessment
years supplying their retained building cards. The detailed review recomputes
parent areas and containment using preserved administrative source polygons;
the historical successor table remains supporting evidence, not a new source
acquisition or a land-allocation decision.

`multicard_project_query_geometries.gpkg` preserves the computed query geometries
from the archived commercial construction audit for a fixed-snapshot review of
all 248 retained multicard projects. It is not a raw parcel source or a substitute
for restoring that producer. The audit distinguishes actual construction-year
parcel polygons from 100-foot search buffers and missing geometry. Its current
parcel input is the pinned 2025 parcel universe. Point containment identifies
review leads, not accepted parcel allocation or project duplication.

The manual-dependence comparison uses preserved `multicard_external_review_queue.csv`
and `multicard_adjudicated_density_model_input.csv`, copied byte-for-byte from the
same historical audit output directory. Their SHA-256 hashes are respectively
`74f7aaa299611f5268fec8b284b470ea7684aa51dd3f4bf49f95753ad51cacf0` and
`cf6e86f9108aec267114ce168644e6f740b9a3c6f174441e801539735cd382ce`.
Both are derived baselines, not raw data. They isolate the external review's
incremental contribution; the input already incorporates earlier adjudications.

`multicard_manual_episode_decisions.csv` preserves the 42-row decision ledger
before general-rule replacements on September 7, 2026. Its SHA-256 is
`0926d888f264d26b6e270ac4119602f4555e15e32e68b0b7301173f873dff65e`.
The baseline manual-episode comparison reads this fixed reference; production
reads the reduced ledger in construction cleaning. Removed entries remain
available here as audit history, not as active cleaning inputs.
