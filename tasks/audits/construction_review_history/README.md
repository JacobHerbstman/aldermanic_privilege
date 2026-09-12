# Construction review history

This directory preserves the research behind the [committed building corrections](../../new_construction_cleaning/README.md).
The paper builds from those corrections and the recorded Assessor sources.

- `records/` contains source evidence, superseded decisions, and the saved Natchez exhibit used in the logbook.
- `records/production_decisions_before_consolidation/` preserves all 25 decision tables used before the September 12 rewrite, byte for byte.
- `code/decision_consolidation/` records how their active instructions became the single correction CSV, including instructions the previous pipeline no longer applied.
- `source_acquisition/` preserves the optional searches that obtained the recorded inputs.
- `release_audit/` preserves the earlier case investigations, comparisons and recommendations.
- `report/` retains their data reports.

The [earlier research record](history.md) explains the decisions and source recovery.
Archived code and Makefiles retain their historical paths. They are not part of
the current build. The complete pre-rewrite code is available at Git commit
`bb91ea84`.

The current ordinary cleaning is in `tasks/prepare_new_construction/code/`.
The manual-decision task has one correction CSV and no R scripts. No automatically
selected building was converted into a manual exception by this consolidation.
